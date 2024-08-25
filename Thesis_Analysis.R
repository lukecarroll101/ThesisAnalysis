library("readr"); library("dplyr"); library("psych")
library("gdata");library("lavaan");library("broom")
library("rlang"); library("ltm"); library("lm.beta")
library("readxl"); library("ggplot2"); source("score-tests.R")

wccases <- read_csv("ccases-export.csv")
mlist <- c("swl", "panas", "pwb", "hexaco")
for (i in mlist){
  meta_i <- readxl::read_xls("meta.xls", sheet = i)
  assign(paste("meta_", i, sep = ""), meta_i)
  remove(meta_i)
}
meta_pwb <- meta_pwb[!is.na(meta_pwb$ryff42itemnumber),]

v <- list()
v$demographic_items <- scan("test.txt", what='', sep=",")
v$swl_items <- meta_swl$id
v$panasItems_pa <- meta_panas$id[meta_panas$subscale == "pa"]
v$panasItems_na <- meta_panas$id[meta_panas$subscale == "na"]
v$pwb_items <- meta_pwb$id
v$wellbeing_items <- c(v$swl_items, v$panasItems_pa, v$panasItems_na, v$pwb_items)
v$intelligence <- c("intelligence","verbal" ,"abstract" ,"numeric") # intelligence data starts back as far as 2011.

scored <- list()
scored$swl <- score_test(meta_swl, wccases, subscale_name = "subscale")
scored$panas <- score_test(meta_panas, wccases, subscale_name = "subscale")
scored$pwb <- score_test(meta_pwb, wccases, subscale_name = "subscale") # Not all of the PWB items are in the wccases DF, so it gives an error.
scored$hexaco <- score_test(meta_hexaco, wccases, subscale_name = "factor", reverse = "reversed")
scored$hexaco$scores$other <- NULL

scores <- lapply(scored, function(X) X$scores)
names(scores) <- NULL
fccases <- data.frame(do.call(cbind, scores))
fccases[,v$intelligence] <- wccases[,v$intelligence]
fccases[,v$demographic_items] <- wccases[,v$demographic_items]
fccases$gender <- dplyr::recode(fccases$gender, "Male" = 1, "Female" = 0)
fccases$income_estimate <- as.numeric(as.character(factor(fccases$income, c("Negative or Zero Income", 
                                                                          "$1 ‐ $9,999 per year ($1 ‐ $189 per week)", 
                                                                          "$10,000 ‐ $19,999 per year ($190 ‐ $379 per week)",
                                                                          "$20,000 ‐ $29,999 per year ($380 ‐ $579 per week)", 
                                                                          "$30,000 ‐ $39,999 per year ($580 ‐ $769 per week)", 
                                                                          "$40,000 ‐ $49,999 per year ($770 ‐ $959 per week)", 
                                                                          "$50,000 ‐ $59,999 per year ($960 ‐ $1149 per week)", 
                                                                          "$60,000 ‐ $79,999 per year ($1150 ‐ $1529 per week)", 
                                                                          "$80,000 ‐ $99,999 per year ($1530 ‐ $1919 per week)", 
                                                                          "$100,000 ‐ $124,999 per year ($1920 ‐ $2399 per week)", 
                                                                          "$125,000 ‐ $149,999 per year ($2400 ‐ $2879 per week)", 
                                                                          "$150,000 ‐ $199,999 per year ($2880 ‐ $3839 per week)", 
                                                                          "$200,000 or more per year ($3840 or more per week)"),
                                                         c(0, 5000, 15000, 25000, 35000, 45000, 55000, 70000, 90000, 112500, 137500, 175000, 250000))))
fccases$education_numeric <- as.numeric(as.character(factor(fccases$education,
                                                           c("Did not complete High school",
                                                             "Completed High school",
                                                             "Apprenticeship, Diploma, TAFE certificate, etc", 
                                                             "Bachelor's degree",
                                                             "Master's degree",  
                                                             "Doctorate, PhD"),
                                                           c(1,2,3,4,5,6))))
missing_count <- apply(fccases[,c(v$intelligence, "swl", names(scored$panas$scores), names(scored$pwb$scores), names(scored$hexaco$scores))], 1, function(x) sum(is.na(x)))
table(missing_count)
as.matrix(colSums(!is.na(fccases)))

# Filter data frame to only include participants with intelligence data
fccases <- fccases[apply(fccases[, v$intelligence], 1, function(x) any(!is.na(x))), ]

# Demographic information
table(fccases$gender) # Male = 1, Female = 0
psych::describe(fccases)

desc <- list() # potentially include income and maybe education
cor_var <- c(names(fccases[1:9]),names(fccases[16:19]),names(fccases[10:15]),"gender","income_estimate","education_numeric")
desc$cor <- cor(fccases[,cor_var],method = "pearson", use = "pair")
desc$mean <- sapply(na.omit(fccases[,cor_var]), mean)
desc$sd <- sapply(na.omit(fccases[,cor_var]), sd)
desc$cron <- c(scored$swl$alpha, scored$panas$alpha,scored$pwb$alpha, numeric(4), scored$hexaco$alpha[1:6], numeric(3)) # This line needs to be ammeded to relfect current data
desc$tab <- round(data.frame(mean = desc$mean, sd = desc$sd, cronbachs_alpha = desc$cron, desc$cor),2)
desc$tab
write.csv(desc$tab, file = "description_table.csv")

table(complete.cases(fccases[,cor_var][1:19]))
# fccases <- fccases[complete.cases(fccases[,cor_var][1:19]),cor_var]
# as.matrix(colSums(!is.na(temp)))

#Compute Regression model
fits_lm <- list()
for (scale in c(cor_var[1:9])) {
  formula <- as.formula(paste(scale, "~ intelligence + honestyhumility + emotionality + extraversion + agreeableness + conscientiousness + openness + gender + income_estimate + education_numeric"))
  model <- lm(formula, data = fccases)
  fits_lm[[scale]] <- model}
summary_df <- data.frame(Predictors = c("Intelligence", "Honesty Humility", 
                                        "Emotionality", "Extraversion", "Agreeableness", 
                                        "Conscientiousness", "Openness","Gender", "Income", "Education", "Adjusted R^2"))
for (scalee in c(cor_var[1:9])) {
  model <- fits_lm[[scalee]]
  summary <- summary(lm.beta::lm.beta(model))
  summary_df[[scalee]] <- round(c(summary$coefficients[,2][2:11],summary$adj.r.squared),3)
  print(scalee)
  print(summary)}
summary_df

fits_lm_abs <- list()
for (scale_abs in c(cor_var[1:9])) {
  formula <- as.formula(paste(scale_abs, "~ abstract + honestyhumility + emotionality + extraversion + agreeableness + conscientiousness + openness + gender + income_estimate + education_numeric"))
  model <- lm(formula, data = fccases)
  fits_lm_abs[[scale_abs]] <- model}
summary_df_abs <- data.frame(Predictors = c("Abstract Reasoning", "Honesty Humility", 
                                            "Emotionality", "Extraversion", "Agreeableness", 
                                            "Conscientiousness", "Openness","Gender", "Income", "Education", "Adjusted R^2"))
for (scalee_abs in c(cor_var[1:9])) {
  model <- fits_lm_abs[[scalee_abs ]]
  summary <- summary(lm.beta::lm.beta(model))
  summary_df_abs[[scalee_abs]] <- round(c(summary$coefficients[,2][2:11],summary$adj.r.squared),3)
  print(scalee_abs)
  print(summary)}
summary_df_abs
write.csv(summary_df, file = "regression_table.csv")
write.csv(summary_df_abs, file = "regression_abs_table.csv")
# write_xlsx(list(summary_df = summary_df, summary_df_abs = summary_df_abs), path = "regression_tables.xlsx")

##########################################################################################################################


fits_lm_test <- list()
for (scale_test in c(cor_var[5:13])) {
  formula <- as.formula(paste(scale_test, "~ verbal + abstract + numeric"))
  model <- lm(formula, data = fccases)
  fits_lm_test[[scale_test]] <- model}

summary_df_test <- data.frame(Predictors = c("Verbal Reasoning", 
                                            "Abstract Reasoning", "Numeric Reasoning", "Adjusted R^2"))
for (scalee_test in c(cor_var[5:13])) {
  model <- fits_lm_test[[scalee_test]]
  summary <- summary(lm.beta::lm.beta(model))
  summary_df_test[[scalee_test]] <- round(c(summary$coefficients[,2][2:4],summary$adj.r.squared),3)
  print(scalee_test)
  print(summary)}
summary_df_test

x <- summary(lm.beta::lm.beta(fits_lm[["swl"]]))
x
x$coefficients

cor_var
cor(fccases$swl, fccases$autonomy, method = "pearson", use = "pair")
cor.test(fccases$swl, fccases$autonomy, method = "pearson", use = "pair")

play <- c("intelligence","verbal","abstract","numeric","swl","pa","na","autonomy","emastery",
          "pgrowth","prelwo","plife","selfaccept")
source("0.1 Correlation Function.R")
sig_cor <- emmett_cor_matrix_function(cor_var, fccases[,cor_var], table = FALSE)
write.csv(sig_cor, file = "sig_cor_table.csv")
sig_cor2 <- emmett_cor_matrix_function(play, fccases[,cor_var], table = FALSE)
write.csv(sig_cor2, file = "sig_cor_table2.csv")

library(Hmisc)

desc1 <- list()
# I don't trust the rcorr correlations, check against cor.test
desc1$corP <- rcorr(as.matrix(fccases[,cor_var]), type = "pearson")$P
desc1$cor <- cor(na.omit(fccases[,cor_var]), use = "pair")

# Calculate correlation matrix and p-values
cor_matrix <- cor(na.omit(fccases[, cor_var]), use = "pair")
p_values <- rcorr(as.matrix(fccases[, cor_var]))$P

# Function to apply significance asterisks
apply_significance <- function(correlation, p_value) {
  if (is.na(p_value)) {
    return("")
  } else if (p_value >= 0.05) {
    return("")
  } else if (p_value < 0.05 && p_value >= 0.01) {
    return("*")
  } else {
    return("**")
  }
}

# Apply significance asterisks to correlation matrix
cor_with_significance <- cor_matrix
for (i in 1:nrow(cor_matrix)) {
  for (j in 1:ncol(cor_matrix)) {
    cor_with_significance[i, j] <- paste0(round(cor_matrix[i, j],2), apply_significance(cor_matrix[i, j], p_values[i, j]))
  }
}

# Assign the correlated matrix with significance asterisks to desc$cor
desc1$cor <- cor_with_significance

summary(fccases[,cor_var])

summary(cor.test(na.omit(fccases$swl),na.omit(fccases$na), method = "pearson"))

e <- sprintf("%.2f", as.numeric(cor.test(na.omit(fccases$swl),na.omit(fccases$na), method = "pearson")$estimate))
as.numeric(e)
?cor()

cor(fccases$swl,fccases$pa, use = "pair")


summary


test <- fccases %>%
  filter(across(all_of(cor_var), ~ !is.na(.)))

fccases %>%
  filter(across("age_estimate", ~ !is.na(.)))

?dplyr::across()

as.matrix(colSums(is.na(test)))

fccases %>%
  dplyr::if_all()


k <- summary(lm.beta(fits_lm[["pa"]]))


y <- summary(fits_lm[["pa"]])
y1 <- summary(fits_lm[["emastery"]])
y2 <- summary(fits_lm[["prelwo_Int"]])
y3 <- summary(fits_lm[["prelwo_Abs"]])

summary_df <- data.frame(Predicted = c("pa","emastery","prelwo"))
summary_df[["abstract"]] <- round(c(y$coefficients[,"Pr(>|t|)"][2], y1$coefficients[,"Pr(>|t|)"][2], y3$coefficients[,"Pr(>|t|)"][2]),4)
summary_df[["intelligence"]] <- round(c(numeric(2), y2$coefficients[,"Pr(>|t|)"][2]),4)
summary_df 
write.csv(summary_df, file = "summary_df.csv")


# Calculate correlation coefficients and p-values
cor_matrix <- cor(na.omit(fccases[, cor_var]))
values <- corr.test(na.omit(fccases[, cor_var]))
p_values <- corr.test(na.omit(fccases[, cor_var]))$p
p_values <- round(p_values,4)

paired_dep_r_test <- function(desc, var1, var2, var3) {
  cor1 <- desc$cor[var1,var3]
  cor2 <- desc$cor[var2,var3]
  cor3 <- desc$cor[var1, var2]
  n <- length(fccases[,var1])
  print(cor1)
  
  result <- paired.r(cor1,cor2,cor3, n)
  return(result)
}

paired_dep_r_test(desc, "verbal", "numeric","swl")


for (scale in c("emotional_intelligence", v$EI_scales)) {
  formula <- as.formula(paste(scale, "~ neuroticism + extraversion + openness + agreeableness + conscientiousness + age_years + sex_category"))
  model <- lm(formula, data = at2ccases)
  fits_lm[[scale]] <- model}

summary_df <- data.frame(Predictors = c("neuroticism", "extraversion", "openness", "agreeableness",
                                        "conscientiousness", "Age", "Sex", "Adjusted R^2"))
for (scalee in c("emotional_intelligence", names(fits_lm))) {
  model <- fits_lm[[scalee]]
  summary <- summary(lm.beta::lm.beta(model))
  summary_df[[scalee]] <- round(c(summary$coefficients[,2][2:8],summary$adj.r.squared),3)}
summary_df
write.csv(summary_df, file = "output/regression_table.csv")



plot(fccases$pwb.emastery,fccases$abstract)
ggplot(fccases, aes(x = emastery, y = abstract)) +
  geom_point() +
  geom_smooth(method = lm)

 ?ggplot()


fccases <- data_frame(1545)
fccases$swl <- scored$swl$scores
# fccases$pwb <- scored$pwb$scores
fccases$panas <- scored$panas$scores
fccases$hexaco <- scored$hexaco$scores

colnames(wccases)
table(wccases$problematic_tooquick)



fccases <- tccases$scores
fccases$swl_Jer <- wccases$swl

colnames(wccases)

table(wccases$has_ability)
table(wccases$has_hexaco)
table(wccases$age_estimate)

hist(wccases$age_estimate)
sheetNames("meta.xls")
colnames(wccases)

?read.xls

v$Hintelligence <- c("intelligence","verbal" ,"abstract" ,"numeric")
v$HSWB <- c("swl","pa","na")
v$HPWB <- c("autonomy","emastery","pgrowth","prelwo","plife","selfaccept")
v$HPersonality <- c("honestyhumility","emotionality","extraversion","agreeableness","conscientiousness","openness")


"has_ability","problematic_tooquick"


#Compute descriptives between Scales


desc <- list()
cor_var <- c("intelligence","verbal" ,"abstract" ,"numeric",
             "honestyhumility","emotionality","extraversion","agreeableness","conscientiousness","openness",
             "swl","pa","na", "autonomy","emastery","pgrowth","prelwo","plife","selfaccept" )
desc$cor <- cor(na.omit(wccases[,cor_var]))
desc$mean <- sapply(na.omit(wccases[,cor_var]), mean)
desc$sd <- sapply(na.omit(wccases[,cor_var]), sd)
desc$cron <- c(numeric(2), cronbach.alpha(wccases[,v$EI_scales])[["alpha"]], scored$alpha
               ,scored2$alpha)
desc$tab <- round(data.frame(mean = desc$mean, sd = desc$sd, 
                             cronbachs_alpha = desc$cron, desc$cor),2)
desc$tab <- round(data.frame(desc$cor),2)
desc$tab


write.csv(desc$tab, file = "description_table.csv")
