test <- fccases[,c("wellbeing_year",v$intelligence_date)]
typeof(test$verbal_date)
as.Date(test$verbal_date)

test$date_column <- as.Date(test$verbal_date)
test$year <- format(test$date_column, "%Y")
table(test$year)
table(test$wellbeing_year)
test$diff <- as.numeric(test$wellbeing_year) - as.numeric(test$year)
table(test$diff)

test$prelwo <- fccases$prelwo

cor(test$diff, test$prelwo, method = "pearson" ,use = "pair")




wccases$verbal_date

# Define the variables for the correlation matrix
top_var <- c("swl", "pa", "na", "autonomy", "emastery", "pgrowth", "prelwo", "plife", "selfaccept")
side_var <- c("intelligence", "verbal", "abstract", "numeric", "honestyhumility", "emotionality", 
              "extraversion", "agreeableness", "conscientiousness", "openness", "gender", 
              "income_estimate", "education_numeric")

# Combine the variables into one list
cor_var <- unique(c(side_var, top_var))
desc <- list()
desc$cor <- cor(fccases[,cor_var], method = "pearson", use = "pair")
desc$tab <- round(data.frame(desc$cor), 2)
desc$tab <- desc$tab[c(side_var, top_var), top_var]
print(desc$tab[1:13,])


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
