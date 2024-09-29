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
v$intelligence <- c("intelligence","verbal" ,"abstract" ,"numeric")
v$intelligence_date <- c("verbal_date" ,"abstract_date" ,"numeric_date")

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
fccases[,v$intelligence_date] <- wccases[,v$intelligence_date]
fccases[,v$demographic_items] <- wccases[,v$demographic_items]
fccases$wellbeing_year <- format(as.Date(as.POSIXct(wccases$startdate, format="%d/%m/%Y %H:%M")), "%Y")
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

# Filter data frame to only include participants with at least on intelligence data point
fccases <- fccases[apply(fccases[, v$intelligence], 1, function(x) any(!is.na(x))), ]
as.matrix(colSums(!is.na(fccases)))


# Demographic information
table(fccases$gender) # Male = 1, Female = 0
table(fccases$age_estimate)
psych::describe(fccases)


## Checking for Normality in the data
for (i in c(names(fccases[1:9]),names(fccases[16:19]),names(fccases[10:15]))){
  print(i)
  print(shapiro.test(fccases[[i]]))
  hist(fccases[[i]],
       main = i)
  qqnorm(fccases[[i]])
}

## Create matrix with all distribution histograms for appendix
par(mfrow = c(3, 6)) 
for (i in c(names(fccases[1:9]), names(fccases[16:19]), names(fccases[10:15]))) {
  # Plot the histogram
  hist(fccases[[i]], 
       main = paste("Histogram of", i), 
       xlab = i)
}
par(mfrow = c(1, 1))

# Statistical Analysis----------------------------------------------------------------------------------------------------

# Computing correlations and creating tables/matrices
desc <- list() 
cor_var <- c(names(fccases[1:9]),names(fccases[16:19]),names(fccases[10:15]),"gender","income_estimate","education_numeric")
desc$cor <- cor(fccases[,cor_var],method = "pearson", use = "pair")
desc$mean <- sapply(na.omit(fccases[,cor_var]), mean)
desc$sd <- sapply(na.omit(fccases[,cor_var]), sd)
desc$cron <- c(scored$swl$alpha, scored$panas$alpha,scored$pwb$alpha, numeric(4), scored$hexaco$alpha[1:6], numeric(3)) # This line needs to be ammeded to relfect current data
desc$tab <- round(data.frame(mean = desc$mean, sd = desc$sd, cronbachs_alpha = desc$cron, desc$cor),2)
desc$tab
write.csv(desc$tab, file = "description_table.csv")

## Creating a condense version of the correlation table to inlcude in the write up
top_var <- c("swl", "pa", "na", "autonomy", "emastery", "pgrowth", "prelwo", "plife", "selfaccept")
side_var <- c("intelligence", "verbal", "abstract", "numeric", "honestyhumility", "emotionality", 
              "extraversion", "agreeableness", "conscientiousness", "openness", "gender", 
              "income_estimate", "education_numeric")
desc_matrix <- list()
desc_matrix$cor <- cor(fccases[,cor_var], method = "pearson", use = "pair")
desc_matrix$tab <- round(data.frame(desc_matrix$cor), 2)
desc_matrix$tab <- desc_matrix$tab[c(side_var, top_var), top_var]
print(desc_matrix$tab[1:13,])
write.csv(desc_matrix$tab[1:13,], file = "description_martix.csv")


# Compute Regression model
## Intelligence without Personality
fits_lm_IntOnly <- list()
for (scale_ALL in c(cor_var[1:9])) {
  formula <- as.formula(paste(scale_ALL, "~ intelligence + gender + income_estimate + education_numeric"))
  model <- lm(formula, data = fccases)
  fits_lm_IntOnly [[scale_ALL]] <- model}

summary_df_IntOnly <- data.frame(Predictors = c("Intelligence", "Gender", "Income", "Education", "Adjusted R^2"))
for (scalee_ALL in c(cor_var[1:9])) {
  model <- fits_lm_IntOnly [[scalee_ALL]]
  summary <- summary(lm.beta::lm.beta(model))
  summary_df_IntOnly[[scalee_ALL]] <- round(c(summary$coefficients[,2][2:5],summary$adj.r.squared),3)
  print(scalee_ALL)
  print(summary)}
summary_df_IntOnly

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

fits_lm_ALL <- list()
for (scale_ALL in c(cor_var[1:9])) {
  formula <- as.formula(paste(scale_ALL, "~ numeric + verbal +  abstract + honestyhumility + emotionality + extraversion + agreeableness + conscientiousness + openness + gender + income_estimate + education_numeric"))
  model <- lm(formula, data = fccases)
  fits_lm_ALL[[scale_ALL]] <- model}
summary_df_ALL <- data.frame(Predictors = c("Numeric", "Verbal", "Abstract Reasoning", "Honesty Humility", 
                                            "Emotionality", "Extraversion", "Agreeableness", 
                                            "Conscientiousness", "Openness","Gender", "Income", "Education", "Adjusted R^2"))
for (scalee_ALL in c(cor_var[1:9])) {
  model <- fits_lm_ALL[[scalee_ALL]]
  summary <- summary(lm.beta::lm.beta(model))
  summary_df_ALL[[scalee_ALL]] <- round(c(summary$coefficients[,2][2:13],summary$adj.r.squared),3)
  print(scalee_ALL)
  print(summary)}
summary_df_ALL

## Checking for Normality in the residuals to test if this is an appropriate analysis
for (j in names(fccases[1:9])){
  print(j)
  hist(resid(fits_lm_ALL[[j]]),
       main = j)
}

write.csv(summary_df_IntOnly, file = "regression_table_IntOnly.csv")
write.csv(summary_df, file = "regression_table.csv")
write.csv(summary_df_abs, file = "regression_abs_table.csv")
write.csv(summary_df_ALL, file = "regression_ALL_table.csv", row.names = FALSE)
# write_xlsx(list(summary_df = summary_df, summary_df_abs = summary_df_abs), path = "regression_tables.xlsx")
