predictors <- c("abstract","honestyhumility","emotionality","extraversion","agreeableness","conscientiousness","openness","gender","income_estimate","education_numeric")
dependents <-c("swl","pa","na","autonomy","emastery","pgrowth","prelwo","plife","selfaccept")
data = fccases


regression_predictor_list <- paste(predictors, collapse = " + ")
table_list <- c(predictors, "Adj R2")
table <- as.data.frame(table_list)
significance_star <- c()

for (j in 1:length(dependents)) {
  current_dependent <- dependents[j]
  formula <- as.formula(paste(current_dependent, "~", regression_predictor_list))
  regression_model <- lm(formula, data = data)
  regression_model[[current_dependent]] <- regression_model
  model_summary <- summary(lm.beta::lm.beta(regression_model))
  adjusted_r_squared <- round(model_summary$adj.r.squared[1],2)
  
  regression_coefficient_list <- model_summary$coefficients[,2]
  model_significance_list <- model_summary$coefficients[,5]
  single_coef_with_star_list <- c()
  single_coef_list <- c()
  model_significance <- as.numeric(model_significance_list[1])
  if(model_significance <= 0.05 & model_significance > 0.01){
    significance_star_adjR <- "*"
  }else if(model_significance < 0.01){
    significance_star_adjR <- "**"
  }else{
    significance_star_adjR <- ""
  }
  
  r2_with_star <- paste(adjusted_r_squared, significance_star_adjR)
  
  for(i in 2:length(regression_coefficient_list)){
    single_coef <- as.numeric(round(regression_coefficient_list[i],3))
    single_coef_list <- append(single_coef_list, single_coef)
    
    single_significance <- model_significance_list[i]
    if(single_significance <= 0.05 & single_significance > 0.01){
      significance_star <- "*"
    }else if(single_significance < 0.01){
      significance_star <- "**"
    }else{
      significance_star <- ""
    }
    
    single_coef_with_star <- paste0(single_coef, significance_star)
    single_coef_with_star_list <- append(single_coef_with_star_list, single_coef_with_star)
    
  }
  
  single_coef_with_star_list <- append(single_coef_with_star_list, r2_with_star)
  single_coef_with_star_list_tibble <- tibble(single_coef_with_star_list)
  
  
  
  single_coef_tibble <- tibble(single_coef_list)
  table <- tibble(table, single_coef_with_star_list_tibble)
  
  row_name <- paste(dependents[j])
  table <- table %>% rename_with(~paste(row_name), single_coef_with_star_list)
  
}

table <- as.data.frame(lapply(table, function(x) gsub("^0+", "", x))) %>% 
  mutate_all(~ ifelse(. == "", 0, gsub("^0+", "", .)))
table
#write.csv(table, "tabletest.csv")
