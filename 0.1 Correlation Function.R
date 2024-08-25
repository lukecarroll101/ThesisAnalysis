library(dplyr)
#My cor matrix function
emmett_cor_matrix_function <- function(x, data, table = FALSE){
  include <- length(x)-(length(x)-1)
  variables <- c()
  for(j in 1:length(x)){
    cor_estimates_all <- c()
    signficance_all <- c()
    variable_y <- data[,x[j]]
    var_y_name <- x[j]
    
    start_variable <- include
    for(i in start_variable:length(x)){
      
      na_list <- c(rep(paste(""), (start_variable - 1)))
      variable_x <- data[,x[i]]
      print(paste(var_y_name, x[i]))
      cor <- cor.test(variable_x, variable_y, method = "pearson", use = "pair")
      p_value <- as.numeric(cor$p.value)
      
      
      estimate <- sprintf("%.2f", as.numeric(cor$estimate))
      estimate <- as.numeric(estimate)
      df <- as.numeric(cor[2])
      n <- as.numeric(df + 2)
      sample <- paste0("(n = ", n, ")")
      
      if(p_value <=0.05 & p_value > 0.01){
        significance <- "*"
      }else if(p_value <= 0.01){
        significance <- "**"
      }else{
        significance <- "  "
      }
      cor_estimate_for_table <- paste0(estimate, significance, " ",sample)
      p_value_for_table <- round(p_value, 3)
      cor_estimates_all <- append(cor_estimates_all, cor_estimate_for_table)
      signficance_all <- append(signficance_all, p_value_for_table)
    
    }
    cor_estimates_all <- append(na_list, cor_estimates_all) 
    include <- include +1
    variables <- x
    large_table_estimates <- cor_estimates_all
    single_table <- as.data.frame(tibble(variables, large_table_estimates))  |>
       rename(!!var_y_name := "large_table_estimates")

    table <- merge(table, single_table, sort = FALSE)
  }
  table <- table[, !colnames(table) %in% "x", drop = FALSE]
  return(table)
}
