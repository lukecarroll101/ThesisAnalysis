
# score tests
create_scoring_key <- function(items, scales, reverse) {
  unique_scales <- unique(scales)
  key <- sapply(seq(unique_scales), 
                function(X) ifelse(scales == unique_scales[X], reverse, 0))
  key <- data.frame(key)
  names(key) <- unique_scales
  row.names(key) <- items
  key
}


score_test <- function(meta_data, case_data, subscale_name='subscale_name', id='id', reverse='reverse', ...) {
  require(psych)
  meta_data <- meta_data[!is.na(meta_data[,subscale_name]), ] # remove non-coded items
  scoring_key <- create_scoring_key(meta_data[[id]], 
                                    meta_data[[subscale_name]],  
                                    meta_data[[reverse]])
  scored <- scoreItems(as.matrix(scoring_key), case_data[,rownames(scoring_key)], 
                       missing = TRUE, impute = "none", ...)
  scored$key <- scoring_key
  scored$scores <- data.frame(scored$scores)
  scored$data <- case_data
  scored$data[,colnames(scored$scores)] <- scored$scores
  scored
}