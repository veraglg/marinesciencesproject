str_range <- function(x) {
  if (all(is.na(x)))
    return(NA)
  
  min_val <- min(x, na.rm = T)
  max_val <- max(x, na.rm = T)
  
  sprintf("[%.2f, %.2f]\n", min_val, max_val)
}