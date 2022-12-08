# function to transform labled variables in factors
factorise <- function(var){
  
  lbl_full <- attributes(var)$labels
  lbl_full <- lbl_full[order(lbl_full)]
  valid_values <- names(table(var))
  
  lbl <- lbl_full[lbl_full %in% valid_values] %>% names()
  
  factor(var, labels = lbl)   
}

factorise2 <- function(var){
  
  lbl_full <- attributes(var)$labels
  lbl_full <- lbl_full[order(lbl_full)]
  valid_values <- names(table(var))
  
  lbl <- lbl_full[lbl_full %in% valid_values] %>% names()
  
  factor(var, labels = lbl)   
}
