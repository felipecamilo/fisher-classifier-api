
validate_input <- function(x){
  if(!all(colnames(data[,-1]) %in% colnames(x))){
    return(list(valid = FALSE, error = paste(
      "The following required columns are missing in the request: [",
      paste(colnames(data[,-1])[!(colnames(data[,-1]) %in% colnames(x))], collapse = ", "),
      "]", sep = ""
    )))
  }
  
  if(!all(colnames(x) %in% colnames(data[,-1]))){
    return(list(valid = FALSE, error = paste("[",
                                             paste(colnames(x)[!(colnames(x) %in% colnames(data[,-1]))], collapse = ", "),
                                             "] are not valid columns for prediction.", sep = ""
    )))
  }
  
  if(sum(colSums(is.na(x))) > 0){
    return(list(valid = FALSE, error = paste(
      "The following columns are missing values or have incomplete data: [",
      paste(colnames(x)[colSums(is.na(x)) > 0], collapse = ", "),
      "]", sep = ""
    )))
  }
  
  if(!all(sapply(data[,-1],is.numeric))){
    return(list(valid = FALSE, error = paste("[",
                                             paste(colnames(x)[!sapply(data[,-1],is.numeric)], collapse = ", "),
                                             "] must be numeric columns.", sep = ""
    )))
  }
  
  return(list(valid = TRUE))
}


