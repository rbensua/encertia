load("example_data.RData")
source("compute_DEA.R")
options (warn = -1)

check_data <- function(df){
  
  # Number of columns
  ncols <- ncol(df)
  pass <- 1
  if (ncols < 5 | ncols > 6){
    pass <- 0
  }else{
    # Check types
    if (!all(apply(df[,-1], MARGIN = 2, function(x) is.numeric(x)))){
      pass <- -1
    }else{
      if(!is.character(df[,1])){
        pass <- -2
      }
    }
  }
  return(pass)
}
