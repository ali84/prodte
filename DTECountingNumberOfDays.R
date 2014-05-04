#Ali Reza Asadi 04/16/2014
#counting number of days in the dataset


countday <- function(x, npar=T, print=T){
  wd <- x$weekday[1]
  nd <- 0
  for (i in 1:nrow(x)){
    if (x$weekday[i]!= wd){
      nd <- nd+1
      wd <- x$weekday[i]
    }
  }
  result <- list(nd)
  return(result)
}







