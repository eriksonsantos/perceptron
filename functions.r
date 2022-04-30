yperceptron <- function(x,w){
  
  x <- cbind(-1,x)
  u <- x%*%w
  
  y<- 1.0*(u>=0)
  return(as.matrix(y))
}


