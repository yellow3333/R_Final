myfun <- function(){
  other_team <- c("B","C","D","E")
  A <- rep("A",100)
  B <- sample(other_team,100,replace=T)
  
  data.frame(A,B,)
  
}