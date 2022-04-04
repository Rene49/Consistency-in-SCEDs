psmd <- function(A.scores, B.scores, effect.dir = "+")
{
  obs.value = NA
  
  if(effect.dir == "+")
  {
    obs.value = (mean(B.scores)-mean(A.scores))/(sqrt((((length(A.scores)-1)*var(A.scores))+((length(B.scores)-1)*var(B.scores)))/(length(A.scores)+length(B.scores)-2)))
  }
  if(effect.dir == "-")
  {
    obs.value = (mean(A.scores)-mean(B.scores))/(sqrt((((length(A.scores)-1)*var(A.scores))+((length(B.scores)-1)*var(B.scores)))/(length(A.scores)+length(B.scores)-2)))
  }
  
  return(obs.value)
}