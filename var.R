var_ratio <- function(A.scores,B.scores)
{
  A.var <- var(A.scores)
  B.var <- var(B.scores)
  obs.value <- ifelse(A.var > B.var, A.var/B.var, B.var/A.var)
  return(obs.value)
}