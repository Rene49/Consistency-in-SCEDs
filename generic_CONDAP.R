sampled_matrix <- function(long, short)
{
  number <- length(long) - length(short) + 1
  long.matrix <- matrix(nrow=number, ncol=length(short))
  
  for(i in 1:number)
  {
    good.index <- seq(i, i + length(short) - 1)
    long.matrix[i,] <- long[good.index]
  }
  
  return(long.matrix)
}


OV_MCBD <- function(AB1, AB2)
{
  if(length(AB1) > length(AB2))
  {
    number <- length(AB1) - length(AB2) + 1
    AB1.matrix <- sampled_matrix(AB1, AB2)
    AB2.matrix <- t(matrix(rep(AB2, number), nrow=length(AB2), ncol=number))
  } else
  {
    number <- length(AB2) - length(AB1) + 1
    AB1.matrix <- t(matrix(rep(AB1, number), nrow=length(AB1), ncol=number))
    AB2.matrix <- sampled_matrix(AB2, AB1)
  }
  
  dists <- numeric()
  for(i in 1:number)
  {
    dists[i] <- (dist((rbind(AB1.matrix[i,], AB2.matrix[i,])), method="manhattan")) / min(length(AB1),length(AB2))
  }
  
  avg.dist <- mean(dists)
  return(avg.dist)
}

CONDAP <- function(scores1, scores2)
{
  obs.value = OV_MCBD(scores1,scores2) / sqrt(((length(scores1)-1)*(sd(scores1)^2) + (length(scores2)-1)*(sd(scores2)^2)) / (length(scores1)+ length(scores2)-2))
  return(obs.value)
}