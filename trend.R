OLS_change <- function(A.scores, B.scores)
{
  X2 <- c(A.scores, B.scores)
  dummy <- c(rep(0,length(A.scores)), rep(1,length(B.scores)))
  nsizeAB <- length(X2)
  time_regr_AB <- 0:(nsizeAB-1)
  time_A <- 0:(sum(dummy==0)-1)
  time_B <- sum(dummy==0):(nsizeAB-1)
  
  model1 <- lm(X2~time_regr_AB+dummy)
  model2 <- lm(X2~time_regr_AB+dummy+time_regr_AB*dummy)
  Rsquaremodel1 <- (summary(model1)$r.squared)
  Rsquaremodel2 <- (summary(model2)$r.squared)
  
  fsquaretrend_AB <- (Rsquaremodel2 - Rsquaremodel1) / (1 - Rsquaremodel2)
  return(fsquaretrend_AB)
}