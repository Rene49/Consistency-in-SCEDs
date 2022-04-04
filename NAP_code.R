NAP_calculation <- function(scores.a, scores.b, effect_dir="+")
{
  no_overlap= 0
  comparisons= length(scores.a)*length(scores.b)
  for (j in 1: length(scores.a))
  {
    for (k in 1:length(scores.b))
    {
      if (scores.a[j] < scores.b[k])
      {
        no_overlap = no_overlap + 1
      }
      
      if (scores.a[j] == scores.b[k])
      {
        no_overlap = no_overlap + 0.5
      }
    }
  }
  
  NAP.observed <- no_overlap/comparisons
  
  if (effect_dir == "-")
  {
    NAP.observed <- 1 - NAP.observed
  }
  
  return(NAP.observed)
}