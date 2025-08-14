#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Kernel Functions - Spawning Probability
#'
#' Probability of reproducing, once an individual reaches 'z_repro' they reproduce with probability 'p_spawn'
#'
#' @param z Size at the beginning of the timestep [float]
#' @param pars Data.frame containing the date-indexed parameters [data.frame]
#' @param date Ordinal day to reference proper 'pars' date-indexed parameters [integer]
#' @return Probability of reproducing [binary]
#' @export
#' @family Kernel Functions
Spawning<-function(z,pars,date)
{
  PB.out<-ifelse(z<pars$z_repro[date],0,pars$p_spawn[date])
  return(as.numeric(PB.out))
}

## Notes on Spawning Prob
## p_spawn parameter is generated using the batch spawning algorithm (see GenerateSpawningProbs.R)
## z_repro parameter was estimated based on lab observations of K.Flynn and S.Kadlec

