#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Kernel Functions - Fecundity
#'
#' If an individual reaches 'z_repro' they reproduce with probability provided by the \code{\link{Spawning}} function, and have 'hatch_per_spawn' hatchlings
#' @param z Size at the beginning of the timestep [float]
#' @param pars Data.frame containing the date-indexed parameters[data.frame]
#' @param date Ordinal day to reference proper 'pars' date-indexed parameters [integer]
#' @return Number of hatchlings per spawn[binary]
#' @export
#' @family Kernel Functions
Fecundity<-function(z,pars,date)
{  fecundity.out<-ifelse(z<pars$z_repro[date],0,((z^(pars$fecu_slope[date]))*pars$fecu_intercept[date])*pars$hatch_rate[date])
  return(as.numeric(fecundity.out))
}

## Notes on hatch per spawn
## 4144/11 # eggs per spawn via Markus 1934
## .435 # mean hatching from Divino and Tonn, 2007
## Notes on fecundity power function
## Form and values fit by S. Raimondo based on K.Flynn eggs/day vs weight data
## (See FHM Life History Parameter Derivation R script for details)
