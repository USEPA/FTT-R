#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Kernel Component - Reproduction Kernel
#'
#' Integral projection models (Ellner et al., 2016) have the general structure of:
#' \deqn{n(z1,t+1) = \integral{ (P(z1,z) + R(z1,z))*n(z,t) dz}}
#' Here we define 'R' as 'ReproductionKernel' to b explicit, where it is the product size-dependent reproduction probability, \code{\link{Spawning}}, Size-dependent hatchlings per spawn, \code{\link{Fecundity}}, hatchling growth-transition CDF, \code{\link{HatchlingGrowth}}, and size-dependent survival, \code{\link{Survival}}.
#' \deqn{ReproductionKernel(z1,z,bt,pars,date) = sex_ratio * Spawning(z,pars,date) * Fecundity(z,pars,date)*HatchlingGrowth(z1,bt,pars,date)*Survival(z_hatch,pars,date)}
#'
#'
#' @param z1 Size at the end of the timestep, the size being transitioned to [float]
#' @param z Size at the beginning of the timestep [float]
#' @param bt Population biomass at the beginning of the timestep [float]
#' @param pars Data.frame containing the date-indexed parameters[data.frame]
#' @param date Ordinal day to reference proper 'pars' date-indexed parameters [integer]
#' @return Size-dependent reproduction kernel
#' @export
#' @note This function is provided as a standard to pass to \code{\link{SimulateModel}} for the 'reproductionComponent' argument
#' @family Kernel Components


ReproductionKernel<-function(z1,z,bt,pars,date)
{
  return(pars$sex_ratio[date]*Spawning(z,pars,date)*Fecundity(z,pars,date)*HatchlingGrowth(z1,bt,pars,date)*Survival(pars$z_hatch[date],pars,date))
}

