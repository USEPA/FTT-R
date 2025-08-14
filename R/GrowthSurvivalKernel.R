#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Kernel Component -  Semelparous Adult Growth and Survival
#'
#' Integral projection models (Ellner et al., 2016) have the general structure of:
#' \deqn{n(z1,t+1) = \integral{ (P(z1,z) + R(z1,z))*n(z,t) dz}}
#' Here we define 'P' as 'GrowthSurvivalKernel' to be explicit, where it is the product of adult growth transition CDFs, \code{\link{Growth}}, with the size-dependent probability of survival, \code{\link{Survival}}.
#' \deqn{IteroparousGrowthSurvivalKernel(z1,z,bt,pars,date) = Growth(z1,z,bt,pars,date)*Survival(z,pars,date)}
#'
#' @param z1 Size at the end of the timestep, the size being transitioned to [float]
#' @param z Size at the beginning of the timestep [float]
#' @param bt Population biomass at the beginning of the timestep [float]
#' @param pars Data.frame containing the date-indexed parameters[data.frame]
#' @param date Ordinal day to reference proper 'pars' date-indexed parameters [integer]
#' @return Size-dependent growth and survival kernel for adults
#' @export
#' @note This function is provided as a standard to pass to \code{\link{SimulateModel}} for the 'growthSurvivalComponent' argument
#' @note The 'SemelparousGrowthSurvivalKernel' is distinguished from the 'IteroparousSurvivalKernel' by in the inclusion of a (1-Spawning(z)) factor in the product for 'SemelparousGrowthSurvivalKernel' to account for post-spawning mortality.
#' @family Kernel Components


GrowthSurvivalKernel<-function(z1,z,bt,pars,date)
{
  return(Growth(z1,z,bt,pars,date)*Survival(z,pars,date)*(1-(pars$post_spawning_mortality[date]*Spawning(z,pars,date))))
}


#### NEED TO UPDATE AND COMPLILE THE TWO GROWTH SURVIVALKERNEL COMPONENTS INTO ONE WITH THE NEW POST_SPAWN_MORTALITY PARAMETER. ####
