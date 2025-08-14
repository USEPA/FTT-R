#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Kernel Functions - Hatchling Growth
#'
#' Probabilistic Growth Transition Cumulative Distribution Function.
#' Uses vonBertalanffy to determine mean growth and a theoretical derivation
#' of the associated standard deviation.
#'
#' @param z1 Size at the end of the timestep [float]
#' @param bt Population biomass at time t [float]
#' @param pars Data.frame containing the date-indexed parameters[data.frame]
#' @param date Ordinal day to reference proper 'pars' date-indexed parameters [integer]
#' @return Growth CDF function for new hatchlings
#' @export
#' @family Kernel Functions

HatchlingGrowth<-function(z1,bt,pars,date)
{
  if(pars$is_density_dependent[date]){
  zInfD<-pars$z_inf[date]*exp(-pars$dd_g[date]*bt)+z1*(1-exp(-pars$dd_g[date]*bt))}
    else(zInfD<-pars$z_inf[date])
  mu<-zInfD-(zInfD-pars$z_hatch[date])*exp(-pars$k_g[date])
  sig<-mu*pars$var_e_g[date]
  p.den.grow<-pnorm(z1,mean=mu,sd=sqrt(sig))
  return(p.den.grow)
}


# ## Used for creating associated parameter data file
#
# HatchlingGrowth.parameters.p.promelas<-c(
#   z_inf=74,
#   dd_g=1,
#   b_inf=100,
#   z_hatch=5.6,
#   var_k_g=.009,
#   var_e_g=0.96
# )
#
# ##Creates an RData file to store the parameters
# devtools::use_data(HatchlingGrowth.parameters.p.promelas,overwrite=T)

