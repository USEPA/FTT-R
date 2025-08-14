#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Kernel Functions - Survival
#'
#' Baseline Survival:
#' Logistic size-dependent daily survival probability for adults.
#'
#' Winter Survival:
#' Logistic size-dependent daily survival probability for adults with a minimum cutoff size.
#'
#' Exposure Survival:
#' A survival decrement can be specified to alter the daily survival probability for all individuals in the population.
#'
#' @param z Size at the beginning of the timestep [float]
#' @param pars Data.frame containing the date-indexed parameters[data.frame]
#' @param date Ordinal day to reference proper 'pars' date-indexed parameters [integer]
#' @return A probabliity of survival [float]
#' @export
#' @family Kernel Functions

Survival<-function(z,pars,date){
  s.base<-pars$s_min[date]+((pars$s_max[date]-pars$s_min[date])/(1+exp(pars$s_a[date]*(pars$s_b[date]-z))))
  if(exists("z_winter",where=pars)){
    s.wint<-ifelse(z < pars$z_winter[date],pars$s_winter[date]*s.base,s.base)}
  else(s.wint<-s.base)
  if(exists("survival_decrement",where=pars)){
   s.exp<-s.wint-pars$survival_decrement[date]}
  else(s.exp<-s.wint)
  return(as.numeric(s.exp))}



## AS OF 11/20/20 THESE VALUES ARE OUT OF DATE - SEE LIFE HISTORY PARAMETERS RDATA FILE FOR VALUES AND REFERENCES
## BELOW IS KEPT ONLY FOR REFERENCE  -N.Pollesch

## Used for creating associated parameter data file

# # This defines the parameter names and gives default values for the parameters that available in the data file
# Survival.parameters.p.promelas<-c(
#    s_min=0.5^(1/137),
#    s_max=0.970^(1/365),
#    s_b=.5,
#    s_a=35,
#    z_winter=16,
#    is_winter=0,
#    s_dec=0,
# )

# ##Creates an RData file to store the parameters
# devtools::use_data(Survival.parameters.p.promelas,overwrite=T)

## Notes on derivation of step function for survival
#od("15Sep2018")-od("1May2018") # YOY Growth time in Vandenbos 2006
#74-(74-5.6)*exp(-0.008*137) # Growth over 137 days
#74-(74-5.6)*exp(-0.008*30) # Growth over 30 days ~~20mm
#js<-.5^(1/137) # YOY Survival in Vandenbos 2006
#~20 # Length of YOY growth in Vandenbos 2006
## Fit a logistic function so that transition between YOY survival and Adult survival happens between ~20mm and ~50mm
