#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Threshold Concentration Effect Model
#'
#' This simplified effects model uses an lethal concentration for a given percent of the population to determine threshold type effects.
#' Specifically, a concentration 'lc_conc' and an associated percent 'lc_percent' combined with a time series of daily exposures and
#' when the 'exposure_concentrations' exceed the 'lc_conc' value, there is an 'lc_percent' decrease in survival that day.
#' @param exposure_concentrations time series (one concentration for each day, 365 days in length) of chemical exposures that are used to test if lc_conc is above or below [vector]
#' @param lc_conc Lethal concentration for 'lc_percent' [float]
#' @param lc_percent Percentage lethality in population for 'lc_conc' [float]
#' @return Time series of survival decrements for the associated 'exposure_concentrations' time series. [vector]
#' @export
#'
TCEM<-function(exposure_concentrations,lc_conc,lc_percent){
exceed<-rep(FALSE,length(exposure_concentrations))
exceed[which(exposure_concentrations > lc_conc)]<-TRUE
return(lc_percent*exceed)
}
