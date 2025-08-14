#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#
#
#' Time and Date Functions
#'
#' These a group of functions that support other functions within the model
#' @param sec TRUE/FALSE to include seconds in time output
#' @describeIn TimeExt Modified system time function formatted for use in naming files
#' @export
TimeExt<-function(sec=F){
  h<-substring(Sys.time(),12,13)
  m<-substring(Sys.time(),15,16)
  s<-substring(Sys.time(),18,19)
  if(sec){
    return(paste(h,m,s,sep="_"))}
  else{
    return(paste(h,m,sep="_"))
  }
}
#' @describeIn TimeExt Ordinal day function turns a date string into an integer
#' @param date Date in format 'DateMonthYear', ex. '01Jan20'
#' @export
OrdinalDate<-function(date){
  tmp <- as.POSIXlt(date, format = "%d%b%y")
  return((tmp$yday)+1)
}

#' @describeIn TimeExt Ordinal day duration function, takes to ordinal dates and provides all ordinal dates in between them as a vector
#' @param startDate Beginning ordinal date
#' @param endDate Ending ordinal date
#' @export
DateDuration<-function(startDate,endDate){
  if(startDate>endDate)
  {days<-c(1:endDate,startDate:365)}
  else
  {days<-startDate:endDate}
  return(as.vector(days))
}

#' @describeIn TimeExt This function is used to create a vector of TRUE/FALSE values where each day in the range specified is TRUE and those not within the duration are FALSE - used for winter dates here
#' @param winterStartDate Starting date for the duration [Ordinal date integer]
#' @param winterEndDate Ending date for the duration [Ordindal date interger]
#' @return A vector of length 365 where dates within 'startDate' and 'endDate' are TRUE all others are FALSE
#' @export
SetWinterDates<-function(winterStartDate,winterEndDate){
  wds<-rep(F,365)
  trueDates<-DateDuration(winterStartDate,winterEndDate)
  wds[trueDates]<-T
  return(wds)

}

#' @describeIn TimeExt This function takes a vector of growth percents from exposure and creates a boolean for each day that a growth percent exists.  Returns vector same length as input vector.
#' @param growth_percents The daily Kappa growth percent decreases from baseline
#' @export
#' @return A vector of length length(growth_percents) with TRUE for each day where growth percent differs from 1
SetExposureDates<-function(growth_percents){
  #build a vector of FALSE for each day in length of growth percents
  growth_effect_TF<-rep(FALSE,length(growth_percents))
  for(i in 1:length(growth_percents)){
    if(growth_percents[i]<1){growth_effect_TF[i]<-TRUE}}

    return(growth_effect_TF)
}
