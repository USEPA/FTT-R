#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

##########################################################################
### Functions to compute nodes and weights for Gauss-Legendre quadrature
### on one interval, or on a set of subintervals within an interval
##########################################################################

#' Computation Functions - Ellner et al., 2016 with some modifications
#'
#' This set of functions are used in the numerical discretization routines for \code{\link{SimulateModel}}
#'
#'
#' @describeIn gaussQuadInt Gauss-Legendre quadrature on interval (L,U)
#' @param L Lower bound for quadrature [float]
#' @param U Upper bound for quadrature [float]
#' @param order Number of quadrature points [integer]
#' @return a quadrature set of weights and nodes for computation
#' @export
# Gauss-Legendre quadrature on interval (L,U)
gaussQuadInt <- function(L,U,order=7) {
  # nodes and weights on [-1,1]
  out <- gauss.quad(order); #GL is the default
  w <- out$weights; x <- out$nodes;
  weights=0.5*(U-L)*w;
  nodes=0.5*(U+L) + 0.5*(U-L)*x;
  return(list(weights=weights,nodes=nodes));
}

### For comparison, midpoint rule
#' @describeIn gaussQuadInt Midpoint quadrature rule
#' @param m Number of points in interval from L to U
#' @export
midpointInt <- function(L,U,m) {
  h=(U-L)/m; nodes=L+(1:m)*h-h/2;
  weights=rep(h,m);
  return(list(weights=weights,nodes=nodes));
}

#' @describeIn gaussQuadInt Gaussian-Legendre quadrature on subintervals of (L,U).
#' @param intervals User can specify the number of subintervals
#' @param breaks The locations of breaks between subintervals
#' @export
gaussQuadSub <- function(L,U,order=7,intervals=1,breaks=NULL) {
  # nodes and weights on [-1,1]
  out <- gauss.quad(order); w <- out$weights; x <- out$nodes;

  # compute subinterval endpoints
  if(is.null(breaks)){
    h <- (U-L)/intervals;
    b <- L + (1:intervals)*h;
    a <- b-h;
  } else {
    intervals <- 1+length(breaks);
    a <- c(L,breaks);
    b <- c(breaks,U);
  }
  weights=as.vector(sapply(1:intervals,function(j) 0.5*(b[j]-a[j])*w))
  nodes=as.vector(sapply(1:intervals,function(j) 0.5*(a[j]+b[j]) + 0.5*(b[j]-a[j])*x))
  return(list(weights=weights,nodes=nodes));
}

#' @describeIn gaussQuadInt Integrate a function FUN of 2 variables using specified weights and nodes in each variable. FUN must accept two vectors as arguments and return a vector of function values
#' @param FUN Function of two variables to integrate
#' @param wts1 Weights for first variable
#' @param wts2 Weights for second variable
#' @param nodes1 Nodes for first variable
#' @param nodes2 Nodes for second variable
#' @export
quad2D <- function(FUN,wts1,wts2,nodes1,nodes2) {
  X=expand.grid(nodes1,nodes2);
  W=expand.grid(wts1,wts2);
  fval=FUN(X[,1],X[,2]);
  int=sum(fval*W[,1]*W[,2]);
  return(int)
}

#' @describeIn gaussQuadInt Allometric scaling function ZToB takes (mm) to (g)
#' @param z Size in mm
#' @param alloSlope Slope parameter of allometric function
#' @param alloIntercept Intercept parameter of allometric function
#' @export
ZToB<-function(z,alloIntercept,alloSlope){
  return(alloIntercept*z^alloSlope)
}

#' @describeIn gaussQuadInt Allometric scaling function BToZ takes (g) to (mm)
#' @param b mass in g
#' @param alloSlope Slope parameter of allometric function
#' @param alloIntercept Intercept parameter of allometric function
#' @export
BToZ<-function(b,alloIntercept,alloSlope){
  return((b/alloIntercept)^(1/alloSlope))
}

#' @describeIn gaussQuadInt Change parameter template to data frame for computation
#' @param profileObject A FishToxTranslator parameter template file, either a 'read.csv()' of a template .csv file, i.e. parameterTemplate=read.csv("specie_profile.csv") OR parameterTemplate=get("scientific_name") OR tempProfile object from TemplateToTempProfile()
#' @param daysToRep number of days to replicate template life history parameters for [default=365]
#' @export
TemplateToParameters<-function(profileObject,daysToRep=365)
{dfOut<-setNames(data.frame(matrix(NA,ncol=nrow(profileObject))),as.character(profileObject$id))
dfOut[1,]<-profileObject$value
dfOut<-cbind(ordinal_date=1:daysToRep,dfOut)
if(daysToRep==1){for(i in 2:24){
  dfOut[i]<-as.numeric(dfOut[i])
}}
return(dfOut)
}

#' @describeIn gaussQuadInt Change template data file into a temporary species object for visualization
#' @param profileDataFile location of species profile template (.csv) to upload [string,file location]
#' @export
TemplateToTempProfile<-function(profileDataFile){
  tempPars<-TemplateToParameters(read.csv(profileDataFile),daysToRep=1)
  tempProfile<-list(common_name=tempPars$common_name,scientific_name=tempPars$scientific_name,tPars=tempPars)
  return(tempProfile) #tempProfile object contains common name as string, scientific name as string, and a species profile-like parameter set
  }

#' @describeIn gaussQuadInt Generate a species template .csv file for creating new species life histories and importing back
#' @param fileName name for output .csv file. Output will be "fileName.csv" with the .csv automatically appended to the fileName string supplied [string] [default="Profile-template"]
#' @export
GenerateSpeciesTemplate<-function(fileName="Profile-template"){
  lifeHistoryTemplate<-subset(parameters_master,in_profile==T)
  lifeHistoryTemplate<-add_column(lifeHistoryTemplate,value=NA,.after="id")
  lifeHistoryTemplate<-lifeHistoryTemplate[,1:5]
  write.csv(lifeHistoryTemplate,paste0(fileName,".csv"),row.names=F)
}

#' @describeIn gaussQuadInt Creates a temporary species profile that can be used in visualization. This temporary profile is added to species_library object within the session
#' @param profileDataFile name of input .csv Profile-template. [string, .csv]
#' @export
TemplateToProfile<-function(profileDataFile){
tempPars<-TemplateToParameters(read.csv(profileDataFile),daysToRep=1)
sn_lower<-tolower(tempPars$scientific_name)
sn_underscore<-gsub(" ","_",sn_lower)
if("species_library" %in% ls(envir=.GlobalEnv)){
tempDFRow<-data.frame(tempPars$common_name,tempPars$scientific_name,sn_underscore,F,T)
names(tempDFRow)<-names(species_library)
species_library<<-rbind(species_library,tempDFRow)
assign(sn_underscore,tempPars,envir=.GlobalEnv)}
else{data("species_library")
     tempDFRow<-data.frame(tempPars$common_name,tempPars$scientific_name,sn_underscore,F,T)
     names(tempDFRow)<-names(species_library)
     species_library<<-rbind(species_library,tempDFRow)
     assign(sn_underscore,tempPars,envir=.GlobalEnv)}
return(get(sn_underscore))
}
