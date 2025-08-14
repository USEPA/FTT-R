#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#### Header Info ####
## Normalization Functions for Parameter,  s_b

## Information from parameters_master.RData
# t(FishToxTranslator::parameters_master[ 13 ,]) #Uncomment to pull data from package into R

#### Derivation Notes: ####

#### Example Normalizations: ####
## Idea: Fit the survival parameters of s_a, s_b, s_min, and s_max using minimal data points
## For example can we fit this function using values of, using z_hatch, z_inf, and z_repro

## Assumption, the function must fit (z_hatch,s_min), (z_repro,mean(s_min,s_max)), and (z_inf, s_max)

## Functional form of survival(s_a,s_b,s_min,s_max)= s_min + ((s_max - s_min)/(1 + exp(s_a * (s_b - z))))

survFit<-function(z,s_a,s_b,s_min,s_max){
  s_min + ((s_max - s_min)/(1 + exp(s_a * (s_b - z))))
}

fitParams<-data.frame(s_min=NA,s_max=NA,s_a=NA,s_b=NA,z_hatch=NA,z_repro=NA,z_inf=NA)

#Try for FHM

fitParams$s_min<-.9902
fitParams$s_max<-.9937
fitParams$z_hatch<-5.4
fitParams$z_inf<-73
fitParams$z_repro<-49
fitParams

createFitData<-function(fitParams){
  fitData<-data.frame(size=NA,survival=NA)
  fitData[1,]<-c(fitParams$z_hatch,fitParams$s_min)
  fitData[2,]<-c(fitParams$z_repro,mean(c(fitParams$s_min,fitParams$s_max)))
  fitData[3,]<-c(fitParams$z_inf,fitParams$s_max)
  return(fitData)
}


fitData<-createFitData(fitParams)
fitData[4,]<-c(fitParams$z_repro,mean(c(fitParams$s_min,fitParams$s_max)))
fitData[5,]<-c(fitParams$z_repro,mean(c(fitParams$s_min,fitParams$s_max)))

#plot to confirm proper data.frame
plot(fitData, col="black")
eval(fitParams$s_min)
# Fitting code using nonlinear least square:
fit <- nls(survival ~ .9902 + ((.9937 - .9902)/(1 + exp(.5 * (s_b - size)))), data = fitData,start = list(s_b = 20),algorithm="plinear")
#fit <- nls( ~ survFit(z,s_a,s_b,s_min,s_max), control=nlc, data = data.all.med, start = list(s_a = 1, s_b = 1))
fit


sizes<-seq(from=fitParams$z_hatch,to=fitParams$z_inf,length.out=100)

fittedData<-data.frame(size=sizes,survival=predict(fit,list(size=sizes)))

plot(fitData)
lines(survFit(z=sizes,s_a=.5,s_b=40,s_min=.9902,s_max=.9937))
