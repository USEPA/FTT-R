#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#
#
#' Fish Toxicity Translator: Plotting Functions
#'
#' This group of functions are used for plotting various components of the FishToxTranslator model
#'
#'
#' @describeIn PlotGrowth This function plots the daily growth function by showing mean daily growth increment and standard deviation by size
#'
#' @param pars A date indexed FishToxTranslator parameter data.frame() or list of data.frames [data.frame]
#' @param date Ordinal date used to access date specific FishToxTranslator parameters [integer]
#' @param sizesToPlot Number size classes/divisions to plot between the lower and upper size limits. Default=100. [integer]
#' @param bt For [PlotGrowth], Population biomass for date plotted.  These values are produced as an output from SimulateModel(), but can be input for explore different density dependent growth.  Default value is 0 to represent density independence. [float]
#' @param plotconf95s For [PlotGrowth], T/F for including standard deviations of daily growth increments in the plot [boolean]
#' @param forProfile T/F to indicate this plot is for a species profile instead of simulations/visualizations after a 'pars' object has been created [boolean]
#' @param species for [PlotGrowth, PlotYearlyGrowth], When plotting for species profiles, provide species common name instead of pars object to plot from life history parameters directly [string]
#' @param ... for [PlotGrowthTrajectory, PlotYearlyGrowth], can pass arguments to Growth(...) function
#' @return Returns a plot for visualization of FishToxTranslator model outputs and functions
#' @export
PlotGrowth <- function (pars,date,sizesToPlot=100,bt=0,plotconf95s=T,forProfile=F,species=NA,...) {
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  ##Profile version of plotting growth
  if(forProfile){ #creates the profile version of the growth plots, otherwise creates interactive GUI version
    ###
    #Create species specific parameter list, sPars
    speciesIn<-species #define species (this will normally be passed into function)
    if(speciesIn %in% FishToxTranslator::species_library$common_name){
    species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
    species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
    species_data<-get(species_data_name)
    sPars<-as.data.frame(t(species_data$value))
    names(sPars)<-t(species_data$id)

    sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
    sizes<-sizes[-length(sizes)]
    avg<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=T,...)[,1]-sizes
    sdev<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=T,...)[,2]
    conf95<-1.96*sdev
    plot(sizes, avg,
         ylim=range(c(avg-conf95, avg+conf95)),
         pch=19, xlab="Size (mm)", ylab=ifelse(plotconf95s,"Mean growth (mm) 95% CI","Mean growth (mm)"),
         main=paste("Mean daily growth increments \n",species," (", species_sci_name,")",sep=""),
         axes=T,col="black")

      if(!min(conf95)>0){
      points(sizes, avg,
             ylim=range(c(avg-conf95, avg+conf95)),
             pch=19,col="black")}
      else{
      if(plotconf95s==T){arrows(sizes, avg-conf95, sizes, avg+conf95, length=0, angle=90, code=3,col="black")}
      points(sizes, avg,ylim=range(c(avg-conf95, avg+conf95)),pch=19,col="black")}
    }
    else{return(writeLines(c(paste("Non-matching species common name. Current options are:"),paste(FishToxTranslator::species_library$common_name,collapse=", "))))}
  }
  ###THIS IS WHERE THE OLD PLOTGROWTH FUNCTION BEGAN###
  else{
    if(missing(date)){return(print("Please provide a date"))}
    if(is.data.frame(pars)){
      sizes<-seq(from=pars$z_hatch[date],to=pars$z_inf[date],length.out=sizesToPlot)
      avg<-Growth(sizes,sizes,bt,pars,date=date,muSDOut=T)[,1]-sizes
      sdev<-Growth(sizes,sizes,bt,pars,date=date,muSDOut=T)[,2]
      conf95<-1.96*sdev
      plot(sizes, avg,
           ylim=range(c(avg-conf95, avg+conf95)),
           pch=19, xlab="Size (mm)", ylab=ifelse(plotconf95s,"Mean growth (mm) +/- SD","Mean growth (mm)"),
           main=paste("Daily growth increments - Ordinal date: ",date,sep=""),
           axes=T,col="lightblue")

      if(!min(conf95)>0){
        points(sizes, avg,
               ylim=range(c(avg-conf95, avg+conf95)),
               pch=19,col="steelblue")}
      else{
        if(plotconf95s==T){arrows(sizes, avg-conf95, sizes, avg+conf95, length=0, angle=90, code=3,col="lightblue")}
        points(sizes, avg,ylim=range(c(avg-conf95, avg+conf95)),pch=19,col="steelblue")}}
    else{nParSets<-length(pars)
    avgs<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    sdevs<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    conf95s<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    allsizes<-matrix(NA,nrow=nParSets,ncol=sizesToPlot) #created to store and capture maximum and minimum sizes being plotted to set proper xlimits
    sizes<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    for(i in 1:nParSets){
      allsizes[i,]<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot) #storing sizes to be recalled later for xlimits for plotting
      sizes[i,]<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot)
      avgs[i,]<-Growth(sizes[i,],sizes[i,],bt,pars[[i]],date=date,muSDOut=T)[,1]-sizes[i,]
      sdevs[i,]<-Growth(sizes[i,],sizes[i,],bt,pars[[i]],date=date,muSDOut=T)[,2]
      conf95s[i,]<-1.96*sdevs[i,] }

    plot(x=seq(from=min(allsizes),to=max(allsizes),length.out=sizesToPlot), y=avgs[1,],xlim=c(min(allsizes),max(allsizes)),ylim=c(min(avgs)-max(conf95s),max(avgs)+max(conf95s)),col=natecols(nParSets)[1],pch="", xlab="Size (mm)", ylab=ifelse(plotconf95s,"Mean growth (mm) +/- SD","Mean growth (mm)"),
         main=paste("Daily growth increments - Ordinal date: ",date,sep="")) #Plot command modified to attempt to allow for rescaling for species with different z_hatch and z_inf values

    for(i in 1:nParSets){
      if(!min(conf95s[i,])>0){
        points(x=sizes[i,],y=avgs[i,],pch=14+i,col=natecols(nParSets)[i])}
      else{
        if(plotconf95s==T){arrows(sizes[i,], avgs[i,]-conf95s[i,], sizes[i,], avgs[i,]+conf95s[i,], length=0, angle=90, code=3,col=natecols(nParSets)[i])}
        points(x=sizes[i,],y=avgs[i,],pch=14+i,col=natecols(nParSets)[i])}}
    }
    for(i in 1:nParSets){
      abline(v=pars[[i]]$z_inf[date],col=natecols(nParSets)[i])
      axis(1, at = pars[[i]]$z_inf[date],
           labels = F,col=natecols(nParSets)[i])
      mtext("Max size", at=pars[[i]]$z_inf[date],col=natecols(nParSets)[i])
    }
    legend("bottomleft",inset=.01, cex=1, legend = names(pars), xpd = TRUE,
           horiz = FALSE, col = natecols(nParSets), pch=seq(from=15,to=(15+nParSets)), bty = "n")
  }
}


#' @describeIn PlotGrowth This function plots the daily survival probability by size
#' @export
PlotSurvival<- function (pars,date,sizesToPlot=100,forProfile=F,species=NA) {
  if(forProfile){
    #Create species specific parameter list, sPars
    speciesIn<-species #define species (this will normally be passed into function)
    if(speciesIn %in% FishToxTranslator::species_library$common_name){
    species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
    species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
    species_data<-get(species_data_name)
    sPars<-as.data.frame(t(species_data$value))
    names(sPars)<-t(species_data$id)

    sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
    surv<-Survival(sizes,sPars,date=1)
    plot(sizes,surv,col="black",pch=18,xlab="Size (mm)",ylab="Survival probability",main=paste("Daily survival probability \n",species," (", species_sci_name,")",sep=""))

    points(sizes,surv,col="black",pch=18,)}
    else{return(writeLines(c(paste("Non-matching species common name. Current options are:"),paste(FishToxTranslator::species_library$common_name,collapse=", "))))}
  }
  else{

    if(missing(date)){return(print("Please provide a date"))}
    natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
    if(is.data.frame(pars)){
      sizes<-seq(from=pars$z_hatch[date],to=pars$z_inf[date],length.out=sizesToPlot)
      surv<-Survival(sizes,pars,date)
      plot(sizes,surv,col="salmon",pch=18,xlab="Size (mm)",ylab="Survival probability",main=paste("Daily survival probability - Ordinal date: ",date,sep=""))

      points(sizes,surv,col="salmon",pch=18,)}
    else{nParSets<-length(pars)
    survs<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    sizes<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    for(i in 1:nParSets){
      sizes[i,]<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot)
      survs[i,]<-Survival(sizes[i,],pars[[i]],date=date)}

    plot(sizes[i,],survs[1,],xlim=c(min(sizes),max(sizes)),ylim=c(min(survs),max(survs)),col=natecols(nParSets)[1],pch="",xlab="Size (mm)",ylab="Survival probability",main=paste("Daily survival probability - Ordinal date: ",date,sep=""))

    legend("topleft", cex=1, legend = names(pars), xpd = TRUE,
           horiz = FALSE, col = natecols(nParSets), pch=seq(from=15,to=(15+nParSets)), bty = "n")
    for(i in 1:nParSets){
      points(sizes[i,],survs[i,],col=natecols(nParSets)[i],pch=18)}
    }
    for(i in 1:nParSets){
      abline(v=pars[[i]]$z_inf[date],col=natecols(nParSets)[i])
      axis(1, at = pars[[i]]$z_inf[date],
           labels = F,col=natecols(nParSets)[i])
      mtext("Max size", at=pars[[i]]$z_inf[date],col=natecols(nParSets)[i])
    }
  }
}
#' @describeIn PlotGrowth This function plots the daily size-dependent reproductive output of females per female per day
#' @param reprolines for [PlotReproduction], T/F to include vertical lines to indicate reproductively mature size. Default = F [boolean]
#' @export
PlotReproduction<- function (pars,date,reprolines=F,sizesToPlot=100,forProfile=F,species=NA) {
  if(forProfile){
    date=1
    #Create species specific parameter list, sPars
    speciesIn<-species #define species (this will normally be passed into function)

    if(speciesIn %in% FishToxTranslator::species_library$common_name){
    species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
    species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
    species_data<-get(species_data_name)
    sPars<-as.data.frame(t(species_data$value))
    names(sPars)<-t(species_data$id)

    sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
    repro<-sPars$sex_ratio[date]*Fecundity(sizes,sPars,date)*Survival(sPars$z_hatch[date],sPars,date)
    plot(sizes,repro,col="black",pch=18,xlab="Size (mm)",ylab="Female hatchlings/female/per day ",main=paste("Daily maximum reproductive output \n",species," (", species_sci_name,")",sep=""))

    points(sizes,repro,col="black",pch=18)}
    else{return(writeLines(c(paste("Non-matching species common name. Current options are:"),paste(FishToxTranslator::species_library$common_name,collapse=", "))))}
  }
  else{

    if(missing(date)){return(print("Please provide a date"))}
    natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
    if(is.data.frame(pars)){
      sizes<-seq(from=pars$z_hatch[date],to=pars$z_inf[date],length.out=sizesToPlot)
      repro<-pars$sex_ratio[date]*Spawning(sizes,pars,date)*Fecundity(sizes,pars,date)*Survival(pars$z_hatch[date],pars,date)
      plot(sizes,repro,col="salmon",pch=18,xlab="Size (mm)",ylab="Avg female hatchlings/female/per day ",main=paste("Daily reproductive output - Ordinal date: ",date,sep=""))

      points(sizes,repro,col="salmon",pch=18)}
    else{nParSets<-length(pars)
    sizes<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    repros<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    for(i in 1:nParSets){
      sizes[i,]<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot)
      repros[i,]<-pars[[i]]$sex_ratio[date]*Spawning(sizes[i,],pars[[i]],date)*Fecundity(sizes[i,],pars[[i]],date)*Survival(pars[[i]]$z_hatch[date],pars[[i]],date)}

    plot(x=seq(from=min(sizes),to=max(sizes),length.out=sizesToPlot),repros[1,],ylim=c(min(repros),max(repros)),col=natecols(nParSets)[1],pch="",xlab="Size (mm)",ylab="Avg female hatchlings/female/per day ",main=paste("Daily reproductive output - Ordinal date: ",date,sep=""))

    legend("topleft", cex=1, legend = names(pars), xpd = TRUE,
           horiz = FALSE, col = natecols(nParSets), pch=seq(from=15,to=(15+nParSets)), bty = "n")
    for(i in 1:nParSets){
      points(sizes[i,],repros[i,],col=natecols(nParSets)[i],pch=18)}
    for(i in 1:nParSets){
      abline(v=pars[[i]]$z_inf[date],col=natecols(nParSets)[i])
      axis(1, at = pars[[i]]$z_inf[date],
           labels = F,col=natecols(nParSets)[i])
      mtext("Max size", at=pars[[i]]$z_inf[date],col=natecols(nParSets)[i])}
    for(i in 1:nParSets){
      if(reprolines){abline(v=pars[[i]]$z_repro[date],col=natecols(nParSets)[i],lty="dotted")
        axis(1, at = pars[[i]]$z_repro[date],
             labels = F,col=natecols(nParSets)[i])
        mtext("Repro size",side=1, at=pars[[i]]$z_repro[date],col=natecols(nParSets)[i])}
    }
    }
  }
}

#' @describeIn PlotGrowth This function plots daily spawning probability
#' @export
PlotSpawningProbs<- function (pars) {
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  if(is.data.frame(pars)){
    if(exists('p_spawn',where=pars)){
    plot(x=1:365,y=pars$p_spawn,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Daily spawning probability",main="Daily spawning probability")

    points(x=1:365,y=pars$p_spawn,col=natecols(1),pch=18)}
    else{return(print("Parameter set does not have daily spawning probability values"))}}
  else{nParSets<-length(pars)
  spawns<-matrix(NA,nrow=nParSets,ncol=365)
  for(i in 1:nParSets){
    spawns[i,]<-pars[[i]]$p_spawn}
  plot(x=1:365,spawns[1,],ylim=c(min(spawns),max(spawns)),col=natecols(nParSets)[1],pch=15,xlab="Ordinal date",ylab="Daily spawning probability",main="Daily spawning probability")

  legend("topleft", cex=1, legend = names(pars), xpd = TRUE,
         horiz = FALSE, col = natecols(nParSets), pch=seq(from=15,to=(15+nParSets)), bty = "n")
  for(i in 1:nParSets){
    points(x=1:365,spawns[i,],col=natecols(nParSets)[i],pch=14+i)}
  }
}

#' @describeIn PlotGrowth This function plots daily survival decrements
#' @export
PlotSurvivalDecrements<- function (pars) {
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  if(is.data.frame(pars)){
    if(exists('survival_decrement',where=pars)){
      plot(x=1:365,y=-pars$survival_decrement,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Daily survival decrement",main="Daily survival decrement",type="l")

      lines(x=1:365,y=-pars$survival_decrement,col=natecols(1),pch=18)}
    else{return(print("Parameter set does not have survival decrement values"))}}
  else{nParSets<-length(pars)
  parsWithSDec<-list()
  SDecCheck<-c()
  for(i in 1:nParSets){
    SDecCheck[i]<-exists("survival_decrement",where=pars[[i]])
  }
  if(any(SDecCheck==F)){
    print(paste("The parameter set ",names(pars)[!SDecCheck]," does not have survival decrements and cannot be plotted"))
  }
  parsWithSDec<-pars[SDecCheck]
  nParsWithSDec<-length(parsWithSDec)
  SDecs<-matrix(NA,nrow=nParsWithSDec,ncol=365)
  for(i in 1:nParsWithSDec){
  SDecs[i,]<-parsWithSDec[[i]]$survival_decrement}
  plot(x=1:365,y=-SDecs[1,],ylim=c(-max(SDecs),-min(SDecs)),col=natecols(nParSets)[1],pch=15,xlab="Ordinal date",ylab="Daily survival decrement",main="Daily survival decrement",type="l")

  legend("bottomleft", cex=1, legend = names(parsWithSDec), xpd = TRUE,
         horiz = FALSE, col = natecols(nParsWithSDec),lty=1, bty = "n")
  for(i in 1:nParsWithSDec){
    lines(x=1:365,-SDecs[i,],col=natecols(nParsWithSDec)[i],pch=14+i,type="l")}
  }
}

#' @describeIn PlotGrowth This function plots daily growth percents
#' @export
PlotGrowthPercents<- function (pars) {
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  if(is.data.frame(pars)){
    if(exists('growth_percent',where=pars)){
      plot(x=1:365,y=-pars$growth_percent,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Daily growth percent reduction",main="Daily percent reduction in vB growth 'k' parameter",type="l")

      lines(x=1:365,y=-pars$growth_percent,col=natecols(1),pch=18)}
    else{return(print("Parameter set does not have growth percent values"))}}
  else{nParSets<-length(pars)
  parsWithSDec<-list()
  GPCheck<-c()
  for(i in 1:nParSets){
    GPCheck[i]<-exists("growth_percent",where=pars[[i]])
  }
  if(any(GPCheck==F)){
    print(paste("The parameter set ",names(pars)[!GPCheck]," does not have growth percents and cannot be plotted"))
  }
  parsWithGP<-pars[GPCheck]
  nParsWithGP<-length(parsWithGP)
  GPs<-matrix(NA,nrow=nParsWithGP,ncol=365)
  for(i in 1:nParsWithGP){
    GPs[i,]<-parsWithGP[[i]]$growth_percent}
  plot(x=1:365,y=GPs[1,],ylim=c(0,1),col=natecols(nParSets)[1],pch=15,xlab="Ordinal date",ylab="Daily growth percent reduction",main="Daily percent reduction in vB growth 'k' parameter",type="l")

  legend("bottomleft", cex=1, legend = names(parsWithGP), xpd = TRUE,
         horiz = FALSE, col = natecols(nParsWithGP),lty=1, bty = "n")
  for(i in 1:nParsWithGP){
    lines(x=1:365,GPs[i,],col=natecols(nParsWithGP)[i],pch=14+i,type="l")}
  }
}

#' @describeIn PlotGrowth This function plots daily exposure concentrations
#' @export
PlotExposureConcentrations<- function (pars) {
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  if(is.data.frame(pars)){
    if(exists('exp_concentrations',where=pars)){
      plot(x=1:365,y=pars$exp_concentrations,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Exposure concentrations",main="Daily exposure concentrations",type="l")

      lines(x=1:365,y=pars$exp_concentrations,col=natecols(1),pch=18)}
    else{return(print("Parameter set does not have daily exposure concentrations"))}}
  else{nParSets<-length(pars)
  parsWithECoc<-list()
  ECocCheck<-c()
  for(i in 1:nParSets){
    ECocCheck[i]<-exists("exp_concentrations",where=pars[[i]])
  }
  if(any(ECocCheck==F)){
    print(paste("The parameter set ",names(pars)[!ECocCheck]," does not have exposure concentrations and cannot be plotted"))
  }
  parsWithECoc<-pars[ECocCheck]
  nParsWithECoc<-length(parsWithECoc)
  ECocs<-matrix(NA,nrow=nParsWithECoc,ncol=365)
  for(i in 1:nParsWithECoc){
    ECocs[i,]<-parsWithECoc[[i]]$exp_concentrations}
  plot(x=1:365,y=ECocs[1,],ylim=c(min(ECocs),max(ECocs)),col=natecols(nParSets)[1],pch=15,xlab="Ordinal date",ylab="Exposure concentrations",main="Daily exposure concentrations",type="l")

  legend("topleft", cex=1, legend = names(parsWithECoc), xpd = TRUE,
         horiz = FALSE, col = natecols(nParsWithECoc), lty=1, bty = "n")
  for(i in 1:nParsWithECoc){
    lines(x=1:365,ECocs[i,],col=natecols(nParsWithECoc)[i],pch=14+i,type="l")}
  }
}

#' @describeIn PlotGrowth This function plots yearly growth trajectory from species parameters
#' @param invert for [PlotLengthToMass], can invert this allometric relationship to plot Mass to Length instead [boolean]
#' @export
PlotLengthToMass<-function(species,sizesToPlot=100,invert=F){
  #ensure/import species data
  speciesIn<-species #define species (this will normally be passed into function)
  if(!(speciesIn %in% FishToxTranslator::species_library$common_name)){
    return(writeLines(c(paste("Non-matching species common name. Current options are:"),paste(FishToxTranslator::species_library$common_name,collapse=", "))))}
  else{
    species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
    species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
    species_data<-get(species_data_name)
    sPars<-as.data.frame(t(species_data$value))
    names(sPars)<-t(species_data$id)
    if(!invert){
    sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
    plot(sizes,ZToB(sizes,sPars$allo_intercept,sPars$allo_slope),pch=20,xlab="Length (mm)", ylab="Mass (g)",
         main=paste("Allometric Scaling - Length to Mass \n",species," (", species_sci_name,")",sep=""))}
    else{masses<-seq(from=ZToB(sPars$z_hatch,sPars$allo_intercept,sPars$allo_slope),to=ZToB(sPars$z_inf,sPars$allo_intercept,sPars$allo_slope),length.out=sizesToPlot)
        plot(masses,BToZ(masses,sPars$allo_intercept,sPars$allo_slope),pch=20,xlab="Mass (g)", ylab="Length (mm)",
           main=paste("Allometric Scaling - Mass to Length \n",species," (", species_sci_name,")",sep=""))}
  }
}

#' @describeIn PlotGrowth This function plots an arbitrarily long growth trajectory from species parameters
#' @param noOfDays for [PlotGrowthTrajectory], when plotting non annual growth trajectory can specify number of days to plot for [integer]
#' @export

PlotGrowthTrajectory<-function(species,sizesToPlot=100,noOfDays=365,...){
  #ensure/import species data
  speciesIn<-species #define species (this will normally be passed into function)
  if(speciesIn %in% FishToxTranslator::species_library$common_name){
    species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
    species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
    species_data<-get(species_data_name)
    sPars<-as.data.frame(t(species_data$value))
    names(sPars)<-t(species_data$id)



    #initialize lists
    z<-c() #mean size after growth
    zm<-c() #min size after growth (2sd below)
    zM<-c() #max size after growth (2sd above)
    g<<-c() #growth value
    gm<-c() #min growth value
    gM<-c() #max growth value
    sdm<-c() # standard deviation below
    sdM<-c() # standard deviation above

    #size intervals to measure growth at (usually 100 points between z_hatch and z_inf)
    sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
    z[1]<-zm[1]<-zM[1]<-sPars$z_hatch #initialize starting size as z_hatch

    for(i in 1:noOfDays){
      #growth from mean
      g[i]<<-Growth(sizes,z[i],bt,sPars,date=1,muSDOut=T,...)[,1]-z[i]
      #growth from 95min
      gm[i]<-Growth(sizes,zm[i],bt,sPars,date=1,muSDOut=T,...)[,1]-zm[i]
      sdm[i]<-Growth(sizes,zm[i],bt,sPars,date=1,muSDOut=T,...)[,2]
      #growth from 95max
      gM[i]<-Growth(sizes,zM[i],bt,sPars,date=1,muSDOut=T,...)[,1]-zM[i]
      sdM[i]<-Growth(sizes,zM[i],bt,sPars,date=1,muSDOut=T,...)[,2]
      #update sizes with growth
      z[i+1]<-z[i]+g[i]
      zM[i+1]<-min(sPars$z_inf,zM[i]+gM[i]+1.96*sdM[i])
      zm[i+1]<-zm[i]+gm[i]-1.96*sdm[i]
    }

    #plot sizes, mean growth
    plot(z[1:noOfDays],pch=20,xlab="Days from hatch", ylab="Length (mm)",
         main=paste("Growth Trajectory - ",noOfDays," days","\n",species," (", species_sci_name,")",sep=""),ylim=c(sPars$z_hatch-(0.05*sPars$z_inf),1.05*sPars$z_inf))

    #use arrows to signify max and min growth
    suppressWarnings(arrows(2:noOfDays, zm[2:noOfDays], 2:noOfDays, zM[2:noOfDays], length=0, angle=90, code=3,col="gray")) #suppress warnings for messages about indeterminate arrow length which can occur when difference between zM and zm is very small.
    #use lines to signify max and minimum size values from parameters
    abline(h=sPars$z_inf,col="steelblue")
    abline(h=sPars$z_hatch,col='steelblue')
    #replot points to have above lines
    points(z[1:noOfDays],pch=20)
    #add labels to horizontal lines
    text(x=noOfDays/2,y=sPars$z_inf+1.5,paste0("Max length: ",sPars$z_inf,"mm"),col="steelblue")
    text(x=noOfDays/2,y=sPars$z_hatch-2.5,paste("Hatch length: ",sPars$z_hatch,"mm"),col="steelblue")
  }
  else{return(writeLines(c(paste("Non-matching species common name. Current options are:"),paste(FishToxTranslator::species_library$common_name,collapse=", "))))}
}

#' @describeIn PlotGrowth This function plots an arbitrarily long survival trajectory from species parameters
#' @param logY for [PlotSurvivalTrajectory] T/F to log transform the Y (survival probability) axis.  Default = T [boolean]
#' @export
PlotSurvivalTrajectory<-function(species,sizesToPlot=100,noOfDays=365,logY=T,...){
  #ensure/import species data
  speciesIn<-species #define species (this will normally be passed into function)
  if(speciesIn %in% FishToxTranslator::species_library$common_name){
    species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
    species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
    species_data<-get(species_data_name)
    sPars<-as.data.frame(t(species_data$value))
    names(sPars)<-t(species_data$id)

    #initialize lists
    z<-c() #mean size after growth
    zm<-c() #min size after growth (2sd below)
    zM<-c() #max size after growth (2sd above)
    g<<-c() #growth value
    gm<-c() #min growth value
    gM<-c() #max growth value
    sdm<-c() # standard deviation below
    sdM<-c() # standard deviation above
    ## Survival compoent
    s<-c() #mean survival
    sm<-c() #min size after growth
    sM<-c() #max size after growth
    ## Cumulative survival
    scum<-c()
    scumm<-c()
    scumM<-c()


    #size intervals to measure growth at (usually 100 points between z_hatch and z_inf)
    sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
    z[1]<-zm[1]<-zM[1]<-sPars$z_hatch #initialize starting size as z_hatch
    s[1]<-sm[1]<-sM[1]<-scum[1]<-scumm[1]<-scumM[1]<-Survival(z[1],sPars,date=1)
    for(i in 1:noOfDays){
      #growth from mean
      g[i]<<-Growth(sizes,z[i],bt,sPars,date=1,muSDOut=T,...)[,1]-z[i]
      #growth from 95min
      gm[i]<-Growth(sizes,zm[i],bt,sPars,date=1,muSDOut=T,...)[,1]-zm[i]
      sdm[i]<-Growth(sizes,zm[i],bt,sPars,date=1,muSDOut=T,...)[,2]
      #growth from 95max
      gM[i]<-Growth(sizes,zM[i],bt,sPars,date=1,muSDOut=T,...)[,1]-zM[i]
      sdM[i]<-Growth(sizes,zM[i],bt,sPars,date=1,muSDOut=T,...)[,2]
      #update sizes with growth
      z[i+1]<-z[i]+g[i]
      zM[i+1]<-min(sPars$z_inf,zM[i]+gM[i]+1.96*sdM[i])
      zm[i+1]<-zm[i]+gm[i]-1.96*sdm[i]
      s[i+1]<-Survival(z[i],sPars,date=1)
      sm[i+1]<-Survival(zm[i+1],sPars,date=1)
      sM[i+1]<-Survival(zM[i+1],sPars,date=1)
      scum[i+1]<-scum[i]*s[i+1]
      scumm[i+1]<-scumm[i]*sm[i+1]
      scumM[i+1]<-scumM[i]*sM[i+1]
    }

    ## plot cumulative survival growth with log scale for survival probability

    if(logY){
    plot(scum[1:noOfDays],pch=20,xlab="Days from hatch", ylab="Survival probability",log='y',
         main=paste("Cumulative Survival Trajectory - ",noOfDays," days","\n",species," (", species_sci_name,")",sep=""))
    arrows(2:noOfDays, scumm[2:noOfDays], 2:noOfDays, scumM[2:noOfDays], length=0, angle=90, code=3,col="gray")
    points(scum[1:noOfDays],pch=20)}

    if(!logY){ plot(scum[1:noOfDays],pch=20,xlab="Days from hatch", ylab="Survival probability",
                    main=paste("Cumulative Survival Trajectory - ",noOfDays," days","\n",species," (", species_sci_name,")",sep=""))
      arrows(2:noOfDays, scumm[2:noOfDays], 2:noOfDays, scumM[2:noOfDays], length=0, angle=90, code=3,col="gray")
      points(scum[1:noOfDays],pch=20)}
  }
  else if(speciesIn %in% species_library$common_name){
    species_data_name<-subset(unique(species_library), common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
    species_sci_name<-subset(unique(species_library), common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
    sPars<-get(species_data_name)

    #initialize lists
    z<-c() #mean size after growth
    zm<-c() #min size after growth (2sd below)
    zM<-c() #max size after growth (2sd above)
    g<<-c() #growth value
    gm<-c() #min growth value
    gM<-c() #max growth value
    sdm<-c() # standard deviation below
    sdM<-c() # standard deviation above
    ## Survival compoent
    s<-c() #mean survival
    sm<-c() #min size after growth
    sM<-c() #max size after growth
    ## Cumulative survival
    scum<-c()
    scumm<-c()
    scumM<-c()


    #size intervals to measure growth at (usually 100 points between z_hatch and z_inf)
    sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
    z[1]<-zm[1]<-zM[1]<-sPars$z_hatch #initialize starting size as z_hatch
    s[1]<-sm[1]<-sM[1]<-scum[1]<-scumm[1]<-scumM[1]<-Survival(z[1],sPars,date=1)
    for(i in 1:noOfDays){
      #growth from mean
      g[i]<<-Growth(sizes,z[i],bt,sPars,date=1,muSDOut=T,...)[,1]-z[i]
      #growth from 95min
      gm[i]<-Growth(sizes,zm[i],bt,sPars,date=1,muSDOut=T,...)[,1]-zm[i]
      sdm[i]<-Growth(sizes,zm[i],bt,sPars,date=1,muSDOut=T,...)[,2]
      #growth from 95max
      gM[i]<-Growth(sizes,zM[i],bt,sPars,date=1,muSDOut=T,...)[,1]-zM[i]
      sdM[i]<-Growth(sizes,zM[i],bt,sPars,date=1,muSDOut=T,...)[,2]
      #update sizes with growth
      z[i+1]<-z[i]+g[i]
      zM[i+1]<-min(sPars$z_inf,zM[i]+gM[i]+1.96*sdM[i])
      zm[i+1]<-zm[i]+gm[i]-1.96*sdm[i]
      s[i+1]<-Survival(z[i],sPars,date=1)
      sm[i+1]<-Survival(zm[i+1],sPars,date=1)
      sM[i+1]<-Survival(zM[i+1],sPars,date=1)
      scum[i+1]<-scum[i]*s[i+1]
      scumm[i+1]<-scumm[i]*sm[i+1]
      scumM[i+1]<-scumM[i]*sM[i+1]
    }

    ## plot cumulative survival growth with log scale for survival probability

    if(logY){
      plot(scum[1:noOfDays],pch=20,xlab="Days from hatch", ylab="Survival probability",log='y',
           main=paste("Cumulative Survival Trajectory - ",noOfDays," days","\n",species," (", species_sci_name,")",sep=""))
      arrows(2:noOfDays, scumm[2:noOfDays], 2:noOfDays, scumM[2:noOfDays], length=0, angle=90, code=3,col="gray")
      points(scum[1:noOfDays],pch=20)}

    if(!logY){ plot(scum[1:noOfDays],pch=20,xlab="Days from hatch", ylab="Survival probability",
                    main=paste("Cumulative Survival Trajectory - ",noOfDays," days","\n",species," (", species_sci_name,")",sep=""))
      arrows(2:noOfDays, scumm[2:noOfDays], 2:noOfDays, scumM[2:noOfDays], length=0, angle=90, code=3,col="gray")
      points(scum[1:noOfDays],pch=20)}
  }
  else{return(writeLines(c(paste("Non-matching species common name. Current options are:"),paste(FishToxTranslator::species_library$common_name,collapse=", "))))}
}

# ## IN PROGRESS
# PlotGrowthProfile <- function (species,tempSpecies=NA,sizesToPlot=100,bt=0,plotconf95s=T,...) {
#
#     #creates the profile version of the growth plots from a temporary profile object otherwise it creates it from a species in the species library
#     if(!anyNA(tempSpecies,recursive=T)){
#       if(!is.list(tempSpecies)){print("Plese provide a TemplateToProfile() list object as input")}
#       else{
#       species<-tempSpecies$common_name
#         species_sci_name<-tempSpecies$scientific_name
#         sPars<-tempSpecies$tPars
#
#         sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
#         sizes<-sizes[-length(sizes)]
#         avg<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=T,...)[,1]-sizes
#         sdev<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=T,...)[,2]
#         conf95<-1.96*sdev
#         plot(sizes, avg,
#              ylim=range(c(avg-conf95, avg+conf95)),
#              pch=18, xlab="Size (mm)", ylab=ifelse(plotconf95s,"Mean growth (mm) 95% CI","Mean growth (mm)"),
#              main=paste("Mean daily growth increments \n",species," (", species_sci_name,")",sep=""),
#              axes=T,col="black")
#
#         if(!min(conf95)>0){
#           points(sizes, avg,
#                  ylim=range(c(avg-conf95, avg+conf95)),
#                  pch=18,col="black")}
#         else{
#           if(plotconf95s==T){#arrows(sizes, avg-conf95, sizes, avg+conf95, length=0, angle=90, code=3,col="black")
#             }
#           points(sizes, avg,ylim=range(c(avg-conf95, avg+conf95)),pch=18,col="black")
#           polygon(c(sizes, rev(sizes)), c(avg-conf95, rev(avg+conf95)),
#                   col=adjustcolor('steelblue', alpha=0.25),border=NA)}
#       }
#     }
#     ###
#     #Create species specific parameter list, sPars
#     else{
#       speciesIn<-species #define species (this will normally be passed into function)
#       if(speciesIn %in% FishToxTranslator::species_library$common_name){
#       species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
#       species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
#       species_data<-get(species_data_name)
#       #sPars<-as.data.frame(t(species_data$value))
#       #names(sPars)<-t(species_data$id)
#       sPars<-TemplateToParameters(species_data,daysToRep=1)
#
#
#       sizes<<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
#       sizes<<-sizes[-length(sizes)]
#       dists<<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=F,CDF=F,...)
#       avg<<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=T,...)[,1]-sizes
#       sdev<<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=T,...)[,2]
#       conf95<-1.96*sdev
#       plot(sizes, avg,
#            ylim=range(c(avg-conf95, avg+conf95)),
#            pch=19, xlab="Size (mm)", ylab=ifelse(plotconf95s,"Mean growth (mm) 95% CI","Mean growth (mm)"),
#            main=paste("Mean daily growth increments \n",species," (", species_sci_name,")",sep=""),
#            axes=T,col="black")
#
#       if(!min(conf95)>0){
#         points(sizes, avg,
#                ylim=range(c(avg-conf95, avg+conf95)),
#                pch=19,col="black")}
#       else{
#         if(plotconf95s==T){arrows(sizes, avg-conf95, sizes, avg+conf95, length=0, code=3,col="steelblue")}
#         points(sizes, avg,ylim=range(c(avg-conf95, avg+conf95)),pch=19,col="black",type="b")
#         }
#     }
#     else{return(writeLines(c(paste("Non-matching species common name. Current options are:"),paste(FishToxTranslator::species_library$common_name,collapse=", "))))}
#     }
# }
# #
# #
# #
#
# # library(FishToxTranslator)
# #
# # TemplateToParameters(get("pimephales_promelas"),daysToRep=1)
# #
# # tempBG<-TemplateToTempSpecies("BluegillTest","Bluegill test","bluegill_template.csv")
# #
# # PlotGrowthProfile(tempSpecies=tempBG)
# #
# # PlotGrowthProfile(species="Bluegill")
# #
# # PlotSurvivalTrajectory("Fathead Minnow",noOfDays=365)
# #
# #
#
