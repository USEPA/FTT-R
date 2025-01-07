#~             ,''''''''''''''.
#~~           /   USEPA FISH   \
#~   >~',*>  <  TOX TRANSLATOR  )
#~~           \ v1.0 "Doloris" /
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#
#' Fish Toxicity Translator: Plotting Functions
#'
#' This group of functions are used for plotting various components of the FishToxTranslator model
#'
#'
#' @describeIn PlotGrowth This function plots the daily growth function by showing mean daily growth increment and standard deviation by size
#'
#'
#' @param pars A date indexed FishToxTranslator parameter data.frame() or list of data.frames
#' @param date Ordinal date used to access date specific FishToxTranslator parameters
#' @param sizesToPlot Number size classes/divisions to plot between the lower and upper size limits. Default=100.
#' @param bt For [PlotGrowth], Population biomass for date plotted.  These values are produced as an output from SimulateModel(), but can be input for explore different density dependent growth.  Default value is 0 to represent density independence.
#' @param plotSDevs For [PlotGrowth], T/F for including standard deviations of daily growth increments in the plot
#' @param ForProfile T/F to indicate this plot is for a species profile instead of simulations/visualizations after a 'pars' object has been created
#' @param species for [PlotGrowth, PlotYearlyGrowth], When plotting for species profiles, provide species common name instead of pars object to plot from life history parameters directly
#' @return Returns a plot for visualization of FishToxTranslator model outputs and functions
PlotGrowth <- function (pars,date,sizesToPlot=100,bt=0,plotSDevs=T,ForProfile=F,species=NA) {
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  ##Profile version of plotting growth
  if(ForProfile){ #creates the profile version of the growth plots, otherwise creates interactive GUI version
    ###
    #Create species specific parameter list, sPars
    speciesIn<-species #define species (this will normally be passed into function)
    species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
    species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
    species_data<-get(species_library$parameter_data)
    sPars<-as.data.frame(t(species_data$value))
    names(sPars)<-t(species_data$id)

    sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
    avg<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=T)[,1]-sizes
    sdev<-Growth(sizes,sizes,bt,sPars,date=1,muSDOut=T)[,2]
    plot(sizes, avg,
         ylim=range(c(avg-sdev, avg+sdev)),
         pch=19, xlab="Size (mm)", ylab=ifelse(plotSDevs,"Mean growth (mm) +/- SD","Mean growth (mm)"),
         main=paste("Mean daily growth increments \n",species," (", species_sci_name,")",sep=""),
         axes=T,col="black")

    if(!min(sdev)>0){
      points(sizes, avg,
             ylim=range(c(avg-sdev, avg+sdev)),
             pch=19,col="black")}
    else{
      if(plotSDevs==T){arrows(sizes, avg-sdev, sizes, avg+sdev, length=0.05, angle=90, code=3,col="black")}
      points(sizes, avg,ylim=range(c(avg-sdev, avg+sdev)),pch=19,col="black")}
  }
  ###THIS IS WHERE THE OLD PLOTGROWTH FUNCTION BEGAN###
  else{
    if(missing(date)){return(print("Please provide a date"))}
    if(is.data.frame(pars)){
      sizes<-seq(from=pars$z_hatch[date],to=pars$z_inf[date],length.out=sizesToPlot)
      avg<-Growth(sizes,sizes,bt,pars,date=date,muSDOut=T)[,1]-sizes
      sdev<-Growth(sizes,sizes,bt,pars,date=date,muSDOut=T)[,2]
      plot(sizes, avg,
           ylim=range(c(avg-sdev, avg+sdev)),
           pch=19, xlab="Size (mm)", ylab=ifelse(plotSDevs,"Mean growth (mm) +/- SD","Mean growth (mm)"),
           main=paste("Mean daily growth increments \n Ordinal date: ",date,sep=""),
           axes=T,col="lightblue")

      if(!min(sdev)>0){
        points(sizes, avg,
               ylim=range(c(avg-sdev, avg+sdev)),
               pch=19,col="steelblue")}
      else{
        if(plotSDevs==T){arrows(sizes, avg-sdev, sizes, avg+sdev, length=0.05, angle=90, code=3,col="lightblue")}
        points(sizes, avg,ylim=range(c(avg-sdev, avg+sdev)),pch=19,col="steelblue")}}
    else{nParSets<-length(pars)
    avgs<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    sdevs<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
    allsizes<-matrix(NA,nrow=nParSets,ncol=sizesToPlot) #created to store and capture maximum and minimum sizes being plotted to set proper xlimits
    for(i in 1:nParSets){
      allsizes[i,]<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot) #storing sizes to be recalled later for xlimits for plotting
      sizes<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot)
      avgs[i,]<-Growth(sizes,sizes,bt,pars[[i]],date=date,muSDOut=T)[,1]-sizes
      sdevs[i,]<-Growth(sizes,sizes,bt,pars[[i]],date=date,muSDOut=T)[,2] }

    plot(x=sizes,y=avgs[1,],xlim=c(min(allsizes),max(allsizes)),ylim=c(min(avgs)-max(sdevs),max(avgs)+max(sdevs)),col=natecols(nParSets)[1],pch=15, xlab="Size (mm)", ylab=ifelse(plotSDevs,"Mean growth (mm) +/- SD","Mean growth (mm)"),
         main=paste("Mean daily growth increments \n Ordinal date: ",date,sep="")) #Plot command modified to attempt to allow for rescaling for species with different z_hatch and z_inf values

    legend("topright", legend = names(pars), xpd = TRUE,
           horiz = FALSE, col = natecols(nParSets), pch=seq(from=15,to=(15+nParSets)), bty = "n")
    for(i in 1:nParSets){
      if(!min(sdevs[i,])>0){
        points(x=sizes,y=avgs[i,],pch=14+i,col=natecols(nParSets)[i])}
      else{
        if(plotSDevs==T){arrows(sizes, avgs[i,]-sdevs[i,], sizes, avgs[i,]+sdevs[i,], length=0.05, angle=90, code=3,col=natecols(nParSets)[i])}
        points(x=sizes,y=avgs[i,],pch=14+i,col=natecols(nParSets)[i])}}
    }
  }
}



#' @describeIn PlotGrowth This function plots the daily survival probability by size

PlotSurvival<- function (pars,date,sizesToPlot=100) {
  if(missing(date)){return(print("Please provide a date"))}
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  if(is.data.frame(pars)){
    sizes<-seq(from=pars$z_hatch[date],to=pars$z_inf[date],length.out=sizesToPlot)
    surv<-Survival(sizes,pars,date)
    plot(sizes,surv,col="salmon",pch=18,xlab="Size (mm)",ylab="Survival probability",main=paste("Daily survival probability \n Ordinal date: ",date,sep=""))

    points(sizes,surv,col="salmon",pch=18,)}
  else{nParSets<-length(pars)
  survs<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
  for(i in 1:nParSets){
    sizes<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot)
    survs[i,]<-Survival(sizes,pars[[i]],date=date)}

    plot(sizes,survs[1,],ylim=c(min(survs),max(survs)),col=natecols(nParSets)[1],pch=15,xlab="Size (mm)",ylab="Survival probability",main=paste("Daily survival probability \n Ordinal date: ",date,sep=""))

    legend("bottomright", cex=1, legend = names(pars), xpd = TRUE,
           horiz = FALSE, col = natecols(nParSets), pch=seq(from=15,to=(15+nParSets)), bty = "n")
    for(i in 1:nParSets){
    points(sizes,survs[i,],col=natecols(nParSets)[i],pch=18)}
  }
}

#' @describeIn PlotGrowth This function plots the daily size-dependent reproductive output of females per female per day

PlotReproduction<- function (pars,date,sizesToPlot=100) {
  if(missing(date)){return(print("Please provide a date"))}
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  if(is.data.frame(pars)){
    sizes<-seq(from=pars$z_hatch[date],to=pars$z_inf[date],length.out=sizesToPlot)
    repro<-pars$sex_ratio[date]*Spawning(sizes,pars,date)*Fecundity(sizes,pars,date)*Survival(pars$z_hatch[date],pars,date)
    plot(sizes,repro,col="salmon",pch=18,xlab="Size (mm)",ylab="Avg female hatchlings/female/per day ",main=paste("Daily reproductive output \n Ordinal date: ",date,sep=""))

    points(sizes,repro,col="salmon",pch=18)}
  else{nParSets<-length(pars)
  repros<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
  for(i in 1:nParSets){
    sizes<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot)
    repros[i,]<-pars[[i]]$sex_ratio[date]*Spawning(sizes,pars[[i]],date)*Fecundity(sizes,pars[[i]],date)*Survival(pars[[i]]$z_hatch[date],pars[[i]],date)}

  plot(sizes,repros[1,],ylim=c(min(repros),max(repros)),col=natecols(nParSets)[1],pch=15,xlab="Size (mm)",ylab="Avg female hatchlings/female/per day ",main=paste("Daily reproductive output \n Ordinal date: ",date,sep=""))

  legend("topleft", cex=1, legend = names(pars), xpd = TRUE,
         horiz = FALSE, col = natecols(nParSets), pch=seq(from=15,to=(15+nParSets)), bty = "n")
  for(i in 1:nParSets){
    points(sizes,repros[i,],col=natecols(nParSets)[i],pch=18)}
  }
}

#' @describeIn PlotGrowth This function plots daily spawning probability

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

  legend("bottomright", cex=1, legend = names(parsWithSDec), xpd = TRUE,
         horiz = FALSE, col = natecols(nParsWithSDec),lty=1, bty = "n")
  for(i in 1:nParsWithSDec){
    lines(x=1:365,-SDecs[i,],col=natecols(nParsWithSDec)[i],pch=14+i,type="l")}
  }
}

#' @describeIn PlotGrowth This function plots daily growth percents

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
PlotYearlyGrowth<-function(species,sizesToPlot=100){
  #ensure/import species data
  speciesIn<-species #define species (this will normally be passed into function)
  if(speciesIn %in% FishToxTranslator::species_library$common_name){
  species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
  species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
  species_data<-get(species_library$parameter_data)
  sPars<-as.data.frame(t(species_data$value))
  names(sPars)<-t(species_data$id)

  #initialize lists
  z<-c() #mean size after growth
  zm<-c() #min size after growth (2sd below)
  zM<-c() #max size after growth (2sd above)
  g<-c() #growth value
  gm<-c() #min growth value
  gM<-c() #max growth value
  sd2m<-c() #2 standard deviations below
  sd2M<-c() #2 standard deviations above

  #size intervals to measure growth at (usually 100 points between z_hatch and z_inf)
  sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
  z[1]<-zm[1]<-zM[1]<-sPars$z_hatch #initialize starting size as z_hatch

  for(i in 1:365){
    #growth from mean
    g[i]<-Growth(sizes,z[i],bt,sPars,date=1,muSDOut=T)[,1]-z[i]
    #growth from 95min
    gm[i]<-Growth(sizes,zm[i],bt,sPars,date=1,muSDOut=T)[,1]-zm[i]
    sd2m[i]<-Growth(sizes,zm[i],bt,sPars,date=1,muSDOut=T)[,2]
    #growth from 95max
    gM[i]<-Growth(sizes,zM[i],bt,sPars,date=1,muSDOut=T)[,1]-zM[i]
    sd2M[i]<-Growth(sizes,zM[i],bt,sPars,date=1,muSDOut=T)[,2]
    #update sizes with growth
    z[i+1]<-z[i]+g[i]
    zM[i+1]<-min(sPars$z_inf,zM[i]+gM[i]+2*sd2M[i])
    zm[i+1]<-zm[i]+gm[i]-2*sd2m[i]
  }
  #plot sizes, mean growth
  plot(z[1:365],pch=20,xlab="Days from hatch", ylab="Length (mm)",
       main=paste("Yearly Growth Trajectory \n",species," (", species_sci_name,")",sep=""),ylim=c(subset(data, id=="z_hatch")$value-4,subset(data, id=="z_inf")$value+4))
  #use arrows to signify max and min growth
  arrows(2:365, zm[2:365], 2:365, zM[2:365], length=0.05, angle=90, code=3,col="gray")
  #use lines to signify max and minimum size values from parameters
  abline(h=sPars$z_inf,col="lightblue")
  abline(h=sPars$z_hatch,col='lightblue')
  #replot points to have above lines
  points(z[1:365],pch=20)
  #add labels to horizontal lines
  text(x=365/2,y=sPars$z_inf+1.5,paste0("Max length: ",sPars$z_inf,"mm"),col="lightblue")
  text(x=365/2,y=sPars$z_hatch-2.5,paste("Hatch length: ",sPars$z_hatch,"mm"),col="lightblue")
  }
  else{return(print("Non-matching species common name"))}
}
