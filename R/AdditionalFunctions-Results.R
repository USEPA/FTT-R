#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#
#
#' Fish Toxicity Translator: Model Results Functions
#'
#' This group of functions are used for summarizing results from Fish Toxicity Translator model runs
#'
#' @describeIn SummaryTable provides a data.frame() with summary results for each model output from the SimulateModel result stored in the 'modelOutputList'
#'
#' @param modelOutputList A list object with the output from \code{\link{SimulateModel}}.  This can be a single \code{SimulateModel} output or a list of \code{SimulateModel} output lists.
#' @return Creates various model result summary objects or plots for Fish Toxicity Translator model scenario runs.
#' @export
SummaryTable <- function(modelOutputList){
  if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){
    summaryDF<-data.frame(StartingPopulation=NA,
                          FinalPopulation=NA,
                          StartingBiomass=NA,
                          FinalBiomass=NA,
                          StartingMeanSize=NA,
                          FinalMeanSize=NA,
                          MinDailyGrowthPotential=NA,
                          MaxDailyGrowthPotential=NA,
                          AnnualLambda=NA,
                          AnnualMinGrowthPotential=NA,
                          AnnualMaxGrowthPotential=NA)

      summaryDF$MaxDailyGrowthPotential<-max(modelOutputList$dailySummary$maxcsum[1:365])
      summaryDF$MinDailyGrowthPotential<-min(modelOutputList$dailySummary$mincsum[1:365])
      summaryDF$StartingPopulation<-modelOutputList$dailySummary$population[1]
      summaryDF$FinalPopulation<-modelOutputList$dailySummary$population[366]
      summaryDF$StartingBiomass<-modelOutputList$dailySummary$biomass[1]
      summaryDF$FinalBiomass<-modelOutputList$dailySummary$biomass[366]
      summaryDF$StartingMeanSize<-modelOutputList$midpoints%*%modelOutputList[["sizes"]][[1]]/sum(modelOutputList[["sizes"]][[1]])
      summaryDF$FinalMeanSize<-modelOutputList$midpoints%*%modelOutputList[["sizes"]][[366]]/sum(modelOutputList[["sizes"]][[366]])
      summaryDF$AnnualLambda<-Re(eigen(modelOutputList$cumulativeTransitionKernel)$values[1])
      summaryDF$AnnualMaxGrowthPotential<-max(colSums(modelOutputList$cumulativeTransitionKernel))
      summaryDF$AnnualMinGrowthPotential<-min(colSums(modelOutputList$cumulativeTransitionKernel))
    }
  else{
  nModels<-length(modelOutputList)
  summaryDF<-data.frame(ScenarioName=rep(NA,nModels),
                        StartingPopulation=rep(NA,nModels),
                        FinalPopulation=rep(NA,nModels),
                        StartingBiomass=rep(NA,nModels),
                        FinalBiomass=rep(NA,nModels),
                        StartingMeanSize=rep(NA,nModels),
                        FinalMeanSize=rep(NA,nModels),
                        MinDailyGrowthPotential=rep(NA,nModels),
                        MaxDailyGrowthPotential=rep(NA,nModels),
                        AnnualLambda=rep(NA,nModels),
                        AnnualMinGrowthPotential=rep(NA,nModels),
                        AnnualMaxGrowthPotential=rep(NA,nModels))

  for(i in 1:nModels){
  summaryDF$ScenarioName[i]<-names(modelOutputList)[i]
  summaryDF$MaxDailyGrowthPotential[i]<-max(modelOutputList[[i]]$dailySummary$maxcsum[1:365])
  summaryDF$MinDailyGrowthPotential[i]<-min(modelOutputList[[i]]$dailySummary$mincsum[1:365])
  summaryDF$StartingPopulation[i]<-modelOutputList[[i]]$dailySummary$population[1]
  summaryDF$FinalPopulation[i]<-modelOutputList[[i]]$dailySummary$population[366]
  summaryDF$StartingBiomass[i]<-modelOutputList[[i]]$dailySummary$biomass[1]
  summaryDF$FinalBiomass[i]<-modelOutputList[[i]]$dailySummary$biomass[366]
  summaryDF$StartingMeanSize[i]<-modelOutputList[[i]]$midpoints%*%modelOutputList[[i]][["sizes"]][[1]]/sum(modelOutputList[[i]][["sizes"]][[1]])
  summaryDF$FinalMeanSize[i]<-modelOutputList[[i]]$midpoints%*%modelOutputList[[i]][["sizes"]][[366]]/sum(modelOutputList[[i]][["sizes"]][[366]])
  summaryDF$AnnualLambda[i]<-Re(eigen(modelOutputList[[i]]$cumulativeTransitionKernel)$values[1])
  summaryDF$AnnualMaxGrowthPotential[i]<-max(colSums(modelOutputList[[i]]$cumulativeTransitionKernel))
  summaryDF$AnnualMinGrowthPotential[i]<-min(colSums(modelOutputList[[i]]$cumulativeTransitionKernel))
  }}
  is.num <- sapply(summaryDF, is.numeric)
  summaryDF[is.num]<-lapply(summaryDF[is.num],round,3)
return(summaryDF)
  }

#' @describeIn SummaryTable Function creates a list of matrices for comparing each \code{\link{SummaryTable}} element to corresponding summary elements from each scenario included in 'modelOutputList'
#' @export
SummaryMatrix<-function(modelOutputList){
  if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){return(print("Please supply a model output list with for at least two scenarios for comparison to generate summary matrices"))}
if(length(modelOutputList)<2){return(print("Please supply a model output list with for at least two scenarios for comparison to generate summary matrices"))}
summ<-SummaryTable(modelOutputList)
stlen<-ncol(summ)
scenlen<-nrow(summ)
sumMat<-list()
for(k in 2:stlen){
  sumMat[[k-1]]<-matrix(NA,nrow=scenlen,ncol=scenlen)
  for(i in 1:scenlen){
    for(j in 1:scenlen){
      #if(i==j){sumMat[[k-1]][i,j]<-summ[i,k]}
      if(!i==j){sumMat[[k-1]][i,j]<-as.numeric(summ[i,k]/summ[j,k])}}}
  rownames(sumMat[[k-1]])<-summ[,1]
  colnames(sumMat[[k-1]])<-summ[,1]}
names(sumMat)<-names(summ)[2:stlen]
return(sumMat)
}

#' @describeIn SummaryTable Function creates a series of plot.matrix images to compare each \code{\link{SummaryTable}} element to corresponding summary elements from each scenario included in 'modelOutputList'
#' @export
PlotSummaryMatrix<-function(modelOutputList){
  if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){return(print("Please supply a model output list with for at least two scenarios for comparison to generate summary matrices"))}
  if(length(modelOutputList)<2){return(print("Please supply a model output list with for at least two scenarios for comparison to generate summary matrices"))}
  natesummcols<-colorRampPalette(c("purple","steelblue","white","orange","maroon"))
  sumMat<-SummaryMatrix(modelOutputList)
  sumTab<-SummaryTable(modelOutputList)
  sumMatLen<-length(sumMat)
  scenarioNames<-rownames(sumMat[[1]])
  scenLen<-length(rownames(sumMat[[1]]))

  ## Set plot margins and multi-plot arrangement
  par(mar=c(5.1, 4.1, 4.1, 4.1))
  par(mfrow=c(3,4))#Assumes there are 11 summary matrices to plot
  xl <- 1.75
  yb <- 1
  xr <- 2
  yt <- 2
  plot(NA,type="n",xlab="",ylab="",xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n",main="PlotSummaryMatrix: Row to Column Ratios \n Diagonals are Scenario Values",cex=1.1)
  rect(
    xl,
    head(seq(yb,yt,(yt-yb)/11),-1),
    xr,
    tail(seq(yb,yt,(yt-yb)/11),-1),
    col=natesummcols(12)
  )
    mtext(c(0,.1,.25,.5,.75,.99,1.01,2,5,10,100,1000),padj=.1,side=4,at=c(1,(head(seq(yb,yt,(yt-yb)/11),-1))+(1/13)),las=2,cex=.7)
    mtext(strtrim(c("Scenario Key:",paste(LETTERS[1:scenLen],"-",scenarioNames)),width=20),font=c(2,rep(3,scenLen)),adj=0,line=0,side=2,at=rev(c(1,(head(seq(yb,yt,(yt-yb)/11),-1))+(1/13)))[1:(scenLen+1)],las=2,cex=.8)
    for(i in 1:sumMatLen){
    rownames(sumMat[[i]])<-colnames(sumMat[[i]])<-LETTERS[1:scenLen]
      plot(sumMat[[i]],breaks=c(0,.1,.25,.5,.75,.99,1.01,2,5,10,100,1000),xlab="",ylab="",las=1,border=NA,na.print=F,cex.axis=.75,col=natesummcols(12),main=names(sumMat)[i],digits=4,fmt.cell='%.2f',fmt.key='%.2f',key=NULL)
      text(x=(1:scenLen),y=(scenLen:1),labels=paste(LETTERS[1:scenLen],"= \n",sprintf('%.2f',sumTab[1:scenLen,i+1]),sep=""),font=2)
      }
  par(mar=c(5.1, 4.1, 4.1, 2.1)) #Reset plot pars
  par(mfrow=c(1,1))
}


#' @describeIn SummaryTable Function plots daily biomass results
#' @export
PlotBiomass<- function (modelOutputList,relative=F){
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))

  if(relative==F){

  if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){
    plot(x=1:366,y=modelOutputList$dailySummary$biomass,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Population Biomass",main="Projected Daily Population Biomass")

    points(x=1:366,y=modelOutputList$dailySummary$biomass,col=natecols(1),pch=18)}
  else{
    nModels<-length(modelOutputList)
  biomasses<-matrix(NA,nrow=nModels,ncol=366)
  for(i in 1:nModels){
    biomasses[i,]<-modelOutputList[[i]]$dailySummary$biomass}
  plot(x=1:366,biomasses[1,],ylim=c(min(biomasses),max(biomasses)),col=natecols(nModels)[1],pch=15,xlab="Ordinal date",ylab="Population biomass",main="Projected Daily Population Biomass")

  legend("bottomleft", cex=1, legend = names(modelOutputList), xpd = TRUE,
         horiz = FALSE, col = natecols(nModels), pch=seq(from=15,to=(15+nModels)), bty = "n")
  for(i in 1:nModels){
    points(x=1:366,biomasses[i,],col=natecols(nModels)[i],pch=14+i)}
  }
  }
  else if(relative==T){
    if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){
      plot(x=1:366,y=modelOutputList$dailySummary$relativeBiomass,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Population Biomass (% of Baseline)",main="Projected Daily Population Biomass (% of Baseline)")

      points(x=1:366,y=modelOutputList$dailySummary$relativeBiomass,col=natecols(1),pch=18)}
    else{
      nModels<-length(modelOutputList)
      biomasses<-matrix(NA,nrow=nModels,ncol=366)
      for(i in 1:nModels){
        biomasses[i,]<-modelOutputList[[i]]$dailySummary$relativeBiomass}
      plot(x=1:366,biomasses[1,],ylim=c(min(biomasses),max(biomasses)),col=natecols(nModels)[1],pch=15,xlab="Ordinal date",ylab="Population biomass (% of Baseline)",main="Projected Daily Population Biomass (% of Baseline)")

      legend("bottomleft", cex=1, legend = names(modelOutputList), xpd = TRUE,
             horiz = FALSE, col = natecols(nModels), pch=seq(from=15,to=(15+nModels)), bty = "n")
      for(i in 1:nModels){
        points(x=1:366,biomasses[i,],col=natecols(nModels)[i],pch=14+i)}
    }
  }
}

#' @describeIn SummaryTable Function plots daily population projection results
#' @export
PlotPopulation<- function (modelOutputList,relative=F){
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))

  if(relative==F){

  if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){
    plot(x=1:366,y=modelOutputList$dailySummary$population,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Population (# of individuals)",main="Projected Daily Population")

    points(x=1:366,y=modelOutputList$dailySummary$population,col=natecols(1),pch=18)}
  else{
    nModels<-length(modelOutputList)
    populations<-matrix(NA,nrow=nModels,ncol=366)
    for(i in 1:nModels){
      populations[i,]<-modelOutputList[[i]]$dailySummary$population}

    plot(x=1:366,populations[1,],ylim=c(min(populations),max(populations)),col=natecols(nModels)[1],pch=15,xlab="Ordinal date",ylab="Population (# of individuals)",main="Projected Daily Population")

    legend("bottomleft", cex=1, legend = names(modelOutputList), xpd = TRUE,
           horiz = FALSE, col = natecols(nModels), pch=seq(from=15,to=(15+nModels)), bty = "n")
    for(i in 1:nModels){
      points(x=1:366,populations[i,],col=natecols(nModels)[i],pch=14+i)}

  }
  }
  else if(relative==T){
    if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){
      plot(x=1:366,y=modelOutputList$dailySummary$relativePopulation,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Population (% of Baseline) ",main="Projected Daily Population (% of Baseline)")

      points(x=1:366,y=modelOutputList$dailySummary$relativePopulation,col=natecols(1),pch=18)}
    else{
      nModels<-length(modelOutputList)
      populations<-matrix(NA,nrow=nModels,ncol=366)
      for(i in 1:nModels){
        populations[i,]<-modelOutputList[[i]]$dailySummary$relativePopulation}

      plot(x=1:366,populations[1,],ylim=c(min(populations),max(populations)),col=natecols(nModels)[1],pch=15,xlab="Ordinal date",ylab="Population (% of Baseline)",main="Projected Daily Population (% of Baseline)")

      legend("bottomleft", cex=1, legend = names(modelOutputList), xpd = TRUE,
             horiz = FALSE, col = natecols(nModels), pch=seq(from=15,to=(15+nModels)), bty = "n")
      for(i in 1:nModels){
        points(x=1:366,populations[i,],col=natecols(nModels)[i],pch=14+i)}

    }
  }
}


#' @describeIn SummaryTable Function plots the daily mean size of individuals in the population
#' @export
PlotMeanSize<- function(modelOutputList){
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){
    meanSize<-c()
    for(i in 1:366){
    meanSize[i]<-modelOutputList$midpoints%*%modelOutputList[["sizes"]][[i]]/sum(modelOutputList[["sizes"]][[i]])}
    plot(x=1:366,y=meanSize,col=natecols(1),pch=18,xlab="Ordinal date",ylab="Mean size",main="Projected Daily Mean Size")

    points(x=1:366,y=meanSize,col=natecols(1),pch=18)}
  else{
    nModels<-length(modelOutputList)
    meanSizes<-matrix(NA,nrow=nModels,ncol=366)
    for(i in 1:nModels){
      meanSize<-c()
      for(j in 1:366){
        meanSize[j]<-modelOutputList[[i]]$midpoints%*%modelOutputList[[i]][["sizes"]][[j]]/sum(modelOutputList[[i]][["sizes"]][[j]])}
        meanSizes[i,]<-meanSize
        }

    plot(x=1:366,meanSizes[1,],ylim=c(min(meanSizes),max(meanSizes)),col=natecols(nModels)[1],pch=15,xlab="Ordinal date",ylab="Mean size",main="Projected Daily Mean Size")

    legend("topright", cex=1, legend = names(modelOutputList), xpd = TRUE,
           horiz = FALSE, col = natecols(nModels), pch=seq(from=15,to=(15+nModels)), bty = "n")
    for(i in 1:nModels){
      points(x=1:366,meanSizes[i,],col=natecols(nModels)[i],pch=14+i)}
  }
}

#' @describeIn SummaryTable This function plots the daily minimum and maximum column sums to provide bounds on growth potential
#' @export
PlotGrowthPotential<- function(modelOutputList){
  natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
  if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){
    dates <- 1:length(modelOutputList$dailySummary$mincsum[1:365])
    mincsums <- modelOutputList$dailySummary$mincsum[1:365]
    maxcsums <- modelOutputList$dailySummary$maxcsum[1:365]
    plot(dates,mincsums,type="b",col=natecols(1),bty="L",pch=15,lwd=2,xlab="Ordinal date",ylab="Growth potential (females/female/day)",main="Projected Daily Bounds on Growth Potentials",ylim=c(min(modelOutputList$mincsum[1:365]),max(modelOutputList$maxcsum[1:365])))

    points(dates,maxcsums,type="b",col=natecols(1),lwd=2)
    polygon(c(dates,rev(dates)),c(maxcsums,rev(mincsums)),col=adjustcolor(natecols(1),alpha.f=0.5))}
  else{
    nModels<-length(modelOutputList)
    mincsums<-matrix(NA,nrow=nModels,ncol=365)
    maxcsums<-matrix(NA,nrow=nModels,ncol=365)
    for(i in 1:nModels){
      mincsums[i,]<-modelOutputList[[i]]$dailySummary$mincsum[1:365]
      maxcsums[i,]<-modelOutputList[[i]]$dailySummary$maxcsum[1:365]
    }
    dates<-1:length(modelOutputList[[1]]$dailySummary$mincsum[1:365])
    plot(dates,mincsums[1,],type="b",bty="L",pch=15,xlab="Ordinal date",ylab="Growth potential (females/female/day)",main="Projected Daily Bounds on Growth Potentials", ylim=c(min(mincsums),max(maxcsums)),col=natecols(nModels)[1])

    #points(dates,maxcsums[1,],type="l",col=natecols(1))
    #polygon(c(dates,rev(dates)),c(maxcsums[1,],rev(mincsums[1,])),col=adjustcolor(natecols(nModels)[i],alpha.f=0.75),density=50)
    legend("topleft", cex=1, legend = names(modelOutputList), xpd = TRUE,
           horiz = FALSE, col = natecols(nModels), pch=seq(from=15,to=(15+nModels)), bty = "n")
    for(i in 1:nModels){
      lines(dates,mincsums[i,],lwd=2,col=natecols(nModels)[i],type="b",pch=14+i)
      lines(dates,maxcsums[i,],lwd=2,col=natecols(nModels)[i],type="b",pch=14+i)
      polygon(c(dates,rev(dates)),c(maxcsums[i,],rev(mincsums[i,])),col=adjustcolor(natecols(nModels)[i],alpha.f=0.5))}
  }
}

#' @describeIn SummaryTable Function to plot discretized transition kernels
# This function is adapted from the Ellner et al., MatrixImage function
#' @param A for [KernelImage] matrix for plotting [matrix]
#' @param x for [KernelImage] specify x length. Default = NULL [vector]
#' @param y for [KernelImage] specify y length. Default = NULL [vector]
#' @param col for [KernelImage] colorRampPalette for plot. Default color palette is provided [colorRampPalette]
#' @param just.legend for [KernelImage] just plot legend? Default = FALSE [boolean]
#' @param bw for [KernelImage] black and white color? Default = FALSE [boolean]
#' @param my.x.axis for [KernelImage] provide own x axis labels. Default = NA
#' @param matLabels for [KernelImage] provide own matrix(?) labels. Default = NULL
#' @param do.contour for [KernelImage] include contour lines? Default = TRUE [boolean]
#' @param do.legend for [KernelImage] include legend? Default = TRUE [boolean]
#' @param ... for [KernelImage] specify additional arguments to image() function
#' @export
KernelImage<-function(A, x=NULL, y=NULL, col=rev(colorRampPalette(c('red','yellow','green','blue','white'))(100)),
                     just.legend=FALSE,bw=FALSE, my.x.axis=NA, matLabels=NULL, do.contour=TRUE, do.legend=TRUE,...) {
  if(do.legend) layout(mat=cbind(matrix(1,5,5),rep(2,5)));
  par(mar=c(6,5,3,2));
  if(is.null(x)) x=1:ncol(A);
  if(is.null(y)) y=1:nrow(A);
  nx=length(x); ny=length(y);
  x1=c(1.5*x[1]-0.5*x[2],1.5*x[nx]-0.5*x[nx-1]);
  y1=c(1.5*y[1]-0.5*y[2],1.5*y[ny]-0.5*y[ny-1]);
  if(bw) col=grey( (200:50)/200 );
  if(!just.legend){image(list(x=x,y=y,z=t(A)),xlim=x1,ylim=rev(y1),col=col,cex.axis=1.5,cex.lab=1.5,bty="u",...);
    if(!is.na(any(my.x.axis))){
      axis(side=1,at=x,labels=my.x.axis)
      abline(v=ny*(1:(nx/ny)-1))}
    if(!is.null(matLabels)){
      text(x=((ny/2)+ny*(0:((nx/ny)-1))),y=.05*ny,labels=matLabels,font=2)
    }
    abline(v=range(x1)); abline(h=range(y1));}
  if(do.contour) contour(x,y,t(A),nlevels=5,labcex=1.2,add=TRUE);

  if(do.legend) {
    l.y=seq(min(A),max(A),length=100);
    par(mar=c(6,2,3,2))
    image(list(x=1:2,y=l.y,z=rbind(l.y,l.y)),col=col,bty="o",xaxt="n",yaxt="n");
    axis(side=2,cex.axis=1.5,at=pretty(seq(min(A),max(A),length=10)));
  }
}

#' @describeIn SummaryTable Function plots cumulative discretized transition Kernels
#' @export
PlotTransitionKernel<-function(modelOutputList){
  # This is a check to see if the modelOutputList contains only a single model result
  if(any(names(modelOutputList) %in% c('sizes', 'biomass', 'population', "mincsum", 'maxcsum', 'varcsum', 'cumulativeTransitionKernel', 'midpoints'))){
    KernelImage(modelOutputList$cumulativeTransitionKernel)
    par(mar=c(5.1, 4.1, 4.1, 2.1)) #Reset plot pars
    par(mfrow=c(1,1))}
  else{
    nModels<-length(modelOutputList)
    rowLen<-nrow(modelOutputList[[1]]$cumulativeTransitionKernel)
    colLen<-nrow(modelOutputList[[1]]$cumulativeTransitionKernel)
    allColLen<-colLen*nModels
    matList<-list()
    for(i in 1:nModels){
      matList[[i]]<-as.matrix(modelOutputList[[i]]$cumulativeTransitionKernel)
    }
    allMats<-do.call(cbind,matList)
    KernelImage(allMats,my.x.axis=rep(1:colLen,nModels),main="Annual Transition Kernel",xlab="Size Class (Day 1)",ylab="Size Class (Day 365)",matLabels=names(modelOutputList),xaxt='n')
    mtext(side=4,"Transition density",line=.5)
    # Units on transition matrix are females/female (Needs to be verified)
    par(mar=c(5.1, 4.1, 4.1, 2.1)) #Reset plot pars
    par(mfrow=c(1,1))}
}

#' @describeIn SummaryTable Formats scenario parameters for exporting to excel documents
#' @param scenarioToExport for [FormatExportScenario] the scenario name within the parameters (parsList) to export [string]
#' @param parsList for [FormatExportScenario] parameters list object to be used to search for parameters associated with scenarioToExport [list]
#' @param descList for [FormatExportScenario] descriptions list object to be used to search for description associated wth scenarioToExprt [list]
#' @export
FormatExportScenario<-function(scenarioToExport,parsList=parameters,descList=scenarioDescriptions){
  name<-paste0(scenarioToExport)
  ifelse(!is.null(descList[[scenarioToExport]]),
         desc<-paste0(descList[[scenarioToExport]]),desc<-"No Scenario Description")
  exportDate<-paste0(date())
  versionInfo<-paste0(version$version.string)
  scenarioPars<-parsList[[scenarioToExport]]
  exportList<-list(Metadata=data.frame(Field=rbind("Scenario Name","Scenario Description","Scenario Exported","Version Info"), Value=rbind(name,desc,exportDate,versionInfo)),Parameters=scenarioPars)
  return(exportList)
}

#' @describeIn SummaryTable Formats results parameters for exporting to excel documents
#' @param runID for [FormatExportResults] completed simulation run name string to gather info from resultsList [string]
#' @param scenarioID for [FormatExportResults] completed scenario name string within to gather information from results list along with runID [string]
#' @param scenDescList for [FormatExportResults] completed list that is storing scenario descriptions. Default = scenarioDescriptions [list]
#' @param resultsList for [FormatExportResults] completed list that is storing results from simulations. Default = modelRuns [list]
#' @param runInfoList for [FormatExportResults] completed list that is storing simulation/model run information. Default = modelRunInfo [list]
#' @export
FormatExportResults<-function(runID,scenarioID,scenDescList=scenarioDescriptions,resultsList=modelRuns,runInfoList=modelRunInfo){
  exportList<-list()
  exportDate<-paste0(date())
  versionInfo<-paste0(version$version.string)
  #convert size list to data.frame
  sizesOut<-as.data.frame(do.call(rbind, resultsList[[runID]][[scenarioID]]$sizes))
  kernelOut<-as.data.frame.matrix(resultsList[[runID]][[scenarioID]]$cumulativeTransitionKernel)
  midpointsOut<-as.data.frame.vector(resultsList[[runID]][[scenarioID]]$midpoints)
  names(midpointsOut)<-"midpoints"
  exportList<-list(Metadata=data.frame(Field=rbind("Run ID","Scenario Name","Scenario Description","Export Date","Version Info"), Value=rbind(runID,scenarioID,scenDescList[[scenarioID]],exportDate,versionInfo)),
                   ModelRunInfo=runInfoList[[runID]][["modelRunParams"]],
                   dailySummary=resultsList[[runID]][[scenarioID]]$dailySummary,
                   midpoints=midpointsOut,
                   sizes=sizesOut,
                   annualKernel=kernelOut)
  return(exportList)
}
