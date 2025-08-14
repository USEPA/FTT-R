#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Simulate Model
#'
#' Uses numerical integration to create a discretized transition kernel and uses the kernel to project daily size distributions and population biomass
#' @param par The data.frame that contains date-indexed kernel function parameters for the scenario being modeled [data.frame]
#' @param n_0 Intial number of individuals in the system to simulate - assumed uniform distribution of these individuals across the size classes and size range [float]
#' @param z_t_0 Initial distribution of sizes (not normalized) - If z_t_0 is specified it will overwrite values of n_0 specified [vector, 1 x num_size_classes]
#' @param growthSurvivalComponent Kernel component representing adult growth and survival [function(z1,z,bt,pars,date)]
#' @param reproductionComponent Kernel component representing reproduction [function(z1,z,bt,pars,date)]
#' @param dates Ordinal dates to create discretized kernel - default is c(1,...,365) [vector]
#' @param num_size_classes The number of classes to discretize to [integer]
#' @param solver_order Order for the gaussian quadrature rule used in numerical integration [integer]
#' @param allOut T/F to indicate if additional outputs should be generated or not. Default = F [boolean]
#' @return A list is returned without outputs that include daily size vectors [1 x num_size_classes], population [float], and biomass [float]. A cumulative transition kernel is also output [matrix, num_size_classes x num_size_classes]. If 'allOut=T', daily transition kernels are output in addition to previous outputs [matrix, num_size_classes x num_size_classes].
#' @export
#' @note Any densities used in the kernels must use the CDF as opposed to the PDF due to the numerical integration technique being utilized. See Ellner et al., 2016 p 171

SimulateModel<-function(par, n_0=100, z_t_0=NA, growthSurvivalComponent=GrowthSurvivalKernel,reproductionComponent=ReproductionKernel, dates=1:365, num_size_classes=100, solver_order=3,allOut=F)
  {
  Z<-list()
  for(i in dates){
    Z[[i]]<-rep(0,num_size_classes)}  # Initialize a list of size distribution vectors for each time-step
  B<-vector() # Initialize a vector for total biomass at each time step
  N<-vector() # Initialize a vector for population at each time step
  FTs_out <- list() # Initialize a list for storing daily discretized kernels
  minColSums<-vector()
  maxColSums<-vector()
  varColSums<-vector()

  upperBound<-par$z_inf[1] #Use maximum and minimum size to set bounds of integration
  lowerBound<-par$z_hatch[1]

  h <- (upperBound - lowerBound)/num_size_classes
  meshpts <- lowerBound + ((1:num_size_classes) - 1/2) * h
  out <- gaussQuadInt(-h/2, h/2, solver_order)
  quad <- expand.grid(x1 = meshpts, x = meshpts, map = seq.int(solver_order))
  quad <- transform(quad, nodes = out$nodes[map], weights = out$weights[map])

  if(!is.na(any(z_t_0))){
  Z[[1]]<-z_t_0} # Assign input as first item in size distribution list
    else(Z[[1]]<-rep(n_0/length(meshpts),length(meshpts))) #Assign initial distribution as uniform with N0 individuals
  N[1]<-sum(Z[[1]])
  B[1]<-as.numeric(Z[[1]]%*%unlist(lapply(meshpts,ZToB,alloIntercept=par$allo_intercept[1],alloSlope=par$allo_slope[1])))

  p<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)
  r<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)
  k<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)

  for (i in dates) {

    p <- with(quad, {
      pvals1 <- growthSurvivalComponent(z1=x1 - h/2, z=x + nodes, bt=B[i], pars=par, date=i)
      pvals2 <- growthSurvivalComponent(z1=x1 + h/2, z=x + nodes, bt=B[i], pars=par, date=i)
      weights * (pvals2 - pvals1)
    })

    r <- with(quad, {
      fvals1 <- reproductionComponent(z1=x1 - h/2, z=x + nodes, bt=B[i], pars=par, date=i)
      fvals2 <- reproductionComponent(z1=x1 + h/2, z=x + nodes, bt=B[i], pars=par, date=i)
      weights * (fvals2 - fvals1)
    })

    dim(p) <- c(num_size_classes, num_size_classes, solver_order)
    dim(r) <- c(num_size_classes, num_size_classes, solver_order)

    r <- apply(r, c(1, 2), sum)/h
    p <- apply(p, c(1, 2), sum)/h
    k <- r + p


    #Update sizes, population, and biomass
    Z[[i+1]]<-as.vector(k%*%Z[[i]])
    N[i+1]<-sum(Z[[i+1]])
    lengthToBiomass<-unlist(lapply(meshpts,ZToB,alloIntercept=par$allo_intercept[i],alloSlope=par$allo_slope[i]))
    B[i+1]<-as.numeric(as.vector(lengthToBiomass)%*%Z[[i]])

    #Summary stats
    minColSums[i]<-min(colSums(k))
    maxColSums[i]<-max(colSums(k))
    varColSums[i]<-var(colSums(k))

    #Turn the Z list into a data.frame() for export


    if(i == min(dates)){
      kCum<-list()
      kCum[[i]]<-k}
    if(length(dates)>1 && i>min(dates))kCum[[i]]<-kCum[[i-1]]%*%k
    if(allOut){FTs_out[[i]] <- list(sizes=Z, biomass=B, population=N, mincsum=minColSums, maxcsum=maxColSums, varcsum=varColSums, dailyTransitionKernel = k, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts)}
  }

  if(!allOut){
  # zOut<-data.frame(matrix(unlist(Z), nrow=length(Z), ncol=num_size_classes, byrow=T),stringsAsFactors=FALSE)
  # sizeclassnames<-c()
  # for(i in 1:num_size_classes){sizeclassnames[i]<-paste0("z",i)}
  # names(zOut)<-sizeclassnames
  # mptsout<-as.data.frame(t(meshpts))
  # names(mptsout)<-sizeclassnames
  dailySummaryOut<-data.frame(biomass=B,population=N,mincsum=c(minColSums,NA),maxcsum=c(maxColSums,NA),varcsum=c(varColSums,NA))
  FTs_out<-list(sizes=Z, dailySummary=dailySummaryOut, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts, simlength=length(dates))}
  # FTs_out<-list(sizes=Z, biomass=B, population=N, mincsum=minColSums, maxcsum=maxColSums, varcsum=varColSums, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts)}
  return(FTs_out)
}












SimulateModelMulti<-function(par, n_0=100, z_t_0=NA, growthSurvivalComponent=GrowthSurvivalKernel,reproductionComponent=ReproductionKernel, dates=1:365, noOfYears=1, num_size_classes=100, solver_order=3,allOut=F)
{
  Z<-list()
  if(noOfYears==1){
  for(i in dates){
    Z[[i]]<-rep(0,num_size_classes)}  # Initialize a list of size distribution vectors for each time-step
  B<-vector() # Initialize a vector for total biomass at each time step
  N<-vector() # Initialize a vector for population at each time step
  FTs_out <- list() # Initialize a list for storing daily discretized kernels
  minColSums<-vector()
  maxColSums<-vector()
  varColSums<-vector()

  upperBound<-par$z_inf[1] #Use maximum and minimum size to set bounds of integration
  lowerBound<-par$z_hatch[1]

  h <- (upperBound - lowerBound)/num_size_classes
  meshpts <- lowerBound + ((1:num_size_classes) - 1/2) * h
  out <- gaussQuadInt(-h/2, h/2, solver_order)
  quad <- expand.grid(x1 = meshpts, x = meshpts, map = seq.int(solver_order))
  quad <- transform(quad, nodes = out$nodes[map], weights = out$weights[map])

  if(!is.na(any(z_t_0))){
    Z[[1]]<-z_t_0} # Assign input as first item in size distribution list
  else(Z[[1]]<-rep(n_0/length(meshpts),length(meshpts))) #Assign initial distribution as uniform with N0 individuals
  N[1]<-sum(Z[[1]])
  B[1]<-as.numeric(Z[[1]]%*%unlist(lapply(meshpts,ZToB,alloIntercept=par$allo_intercept[1],alloSlope=par$allo_slope[1])))

  p<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)
  r<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)
  k<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)

  for (i in dates) {

    p <- with(quad, {
      pvals1 <- growthSurvivalComponent(z1=x1 - h/2, z=x + nodes, bt=B[i], pars=par, date=i)
      pvals2 <- growthSurvivalComponent(z1=x1 + h/2, z=x + nodes, bt=B[i], pars=par, date=i)
      weights * (pvals2 - pvals1)
    })

    r <- with(quad, {
      fvals1 <- reproductionComponent(z1=x1 - h/2, z=x + nodes, bt=B[i], pars=par, date=i)
      fvals2 <- reproductionComponent(z1=x1 + h/2, z=x + nodes, bt=B[i], pars=par, date=i)
      weights * (fvals2 - fvals1)
    })

    dim(p) <- c(num_size_classes, num_size_classes, solver_order)
    dim(r) <- c(num_size_classes, num_size_classes, solver_order)

    r <- apply(r, c(1, 2), sum)/h
    p <- apply(p, c(1, 2), sum)/h
    k <- r + p


    #Update sizes, population, and biomass
    Z[[i+1]]<-as.vector(k%*%Z[[i]])
    N[i+1]<-sum(Z[[i+1]])
    lengthToBiomass<-unlist(lapply(meshpts,ZToB,alloIntercept=par$allo_intercept[i],alloSlope=par$allo_slope[i]))
    B[i+1]<-as.numeric(as.vector(lengthToBiomass)%*%Z[[i]])

    #Summary stats
    minColSums[i]<-min(colSums(k))
    maxColSums[i]<-max(colSums(k))
    varColSums[i]<-var(colSums(k))

    #Turn the Z list into a data.frame() for export


    if(i == min(dates)){
      kCum<-list()
      kCum[[i]]<-k}
    if(length(dates)>1 && i>min(dates))kCum[[i]]<-kCum[[i-1]]%*%k
    if(allOut){FTs_out[[i]] <- list(sizes=Z, biomass=B, population=N, mincsum=minColSums, maxcsum=maxColSums, varcsum=varColSums, dailyTransitionKernel = k, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts)}
  }

  if(!allOut){
    # zOut<-data.frame(matrix(unlist(Z), nrow=length(Z), ncol=num_size_classes, byrow=T),stringsAsFactors=FALSE)
    # sizeclassnames<-c()
    # for(i in 1:num_size_classes){sizeclassnames[i]<-paste0("z",i)}
    # names(zOut)<-sizeclassnames
    # mptsout<-as.data.frame(t(meshpts))
    # names(mptsout)<-sizeclassnames
    dailySummaryOut<-data.frame(biomass=B,population=N,mincsum=c(minColSums,NA),maxcsum=c(maxColSums,NA),varcsum=c(varColSums,NA))
    FTs_out<-list(sizes=Z, dailySummary=dailySummaryOut, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts)}
  # FTs_out<-list(sizes=Z, biomass=B, population=N, mincsum=minColSums, maxcsum=maxColSums, varcsum=varColSums, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts)}
  return(FTs_out)}

  ####Attempt to create a multiyear simulation####
  else{
    #Assign new dates (rep by years)
    newdates<-1:(max(dates)*noOfYears)
    #Assign new pars(replicated)
    newpars<-newparsout<-pars
    for(y in 1:(noOfYears-1)){
      newparsout<-rbind(newparsout,newpars)
    }
    #update/renumber ordinal days
    newparsout$ordinal_date<-1:(365*noOfYears)
    #overwrite pars and dates from newdates and newparsout
    pars<-newparsout
    dates<-newdates

    #Run same scripts from before but now for longer periods of time
    for(i in dates){
    Z[[i]]<-rep(0,num_size_classes)}  # Initialize a list of size distribution vectors for each time-step
    B<-vector() # Initialize a vector for total biomass at each time step
    N<-vector() # Initialize a vector for population at each time step
    FTs_out <- list() # Initialize a list for storing daily discretized kernels
    minColSums<-vector()
    maxColSums<-vector()
    varColSums<-vector()

    upperBound<-par$z_inf[1] #Use maximum and minimum size to set bounds of integration
    lowerBound<-par$z_hatch[1]

    h <- (upperBound - lowerBound)/num_size_classes
    meshpts <- lowerBound + ((1:num_size_classes) - 1/2) * h
    out <- gaussQuadInt(-h/2, h/2, solver_order)
    quad <- expand.grid(x1 = meshpts, x = meshpts, map = seq.int(solver_order))
    quad <- transform(quad, nodes = out$nodes[map], weights = out$weights[map])

    if(!is.na(any(z_t_0))){
      Z[[1]]<-z_t_0} # Assign input as first item in size distribution list
    else(Z[[1]]<-rep(n_0/length(meshpts),length(meshpts))) #Assign initial distribution as uniform with N0 individuals
    N[1]<-sum(Z[[1]])
    B[1]<-as.numeric(Z[[1]]%*%unlist(lapply(meshpts,ZToB,alloIntercept=par$allo_intercept[1],alloSlope=par$allo_slope[1])))

    p<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)
    r<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)
    k<-matrix(0,nrow=num_size_classes,ncol=num_size_classes)

    for (i in dates) {

      p <- with(quad, {
        pvals1 <- growthSurvivalComponent(z1=x1 - h/2, z=x + nodes, bt=B[i], pars=par, date=i)
        pvals2 <- growthSurvivalComponent(z1=x1 + h/2, z=x + nodes, bt=B[i], pars=par, date=i)
        weights * (pvals2 - pvals1)
      })

      r <- with(quad, {
        fvals1 <- reproductionComponent(z1=x1 - h/2, z=x + nodes, bt=B[i], pars=par, date=i)
        fvals2 <- reproductionComponent(z1=x1 + h/2, z=x + nodes, bt=B[i], pars=par, date=i)
        weights * (fvals2 - fvals1)
      })

      dim(p) <- c(num_size_classes, num_size_classes, solver_order)
      dim(r) <- c(num_size_classes, num_size_classes, solver_order)

      r <- apply(r, c(1, 2), sum)/h
      p <- apply(p, c(1, 2), sum)/h
      k <- r + p


      #Update sizes, population, and biomass
      Z[[i+1]]<-as.vector(k%*%Z[[i]])
      N[i+1]<-sum(Z[[i+1]])
      lengthToBiomass<-unlist(lapply(meshpts,ZToB,alloIntercept=par$allo_intercept[i],alloSlope=par$allo_slope[i]))
      B[i+1]<-as.numeric(as.vector(lengthToBiomass)%*%Z[[i]])

      #Summary stats
      minColSums[i]<-min(colSums(k))
      maxColSums[i]<-max(colSums(k))
      varColSums[i]<-var(colSums(k))

      #Turn the Z list into a data.frame() for export


      if(i == min(dates)){
        kCum<-list()
        kCum[[i]]<-k}
      if(length(dates)>1 && i>min(dates))kCum[[i]]<-kCum[[i-1]]%*%k
      if(allOut){FTs_out[[i]] <- list(sizes=Z, biomass=B, population=N, mincsum=minColSums, maxcsum=maxColSums, varcsum=varColSums, dailyTransitionKernel = k, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts)}
    }

    if(!allOut){
      # zOut<-data.frame(matrix(unlist(Z), nrow=length(Z), ncol=num_size_classes, byrow=T),stringsAsFactors=FALSE)
      # sizeclassnames<-c()
      # for(i in 1:num_size_classes){sizeclassnames[i]<-paste0("z",i)}
      # names(zOut)<-sizeclassnames
      # mptsout<-as.data.frame(t(meshpts))
      # names(mptsout)<-sizeclassnames
      dailySummaryOut<-data.frame(biomass=B,population=N,mincsum=c(minColSums,NA),maxcsum=c(maxColSums,NA),varcsum=c(varColSums,NA))
      FTs_out<-list(sizes=Z, dailySummary=dailySummaryOut, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts)}
    # FTs_out<-list(sizes=Z, biomass=B, population=N, mincsum=minColSums, maxcsum=maxColSums, varcsum=varColSums, cumulativeTransitionKernel=kCum[[i]], midpoints=meshpts)}
    return(FTs_out)

  }
}
