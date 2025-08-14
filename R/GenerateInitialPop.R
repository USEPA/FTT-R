#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Generate an Initial Population
#'
#' This creates an initial size distribution for indiviudals using either a uniform, random uniform, or random normal distribution
#' @param mpts Midpoints of the discretized kernel that the size distribution is being generated for [vector]
#' @param z_hatch Minimum size of individuals [float]
#' @param z_inf Maximum size of individuals [float]
#' @param n_0 Number of individuals to initialize population with [integer]
#' @param init_dist The initial distribution to follow, three options 'Uniform', 'Random Uniform', 'Normal' \cr Uniform : Exactly uniformly distributed among all size classes \cr Random Uniform: Randomly uniformly distributed ~U[z_min,z_max] \cr Normal: Truncated normal distribution between 'z_min' and 'z_max' with mean = 'z_min'+'z_max'/2 and sd=1
#' @return An initial size distribution [vector]
#' @export

GenerateInitialPop<-function(mpts,z_hatch,z_inf,n_0,init_dist="unif"){
  if(init_dist=="Uniform"){
    n0t0<-rep(n_0/length(mpts),length(mpts))}
  else if(init_dist=="Random Uniform"){
    n0<-runif(n_0,min=z_min,max=z_max)
    brks<-seq(z_hatch,z_inf,by=mpts[2]-mpts[1])
    n0h<-hist(n0,breaks=brks,plot=F)
    n0t0<-n0h$counts
  }
  else if(init_dist=="Normal"){
    n0<-rnorm(n_0,mean(c(z_inf,z_hatch)),.1*mean(c(z_inf,z_hatch)))
    brks<-seq(z_hatch,z_inf,by=mpts[2]-mpts[1])
    n0h<-hist(n0,breaks=brks,plot=F)
    n0t0<-n0h$counts
  }
  else{
    print('Please choose "Uniform","Random Uniform", or "Normal" for distribution choice')
  }
  return(n0t0)
  }
