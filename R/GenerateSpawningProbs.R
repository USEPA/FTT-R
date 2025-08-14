#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Generate Spawning Probabilities
#'
#' This function can be used to simulate daily spawning probability for a population of iteroparous or semelparous spawners based on a limited number of reproductive variables.
#' @param repro_start Onset of reproduction season in ordinal day (Jan 1 = 1 in non-leap year) [integer]
#' @param repro_end End of reproduction season in ordinal day [integer]
#' @param spawns_max_season Maximum number of spawns a female can have in a reproductive season [integer]
#' @param spawn_int Spawning interval or days between spawning events [integer]
#' @param nReps Number of individuals/reps used to generate simulation [integer]
#' @param matReturn True or False controls if a matrix is output. Default is FALSE, TRUE is often used for visualization.  [boolean]
#' @return Daily spawning probability [vector] \cr Option: if(matReturn)=TRUE, output is a (nReps x (repro_end-repro_start)) binary matrix, where an entry of 1 in position i,j represents individual i spawning on day reproductive day j, useful for visualization
#' @note For iteroparous spawners, provide repro_start, repro_end, spawns_max_season, and spawn_int.
#' @note For semelparous spawners, provide repro_start, repro_end, and set spawns_max_season=1
#' @export
#' @family Sub-models

GenerateSpawningProbs<-function(repro_start,repro_end,spawns_max_season,spawn_int,nReps=100000,matReturn=F)
{
  ## Reproductive days
  rDays<-(repro_end+1)-repro_start
  if(spawns_max_season==1){
    prb<-rep(0,365)
    prb[DateDuration(repro_start,repro_end)]<-1/length(DateDuration(repro_start,repro_end))
    return(prb)
  }
   ## Determine possible spawn numbers based on season length, max number of spawns, and spawn_int
  PSN<-c()
  for(i in 0:spawns_max_season){
    if(1+(spawn_int*i)<rDays){
      PSN<-append(PSN,i)
    }
  }
  ## Spawn numbers, uniform sampling of spawning attempts from possible spawn numbers
  SN<-sample(PSN,size=nReps,replace=T)
  SNCts<-as.numeric(table(SN))
  ## Spawning schedule, a binary vector to represent (1) spawning and (0) resting
  SS<-list()
  SS[[1]]<-c(1)
  for(i in 2:max(PSN)){
    SS[[i]]<-append(SS[[i-1]],append(rep(0,spawn_int-1),1))
  }
  ## Possible starting days, a list of vectors of possible starting days depending on spawning schedule and reproductive season lengths
  PSD<-list()
  for(i in 1:max(PSN)){
    PSD[[i]]<-seq(1,(rDays-length(SS[[i]])))
  }
  ## Actual starting days, for each spawn number, uniformly distributed possible starting dates
  ASD<-list()
  for(i in 1:max(PSN)){
    ASD[[i]]<-sample(PSD[[i]],SNCts[i+1],replace=T)
  }
  ## Actual spawning schedules, matrix storing day resolved binary spawning for each rep in sample
  ASS<-matrix(0,nReps,rDays)
  k<-0
  for(i in 1:max(PSN)){
    if(SNCts[i+1]>0){
      for(j in 1:SNCts[i+1]){
        k<-k+1
        ASS[k,ASD[[i]][j]:(ASD[[i]][j]+length(SS[[i]])-1)]<-SS[[i]]
      }}
  }
  ## Daily proportion of spawning females for the year
  prb<-rep(0,365)
  prb[DateDuration(repro_start,repro_end)]<-colSums(ASS)/nReps
  ## Return matrix of spawns on a given day (useful for visualization of spawning)
  ## Default is to NOT return matrix
  if(!matReturn){
    return(prb)
  }
  else{
  return(t(ASS))}
}


# ## This was used to create a list of built-in 'FT.batchspawn' parameters for fathead minnow
# GenerateSpawningProbs.parameters.p.promelas<-c(
#   "repro_start"=od("22May2018"),
#   "repro_end"=od("22Aug2018"),
#   "spawns_max_season"=12,
#   "spawn_int"=3
#   )
# ##Creates an RData file to store the parameters
# devtools::use_data(GenerateSpawningProbs.parameters.p.promelas,overwrite=T)

