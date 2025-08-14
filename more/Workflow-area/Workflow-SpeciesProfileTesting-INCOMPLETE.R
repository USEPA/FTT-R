#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

### This file is created for testing/creating new species profiles
### New species are imported from a species profile template and temporarily added to the Species Library
### for visualization and simulation

library(FishToxTranslator)
getwd()
#setwd("C:/Users/npollesc/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/FTT-R")

#### Import Species Profile From Template ####

## A few options to explore:
### + Create a temp species in the species_library for species in template
### + Create an argument in the functions to load from a temp object

species_library

#### Initialize lists, vectors, and data.frames
# Create an empty vector to store scenario names
scenarioNames<-vector()
# Create an empty list to store scenario descriptions
scenarioDescriptions<-list()
# Create an empty list to store parameters indexed by scenarioNames
parameters<-list()


# Name the scenario
# GUI: Submit baseline name as character string
currentScenarioName<-"Baseline 1" # take input
scenarioNames<-cbind(scenarioNames,currentScenarioName) # append new scenario name to list

# GUI: Submit scenario description as character string likely as a textAreaInput
currentScenarioDescription<-"Baseline 1 for testing"
scenarioDescriptions[[currentScenarioName]]<-currentScenarioDescription

# Define species
# GUI: Can populate list choices from "species_library" object
speciesChoices<-species_library$common_name # create list options
# GUI: Choose species from list
# If Known Species:
chosenSpecies<-speciesChoices[1] # This chooses "Fathead Minnow" (the only choice as of 1/23/25)
# Gather life history parameters for known species
# Match chosen name to FishToxTranslator data() object associated with choice
species_library$parameter_data[which(species_library$common_name==chosenSpecies)]
chosenSpeciesDataObject<-as.character(species_library$parameter_data[which(species_library$common_name==chosenSpecies)])
chosenLifeHistoryParameters<-get(chosenSpeciesDataObject)

parameters[[currentScenarioName]]<-FishToxTranslator:::TemplateToParameters(chosenLifeHistoryParameters)

## Add on spawning

# Run Spawning Algorithm
dailySpawningProb<-GenerateSpawningProbs(repro_start=parameters[[currentScenarioName]]$repro_start[1],
                                         repro_end=parameters[[currentScenarioName]]$repro_end[1],
                                         spawns_max_season=parameters[[currentScenarioName]]$spawns_max_season[1],
                                         spawn_int=parameters[[currentScenarioName]]$spawn_int[1])

# Display plot upon algorithm completion
plot(dailySpawningProb,main="Daily Spawning Probability",xlab="Ordinal Date",ylab="Spawning Probability")

# Add spawning algorithm output to parameter data.frame
parameters[[currentScenarioName]]<-cbind(parameters[[currentScenarioName]],p_spawn=dailySpawningProb)

View(parameters)

## Create a new scenario used bluegill parameters as built into package now
# Name the scenario
# GUI: Submit baseline name as character string
currentScenarioName<-"Baseline 2" # take input
scenarioNames<-cbind(scenarioNames,currentScenarioName) # append new scenario name to list

# GUI: Submit scenario description as character string likely as a textAreaInput
currentScenarioDescription<-"Baseline 2 for testing"
scenarioDescriptions[[currentScenarioName]]<-currentScenarioDescription

# Define species
# GUI: Can populate list choices from "species_library" object
speciesChoices<-species_library$common_name # create list options
# GUI: Choose species from list
# If Known Species:
chosenSpecies<-speciesChoices[2] # This chooses "Fathead Minnow" (the only choice as of 1/23/25)
# Gather life history parameters for known species
# Match chosen name to FishToxTranslator data() object associated with choice
species_library$parameter_data[which(species_library$common_name==chosenSpecies)]
chosenSpeciesDataObject<-as.character(species_library$parameter_data[which(species_library$common_name==chosenSpecies)])
chosenLifeHistoryParameters<-get(chosenSpeciesDataObject)

parameters[[currentScenarioName]]<-FishToxTranslator:::TemplateToParameters(chosenLifeHistoryParameters)

## Add on spawning

# Run Spawning Algorithm
dailySpawningProb<-GenerateSpawningProbs(repro_start=parameters[[currentScenarioName]]$repro_start[1],
                                         repro_end=parameters[[currentScenarioName]]$repro_end[1],
                                         spawns_max_season=parameters[[currentScenarioName]]$spawns_max_season[1],
                                         spawn_int=parameters[[currentScenarioName]]$spawn_int[1])

# Display plot upon algorithm completion
plot(dailySpawningProb,main="Daily Spawning Probability",xlab="Ordinal Date",ylab="Spawning Probability")

# Add spawning algorithm output to parameter data.frame
parameters[[currentScenarioName]]<-cbind(parameters[[currentScenarioName]],p_spawn=dailySpawningProb)

View(parameters)



# # #### Create a second baseline for a different species, based on an uploaded .csv species template file
# # ## In this case, it will be bluegill parameters
# # currentScenarioName<-"Baseline 2" # take input
# # scenarioNames<-cbind(scenarioNames,currentScenarioName) # append new scenario name to list
# #
# # # Submit scenario description as character string likely as a textAreaInput
# # currentScenarioDescription<-"Baseline 2 for testing"
# # scenarioDescriptions[[currentScenarioName]]<-currentScenarioDescription
# #
# # #Upload parameters
# # #testing for new species
# # newLifeHistory<-read.csv("lifeHistoryTemplate_2024-12-17_Bluegill.csv")
# # parameters[[currentScenarioName]]<-FishToxTranslator:::TemplateToParameters(newLifeHistory)
# #
# # # GUI: Run Spawning Algorithm
# # dailySpawningProb<-GenerateSpawningProbs(repro_start=parameters[[currentScenarioName]]$repro_start[1],
# #                                          repro_end=parameters[[currentScenarioName]]$repro_end[1],
# #                                          spawns_max_season=parameters[[currentScenarioName]]$spawns_max_season[1],
# #                                          spawn_int=parameters[[currentScenarioName]]$spawn_int[1])
# #
# # # Display plot upon algorithm completion
# # plot(dailySpawningProb,main="Daily Spawning Probability",xlab="Ordinal Date",ylab="Spawning Probability")
# #
# # # Add spawning algorithm output to parameter data.frame
# # parameters[[currentScenarioName]]<-cbind(parameters[[currentScenarioName]],p_spawn=dailySpawningProb)
# #
# #
# # ## Add on spawning
# #
# # # Run Spawning Algorithm
# # dailySpawningProb<-GenerateSpawningProbs(repro_start=parameters[[currentScenarioName]]$repro_start[1],
# #                                          repro_end=parameters[[currentScenarioName]]$repro_end[1],
# #                                          spawns_max_season=parameters[[currentScenarioName]]$spawns_max_season[1],
# #                                          spawn_int=parameters[[currentScenarioName]]$spawn_int[1])
# #
# # # Display plot upon algorithm completion
# # plot(dailySpawningProb,main="Daily Spawning Probability",xlab="Ordinal Date",ylab="Spawning Probability")
# #
# # # Add spawning algorithm output to parameter data.frame
# # parameters[[currentScenarioName]]<-cbind(parameters[[currentScenarioName]],p_spawn=dailySpawningProb)
# #
# # View(parameters)
#
# #### Choose functions to modify
# PlotSurvival<- function (pars,date,sizesToPlot=100,ForProfile=F,species=NA) {
#   if(ForProfile){
#     #Create species specific parameter list, sPars
#     speciesIn<-species #define species (this will normally be passed into function)
#     species_data_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$parameter_data #gather species parameters from parameters_master
#     species_sci_name<-subset(FishToxTranslator::species_library, common_name==speciesIn)$scientific_name #gather species sci name from parameters_master
#     species_data<-get(species_data_name)
#     sPars<-as.data.frame(t(species_data$value))
#     names(sPars)<-t(species_data$id)
#
#     sizes<-seq(from=sPars$z_hatch,to=sPars$z_inf,length.out=sizesToPlot)
#     surv<-Survival(sizes,sPars,date=1)
#     plot(sizes,surv,col="black",pch=18,xlab="Size (mm)",ylab="Survival probability",main=paste("Daily survival probability \n",species," (", species_sci_name,")",sep=""))
#
#     points(sizes,surv,col="black",pch=18,)
#
#   }
#   else{
#
#   if(missing(date)){return(print("Please provide a date"))}
#   natecols<-colorRampPalette(c("purple","steelblue","lightgreen","orange"))
#   if(is.data.frame(pars)){
#     sizes<-seq(from=pars$z_hatch[date],to=pars$z_inf[date],length.out=sizesToPlot)
#     surv<-Survival(sizes,pars,date)
#     plot(sizes,surv,col="salmon",pch=18,xlab="Size (mm)",ylab="Survival probability",main=paste("Daily survival probability - Ordinal date: ",date,sep=""))
#
#     points(sizes,surv,col="salmon",pch=18,)}
#   else{nParSets<-length(pars)
#   survs<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
#   sizes<-matrix(NA,nrow=nParSets,ncol=sizesToPlot)
#   for(i in 1:nParSets){
#     sizes[i,]<-seq(from=pars[[i]]$z_hatch[date],to=pars[[i]]$z_inf[date],length.out=sizesToPlot)
#     survs[i,]<-Survival(sizes[i,],pars[[i]],date=date)}
#
#   plot(sizes[i,],survs[1,],xlim=c(min(sizes),max(sizes)),ylim=c(min(survs),max(survs)),col=natecols(nParSets)[1],pch="",xlab="Size (mm)",ylab="Survival probability",main=paste("Daily survival probability - Ordinal date: ",date,sep=""))
#
#   legend("bottomright", cex=1, legend = names(pars), xpd = TRUE,
#          horiz = FALSE, col = natecols(nParSets), pch=seq(from=15,to=(15+nParSets)), bty = "n")
#   for(i in 1:nParSets){
#     points(sizes[i,],survs[i,],col=natecols(nParSets)[i],pch=18)}
#   }
#   for(i in 1:nParSets){
#     abline(v=pars[[i]]$z_inf[date],col=natecols(nParSets)[i])
#     axis(1, at = pars[[i]]$z_inf[date],
#          labels = F,col=natecols(nParSets)[i])
#     mtext("Max size", at=pars[[i]]$z_inf[date],col=natecols(nParSets)[i])
#   }
#   }
# }
#
# #### Test functions using parameters
# PlotGrowth(ForProfile=T,species="Fathead Minnow")
#
# #test if ordering of parameters is changed
# pars2<-list()
# pars2[["base1"]]<-parameters[["Baseline 2"]]
# pars2[["base2"]]<-parameters[["Baseline 1"]]
# pars2
#
# PlotSurvival(pars2,date=1)
# ### The above seems to be functioning properly.
#
# #### Plot growth function modifications
#
# PlotGrowth(pars2,date=1)
#
# #### Now for reproduction
#
# PlotReproduction(ForProfile=T,species="Fathead Minnow")
#
# ### Now for growth trajectories (longer than 1 year)
#
# PlotGrowthTrajectory(species="Bluegill",noOfDays=5*365,minGin=0.05)
#
# ### Create a new growth+Survival plot for annual trajectory
# ## Idea: based on annual growth trajectory, create a corresponding survival probability plot by
# ## plugging those outputs into a cumulative survival calculation
#


PlotGrowth(species="Bluegill",ForProfile=T)
PlotReproduction(species="Bluegill",ForProfile=T)
PlotGrowth()


PlotLengthToMass(species="Blill",Invert=T) # Fix non-matching
PlotGrowthTrajectory(species="Bluell",noOfDays=3*365)
PlotSurvivalTrajectory(species="Fathead Minnow")

# PlotReproduction(species="Fathead Minnow",ForProfile=T)


