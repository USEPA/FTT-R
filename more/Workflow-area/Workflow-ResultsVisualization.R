#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#


library(FishToxTranslator)
getwd()
#setwd("C:/Users/npollesc/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/FTT-R")

#### Initialize lists, vectors, and data.frames
# Create an empty vector to store scenario names
scenarioNames<-vector()
# Create an empty list to store scenario descriptions
scenarioDescriptions<-list()
# Create an empty list to store parameters indexed by scnarioNames
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

#### Create a second baseline for a different species, based on an uploaded .csv species template file
## In this case, it will be bluegill parameters
currentScenarioName<-"Baseline 2" # take input
scenarioNames<-cbind(scenarioNames,currentScenarioName) # append new scenario name to list

# Submit scenario description as character string likely as a textAreaInput
currentScenarioDescription<-"Baseline 2 for testing"
scenarioDescriptions[[currentScenarioName]]<-currentScenarioDescription

#Upload parameters
#testing for new species
newLifeHistory<-read.csv("lifeHistoryTemplate_2024-12-17_Bluegill.csv")
parameters[[currentScenarioName]]<-FishToxTranslator:::TemplateToParameters(newLifeHistory)

# GUI: Run Spawning Algorithm
dailySpawningProb<-GenerateSpawningProbs(repro_start=parameters[[currentScenarioName]]$repro_start[1],
                                         repro_end=parameters[[currentScenarioName]]$repro_end[1],
                                         spawns_max_season=parameters[[currentScenarioName]]$spawns_max_season[1],
                                         spawn_int=parameters[[currentScenarioName]]$spawn_int[1])

# Display plot upon algorithm completion
plot(dailySpawningProb,main="Daily Spawning Probability",xlab="Ordinal Date",ylab="Spawning Probability")

# Add spawning algorithm output to parameter data.frame
parameters[[currentScenarioName]]<-cbind(parameters[[currentScenarioName]],p_spawn=dailySpawningProb)


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



##~~~~~~~~~~~~~~~~~~~~~~~~~##
########### RUN #############
##~~~~~~~~~~~~~~~~~~~~~~~~~##
## Create empty lists

#Stores the runIDs
runIDs<-c()
#stores the modelRun outputs
modelRuns<-list()
#stores the modelRun parameters
modelRunParams<-data.frame(noSizeClasses=NA,solverOrder=NA,isPredet=NA,isUnif=NA,unifN0=NA,predetFileString=NA)
#stores the modelRun parameters and scenarios that have been run
modelRunInfo<-list()


## First is to know which scenarios the user wants to run in the model
# GUI: Begin the GUI with a checkbox populated with Scenario Names
scenarioNames
# Assume the user has checked boxes 1,2
chosenScenarioIndices<-1:2

# Names for chosen scenarios
inputScenariosToRun<-scenarioNames[1:2]

## Get info for runID
## Create runIDs vector
runIDs<-c()
## Get input for runID specify 4 character maximum input
inputRunID<-as.character("PreD")

## CHECK FOR CURRENT RUNID existence, if not a duplicate, use it
runID<-inputRunID

runIDs<-cbind(runIDs,runID)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
### Gather Computational parameters ###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

## create empty list to store model run information

# GUI: Gather number of size classes (integer input)
# Default = 100 Include a note that says: "Note: Changing computational parameters from default values may result in numerical approximation issues and/or long run times"
inputNoSizeClasses<-100

modelRunParams$noSizeClasses<-inputNoSizeClasses
num_size_classes<-inputNoSizeClasses
# Input - Numerical solver order
# Default = 3
inputSolverOrder<-3

modelRunParams$solverOrder<-inputSolverOrder

##  Gather Simulation parameters
# GUI: Initial distribution
# Two Options In List: "Uniform" or "Predetermined"
# #### ~ If "Predetermined" is chosen from the list
#
# ## set the modelRunInfo isPredet to TRUE
# modelRunParams$isPredet<-TRUE
# ## set modelRunInfo isUnif to FALSE and set unifN0 to NA, since it won't be specified
# modelRunParams$isUnif<-FALSE
# modelRunParams$unifN0<-NA
# ## Generate Predetermined starting distribution template
# predetinitialdist<-data.frame(ordinal_date=1:num_size_classes,density_in_class=NA)
# ## Download exposure template data.frame
# write.csv(predetinitialdist,"predetermined_initial_distribution_template.csv",row.names=F)
#
#
# ## GUI: Upload predetermined initial distribution template
# inputDistFileString<-"predetermined_initial_distribution_example.csv"
# ## Store input file string in modelRunInfo
# modelRunParams$predetFileString<-inputDistFileString
#
# ## Upload the predetermined intial distribution
# inputPredeterminedIntitialDist<-read.csv("predetermined_initial_distribution_example.csv")
#
# ## Plot input of predetermined initial distribution
# plot(x=inputPredeterminedIntitialDist[,1],y=inputPredeterminedIntitialDist[,2],main="User-specified initial distribution",xlab="Size class",ylab="Density of individuals")
#
# ## Assign predetermined initial distribution input the proper variable for simulation
# inputz_t_0<-inputPredeterminedIntitialDist[,2]
#

#### Run the scenarios in the scenarioNames list ####
# GUI: Create a "Run Simulation(s)" button
# When button is clicked display the following text: "Model runs take between 2 and 5 minutes each. An alert window
# will be created when runs are completed"

# Initialize lists to store model output
# temp List stores the most recent model runs
# tempOutputs<-list()

# #Set up parallel model runs using package "parallel"
# numCores<-detectCores()
# cl<-makeCluster(numCores)
# clusterExport(cl,"parameters")
# tempOutputs<-parLapply(cl,parameters[inputScenariosToRun],SimulateModel,z_t_0=inputz_t_0)
# stopCluster(cl)
#
# # Add the tempOutputs to the modelRuns list
# modelRuns[[runID]]<-tempOutputs
#
# # Add the model run information to the modelRunInfo list()
# modelRunInfo[[runID]][["modelRunParams"]]<-modelRunParams
# modelRunInfo[[runID]][["modelRunScenarios"]]<-inputScenariosToRun

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Do another model run with Uniform instead of predetermined dist  ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###


# Complete a second run with a new runID and new uniform outputs
# Get input for runID specify 3 character maximum input
inputRunID<-as.character("U")

# CHECK FOR CURRENT RUNID existence, if not a duplicate, use it
runID<-inputRunID

# Append runID to runIDs list
runIDs<-c(runIDs,runID)

# Reinitilize ModelRunParams data.frame() for this modelRun
modelRunParams<-data.frame(noSizeClasses=NA,solverOrder=NA,isPredet=NA,isUnif=NA,unifN0=NA,predetFileString=NA)

# GUI: Gather number of size classes (integer input)
# Default = 100 Include a note that says: "Note: Changing computational parameters from default values may result in numerical approximation issues and/or long run times"
inputNoSizeClasses<-100

modelRunParams$noSizeClasses<-inputNoSizeClasses

# Input - Numerical solver order
# Default = 3
inputSolverOrder<-3

modelRunParams$solverOrder<-inputSolverOrder


modelRunParams$isUnif<-TRUE
modelRunParams$isPredet<-FALSE
modelRunParams$predetFileString<-NA
# GUI: Need number of individuals to start simulation(s)
# Default=100
# Assume input is 100
inputNumberInitInd<-100
modelRunParams$unifN0<-inputNumberInitInd


# Run the model
## Run the model in parallel
# #Set up parallel model runs using package "parallel"
# numCores<-detectCores()
# cl<-makeCluster(numCores)
# clusterExport(cl,"parameters")
# tempOutputs<-parLapply(cl,parameters[inputScenariosToRun],SimulateModel,n_0=inputNumberInitInd)
# stopCluster(cl)
## End Run in paraller

## Run the model
tempOutputs<-lapply(parameters[inputScenariosToRun],SimulateModel,n_0=inputNumberInitInd)
## End run the model


#tempOutputs<-lapply(parameters[inputScenariosToRun],SimulateModel,n_0=inputNumberInitInd)

# Add the tempOutputs to the modelRuns list
modelRuns[[runID]]<-tempOutputs

modelRunInfo[[runID]][["modelRunParams"]]<-modelRunParams
modelRunInfo[[runID]][["modelRunScenarios"]]<-inputScenariosToRun

#### RESULTS ####
## First is to know which scenarios the user wants to run in the model

# GUI: Begin the GUI with a checkbox populated with results Names
resultNames<-names(unlist(modelRuns,recursive=F))

# By default, use all names that provided as input to the run function
chosenResultIndices<-1:2

# Names for chosen scenarios
inputScenariosForResults<-resultNames[chosenResultIndices]

inputScenariosForResults
### Results sub section title: Scenario Results Summary
# Provide button to view the summary table dynamically in main panel for chosen names
summaryTableResults<-SummaryTable(unlist(modelRuns,recursive=F)[inputScenariosForResults])

### Provide buttons to view selected plots
# Projected daily population (# of individuals)
FishToxTranslator:::PlotPopulation(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Projected daily population biomass
FishToxTranslator:::PlotBiomass(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Projected daily mean size in population
FishToxTranslator:::PlotMeanSize(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Plot growth potentials
FishToxTranslator:::PlotGrowthPotential(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Plot Annual Transition Kernels

FishToxTranslator:::PlotTransitionKernel(unlist(modelRuns,recursive=F)[inputScenariosForResults])


View(tempOutputs)

### Results sub section title: Comparison of Scenario Results
# Plot the summary matrices
FishToxTranslator:::PlotSummaryMatrix(unlist(modelRuns,recursive=F)[inputScenariosForResults])
summMats<-SummaryMatrix(unlist(modelRuns,recursive=F)[inputScenariosForResults])
FishToxTranslator:::SummaryTable(unlist(modelRuns,recursive=F)[inputScenariosForResults])


### Now to try to fix the problem when two results/runs have a different number of sizes classes in the discretization.
# Complete a second run with a new runID and new uniform outputs
# Get input for runID specify 3 character maximum input
inputRunID<-as.character("U2")

# CHECK FOR CURRENT RUNID existence, if not a duplicate, use it
runID<-inputRunID

# Append runID to runIDs list
runIDs<-c(runIDs,runID)

# Reinitilize ModelRunParams data.frame() for this modelRun
modelRunParams<-data.frame(noSizeClasses=NA,solverOrder=NA,isPredet=NA,isUnif=NA,unifN0=NA,predetFileString=NA)

# GUI: Gather number of size classes (integer input)
# Default = 100 Include a note that says: "Note: Changing computational parameters from default values may result in numerical approximation issues and/or long run times"
inputNoSizeClasses<-300

modelRunParams$noSizeClasses<-inputNoSizeClasses

# Input - Numerical solver order
# Default = 3
inputSolverOrder<-3

modelRunParams$solverOrder<-inputSolverOrder


modelRunParams$isUnif<-TRUE
modelRunParams$isPredet<-FALSE
modelRunParams$predetFileString<-NA
# GUI: Need number of individuals to start simulation(s)
# Default=100
# Assume input is 100
inputNumberInitInd<-100
modelRunParams$unifN0<-inputNumberInitInd


# Run the model
## Run the model in parallel
# #Set up parallel model runs using package "parallel"
# numCores<-detectCores()
# cl<-makeCluster(numCores)
# clusterExport(cl,"parameters")
# tempOutputs<-parLapply(cl,parameters[inputScenariosToRun],SimulateModel,n_0=inputNumberInitInd)
# stopCluster(cl)
## End Run in paraller

## Run the model
tempOutputs2<-lapply(parameters[inputScenariosToRun],SimulateModel,n_0=inputNumberInitInd,num_size_classes=inputNoSizeClasses)
## End run the model


#tempOutputs<-lapply(parameters[inputScenariosToRun],SimulateModel,n_0=inputNumberInitInd)

# Add the tempOutputs to the modelRuns list
modelRuns[[runID]]<-tempOutputs2

modelRunInfo[[runID]][["modelRunParams"]]<-modelRunParams
modelRunInfo[[runID]][["modelRunScenarios"]]<-inputScenariosToRun


View(modelRuns)

resultNames<-names(unlist(modelRuns,recursive=F))

# By default, use all names that provided as input to the run function
chosenResultIndices<-c(1,3)

# Names for chosen scenarios
inputScenariosForResults<-resultNames[chosenResultIndices]

inputScenariosForResults
### Results sub section title: Scenario Results Summary
# Provide button to view the summary table dynamically in main panel for chosen names
summaryTableResults<-SummaryTable(unlist(modelRuns,recursive=F)[inputScenariosForResults])

### Provide buttons to view selected plots
# Projected daily population (# of individuals)
FishToxTranslator:::PlotPopulation(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Projected daily population biomass
FishToxTranslator:::PlotBiomass(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Projected daily mean size in population
FishToxTranslator:::PlotMeanSize(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Plot growth potentials
FishToxTranslator:::PlotGrowthPotential(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Plot Annual Transition Kernels

FishToxTranslator:::PlotTransitionKernel(unlist(modelRuns,recursive=F)[inputScenariosForResults])

FishToxTranslator:::PlotSummaryMatrix(unlist(modelRuns,recursive=F)[inputScenariosForResults])
summMats<-SummaryMatrix(unlist(modelRuns,recursive=F)[inputScenariosForResults])
FishToxTranslator:::SummaryTable(unlist(modelRuns,recursive=F)[inputScenariosForResults])

