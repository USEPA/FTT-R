#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#



##~~~~~~~~~~~~~~~~~~~~~~~~~##
######## FRONT MATTER #######
##~~~~~~~~~~~~~~~~~~~~~~~~~##

#### Load packages and data, set working directory
library(FishToxTranslator)

## Set working directory
setwd("~/GitHub/FTT-R/more/Workflow-area")

# View datasets included in the FishToxTranslator package
#View(parameters_master) # Master parameter list with descriptions of all parameters
#View(species_library) # Species library list (only includes fathead minnow currently)
#View(life_history_parameters_p.promelas) # Parameters for fathead minnow

#### Initialize lists, vectors, and data.frames
# Create an empty vector to store scenario names
scenarioNames<-vector()
# Create an empty list to store scenario descriptions
scenarioDescriptions<-list()
# Create an empty list to store parameters indexed by scnarioNames
parameters<-list()

##~~~~~~~~~~~~~~~~~~~~~~~~~##
######### BASELINE ##########
##~~~~~~~~~~~~~~~~~~~~~~~~~##

# Name the scenario
# GUI: Submit baseline name as character string
currentScenarioName<-"Baseline" # take input
scenarioNames<-cbind(scenarioNames,currentScenarioName) # append new scenario name to list

# GUI: Submit scenario description as character string likely as a textAreaInput
currentScenarioDescription<-"This is the baseline scenario for Fathead Minnow using the default parameters for growth, reproduction, and survival"
scenarioDescriptions[[currentScenarioName]]<-currentScenarioDescription

# Define species
# GUI: Can populate list choices from "species_library" object
speciesChoices<-species_library$common_name # create list options
# GUI: Choose species from list
# If Known Species:


chosenSpecies<-speciesChoices[1] # This chooses "Fathead Minnow" (the only choice)
# Gather life history parameters for known species
# Match chosen name to FishToxTranslator data() object associated with choice
species_library$parameter_data[which(species_library$common_name==chosenSpecies)]
chosenSpeciesDataObject<-as.character(species_library$parameter_data[which(species_library$common_name==chosenSpecies)])
chosenLifeHistoryParameters<-get(chosenSpeciesDataObject)
parameters[[currentScenarioName]]<-TemplateToParameters(chosenLifeHistoryParameters)

# GUI: Run Spawning Algorithm
dailySpawningProb<-GenerateSpawningProbs(repro_start=parameters[[currentScenarioName]]$repro_start[1],
                      repro_end=parameters[[currentScenarioName]]$repro_end[1],
                      spawns_max_season=parameters[[currentScenarioName]]$spawns_max_season[1],
                      spawn_int=parameters[[currentScenarioName]]$spawn_int[1])

# Display plot upon algorithm completion
plot(dailySpawningProb,main="Daily Spawning Probability",xlab="Ordinal Date",ylab="Spawning Probability")

# Add spawning algorithm output to parameter data.frame
parameters[[currentScenarioName]]<-cbind(parameters[[currentScenarioName]],p_spawn=dailySpawningProb)


##~~~~~~~~~~~~~~~~~~~~~~~~~##
######### STRESSORS #########
##~~~~~~~~~~~~~~~~~~~~~~~~~##

## In this section, I run through Stressor Scenario Creation
## I begin with Chemical Stressor - TCEM, follow by Chemical Stressor with pre-determined effects, and end with Winter

#### ~ Chemical Stressor - TCEM ####

# Select scenario to apply stressors to

# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[1] #Assuming user chooses the 1st scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"TCEM_diaz_survival"
# Add the new scenario name to the list of the scenario names
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the a new stressor parameter set by copying the underlying scenario, indexed using the input name above
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[1] #Choose chemical stressor

# Chemical: Survival - First needs daily chemical exposure concentrations

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("workflow-data/diaz_exposure.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is TCEM Gather TCEM parameters:
# Can find all needed TCEM parameters by using the parameters_master and matching the string "TCEM" in the called_by category
TCEMNeeds<-parameters_master$id[grepl("TCEM",parameters_master$called_by)]

TCEMNeeds
# Can filter this against known/entered parameters in the scenario to determine which are needed
TCEMInputsRequest<-TCEMNeeds[!TCEMNeeds %in% names(parameters[[inputStressorScenarioName]])]
# Needed parameters are...
as.character(TCEMInputsRequest)
# Info about these parameters can be accessed (And displayed) using the following:
parameters_master$description[parameters_master$id %in% as.character(TCEMInputsRequest)]
# Create empty columns to append needed TCEM parameters
parameters[[inputStressorScenarioName]][,as.character(TCEMInputsRequest)]<-NA
# GUI: Get TCEM Parameters and append to parameters data.frame

parameters[[inputStressorScenarioName]]$chem_id<-"Example Chemical"
parameters[[inputStressorScenarioName]]$lc_percent<-.5
parameters[[inputStressorScenarioName]]$lc_conc<-2.5

# Run the TCEM algorithm
TCEMOutput<-TCEM(exposure_concentrations=parameters[[inputStressorScenarioName]]$exp_concentrations,lc_con=parameters[[inputStressorScenarioName]]$lc_con[1],lc_percent=parameters[[inputStressorScenarioName]]$lc_percent[1])

# Plot the TCEM predicted daily survival decrement
# Note: The negative of the TCEM function output is plotted here to show survival decrement (although that value is positive)
plot(-TCEMOutput,main="TCEM Daily Survival Decrements",xlab="Ordinal Date",ylab="Daily Survival Decrement",type="l")

# Add TCEM output to parameters data.frame
parameters[[inputStressorScenarioName]]$survival_decrement<-TCEMOutput

# # # GUI: Download Formatted Scenario Parameter File
# # Prompt download window
# write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)


#### ~ Chemical: Survival - Stressor 2 - Pre-determined effects ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[1] #Assuming user chooses the 1st scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"GUTS_diaz_survival"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[1] #Choose chemical stressor

# Chemical: Survival - Stressor need daily chemical exposure concentrations

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("workflow-data/diaz_exposure.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is "predetermined effects"
# Prompt for upload of predetermined effects file (Survival decrement, 365 days)

# GUI: Ask for chemical id associated to predetermined effects
inputChemID<-"Diazinon"
parameters[[inputStressorScenarioName]]$chem_id<-inputChemID
# GUI: Upload predetermined effects template
inputPredeterminedEffectsData<-read.csv("workflow-data/GUTS_diaz_survival.csv")
# Plot input of survival decrement
# Note: The negative of the input survival decrement is plotted here
plot(x=inputPredeterminedEffectsData[,1],y=-inputPredeterminedEffectsData[,2],main=paste("Daily Survival Decrement - ",parameters[[inputStressorScenarioName]]$chem_id[1],sep=""),xlab="Ordinal date",ylab="Survival Decrement",type="l")

# Add predetermined effects data (survival_decrement) data to parameter data.frame
parameters[[inputStressorScenarioName]]$survival_decrement<-inputPredeterminedEffectsData$survival_decrement


# # # GUI: Download Formatted Scenario Parameter File
# # Prompt download window
# write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)


#### ~ Chemical: Growth Stressor ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[1] #Assuming user chooses the 1st scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"diaz_growth"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[2] #Choose chemical: Growth stressor

# Chemical: Survival - Stressor need daily chemical exposure concentrations

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("workflow-data/diaz_exposure.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is "predetermined growth effects"
# Prompt for upload of predetermined growth effects file (Growth percent, 365 days)

# GUI: Ask for chemical id associated to predetermined effects
inputChemID<-"Diazinon"
parameters[[inputStressorScenarioName]]$chem_id<-inputChemID
# GUI: Upload predetermined growth effects template
inputPredeterminedGrowthEffectsData<-read.csv("workflow-data/diaz_growth.csv")
# Plot input of growth percent
plot(x=inputPredeterminedGrowthEffectsData[,1],y=inputPredeterminedGrowthEffectsData[,2],main=paste("Daily Growth Percent - ",parameters[[inputStressorScenarioName]]$chem_id[1],sep=""),xlab="Ordinal date",ylab="Growth Percent",type="l")

# Add predetermined growth effects (growth_percent) data to parameter data.frame
parameters[[inputStressorScenarioName]]$growth_percent<-inputPredeterminedGrowthEffectsData$growth_percent

# Set Growth Effect dates
growthEffectsDates<-SetExposureDates(parameters[[inputStressorScenarioName]]$growth_percent)
parameters[[inputStressorScenarioName]]$are_growth_effects<-growthEffectsDates

# # # GUI: Download Formatted Scenario Parameter File
# # Prompt download window
# write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)




#
#
# #### ~ Chemical: TCEM and Growth Stressor ####
#
# # Select scenario to apply stressors to
# # GUI: Populate select scenario list with current scenario names
# # scenarioNames
# chosenUnderlyingScenario<-scenarioNames[2] #Assuming user chooses the 2nd "TCEM" scenario name from the scenario name list
#
# # GUI: Name the stressor scenario
# inputStressorScenarioName<-"TCEM+GrowthEffect"
# scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)
#
# # Create the new stressor parameter set by copying the underlying scenario
# parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]
#
# # GUI:Choose stressor type
# stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")
#
# chosenStressor<-stressorTypeList[2] #Choose chemical: Growth stressor
#
# # Chemical: Survival - Stressor need daily chemical exposure concentrations
#
# # GUI: Upload exposure concentrations
# inputExpConcentrationData<-read.csv("exposure_concentration_example.csv")
# # Plot input of exposure concentrations
# plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# # add concentration data to parameter data frame
# parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations
#
#
# # GUI: Choose Effect Type
# # If effect type is "predetermined growth effects"
# # Prompt for upload of predetermined growth effects file (Growth percent, 365 days)
#
# # Offer Pre-determined effects template
# # Create empty predetermined growth effects data.frame
# predeteffDF<-data.frame(ordinal_date=1:365,growth_percent=NA)
# # Download exposure template data.frame
# write.csv(predeteffDF,"predetermined_growth_effects_template.csv",row.names=F)
#
# # GUI: Ask for chemical id associated to predetermined effects
# inputChemID<-"Diazinon"
# parameters[[inputStressorScenarioName]]$chem_id<-inputChemID
# # GUI: Upload predetermined growth effects template
# inputPredeterminedGrowthEffectsData<-read.csv("predetermined_growth_effects_example.csv")
# # Plot input of growth percent
# plot(x=inputPredeterminedGrowthEffectsData[,1],y=inputPredeterminedGrowthEffectsData[,2],main=paste("Daily Growth Percent - ",parameters[[inputStressorScenarioName]]$chem_id[1],sep=""),xlab="Ordinal date",ylab="Growth Percent",type="l")
#
# # Add predetermined growth effects (growth_percent) data to parameter data.frame
# parameters[[inputStressorScenarioName]]$growth_percent<-inputPredeterminedGrowthEffectsData$growth_percent
#
# # Set Growth Effect dates
# growthEffectsDates<-SetExposureDates(parameters[[inputStressorScenarioName]]$growth_percent)
# parameters[[inputStressorScenarioName]]$are_growth_effects<-growthEffectsDates
#
# # # GUI: Download Formatted Scenario Parameter File
# # Prompt download window
# write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)

#### ~ Chemical: GUTS and Growth Stressor ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[3] #Assuming user chooses the 3rd "GUTS" scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"GUTS+Growth"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[2] #Choose chemical: Growth stressor

# Chemical: Survival - Stressor need daily chemical exposure concentrations

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("workflow-data/diaz_exposure.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is "predetermined growth effects"
# Prompt for upload of predetermined growth effects file (Growth percent, 365 days)

# GUI: Ask for chemical id associated to predetermined effects
inputChemID<-"Diazinon"
parameters[[inputStressorScenarioName]]$chem_id<-inputChemID
# GUI: Upload predetermined growth effects template
inputPredeterminedGrowthEffectsData<-read.csv("workflow-data/diaz_growth.csv")
# Plot input of growth percent
plot(x=inputPredeterminedGrowthEffectsData[,1],y=inputPredeterminedGrowthEffectsData[,2],main=paste("Daily Growth Percent - ",parameters[[inputStressorScenarioName]]$chem_id[1],sep=""),xlab="Ordinal date",ylab="Growth Percent",type="l")

# Add predetermined growth effects (growth_percent) data to parameter data.frame
parameters[[inputStressorScenarioName]]$growth_percent<-inputPredeterminedGrowthEffectsData$growth_percent

# Set Growth Effect dates
growthEffectsDates<-SetExposureDates(parameters[[inputStressorScenarioName]]$growth_percent)
parameters[[inputStressorScenarioName]]$are_growth_effects<-growthEffectsDates

# # # GUI: Download Formatted Scenario Parameter File
# # Prompt download window
# write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)


#### ~ Winter Stressor ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[1] #Assuming user chooses the 1st scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"Overwinter"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose winter type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[3] #Choose winter stressor

# Exposure for winter is determined by the start and end date

# GUI: Input winter start date and winter end date
# OrdinalDate("21Dec2019")
inputWinterStart<-356
# OrdinalDate("18March2019")
inputWinterEnd<-78

# GUI: Effect for winter is determined by the winter size-cutoff
inputWinterSizeCutoff<-44
parameters[[inputStressorScenarioName]]$z_winter<-inputWinterSizeCutoff

# Plot decreasing survival over winter based on duration and size cutoff
winterLength<-length(DateDuration(356,78))
dailyWinterSurvival<-.0001^(1/winterLength)
plot(dailyWinterSurvival^(1:winterLength),
     xlab="Winter day",
     ylab="Cumulative survival probability",
     main=paste("Cumulative winter survival for individuals < ",inputWinterSizeCutoff," mm", sep=""))

# Calculate Winter daily survival: Needs winter start and end dates
winterSurvivalProbs<-GenerateWinterSurvival(winterStartDate=inputWinterStart,winterEndDate=inputWinterEnd)
parameters[[inputStressorScenarioName]]$s_winter<-winterSurvivalProbs

# Set winter dates
winterDates<-SetWinterDates(winterStartDate=inputWinterStart,winterEndDate=inputWinterEnd)
parameters[[inputStressorScenarioName]]$is_winter<-winterDates

# # # GUI: Download Formatted Scenario Parameter File
# # Prompt download window
# write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)

#### ~ Density Dependence ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[1] #Assuming user chooses the 1st scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"Density Dependence"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose winter type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter","Density Dependence")

chosenStressor<-stressorTypeList[4] #Choose density dependence

## ADD density dependence
parameters[[inputStressorScenarioName]]$is_density_dependent<-1

# # # GUI: Download Formatted Scenario Parameter File
# # Prompt download window
# write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)






##~~~~~~~~~~~~~~~~~~~~~~~~~##
#### VISUALIZE SCENARIOS ####
##~~~~~~~~~~~~~~~~~~~~~~~~~##

# GUI: Begin the GUI with a checkbox populated with Scenario Names
scenarioNames
# Assume the user has checked boxes 1,2 and 4
chosenScenarioIndices<-1:7

# Names for chosen scenarios
inputScenariosToVis<-scenarioNames[1:7]

# Create a date input
dateInput<-150 #assume scroller is at day 150
par(mfrow=c(1,1))
### Create buttons to plot the following:

# Plot Growth
PlotGrowth(parameters[inputScenariosToVis],dateInput)

#Plot Survival
PlotSurvival(parameters[inputScenariosToVis],dateInput)

#Plot Reproduction
PlotReproduction(parameters[inputScenariosToVis],dateInput)

####
# Visualize Scenario Parameters
####
# Note: These visualizations are for each date in the scenario
# and may not apply to all scenarios.  They also do not need date sliders.

# Plot Spawning Probabilities
PlotSpawningProbs(parameters[inputScenariosToVis])
# Plot Survival Decrements
PlotSurvivalDecrements(parameters[inputScenariosToVis])
# Plot Exposure Concentrations
PlotExposureConcentrations(parameters[inputScenariosToVis])


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
chosenScenarioIndices<-1:7

# Names for chosen scenarios
inputScenariosToRun<-scenarioNames[1:7]

# Complete a second run with a new runID and new uniform outputs
# Get input for runID specify 3 character maximum input
inputRunID<-as.character("Y1")

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
chosenResultIndices<-1:7

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


View(modelRuns)

#### Modify results to include relative values to baseline ####
## Baseline population levels are in the dailySummary

baseline_population <- modelRuns$Y1$Baseline$dailySummary$population
baseline_biomass <- modelRuns$Y1$Baseline$dailySummary$biomass
# Iterate over each scenario in modelRuns$Y1
for (scenario_name in names(modelRuns$Y1)) {
  if (scenario_name != "Baseline") {
    # Extract the population for the current scenario
    current_population <- modelRuns$Y1[[scenario_name]]$dailySummary$population
    current_biomass<-modelRuns$Y1[[scenario_name]]$dailySummary$biomass
    # Calculate the relative population (current / baseline)
    relativePopulation <- current_population / baseline_population
    relativeBiomass <- current_biomass / baseline_biomass
    # Store the result in a new dailySummary object
    modelRuns$Y1[[scenario_name]]$dailySummary$relativePopulation <- relativePopulation
    modelRuns$Y1[[scenario_name]]$dailySummary$relativeBiomass <- relativeBiomass
  }
}
##Ensure that the baseline still has relative values, but that they are properly assigned as a value of "1"
modelRuns$Y1$Baseline$dailySummary$relativePopulation<-rep(1,length(modelRuns$Y1$Baseline$dailySummary$population))
modelRuns$Y1$Baseline$dailySummary$relativeBiomass<-rep(1,length(modelRuns$Y1$Baseline$dailySummary$population))


#### Modified PlotBiomass and PlotPopulation functions for relative values included in modelRuns output

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


# Projected daily population biomass
PlotBiomass(unlist(modelRuns,recursive=F)[inputScenariosForResults],relative=T)
PlotPopulation(unlist(modelRuns,recursive=F)[inputScenariosForResults],relative=T)

#### Extract and compile time series for pop, biomass, relpop, and relbiomass ####

library(dplyr)

# Function to compile time series data into a data frame
compileTimeSeries <- function(modelRuns) {
  # Initialize an empty list to store data frames for each scenario
  scenario_data_list <- list()

  # Iterate over each scenario in modelRuns$Y1
  for (scenario_name in names(modelRuns$Y1)) {
    # Extract dailySummary for the current scenario
    daily_summary <- modelRuns$Y1[[scenario_name]]$dailySummary

    # Create a data frame for the current scenario
    scenario_data <- data.frame(
      Day = 1:366,
      Population = daily_summary$population,
      RelativePopulation = daily_summary$relativePopulation,
      Biomass = daily_summary$biomass,
      RelativeBiomass = daily_summary$relativeBiomass
    )

    # Add a column for the scenario name
    scenario_data <- scenario_data %>%
      mutate(Scenario = scenario_name)

    # Append the data frame to the list
    scenario_data_list[[scenario_name]] <- scenario_data
  }

  # Combine all scenario data frames into a single data frame
  combined_data <- bind_rows(scenario_data_list)

  # Return the combined data frame
  return(combined_data)
}

# Compile the time series data into a data frame
compiled_data <- compileTimeSeries(modelRuns)

# Export the data frame to a CSV file
write.csv(compiled_data, "scenario_time_series.csv", row.names = FALSE)

# Print the first few rows of the compiled data for verification
print(head(compiled_data))

#### Wideform ###

# Load necessary library
library(tidyr)

# Function to compile time series data into a wide format data frame
compileTimeSeriesWide <- function(modelRuns) {
  # Initialize an empty list to store data frames for each scenario
  scenario_data_list <- list()

  # Iterate over each scenario in modelRuns$Y1
  for (scenario_name in names(modelRuns$Y1)) {
    # Extract dailySummary for the current scenario
    daily_summary <- modelRuns$Y1[[scenario_name]]$dailySummary

    # Create a data frame for the current scenario
    scenario_data <- data.frame(
      Day = 1:366,
      Population = daily_summary$population,
      RelativePopulation = daily_summary$relativePopulation,
      Biomass = daily_summary$biomass,
      RelativeBiomass = daily_summary$relativeBiomass
    )

    # Add a column for the scenario name to identify columns uniquely
    scenario_data <- scenario_data %>%
      mutate(Scenario = scenario_name)

    # Reshape the data to wide format
    scenario_data_wide <- pivot_longer(scenario_data, cols = -c(Day, Scenario), names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = c(Scenario, Metric), values_from = Value)

    # Append the wide data frame to the list
    scenario_data_list[[scenario_name]] <- scenario_data_wide
  }

  # Combine all scenario-wide data frames into a single data frame
  combined_data_wide <- reduce(scenario_data_list, full_join, by = "Day")

  # Return the combined wide data frame
  return(combined_data_wide)
}

# Compile the time series data into a wide format data frame
compiled_data_wide <- compileTimeSeriesWide(modelRuns)

# Export the wide data frame to a CSV file
write.csv(compiled_data_wide, "scenario_time_series_wide.csv", row.names = FALSE)

# Print the first few rows of the compiled wide data for verification
print(head(compiled_data_wide))


#### Also write a csv of the summary Table results ####

write.csv(summaryTableResults,"Scenario_Summary_Results.csv",row.names=FALSE)
