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

#### Description ##
## This file is being based off of "Workflow-FishToxTranslator.R" to recreate simulations from Pollesch et al., 2022
## To compare outputs to the agent-based model (ABM) being developed by Accolla et al.
## The scenarios being created are:
## 1) Baseline - Fathead Minnow
## 2) Winter
## 3)	DD
## 4)	Diazinon effects on growth
## 5)	Diazinon effects on survival (GUTS)



#### Load packages and data, set working directory
library(FishToxTranslator)

## View datasets included in the FishToxTranslator package
#View(parameters_master) # Master parameter list with descriptions of all parameters
#View(species_library) # Species library list (only includes fathead minnow currently)
#View(pimephales_promelas) # Parameters for fathead minnow


######## LEFT OFF HERE 7/16/25


#### Initialize lists, vectors, and data.frames
# Create an empty vector to store scenario names
scenarioNames<-vector()
# Create an empty list to store scenario descriptions
scenarioDescriptions<-list()
# Create an empty list to store parameters indexed by scenarioNames
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


# GUI: Offer life history parameter template if "New" species chosen
lifeHistoryTemplate<-subset(parameters_master,gui_category=="baseline")
lifeHistoryTemplate<-add_column(lifeHistoryTemplate,value=NA,.after="id")
lifeHistoryTemplate<-lifeHistoryTemplate[,1:5]
# This data.frame (or .csv) should be open automatically in excel
write.csv(lifeHistoryTemplate,"lifeHistoryTemplate_1.csv",row.names=F)
# Note each value in the template created must be filled out
newLifeHistory<-read.csv("newLifeHistory.csv")
parameters[[currentScenarioName]]<-TemplateToParameters(newLifeHistory)


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
inputStressorScenarioName<-"TCEMAcuteSurvival"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

## CHANGED
# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[1] #Choose chemical stressor

# Chemical: Survival - First needs daily chemical exposure concentrations

# Offer Exposure concentration template
# Create empty exposure data.frame
expDF<-data.frame(ordinal_date=1:365,exp_concentrations=NA)
# Download exposure template data.frame
write.csv(expDF,"exposure_concentration_template.csv",row.names=F)

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("exposure_concentration_example.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is TCEM Gather TCEM parameters:
# Can find all needed TCEM parameters by using the parameters_master and matching the string "TCEM" in the called_by category
TCEMNeeds<-parameters_master$id[grepl("TCEM",parameters_master$called_by)]
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

# # GUI: Download Formatted Scenario Parameter File
# Prompt download window
write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)


#### ~ Chemical: Survival - Stressor 2 - Pre-determined effects ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[1] #Assuming user chooses the 1st scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"TKTDAcuteSurvival"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[1] #Choose chemical stressor

# Chemical: Survival - Stressor need daily chemical exposure concentrations

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("exposure_concentration_example.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is "predetermined effects"
# Prompt for upload of predetermined effects file (Survival decrement, 365 days)

# Offer Pre-determined effects template
# Create empty predetermined effects data.frame
predeteffDF<-data.frame(ordinal_date=1:365,survival_decrement=NA)
# Download exposure template data.frame
write.csv(predeteffDF,"predetermined_effects_template.csv",row.names=F)

# GUI: Ask for chemical id associated to predetermined effects
inputChemID<-"Diazinon - 7dpf"
parameters[[inputStressorScenarioName]]$chem_id<-inputChemID
# GUI: Upload predetermined effects template
inputPredeterminedEffectsData<-read.csv("predetermined_effects_example.csv")
# Plot input of survival decrement
# Note: The negative of the input survival decrement is plotted here
plot(x=inputPredeterminedEffectsData[,1],y=-inputPredeterminedEffectsData[,2],main=paste("Daily Survival Decrement - ",parameters[[inputStressorScenarioName]]$chem_id[1],sep=""),xlab="Ordinal date",ylab="Survival Decrement",type="l")

# Add predetermined effects data (survival_decrement) data to parameter data.frame
parameters[[inputStressorScenarioName]]$survival_decrement<-inputPredeterminedEffectsData$survival_decrement


# # GUI: Download Formatted Scenario Parameter File
# Prompt download window
write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)


#### ~ Chemical: Growth Stressor ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[1] #Assuming user chooses the 1st scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"ChronicGrowthEffect"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[2] #Choose chemical: Growth stressor

# Chemical: Survival - Stressor need daily chemical exposure concentrations

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("exposure_concentration_example.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is "predetermined growth effects"
# Prompt for upload of predetermined growth effects file (Growth percent, 365 days)

# Offer Pre-determined effects template
# Create empty predetermined growth effects data.frame
predeteffDF<-data.frame(ordinal_date=1:365,growth_percent=NA)
# Download exposure template data.frame
write.csv(predeteffDF,"predetermined_growth_effects_template.csv",row.names=F)

# GUI: Ask for chemical id associated to predetermined effects
inputChemID<-"Diazinon"
parameters[[inputStressorScenarioName]]$chem_id<-inputChemID
# GUI: Upload predetermined growth effects template
inputPredeterminedGrowthEffectsData<-read.csv("predetermined_growth_effects_example.csv")
# Plot input of growth percent
plot(x=inputPredeterminedGrowthEffectsData[,1],y=inputPredeterminedGrowthEffectsData[,2],main=paste("Daily Growth Percent - ",parameters[[inputStressorScenarioName]]$chem_id[1],sep=""),xlab="Ordinal date",ylab="Growth Percent",type="l")

# Add predetermined growth effects (growth_percent) data to parameter data.frame
parameters[[inputStressorScenarioName]]$growth_percent<-inputPredeterminedGrowthEffectsData$growth_percent

# Set Growth Effect dates
growthEffectsDates<-SetExposureDates(parameters[[inputStressorScenarioName]]$growth_percent)
parameters[[inputStressorScenarioName]]$are_growth_effects<-growthEffectsDates

# # GUI: Download Formatted Scenario Parameter File
# Prompt download window
write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)

#### ~ Chemical: TCEM and Growth Stressor ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[2] #Assuming user chooses the 2nd "TCEM" scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"TCEM+GrowthEffect"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[2] #Choose chemical: Growth stressor

# Chemical: Survival - Stressor need daily chemical exposure concentrations

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("exposure_concentration_example.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is "predetermined growth effects"
# Prompt for upload of predetermined growth effects file (Growth percent, 365 days)

# Offer Pre-determined effects template
# Create empty predetermined growth effects data.frame
predeteffDF<-data.frame(ordinal_date=1:365,growth_percent=NA)
# Download exposure template data.frame
write.csv(predeteffDF,"predetermined_growth_effects_template.csv",row.names=F)

# GUI: Ask for chemical id associated to predetermined effects
inputChemID<-"Diazinon"
parameters[[inputStressorScenarioName]]$chem_id<-inputChemID
# GUI: Upload predetermined growth effects template
inputPredeterminedGrowthEffectsData<-read.csv("predetermined_growth_effects_example.csv")
# Plot input of growth percent
plot(x=inputPredeterminedGrowthEffectsData[,1],y=inputPredeterminedGrowthEffectsData[,2],main=paste("Daily Growth Percent - ",parameters[[inputStressorScenarioName]]$chem_id[1],sep=""),xlab="Ordinal date",ylab="Growth Percent",type="l")

# Add predetermined growth effects (growth_percent) data to parameter data.frame
parameters[[inputStressorScenarioName]]$growth_percent<-inputPredeterminedGrowthEffectsData$growth_percent

# Set Growth Effect dates
growthEffectsDates<-SetExposureDates(parameters[[inputStressorScenarioName]]$growth_percent)
parameters[[inputStressorScenarioName]]$are_growth_effects<-growthEffectsDates

# # GUI: Download Formatted Scenario Parameter File
# Prompt download window
write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)

#### ~ Chemical: GUTS and Growth Stressor ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[3] #Assuming user chooses the 3rd "GUTS" scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"GUTS+GrowthEffect"
scenarioNames<-cbind(scenarioNames,inputStressorScenarioName)

# Create the new stressor parameter set by copying the underlying scenario
parameters[[inputStressorScenarioName]]<-parameters[[chosenUnderlyingScenario]]

# GUI:Choose stressor type
stressorTypeList<-c("Chemical: Survival","Chemical: Growth","Winter")

chosenStressor<-stressorTypeList[2] #Choose chemical: Growth stressor

# Chemical: Survival - Stressor need daily chemical exposure concentrations

# GUI: Upload exposure concentrations
inputExpConcentrationData<-read.csv("exposure_concentration_example.csv")
# Plot input of exposure concentrations
plot(inputExpConcentrationData,main="Daily Exposure Concentrations",xlab="Ordinal date",ylab="Concentration",type="l")
# add concentration data to parameter data frame
parameters[[inputStressorScenarioName]]$exp_concentrations<-inputExpConcentrationData$exp_concentrations


# GUI: Choose Effect Type
# If effect type is "predetermined growth effects"
# Prompt for upload of predetermined growth effects file (Growth percent, 365 days)

# Offer Pre-determined effects template
# Create empty predetermined growth effects data.frame
predeteffDF<-data.frame(ordinal_date=1:365,growth_percent=NA)
# Download exposure template data.frame
write.csv(predeteffDF,"predetermined_growth_effects_template.csv",row.names=F)

# GUI: Ask for chemical id associated to predetermined effects
inputChemID<-"Diazinon"
parameters[[inputStressorScenarioName]]$chem_id<-inputChemID
# GUI: Upload predetermined growth effects template
inputPredeterminedGrowthEffectsData<-read.csv("predetermined_growth_effects_example.csv")
# Plot input of growth percent
plot(x=inputPredeterminedGrowthEffectsData[,1],y=inputPredeterminedGrowthEffectsData[,2],main=paste("Daily Growth Percent - ",parameters[[inputStressorScenarioName]]$chem_id[1],sep=""),xlab="Ordinal date",ylab="Growth Percent",type="l")

# Add predetermined growth effects (growth_percent) data to parameter data.frame
parameters[[inputStressorScenarioName]]$growth_percent<-inputPredeterminedGrowthEffectsData$growth_percent

# Set Growth Effect dates
growthEffectsDates<-SetExposureDates(parameters[[inputStressorScenarioName]]$growth_percent)
parameters[[inputStressorScenarioName]]$are_growth_effects<-growthEffectsDates

# # GUI: Download Formatted Scenario Parameter File
# Prompt download window
write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)


#### ~ Winter Stressor ####

# Select scenario to apply stressors to
# GUI: Populate select scenario list with current scenario names
# scenarioNames
chosenUnderlyingScenario<-scenarioNames[1] #Assuming user chooses the 1st scenario name from the scenario name list

# GUI: Name the stressor scenario
inputStressorScenarioName<-"WinterStressorScenarioName"
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

# # GUI: Download Formatted Scenario Parameter File
# Prompt download window
write.csv(parameters[[inputStressorScenarioName]],paste("formatted_",inputStressorScenarioName,".csv",sep=""),row.names=F)

##~~~~~~~~~~~~~~~~~~~~~~~~~##
#### VISUALIZE SCENARIOS ####
##~~~~~~~~~~~~~~~~~~~~~~~~~##

# GUI: Begin the GUI with a checkbox populated with Scenario Names
scenarioNames
# Assume the user has checked boxes 1,2 and 4
chosenScenarioIndices<-1:6

# Names for chosen scenarios
inputScenariosToVis<-scenarioNames[1:6]

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
# Assume the user has checked boxes 1,2 and 4
chosenScenarioIndices<-1:6

# Names for chosen scenarios
inputScenariosToRun<-scenarioNames[1:6]

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
#### ~ If "Predetermined" is chosen from the list

## set the modelRunInfo isPredet to TRUE
modelRunParams$isPredet<-TRUE
## set modelRunInfo isUnif to FALSE and set unifN0 to NA, since it won't be specified
modelRunParams$isUnif<-FALSE
modelRunParams$unifN0<-NA
## Generate Predetermined starting distribution template
predetinitialdist<-data.frame(ordinal_date=1:num_size_classes,density_in_class=NA)
## Download exposure template data.frame
write.csv(predetinitialdist,"predetermined_initial_distribution_template.csv",row.names=F)


## GUI: Upload predetermined initial distribution template
inputDistFileString<-"predetermined_initial_distribution_example.csv"
## Store input file string in modelRunInfo
modelRunParams$predetFileString<-inputDistFileString

## Upload the predetermined intial distribution
inputPredeterminedIntitialDist<-read.csv("predetermined_initial_distribution_example.csv")

## Plot input of predetermined initial distribution
plot(x=inputPredeterminedIntitialDist[,1],y=inputPredeterminedIntitialDist[,2],main="User-specified initial distribution",xlab="Size class",ylab="Density of individuals")

## Assign predetermined initial distribution input the proper variable for simulation
inputz_t_0<-inputPredeterminedIntitialDist[,2]


#### Run the scenarios in the scenarioNames list ####
# GUI: Create a "Run Simulation(s)" button
# When button is clicked display the following text: "Model runs take between 2 and 5 minutes each. An alert window
# will be created when runs are completed"

# Initialize lists to store model output
# temp List stores the most recent model runs
tempOutputs<-list()

#Set up parallel model runs using package "parallel"
numCores<-detectCores()
cl<-makeCluster(numCores)
clusterExport(cl,"parameters")
tempOutputs<-parLapply(cl,parameters[inputScenariosToRun],SimulateModel,z_t_0=inputz_t_0)
stopCluster(cl)

# Add the tempOutputs to the modelRuns list
modelRuns[[runID]]<-tempOutputs

# Add the model run information to the modelRunInfo list()
modelRunInfo[[runID]][["modelRunParams"]]<-modelRunParams
modelRunInfo[[runID]][["modelRunScenarios"]]<-inputScenariosToRun

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
#Set up parallel model runs using package "parallel"
numCores<-detectCores()
cl<-makeCluster(numCores)
clusterExport(cl,"parameters")
tempOutputs<-parLapply(cl,parameters[inputScenariosToRun],SimulateModel,n_0=inputNumberInitInd)
stopCluster(cl)


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
chosenResultIndices<-1:6

# Names for chosen scenarios
inputScenariosForResults<-resultNames[chosenResultIndices]

inputScenariosForResults
### Results sub section title: Scenario Results Summary
# Provide button to view the summary table dynamically in main panel for chosen names
summaryTableResults<-SummaryTable(unlist(modelRuns,recursive=F)[inputScenariosForResults])

### Provide buttons to view selected plots
# Projected daily population (# of individuals)
PlotPopulation(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Projected daily population biomass
PlotBiomass(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Projected daily mean size in population
PlotMeanSize(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Plot growth potentials
PlotGrowthPotential(unlist(modelRuns,recursive=F)[inputScenariosForResults])

# Plot Annual Transition Kernels

PlotTransitionKernel(unlist(modelRuns,recursive=F)[inputScenariosForResults])




### Results sub section title: Comparison of Scenario Results
# Plot the summary matrices
PlotSummaryMatrix(unlist(modelRuns,recursive=F)[inputScenariosForResults])
summMats<-SummaryMatrix(unlist(modelRuns,recursive=F)[inputScenariosForResults])


SummaryTable(unlist(modelRuns,recursive=F)[inputScenariosForResults])


##############################################################
##############################################################
##############################################################
# STOP HERE FOR JUST SIMULATION WITHOUT EXPORT/IMPORT
##############################################################
##############################################################
##############################################################







###~~~~~~~~~~~~~~~~~~~~~~~###
###      EXPORT RESULTS   ###
###~~~~~~~~~~~~~~~~~~~~~~~###

## Use a dropdown list to select the runID and the scenarios

## Dropdown with runIDs populated with list from runIDs
runIDs

## Assume user chose "Unif" from drop down
inputRunIDToExport<-"Unif"

## Get the associated scenarios from the "modelRunScenarios" vector in the modelRunInfo list

## Dropdown with scenarios populated with list from modelRunScenarios from the modelRunInfo list for the given runID
modelRunInfo[[inputRunIDToExport]][["modelRunScenarios"]]

## Assume user chose "inputBaselineName"  from drop down
inputScenarioToExport<-"inputBaselineName"

## Pass the inputRunIDToExport and the inputScenarioToExport to the FormatExportResults function
# Save results to export object


expResults<-FormatExportResults("Unif","inputBaselineName")


library(writexl)

write_xlsx(x=expResults,path="ExportResults4.xlsx")


##~~~~~~~~~~~~~~~~~~~~~~~##
## IMPORT A SAVED RESULT ##
##~~~~~~~~~~~~~~~~~~~~~~~##

#install.packages("readxl")
library(readxl)

# Reads in the metadata worksheet of the excel file
metaDataIn<-read_excel("ExportResults4.xlsx",sheet="Metadata")

# Extracts the imported RunID and scenarioName
importedRunID<-as.character(subset(metaDataIn,Field=="Run ID",select=Value))
importedScenarioName<-as.character(subset(metaDataIn,Field=="Scenario Name",select=Value))

# Need to check if importedRunID is in runIDs
importedRunID %in% runIDs
# Need to check if importedScenarioName is in scenarioNames
importedScenarioName %in% scenarioNames

# Use importedScenarioName as default for Submit Scenario Name input field
# submittedName is the verified scenario name from the from input field (basically not a duplicate name)
# Assuming importedScenarioName is the same as the verifiedScenarioName
submittedResultsName<-"importedResults"
submittedRunID<-"ImpR"

# Extracts the Scenario description
importedScenarioDesc<-as.character(subset(metaDataIn,Field=="Scenario Description",select=Value))
importedScenarioDesc
# Once Scenario Name is submitted, append importedScenarioDesc to scenarioDescriptions list
scenarioDescriptions[[submittedResultsName]]<-importedScenarioDesc

# Reads in ModelRunInfo worksheet
modelRunInfoIn<-as.data.frame(read_excel("ExportResults4.xlsx",sheet="ModelRunInfo"))

# Creates list space to store imported modelRunInfo
modelRunInfo[[submittedRunID]]<-list(modelRunParams=data.frame(),modelRunScenarios=c())
# Adds modelRunInfo imported to modelRunInfo list
modelRunInfo[[submittedRunID]][["modelRunParams"]]<-modelRunInfoIn
modelRunInfo[[submittedRunID]][["modelRunScenarios"]]<-cbind(modelRunInfo[[submittedRunID]][["modelRunScenarios"]],submittedResultsName)

# Reads in the modelRuns data from worksheet of the excel file
dailySummaryIn<-as.data.frame(read_excel("ExportResults4.xlsx",sheet="dailySummary"))
midpointsIn<-unname(as.vector(t(read_excel("ExportResults4.xlsx",sheet="midpoints"))))
sizesIn<-as.list(unname(as.data.frame(t(read_excel("ExportResults4.xlsx",sheet="sizes")))))
annualKernelIn<-unname(as.matrix(read_excel("ExportResults4.xlsx",sheet="annualKernel")))

#Once everything is read in, append to modelOutputsList
modelRuns[[submittedRunID]][[submittedResultsName]][["sizes"]]<-sizesIn
modelRuns[[submittedRunID]][[submittedResultsName]][["dailySummary"]]<-dailySummaryIn
modelRuns[[submittedRunID]][[submittedResultsName]][["cumulativeTransitionKernel"]]<-annualKernelIn
modelRuns[[submittedRunID]][[submittedResultsName]][["midpoints"]]<-midpointsIn


###~~~~~~~~~~~~~~~~~~~~~~~ ###
###      EXPORT SCENARIO   ###
###~~~~~~~~~~~~~~~~~~~~~~~ ###

# GUI: Download Formatted Scenario Parameter Excel file with Metadata sheet
# Choose a scenario to export from dropdown
scenarioToExport<-"inputBaselineName"

## Save list object for export, ExportScenario is a function of the chosen Scenario name
exp<-FormatExportScenario(scenarioToExport)

## Export the Scenario Chosen
#install.packages("writexl")
library(writexl)

write_xlsx(x=exp,path="TestNewExport5.xlsx")

###~~~~~~~~~~~~~~~~~~~~~~~###
###     IMPORT SCENARIO   ###
###~~~~~~~~~~~~~~~~~~~~~~~###


#install.packages("readxl")
library(readxl)

# Reads in the metadata worksheet of the excel file
metaDataIn<-read_excel("TestNewExport5.xlsx",sheet="Metadata")

# Extracts the Scenario name
importedScenarioName<-as.character(subset(metaDataIn,Field=="Scenario Name",select=Value))
importedScenarioName

# Use importedScenarioName as default for Submit Scenario Name input field
# submittedName is the verified scenario name from the from input field (basically not a duplicate name)
# Assuming importedScenarioName is the same as the verifiedScenarioName
submittedName<-"importedBaseline"

# Extracts the Scenario description
importedScenarioDesc<-as.character(subset(metaDataIn,Field=="Scenario Description",select=Value))
importedScenarioDesc

# Once Scenario Name is submitted, append importedScenarioDesc to scenarioDescriptions list
scenarioDescriptions[[submittedName]]<-importedScenarioDesc

# Reads in the parameters worksheet of the excel file
parametersIn<-as.data.frame(read_excel("TestNewExport5.xlsx",sheet="Parameters"))
parametersIn

#Once Scenario Name is submitted, append parametersIn to parameters list
parameters[[submittedName]]<-parametersIn


