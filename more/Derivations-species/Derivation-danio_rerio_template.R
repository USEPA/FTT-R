#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov

## ------------------------------------ ##
## ------------------------------------ ##
## --- SPECIES PARAMETER DERIVATION --- ##
## ------- Template Version 1.1 ------- ##
## ------------------------------------ ##
## ------------------------------------ ##

#### GENERAL INFO ####
## This script contains information about the derivation of species specific model parameters
## This script is organized in 'code sections' which can be viewed in RStudio in the document outline.
## You can toggle the outline on/off with (Ctrl + shift + O) in RStudio version 2024.04.2
##
## Note for organization: Often data from literature doesn't fit neatly into the parameter
## id categories in utilized here.  Therefore, data/information/derivations can be included
## in either the general categories (e.g Growth, Survival or Reproduction) or parameter-specific sections.
##
## Note When editing: Please use two hashes '##' to distinguish notes/comments within code
## and single hash '#' to 'comment-out' any code from evaluation


#### Definitions ####
##### Parameter IDs ####
## Notes: These IDs, such as z_hatch, z_inf, etc., reference unique model
## parameters used to run the FishToxTranslator model.  Parameter information can
## be found by running the following command, using 'z_hatch' as the example:
# View(subset(FishToxTranslator::parameters_master,id=='z_hatch'))
##### Study IDs ####
## Notes: Study IDs are unique references to studies contained in the the Fish Species
## Life History Database.  Study IDs included in parameter derivation reference the
## database where results can be stored. Database entries also contain all references to
## source values for parameters used within derivations

#### Availability ####
## A version of this code is available via the FishToxTranslator GitHub online repository
## located at: https://github.com/USEPA/FTT-R/
## Or by contacting Nate Pollesch (pollesch.nathan@epa.gov)

##----END GENERAL INFO SECTION----##

#### SPECIES INFO ####

##### Danio rerio ####
## Notes:
##### zebra danio ####
## Notes: An additional common name is zebrafish

##----END SPECIES INFO SECTION----###

#### PARAMETERS ####

#### Growth/Size ####
## General Notes on Growth/Size:

##### z_hatch ####
## Study IDs: 62, 64
## Notes: Mean value for two studies of wild originating, unsexed hatchlings. Other studies (SID 61 and 63) were not included for unreported length type and laboratory/commercial study conditions
z_hatch_zebradanio <- mean(c(2.8, 3.71, 3.78, 3.89)) 

##### z_inf ####
## Study IDs: 67
## Notes: Maximum reported length for wild female fish brought into laboratory conditions. 
z_inf_zebradanio <- 49

##### k_g ####
## Study IDs: 69, 70 
## Notes: Mean value for both studies of unsexed fish. Values are similar despite varying organism sources
k_g_zebradanio <- mean(c((1.2/365.25), (0.0024)))

##### var_k_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_zebradanio # Kappa (vB growth rate)
z0=z_hatch_zebradanio #Size at hatch (mm)
zinf=z_inf_zebradanio # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_zebradanio , z0 = z_hatch_zebradanio , zinf = z_inf_zebradanio, zrat=0.95 , zratmax=0.99 ){
  
  # sigma_k = variance of k
  # k = growth rate (daily)
  
  TimeToZrat = ( log(1-z0/zinf)-log(1-zrat) )/ k
  krat_max = ( log(1-z0/zinf)-log(1-zratmax) )/ TimeToZrat
  
  
  k99 = qnorm( 0.99 , k , sigma_k )
  
  dif_k <- ( k99 - krat_max ) ^ 2
  
  return(dif_k)
  
}

## Run optimizer at default values VarianceK() function

# Set initial guess
sigma_iter <- 0.1
# Run optimizer at initial guess, lower bound of 0, upper bound of 100 (VERY high upper bound)
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_zebradanio, z0=z_hatch_zebradanio, zinf=z_inf_zebradanio, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_k_g_zebradanio <- k_variance

##### var_e_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_zebradanio # Kappa (vB growth rate)
z0=z_hatch_zebradanio #Size at hatch (mm)
zinf=z_inf_zebradanio # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_zebradanio , z0 = z_hatch_zebradanio , zinf = z_inf_zebradanio, zrat=0.95 , zratmax=0.99 ){
  
  # sigma_k = variance of k
  # k = growth rate (daily)
  
  TimeToZrat = ( log(1-z0/zinf)-log(1-zrat) )/ k
  krat_max = ( log(1-z0/zinf)-log(1-zratmax) )/ TimeToZrat
  
  
  k99 = qnorm( 0.99 , k , sigma_k )
  
  dif_k <- ( k99 - krat_max ) ^ 2
  
  return(dif_k)
  
}

## Run optimizer at default values VarianceK() function

# Set initial guess
sigma_iter <- 0.1
# Run optimizer at initial guess, lower bound of 0, upper bound of 100 (VERY high upper bound)
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_zebradanio, z0=z_hatch_zebradanio, zinf=z_inf_zebradanio, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_e_g_zebradanio <- k_variance

##### dd_g ####
## Study IDs:
## Notes:

##### allo_slope ####
## Study IDs: 76, 884
## Notes: Fitted value from a reported slope value and length-weight measurements taken in lab for wild originating females

# Fitting Allometry
a <- 1.74E-06
b <- 3.51
x<-seq(from=0,to=50,by=1) #defines size range for zebradanio

f<-function(x,a,b){ #defines function and variables in the function
  allo<-a*(x^b) #supply mathematical form of function
  return(allo)} #return gives output

pts<-f(x,1.74E-06,3.51) #creates data points for the reported allometric equation, SID 76

zeb<-data.frame(x=c(0,29,29,29,31,31,31,31,31,32,32,32,32,32,32,32,32,33,33,33,33,33,34,34,34,34,35,35,35,36,36,36,37,40,41,41,42,42,43,43,43,44,44,45,45,45,45,47,47,47,48,48,48,49,49,49,51,51,51,81),y=c(0,.20,.24,.27,.31,.33,.35,.35,.36,.29,.33,.33,.34,.35,.35,.37,.38,.35,.37,.38,.41,.84,.43,.44,.46,.40,.44,.49,.39,.44,.52,.48,.86,.88,.94,.87,.92,.88,.93,1.14,.96,1.17,1.07,1.21,1.24,1.33,1.16,1.2,1.34,1.05,1.25,1.25,1.11,1.14,1.32,1.63,1.44,1.51,1.54,6.74))  #creates data frame for the length (x) and weight (y) measurements taken, SID 884

fit <- nls(y~f(x,a,b), data=data.frame(zeb), start=list(a=1.74E-06, b=3.51))
fit #view 'fit' object

allo_slope_zebradanio <- 3.096

##### allo_intercept ####
## Study IDs: 77, 884
## Notes: Fitted value from a converted intercept and length-weight measurements taken in lab for wild originating females 

# Fitting Allometry
a <- 10^-5.76 #convert out of log-log form 
b <- 3.51
x<-seq(from=0,to=60,by=1) #defines size range for zebradanio

f<-function(x,a,b){ #defines function and variables in the function
  allo<-a*(x^b) #supply mathematical form of function
  return(allo)} #return gives output

pts<-f(x,1.74E-06,3.51) #creates data points for the reported allometric equation, SID 76

zeb<-data.frame(x=c(0,29,29,29,31,31,31,31,31,32,32,32,32,32,32,32,32,33,33,33,33,33,34,34,34,34,35,35,35,36,36,36,37,40,41,41,42,42,43,43,43,44,44,45,45,45,45,47,47,47,48,48,48,49,49,49,51,51,51,81),y=c(0,.20,.24,.27,.31,.33,.35,.35,.36,.29,.33,.33,.34,.35,.35,.37,.38,.35,.37,.38,.41,.84,.43,.44,.46,.40,.44,.49,.39,.44,.52,.48,.86,.88,.94,.87,.92,.88,.93,1.14,.96,1.17,1.07,1.21,1.24,1.33,1.16,1.2,1.34,1.05,1.25,1.25,1.11,1.14,1.32,1.63,1.44,1.51,1.54,6.74))  #creates data frame for the length (x) and weight (y) measurements taken, SID 884

fit <- nls(y~f(x,a,b), data=data.frame(zeb), start=list(a=1.74E-06, b=3.51))
fit #view 'fit' object

allo_intercept_zebradanio <- 8.270e-06

##----END Growth/Size SECTION----##

#### Survival ####
## Notes on survival:

##### s_min ####
## Study IDs: 101, 103, 104
## Notes: Mean converted values for daily survival rate of embryo-larval fish
converted_s_min104 <- .15^(1/18)
converted_s_min103 <- .55^(1/32)
converted_s_min101 <- .983^(1/5)
s_min_zebradanio <- mean(c(converted_s_min101, converted_s_min103, converted_s_min104)) 

##### s_max ####
## Study IDs: 105
## Notes: Survival max is calculated as 99.999% chance of mortality at the average lifespan. Reported lifespan for fish is 3.5 years old. 
s_max_zebradanio <- .001^(1/(3.5*365.25))

##### s_a ####
## Study IDs:
## Notes: fitted using z_repo, s_min, and s_max to have the curve rise at the juvenile to adult transition size and plateau at z_repro 
s_a_zebradanio <- 1.1

##### s_b ####
## Study IDs: 
## Notes: fitted using z_repo, s_min, and s_max to start curve at the juvenile to adult transition size
s_b_zebradanio <- 26

##----END Survival SECTION----##

#### Reproduction ####
## Notes on reproduction:

##### post_spawning_mortality ####
## Study IDs:
## Notes: In reproductive studies no post spawning mortality was noted, assumed that post_spawning_mortality is 0 
post_spawning_mortality_zebradanio <- 0

##### z_repro ####
## Study IDs: 106, 109
## Notes: Mean value for studies with wild originating female fish. 
z_repro_zebradanio <- mean(c(29,30.4))

##### sex_ratio ####
## Study IDs:
## Notes: In reproductive studies no skewed sex ratios were noted, assumed that sex_ratio is .5
sex_ratio_zebradanio <- 0.5

##### fecu_slope ####
## Study IDs: 111
## Notes: NLS fitted the reported value to convert from absilute fecundity to eggs/spawn.

#reproductive length range for FHM
L<-25:50

## Create data sets for fitting
x <- L
y1 <- ((0.025703968*(x^2.87))/6)
datazeb <- data.frame(x = x, y = y1)

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = datazeb, start = list(a = .1, b = 2), algorithm="port")
log_fit #a~0.004284 b~2.870000

fecu_slope_zebradanio <- 2.87
  
##### fecu_intercept ####
## Study IDs: 112
## Notes: NLS fitted from absolute fecundity to eegs/spawn. Converted reported value from log-log form for fecundity power log function for female fish. 

#reproductive length range for FHM
L<-25:50

## Create data sets for fitting
x <- L
y1 <- ((0.025703968*(x^2.87))/6)
datazeb <- data.frame(x = x, y = y1)

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = datazeb, start = list(a = .1, b = 2), algorithm="port")
log_fit #a~0.004284 b~2.870000

fecu_intercept_zebradanio <- 0.004284

##### hatch_rate ####
## Study IDs: 82, 86, 87, 88, 90. 
## Notes: Mean hatch rate for studies including both laboratory and wild unsexed fish. SID 84 was disregarded because of egg survival rates and 85 because of high hatch-ability due to optimized lab conditions 
hatch_rate_zebradanio <- mean(c(.98, .69, .46, .479, .6803, .6858, .7664, .47, .25, .9489)) 

##### spawn_int ####
## Study IDs: 91
## Notes: The reproductive output for the control group of wild originating, female fish held in laboratory conditions and tested based on size was 5 days. Mean for all studies including captive breeding colonies and commercially supplied fish was 4.522 days
#mean(c(5,5,7,1.9,1.5,5,10,1.23,1.59,7))
spawn_int_zebradanio <- 5

##### spawns_max_season ####
## Study IDs: 97
## Notes: Females from a captive breeding colony housed in a laboratory were capable of spawning 12 times. 
spawns_max_season_zebradanio <- 12

##### repro_start ####
## Study IDs: 72, 74, 116
## Notes: Ordinal date for April 15. Beginning of the spawning period for wild fish in native waters, in India/Asia, corresponding with monsoon season.
repro_start_zebradanio <- 105

##### repro_end ####
## Study IDs: 73, 75, 117
## Notes: Ordinal date for August 15. End of the spawning period for wild fish in native waters, in India/Asia, corresponding with monsoon season. 
repro_end_zebradanio <- 227

##----END Reproduction SECTION ----##

##----END PARAMETERS SECTION----##

#### ADDITIONAL INFO ####
## Notes:

##----END ADDITIONAL INFO SECTION ----##
