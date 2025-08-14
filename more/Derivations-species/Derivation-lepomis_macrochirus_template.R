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

##### Lepomis macrochirus ####
## Notes:
##### bluegill ####
## Notes:

##----END SPECIES INFO SECTION----###

#### PARAMETERS ####

#### Growth/Size ####
## General Notes on Growth/Size:

##### z_hatch ####
## Study IDs: 223,240,280
## Notes: mean value for all studies of unsexed hatchlings. All hatch size values are similar and fish are too young to sex for female specific values 
z_hatch_bluegill<- mean(c(4,6,4.5,4,5))

##### z_inf ####
## Study IDs: 303
## Notes: Values for wild females from 'upper and lower river values' only. Emiquon Lake Preserve values disregarded due to higher reported growth rate, maximum length, and reproductive fitness from environmental conditions
urz_inf<-180 
lrz_inf<-170
z_inf_bluegill <- mean(c(urz_inf,lrz_inf))

##### k_g ####
## Study IDs: 301
## Notes: Mean of values for wild females from 'upper and lower river values' that are converted to daily rate 
ur<-0.48 
lr<-0.69
# average and divide by 365 for daily rate
c(ur,lr)/365.25
k_g_bluegill <- mean(c(ur,lr))/365.25

##### var_k_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time. 

# Set the parameters
k=k_g_bluegill # Kappa (vB growth rate)
z0=z_hatch_bluegill #Size at hatch (mm)
zinf=z_inf_bluegill # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_bluegill , z0 = z_hatch_bluegill , zinf = z_inf_bluegill, zrat=0.95 , zratmax=0.99 ){
  
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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_bluegill, z0=z_hatch_bluegill, zinf=z_inf_bluegill, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_k_g_bluegill <- k_variance

##### var_e_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_bluegill # Kappa (vB growth rate)
z0=z_hatch_bluegill #Size at hatch (mm)
zinf=z_inf_bluegill # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_bluegill , z0 = z_hatch_bluegill , zinf = z_inf_bluegill, zrat=0.95 , zratmax=0.99 ){
  
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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_bluegill, z0=z_hatch_bluegill, zinf=z_inf_bluegill, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_e_g_bluegill <- k_variance

##### dd_g ####
## Study IDs:
## Notes:

##### allo_slope ####
## Study IDs: 222, 230, 232, 297, 298
## Notes: mean of reported study slopes
b <-(c(3.118, 3.1, 3.13, 2.94, 3.339, 3.429, 3.05305))
allo_slope_bluegill <- mean(b)

##### allo_intercept ####
## Study IDs:  224, 231, 233, 294, 299
## Notes: mean of study intercepts, converted for the normalization of log-log form and/or length type (FL/SL)
a <- (c(0.00001436, 0.000009929,0.000009754, 0.00001123, 0.000002426, 0.000003742, 0.000002548,0.000009930))
allo_intercept_bluegill <- mean(a)

##----END Growth/Size SECTION----##

#### Survival ####
## Notes on survival:

##### s_min ####
## Study IDs: 216, 234, 285
## Notes: Mean value for normalized survival rate of wild, unsexed, embryo-larval fish
s_min_bluegill <- mean(c(0.7981^(1/5), 0.697^(1/4), 0.9^(1/5)))

##### s_max ####
## Study IDs: 225, 249, 292
## Notes: Survival max is calculated as 99.999% chance of mortality at the average lifespan. Reported lifespan for wild, unsexed fish is 8 years old. 
s_max_bluegill <- .001^(1/(8*365.25))

##### s_a ####
## Study IDs:
## Notes: fitted using z_repo, s_min, and s_max to have the curve rise at the juvenile to adult transition size and plateau at z_repro 
s_a_bluegill <- .5

##### s_b ####
## Study IDs: 
## Notes: fitted using z_repo, s_min, and s_max to start curve at the juvenile to adult transition size
s_b_bluegill <- 67

##----END Survival SECTION----##

#### Reproduction ####
## Notes on reproduction:

##### post_spawning_mortality ####
## Study IDs:
## Notes: In reproductive studies no post spawning mortality was noted, assumed that post_spawning_mortality is 0 
post_spawning_mortality_bluegill <- 0

##### z_repro ####
## Study IDs: 250, 259
## Notes: Mean value for the smallest size/lowest bounds at reproduction for studies with wild female values
z_repro_bluegill <- mean(c(64,80.8))

##### sex_ratio ####
## Study IDs:
## Notes: In reproductive studies no skewed sex ratios were noted, assumed that sex_ratio is .5
sex_ratio_bluegill <- 0.5

##### fecu_slope ####
## Study IDs: 270, 879
## Notes: Fitted value for wild females using reported fecundity power law and egg per gram body weight

#Fitting Fecundity Function
#Create data sets for fitting
#SID 270, eggs per spawn 
L <- (70:260)
x <- L
y1 <- (((x^2.839)*0.004602566)/6) 
data1 <- data.frame(x = x, y = y1)

#SID 879, eggs per gram of body weight per spawn
x2 <-c(0,80,80,111,111,260,260)
y2 <- c(0,(270/6),(680/6),(760/6),(1912/6),(11840/6),(28129/6)) 
data2 <- data.frame(x = x2, y = y2) 

data.all<-data.frame(x=append(x,x2),y=append(y1,y2))

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data.all, start = list(a = 1, b = 1))
log_fit #a~0.001352, b~2.730768

fecu_slope_bluegill <- 2.730768

##### fecu_intercept ####
## Study IDs: 271, 879
## Notes: Fitted value for wild females using normalized fecundity power law and egg per gram body weight

# Fitting Fecundity Function
# Create data sets for fitting
#SID 270, eggs per spawn 
L <- (70:260)
x <- L
y1 <- (((x^2.839)*0.004602566)/6) 
data1 <- data.frame(x = x, y = y1)

#SID 879, eggs per gram of body weight per spawn
x2 <-c(0,80,80,111,111,260,260)
y2 <- c(0,(270/6),(680/6),(760/6),(1912/6),(11840/6),(28129/6)) 
data2 <- data.frame(x = x2, y = y2) 

data.all<-data.frame(x=append(x,x2),y=append(y1,y2))

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data.all, start = list(a = 1, b = 1))
log_fit #a~0.001352, b~2.730768

fecu_intercept_bluegill <- 0.001352  #fitted value 

##### hatch_rate ####
## Study IDs:  254, 261, 273, 276
## Notes: Mean value for wild sourced fish excluding outlier value from SID 273
hatch_rate_bluegill <- mean (c(.939, .17, .20, .33, .53))

##### spawn_int ####
## Study IDs: 284
## Notes: reported inter-spawn interval ranges from 7-10 days for all SIDS, mean of reported interval for wild female study, 284
spawn_int_bluegill <- 8

##### spawns_max_season ####
## Study IDs: 218, 237, 238, 291
## Notes: Spawns per season ranged from 1-11 across multiple studies, for spawning season ranging from day 159-214
spawns_max_season_bluegill <- mean(c(1,11))
  
##### repro_start ####
## Study IDs: 274
## Notes: Ordinal date for 8 June
repro_start_bluegill <- 159
##### repro_end ####
## Study IDs: 275
## Notes: Ordinal date for 2 August
repro_end_bluegill <- 214

##----END Reproduction SECTION ----##

##----END PARAMETERS SECTION----##

#### ADDITIONAL INFO ####
## Notes:

##----END ADDITIONAL INFO SECTION ----##
