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

##### Cyprinodon variegatus ####
## Notes:
##### sheepshead minnow ####
## Notes:

##----END SPECIES INFO SECTION----###

#### PARAMETERS ####

#### Growth/Size ####
## General Notes on Growth/Size:

##### z_hatch ####
## Study IDs: 581
## Notes: Reported size at hatch
z_hatch_sheepsheadminnow <- 4

##### z_inf ####
## Study IDs: 580
## Notes: Reported upper length of range for unsexed/male fish via FishBase.org. Maximum lengths excluded
z_inf_sheepsheadminnow <- 73

##### k_g ####
## Study IDs: 563, 566
## Notes: Mean value for modeled populations of unsexed fish
k_g_sheepsheadminnow <- mean(c(0.013,0.014))

##### var_k_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_sheepsheadminnow # Kappa (vB growth rate)
z0=z_hatch_sheepsheadminnow #Size at hatch (mm)
zinf=z_inf_sheepsheadminnow # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_sheepsheadminnow , z0 = z_hatch_sheepsheadminnow , zinf = z_inf_sheepsheadminnow, zrat=0.95 , zratmax=0.99 ){
  
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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_sheepsheadminnow, z0=z_hatch_sheepsheadminnow, zinf=z_inf_sheepsheadminnow, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_k_g_sheepsheadminnow <- k_variance

##### var_e_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_sheepsheadminnow # Kappa (vB growth rate)
z0=z_hatch_sheepsheadminnow #Size at hatch (mm)
zinf=z_inf_sheepsheadminnow # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_sheepsheadminnow , z0 = z_hatch_sheepsheadminnow , zinf = z_inf_sheepsheadminnow, zrat=0.95 , zratmax=0.99 ){
  
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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_sheepsheadminnow, z0=z_hatch_sheepsheadminnow, zinf=z_inf_sheepsheadminnow, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_e_g_sheepsheadminnow <- k_variance

##### dd_g ####
## Study IDs:
## Notes:

##### allo_slope ####
## Study IDs: 545, 551, 564
## Notes: Mean for all study values for wild and modeled populations of unsexed fish. 
allo_slope_sheepsheadminnow <- mean(c(3.06, 3.35, 3.27))

##### allo_intercept ####
## Study IDs: 546, 553, 565
## Notes: Mean for all study values for wild and modeled populations of unsexed fish.. Intercept values were converted from cm to mm. 
allo_intercept_sheepsheadminnow <- mean(c((0.02188/(10^3.06)), (0.0087/(10^3.27)), (0.023/(10^3.35))))

##----END Growth/Size SECTION----##

#### Survival ####
## Notes on survival:

##### s_min ####
## Study IDs: 536, 537, 549, 560
## Notes: Mean value for hatchling and larval fish only (<30 dph)
s_min_sheepsheadminnow <- mean(c((.78^(1/28)), (.933^(1/4)), (.562^(1/3)), (.96^(1/24))))

##### s_max ####
## Study IDs: 567
## Notes: Survival max is calculated as 99.999% chance of mortality at the average lifespan. Mean value for reported lifespan for wild, unsexed fish is 3 years old.
s_max_sheepsheadminnow <- .001^(1/(3*365.25))

##### s_a ####
## Study IDs:
## Notes:  fitted using z_repo, s_min, and s_max to have the curve rise at the juvenile to adult transition size and plateau at z_repro 
s_a_sheepsheadminnow <- 1

##### s_b ####
## Study IDs:
## Notes: fitted using z_repo, s_min, and s_max to start curve at the juvenile to adult transition size
s_b_sheepsheadminnow <- 25

##----END Survival SECTION----##

#### Reproduction ####
## Notes on reproduction:

##### post_spawning_mortality ####
## Study IDs:
## Notes:In reproductive studies no post spawning mortality was noted, assumed that post_spawning_mortality is 0 
post_spawning_mortality_sheepsheadminnow <- 0

##### z_repro ####
## Study IDs: 862
## Notes: Reported size at maturation for females in lab from a captive breeding colony
z_repro_sheepsheadminnow <- 27 

##### sex_ratio ####
## Study IDs:
## Notes: In reproductive studies no skewed sex ratios were noted, assumed that sex_ratio is .5
sex_ratio_sheepsheadminnow <- 0.5

##### fecu_slope ####
## Study IDs: 836, 545, 551, 564
## Notes: NLS fitted using eggs per female per unit of body weight and allometry. Assumed that fish will spawn three times per year, given that average spawning output is 100-300 eggs per spawn. 

#Plotting relationship of mass to eggs/spawn from SID 836
x<-(27:73) #Reproductively mature size range

#allometric function for eggs per female per unit of body weight
shmfecu <- ((0.00001906 * (x^3.22667))* 66.441 ) / 3

## Create data sets for fitting
shmdata<- data.frame(x = x, y = (shmfecu))

indvshmdata<-data.frame(x=c(39,34),y=c(232.54,140)) # more data

shm.data.all<- rbind(shmdata, indvshmdata)

nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = shm.data.all, start = list(a = 1, b = 1))
log_fit  #a~0.001037   b~3.012222 

fecu_slope_sheepsheadminnow <- 3.012222 

##### fecu_intercept ####
## Study IDs: 836, 546, 552, 565
## Notes:NLS fitted using eggs per female per unit of body weight and allometry. Assumed that fish will spawn three times per year, given that average spawning output is 100-300 eggs per spawn. 

#Plotting relationship of mass to eggs/spawn from Rutter er al., 2012 (See Life History DB)
x<-(27:73) #Reproductively mature size range

#allometric function for eggs per female per unit of body weight per spawn
shmfecu <- ((0.00001906 * (x^3.22667))* 66.441 ) / 3
plot(shmfecu)

## Create data sets for fitting
shmdata<- data.frame(x = x, y = (shmfecu))

indvshmdata<-data.frame(x=c(39,34),y=c(232.54,140)) # more data

shm.data.all<- rbind(shmdata, indvshmdata)

nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = shm.data.all, start = list(a = 1, b = 1))
log_fit #a~0.001037   b~3.012222   

fecu_intercept_sheepsheadminnow <- 0.001037 

##### hatch_rate ####
## Study IDs: 550
## Notes: Reported value
hatch_rate_sheepsheadminnow <- .675

##### spawn_int ####
## Study IDs: 573, 583
## Notes: Midpoint for reported interval of both studies 
spawn_int_sheepsheadminnow <- 4

##### spawns_max_season ####
## Study IDs: 885
## Notes: Fish were only noted to "spawn several times during the season..." From average eggs per spawn and absolute fecundity, 3 spawns can be assumed. 
spawns_max_season_sheepsheadminnow <- 3 

##### repro_start ####
## Study IDs: 572
## Notes: Ordinal date for 1 May. Summer spawning period for northern extent of range, FL, USA
repro_start_sheepsheadminnow <- 121

##### repro_end ####
## Study IDs: 571
## Notes: Ordinal date for 1 September. Summer spawning period for northern extent of range, FL, USA.
repro_end_sheepsheadminnow <- 245

##----END Reproduction SECTION ----##

##----END PARAMETERS SECTION----##

#### ADDITIONAL INFO ####
## Notes:

##----END ADDITIONAL INFO SECTION ----##
