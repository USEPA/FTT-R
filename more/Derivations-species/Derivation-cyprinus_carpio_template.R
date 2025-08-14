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

##### Cyprinus carpio ####
## Notes:
##### common carp ####
## Notes: Other common names include German carp, European carp, leather carp, and mirror carp. Occasionally C. carpio will be called koi, although koi also more fittingly refers to Cyprinus rubrofuscus.  

##----END SPECIES INFO SECTION----###

#### PARAMETERS ####

#### Growth/Size ####
## General Notes on Growth/Size:

##### z_hatch ####
## Study IDs: 320, 387, 395, 419
## Notes: Mean value, all studies reported similar hatch sizes for unsexed hatchlings. 
z_hatch_commoncarp <- mean(c(4.9,6.3428,5.31,4.75))

##### z_inf ####
## Study IDs: 432
## Notes: Longest female reported size for the Caspian Sea, larger than most introduced populations. Carp average sizes of about 800 mm, maximum sizes range from 1000-2000, widespread introduction of carp likely affect this range discrepancy
z_inf_commoncarp <- 1161.582

##### k_g ####
## Study IDs: 433, 439
## Notes: Mean value for unquestionable, female only values/studies via FishBase
k_g_commoncarp <- mean(c(0.001067762, 0.0009582478))

##### var_k_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_commoncarp # Kappa (vB growth rate)
z0=z_hatch_commoncarp #Size at hatch (mm)
zinf=z_inf_commoncarp # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_commoncarp , z0 = z_hatch_commoncarp , zinf = z_inf_commoncarp, zrat=0.95 , zratmax=0.99 ){
  
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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_commoncarp, z0=z_hatch_commoncarp, zinf=z_inf_commoncarp, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_k_g_commoncarp <- k_variance

##### var_e_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_commoncarp # Kappa (vB growth rate)
z0=z_hatch_commoncarp #Size at hatch (mm)
zinf=z_inf_commoncarp # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_commoncarp , z0 = z_hatch_commoncarp , zinf = z_inf_commoncarp, zrat=0.95 , zratmax=0.99 ){
  
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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_commoncarp, z0=z_hatch_commoncarp, zinf=z_inf_commoncarp, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_e_g_commoncarp <- k_variance

##### dd_g ####
## Study IDs:
## Notes:

##### allo_slope ####
## Study IDs: 321, 405, 424, 434, 437
## Notes: Mean value for wild female studies
allo_slope_commoncarp <- mean(c(2.692,2.78,2.8742,2.93,2.7977))

##### allo_intercept ####
## Study IDs: 335, 406, 425, 435, 438
## Notes: Mean value for wild female studies
allo_intercept_commoncarp <- mean(c(6.613*10^-5,0.00001722,0.00008476,0.00006887,0.0000407))

##----END Growth/Size SECTION----##

#### Survival ####
## Notes on survival:

##### s_min ####
## Study IDs: 306, 388
## Notes: Mean minimum survival for larval fish only. 
s_min_commoncarp <- mean(c((0.916^(1/14)),(0.707^(1/14)),(0.418^(1/14)),(0.79^(1/4))))

##### s_max ####
## Study IDs: 317, 344
## Notes: Survival max is calculated as 99.999% chance of mortality at the average lifespan. Mean vlue for reported lifespan for wild, unsexed fish is 24 years old.
yr <- mean(c(28,20))
s_max_commoncarp <- .001^(1/(yr*365.25))

##### s_a ####
## Study IDs:
## Notes: fitted using z_repo, s_min, and s_max to have the curve rise at the juvenile to adult transition size and plateau at z_repro 
s_a_commoncarp <- 0.07

##### s_b ####
## Study IDs:
## Notes: fitted using z_repo, s_min, and s_max to start curve at the juvenile to adult transition size
s_b_commoncarp <- 255

##----END Survival SECTION----##

#### Reproduction ####
## Notes on reproduction:

##### post_spawning_mortality ####
## Study IDs:
## Notes:In reproductive studies no post spawning mortality was noted, assumed that post_spawning_mortality is 0 
post_spawning_mortality_commoncarp <- 0

##### z_repro ####
## Study IDs: 314, 315, 378, 392, 426, 441
## Notes: Mean converted values for female length at maturation for 50% of the population. Smallest size at reproduction for fish from Northern Europe and US populations was 255 mm TL and 104 mm TL for a tropical water population. 
z_repro_commoncarp <- mean(c(292.294, 350.814, 302.934,420, 215, 358.0492))

##### sex_ratio ####
## Study IDs:
## Notes: In reproductive studies no skewed sex ratios were noted, assumed that sex_ratio is .5
sex_ratio_commoncarp <- 0.5

##### fecu_slope ####
## Study IDs: 336, 877
## Notes: fitted value of eggs per spawn for wild females using reported fecundity power law and eggs per age

# Fitting Fecundity Function
# Create data sets for fitting
#SID 336
L <- (0:1200)
x <- L
y1 <- (((x^2.58)*0.03715352)/2) 
data1 <- data.frame(x = x, y = y1)

#SID 877
x2 <- c(292.944,528.514,405.644,696.064,662.554,975.314)
y2 <- c(131000/2,580000/2,158000/2,980000/2,690000/2,1830000/2) 
data2 <- data.frame(x = x2, y = y2) 

data.all<-data.frame(x=append(x,x2),y=append(y1,y2))

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data.all, start = list(a = .1, b = 2))
log_fit #a~0.01878, b~2.57841

fecu_slope_commoncarp <- 2.57841

##### fecu_intercept ####
## Study IDs: 337, 877
## Notes: fitted value of eggs per spawn for wild females using reported fecundity power law and eggs per age

# Fitting Fecundity Function
# Create data sets for fitting
#SID 337
L <- (0:1200)
x <- L
y1 <- (((x^2.58)*0.03715352)/2) 
data1 <- data.frame(x = x, y = y1)

#SID 877
x2 <- c(292.944,528.514,405.644,696.064,662.554,975.314)
y2 <- c(131000/2,580000/2,158000/2,980000/2,690000/2,1830000/2) 
data2 <- data.frame(x = x2, y = y2) 

data.all<-data.frame(x=append(x,x2),y=append(y1,y2))

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data.all, start = list(a = .1, b = 2))
log_fit #a~0.01878, b~2.57841

fecu_intercept_commoncarp <- 0.01878

##### hatch_rate ####
## Study IDs: 308, 389, 447, 861
## Notes: Mean hatch rate for unsexed hatchlings from commercial sources or captive breeding colonies
hatch_rate_commoncarp <- mean (c(.904, .854,.849,.79,.93,.96,.44, .45)) 

##### spawn_int ####
## Study IDs: 357
## Notes: Reported spawn interval for wild females from Spain
spawn_int_commoncarp <- 25

##### spawns_max_season ####
## Study IDs:
## Notes: Max reported spawns per season for French population. US and Canadian populations typically spawn twice if conditions are suitable, but tropical water populations can spawn all year around.
spawns_max_season_commoncarp <- 2

##### repro_start ####
## Study IDs: 345 
## Notes: Converted to ordinal date for 15 May. Canadian populations.
repro_start_commoncarp <- 135

##### repro_end ####
## Study IDs: 346
## Notes: Converted to ordinal date for 15 August. Canadian populations. 
repro_end_commoncarp <- 227

##----END Reproduction SECTION ----##

##----END PARAMETERS SECTION----##

#### ADDITIONAL INFO ####
## Notes:

##----END ADDITIONAL INFO SECTION ----##
