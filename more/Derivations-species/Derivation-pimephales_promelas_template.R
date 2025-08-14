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

##### Pimephales promelas ####
## Notes:
##### fathead minnow ####
## Notes:

##----END SPECIES INFO SECTION----###

#### PARAMETERS ####

#### Growth/Size ####
## General Notes on Growth/Size:

##### z_hatch ####
## Study IDs: 691, 720, 729, 732
## Notes: Mean of all studies. All values for hatch size were similar for the unsexed fish. 
z_hatch_fatheadminnow <- mean(c(5.8425,5.14,5,5,6))

##### z_inf ####
## Study IDs: 703, 707
## Notes: Highest value for lengths within normal ranges, maximum lengths excluded. 
z_inf_fatheadminnow <- 73

##### k_g ####
## Study IDs: 673
## Notes: Mean value at a daily rate for one study. Unsexed, Canadian population of fish
k_g_fatheadminnow <- mean(c((.82/365.25),(.46/365.25), (.93/365.25)))

##### var_k_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_fatheadminnow # Kappa (vB growth rate)
z0=z_hatch_fatheadminnow #Size at hatch (mm)
zinf=z_inf_fatheadminnow # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_fatheadminnow , z0 = z_hatch_fatheadminnow , zinf = z_inf_fatheadminnow, zrat=0.95 , zratmax=0.99 ){
  
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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_fatheadminnow, z0=z_hatch_fatheadminnow, zinf=z_inf_fatheadminnow, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_k_g_fatheadminnow <- k_variance

##### var_e_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time

# Set the parameters
k=k_g_fatheadminnow # Kappa (vB growth rate)
z0=z_hatch_fatheadminnow #Size at hatch (mm)
zinf=z_inf_fatheadminnow # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_fatheadminnow , z0 = z_hatch_fatheadminnow , zinf = z_inf_fatheadminnow, zrat=0.95 , zratmax=0.99 ){
  
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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_fatheadminnow, z0=z_hatch_fatheadminnow, zinf=z_inf_fatheadminnow, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_e_g_fatheadminnow <- k_variance

##### dd_g ####
## Study IDs:
## Notes:

##### allo_slope ####
## Study IDs: 708, 711
## Notes: Mean value for populations of wild, female fish
allo_slope_fatheadminnow <- mean(c(3.29, 3.1952))

##### allo_intercept ####
## Study IDs: 709, 712
## Notes: Mean value for populations of wild, female fish
allo_intercept_fatheadminnow <- mean(c(0.00000376, 0.00000538))

##----END Growth/Size SECTION----##

#### Survival ####
## Notes on survival:

##### s_min ####
## Study IDs: 728
## Notes: Smallest suvival value. Lab studies only. Actual FTT value (96%) comes from cyprinid fishes, not specific to p. promelas. 
s_min_fatheadminnow <- .811^(1/7)

##### s_max ####
## Study IDs:  676, 700
## Notes: Survival max is calculated as 99.999% chance of mortality at the average lifespan. Mean vlue for reported lifespan for wild, unsexed fish is 3 years old.  
s_max_fatheadminnow <- .001^(1/(3*365.25))

##### s_a ####
## Study IDs:
## Notes:  fitted using z_repo, s_min, and s_max to have the curve rise at the juvenile to adult transition size and plateau at z_repro 
s_a_fatheadminnow <- .75

##### s_b ####
## Study IDs:
## Notes:  fitted using z_repo, s_min, and s_max to start curve at the juvenile to adult transition size
s_b_fatheadminnow <- 40

##----END Survival SECTION----##

#### Reproduction ####
## Notes on reproduction:

##### post_spawning_mortality ####
## Study IDs:
## Notes: In reproductive studies no post spawning mortality was noted, assumed that post_spawning_mortality is 0 
post_spawning_mortality_fatheadminnow <- 0

##### z_repro ####
## Study IDs: 685, 726, 727
## Notes: Mean value for wild originating females 
z_repro_fatheadminnow <- mean(c( 40, 45, 62))

##### sex_ratio ####
## Study IDs:
## Notes:  In reproductive studies no skewed sex ratios were noted, assumed that sex_ratio is .5
sex_ratio_fatheadminnow <- .5 

##### fecu_slope ####
## Study IDs: 724, 868
## Notes: fitted value of eggs per spawn for wild females using reported fecundity power law and eggs per gram body weight

# fitting fecundity 
# reproductive length range for FHM
L<-45:75

#NLS Fitting for individual studies
# SID 724, cold water population
x <- L
y1 <- -372+(14*x)
data1 <- data.frame(x = x, y = y1)

nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data1, start = list(a = 1, b = 1))
log_fit #a~0.2813 b~1.8076

#SID 868
fhm_mmSL_g <- (4.283333e-05 * (x^2.74))* 1700 
data3<- data.frame(x = x, y = (fhm_mmSL_g/12))

nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data3, start = list(a = .07276, b = 2.74), algorithm="port")
log_fit #a~0.006068  b~2.74

#NLS fitting both relationship as one
#SID 724
x <- L
yblue <- .2662 * (x^1.8208) 
datablue <- data.frame(x = x, y = yblue)

#SID 868
ygreen <- .006068 * (x^2.74)
datagreen <- data.frame(x = x, y = ygreen)

data_bluegreen<-data.frame(rbind(datablue, datagreen))

nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data_bluegreen, start = list(a = 1, b = 1))
log_fit #a~0.04032   b~2.28012 

fecu_slope_fatheadminnow <- 2.28012 

##### fecu_intercept ####
## Study IDs:725, 868
## Notes: fitted value of eggs per spawn for wild females using reported fecundity power law and eggs per gram body weight

# fitting fecundity 
# reproductive length range for FHM
L<-44:75

#NLS Fitting for individual studies
# SID 724, cold water population
x <- L
y1 <- -372+(14*x) #cold water
data1 <- data.frame(x = x, y = y1)

nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data1, start = list(a = 1, b = 1))
log_fit #a~0.2662 b~1.8208

#SID 868
fhm_mmSL_g <- (4.283333e-05 * (x^2.74))* 1700 
plot(fhm_mmSL_g/12)
data3<- data.frame(x = x, y = (fhm_mmSL_g/12))

nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data3, start = list(a = .07276, b = 2.74), algorithm="port")
log_fit #a~0.006068  b~2.74

#NLS fitting both relationship as one
#SID 724
x <- L
yblue <- .2662 * (x^1.8208) 
datablue <- data.frame(x = x, y = yblue)

#SID 868
ygreen <- .006068 * (x^2.74)
datagreen <- data.frame(x = x, y = ygreen)

data_bluegreen<-data.frame(rbind(datablue, datagreen))

nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data_bluegreen, start = list(a = 1, b = 1))
log_fit #a~0.04056   b~2.27870  
fecu_intercept_fatheadminnow <- 0.04032

##### hatch_rate ####
## Study IDs: 686
## Notes: Mean value for unsexed hatchlings of wild originating fish in Canada. 
hatch_rate_fatheadminnow <- mean(c(.306,.415,.399,.620))
  
##### spawn_int ####
## Study IDs: 684, 689, 692, 715, 722
## Notes: reported interval
spawn_int_fatheadminnow <- 4
  
##### spawns_max_season ####
## Study IDs: 690, 723
## Notes: Reported value
spawns_max_season_fatheadminnow <- 12

##### repro_start ####
## Study IDs: 677, 687, 701
## Notes: Ordinal date for 16 June
repro_start_fatheadminnow <- 167

##### repro_end ####
## Study IDs: 678, 688, 702
## Notes: Ordinal date for 1 August
repro_end_fatheadminnow <- 213
  
##----END Reproduction SECTION ----##

##----END PARAMETERS SECTION----##

#### ADDITIONAL INFO ####
## Notes:

##----END ADDITIONAL INFO SECTION ----##
