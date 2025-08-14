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

##### Oryzias latipes ####
## Notes:
##### Japanese Medaka ####
## Notes: Additional common names are Japanese medaka or Japanese rice fish

##----END SPECIES INFO SECTION----###

#### PARAMETERS ####

#### Growth/Size ####
## General Notes on Growth/Size:

##### z_hatch ####
## Study IDs: 472, 487
## Notes: mean value for all studies of unsexed hatchlings. All hatch size values are similar and fish are too young to sex for female specific values
z_hatch_japanesemedaka <-mean(c(3.8, 4.2, 3.6, 5.8))

##### z_inf ####
## Study IDs: 489
## Notes: Maximum reported total length for broodstock lab populations
z_inf_japanesemedaka <- 52

##### k_g ####
## Study IDs: 486, 490
## Notes: Mean value for unsexed fish in lab from captive breeding colonies
k_g_japanesemedaka <- mean(c(0.00947, 0.010))

##### var_k_g ####
## Study IDs:
## Notes:  Fitted to derive variance in k bounds based on size at a given time.

# Set the parameters
k=k_g_japanesemedaka # Kappa (vB growth rate)
z0=z_hatch_japanesemedaka #Size at hatch (mm)
zinf=z_inf_japanesemedaka # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_japanesemedaka , z0 = z_hatch_japanesemedaka , zinf = z_inf_japanesemedaka, zrat=0.95 , zratmax=0.99 ){

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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_japanesemedaka, z0=z_hatch_japanesemedaka, zinf=z_inf_japanesemedaka, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_k_g_japanesemedaka <- k_variance

##### var_e_g ####
## Study IDs:
## Notes: Fitted to derive variance in k bounds based on size at a given time.

# Set the parameters
k=k_g_japanesemedaka # Kappa (vB growth rate)
z0=z_hatch_japanesemedaka #Size at hatch (mm)
zinf=z_inf_japanesemedaka # Max Length (mm)

z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean

VarianceK <- function( sigma_k , k = k_g_japanesemedaka , z0 = z_hatch_japanesemedaka , zinf = z_inf_japanesemedaka, zrat=0.95 , zratmax=0.99 ){

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
Otpt <- optimize(VarianceK , interval=c(0,100) , k=k_g_japanesemedaka, z0=z_hatch_japanesemedaka, zinf=z_inf_japanesemedaka, zrat=0.95, zratmax=0.99)
Otpt

# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

var_e_g_japanesemedaka <- k_variance

##### dd_g ####
## Study IDs:
## Notes:

##### allo_slope ####
## Study IDs: 492
## Notes: Reported study slope. Best correlated allometry equation for length-weight measurements from various studies and FishBase data.
allo_slope_japanesemedaka <- 2.79

##### allo_intercept ####
## Study IDs: 493
## Notes: Reported study slope. Best correlated allometry equation for length-weight measurements from various studies and FishBase data.
allo_intercept_japanesemedaka <- 2.16E-05

##----END Growth/Size SECTION----##

#### Survival ####
## Notes on survival:

##### s_min ####
## Study IDs: 875
## Notes: Larval survival (7 dph) for unsexed fish in laboratory conditions. High reported survivability for model lab species, not commonly tested in wild conditions
s_min_japanesemedaka <-.88^(1/7)

##### s_max ####
## Study IDs: 457
## Notes: Survival max is calculated as 99.999% chance of mortality at the average lifespan. Reported lifespan for wild, unsexed fish is 16 months old.
s_max_japanesemedaka <- .001^(1/(1.25*365.25))

##### s_a ####
## Study IDs:
## Notes: fitted using z_repo, s_min, and s_max to have the curve rise at the juvenile to adult transition size and plateau at z_repro
s_a_japanesemedaka <- 1

##### s_b ####
## Study IDs:
## Notes: fitted using z_repo, s_min, and s_max to start curve at the juvenile to adult transition size
s_b_japanesemedaka <- 28

##----END Survival SECTION----##

#### Reproduction ####
## Notes on reproduction:

##### post_spawning_mortality ####
## Study IDs:
## Notes: In reproductive studies no post spawning mortality was noted, assumed that post_spawning_mortality is 0
post_spawning_mortality_japanesemedaka <- 0

##### z_repro ####
## Study IDs:495
## Notes: Smallest converted size for study. No length type reported for other studies/reported values for z_repro.
z_repro_japanesemedaka <- 29.425

##### sex_ratio ####
## Study IDs:
## Notes: Assumed, no evidence of a varying sex ratio
sex_ratio_japanesemedaka <- 0.5

##### fecu_slope ####
## Study IDs: 515, 873
## Notes:  Fitted value for eggs per spawn using sources for both captive breeding colonies and wild fish
# Fitting Fecundity Function
#Create data sets for fitting
L<-29:52
W <- 2.16E-05 * L^2.79
x <- L
y1 <- (204.6*W-9.9)/4
fecumed1 <- data.frame(x = x, y = y1)

x2 <- L
y2 <- -.91+.62*(1.177*L)
fecumed2 <- data.frame(x = x, y = y2)

data.all.med<-data.frame(x=append(x,x),y=append(y1,y2))

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data.all.med, start = list(a = 1, b = 1))
log_fit #a~0.01383 b~2.07711

fecu_slope_japanesemedaka <- 2.07711

##### fecu_intercept ####
## Study IDs: 848, 874
## Notes: Fitted value for eggs per spawn using sources for both captive breding colonies and wild fish
# Fitting Fecundity Function
#Create data sets for fitting
L<-29:52
W <- 2.16E-05 * L^2.79
x <- L
y1 <- (204.6*W-9.9)/4
fecumed1 <- data.frame(x = x, y = y1)

x2 <- L
y2 <- -.91+.62*(1.177*L)
fecumed2 <- data.frame(x = x, y = y2)

data.all.med<-data.frame(x=append(x,x),y=append(y1,y2))

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data.all.med, start = list(a = 1, b = 1))
log_fit #a~0.01383 b~2.07711

fecu_intercept_japanesemedaka <- 0.01383

##### hatch_rate ####
## Study IDs: 482, 497, 519, 871, 887
## Notes: Mean hatch rate for SIDs, SID 497 disregarded for reported high hatchability in optimized lab conditions
hatch_rate_japanesemedaka <- mean (c(.758, .73, .9, .744, .587, .5))

##### spawn_int ####
## Study IDs: 466, 467, 473, 483, 520
## Notes: Both wild and laboratory populations report spawning can occur everyday
spawn_int_japanesemedaka <- 1

##### spawns_max_season ####
## Study IDs: 468
## Notes: reported 10-20 spawns per individual
spawns_max_season_japanesemedaka <- mean(c(10,20))

##### repro_start ####
## Study IDs: 503
## Notes: Ordinal date for 19 April
repro_start_japanesemedaka <- 109

##### repro_end ####
## Study IDs: 504
## Notes: Ordinal date for 10 September
repro_end_japanesemedaka <- 253

##----END Reproduction SECTION ----##

##----END PARAMETERS SECTION----##

#### ADDITIONAL INFO ####
## Notes:

##----END ADDITIONAL INFO SECTION ----##
