##~             ,''''''''''''''.
##~~           /   USEPA FISH   \
##~   >~',*>  <  TOX TRANSLATOR  )
##~~           \ v1.0 'Doloris' /
##~             `..............'
##~~
##~  N. Pollesch - pollesch.nathan@epa.gov

#### Header Info ####
## Normalization Functions for Parameter,  allo_intercept

## Information from parameters_master.RData
# t(FishToxTranslator::parameters_master[ 9 ,]) #Uncomment to pull data from package into R

#### Derivation Notes: ####

#### Example Normalizations: ####
### log-log intercept conversion ###
# To convert out of log-log form take raise ten to the power of 'a'
a <-
10^a

# If a is in log-log form and measured in cm/m/etc, take 10 raised to a and multiply using dimensional analysis
a_cm <-
  (10^a_cm)*10

a_m <-
  (10^a_m)*100

### Length conversion for intercept in cm to mm ###
a_cm <-
b <-

a_mm <- a_cm/(10^b)
a_mm
