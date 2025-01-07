##~             ,''''''''''''''.
##~~           /   USEPA FISH   \
##~   >~',*>  <  TOX TRANSLATOR  )
##~~           \ v1.0 'Doloris' /
##~             `..............'
##~~
##~  N. Pollesch - pollesch.nathan@epa.gov

#### Header Info ####
## Normalization Functions for Parameter,  s_max

## Information from parameters_master.RData
# t(FishToxTranslator::parameters_master[ 11 ,]) #Uncomment to pull data from package into R

#### Derivation Notes: ####
#survival max (s_max) is calculated as a 99.999% chance of mortality at the average lifespan

#### Example Normalizations: ####
# lifespan in years is converted to days
# the survival rate (0.1%) for that many days is used by taking .001^(1/(lifespan_in_years*365))
.001^(1/(8*365.25)) #average lifespan of 8 years assumed
