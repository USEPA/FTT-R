##~             ,''''''''''''''.
##~~           /   USEPA FISH   \
##~   >~',*>  <  TOX TRANSLATOR  )
##~~           \ v1.0 'Doloris' /
##~             `..............'
##~~
##~  N. Pollesch - pollesch.nathan@epa.gov

#### Header Info ####
## Normalization Functions for Parameter,  z_inf

## Information from parameters_master.RData
# t(FishToxTranslator::parameters_master[ 2 ,]) #Uncomment to pull data from package into R

#### Derivation Notes: ####

#### Example Normalizations: ####
### SHEEPSHEAD MINNOW ###
### Sheepshead Minnow length-length conversion parameters via FishBase where TL = A + B * mm FL/SL

## SL to TL
# a=0.0, b=1.230
SHM_SL_to_TL <- function(SL) {
  return(0 + (1.230*SL))
}

## FL to TL
# a=0.0, b=1.000
SHM_FL_to_TL <- function(FL) {
  return(0 + (1.000*FL))
}


### CARP ###
### Carp length-length conversion parameters via FishBase where TL = A + B * mm FL/SL

## SL to TL
# a=3.524, b= 1.117; for unsexed adult fish
CRP_AD_SL_to_TL <- function(SL) {
  return(3.524 + (1.117*SL))
}

## FL to TL
# a=1.822, b= 1.064; for unsexed adults
CRP_AD_FL_to_TL <- function(FL) {
  return(1.822 + (1.064*FL))
}


### Zebrafish ###
### Zebrafish length-length conversion parameters via FishBase where TL = A + B * mm FL/SL
## SL to TL
# a=0.0, b=1.165, 1.229, 1.268; mean a=0.0, mean b=1.220667
ZBF_SL_to_TL <- function(SL) {
  return(0 + (1.220667*SL))
}

## FL to TL
# a=0.0, b=1.080, 1.098; mean a=0.0, mean b=1.089
ZBF_FL_to_TL <- function(FL) {
  return(0 + (1.089*FL))
}


### Bluegill ###
### Bluegill length-length conversion parameters via FishBase where TL = A + B * mm FL/SL
## SL to TL
# a=0.0, b=1.160, 1.225, 1.265, 1.267; mean a=0.0, mean b=1.22925
BLG_SL_to_TL <- function(SL) {
  return(0 + (1.22925*SL))
}
## FL to TL
# a=0.0, b=b=1.024, 1.049; mean a=0.0, mean b=1.0365
BLG_FL_to_TL <- function(FL) {
  return(0 + (1.0365*FL))
}


### MEDAKA ###
### Medaka length-length conversion parameters via FishBase where TL = A + B * mm FL/

## SL to TL
# a=0.0, b=1.177;
MED_SL_to_TL <- function(SL) {
  return(0 + (1.177*SL))
}

## FL to TL
# a=0.0, b=1.000
MED_FL_to_TL <- function(FL) {
  return(0 + (1.000*FL))
}


### FATHEAD MINNOW ###
### Fathead Minnow length-length conversion parameters via FishBase where TL = A + B * mm FL/SL

## SL to TL
# a=0.0, b=1.205
FHM_SL_to_TL <- function(SL) {
  return(0 + (1.230*SL))
}

## FL to TL
# a=0.0, b=1.050
FHM_FL_to_TL <- function(FL) {
  return(0 + (1.050*FL))
}
