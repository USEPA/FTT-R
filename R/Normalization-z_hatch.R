##~             ,''''''''''''''.
##~~           /   USEPA FISH   \
##~   >~',*>  <  TOX TRANSLATOR  )
##~~           \ v1.0 'Doloris' /
##~             `..............'
##~~
##~  N. Pollesch - pollesch.nathan@epa.gov

#### Header Info ####
## Normalization Functions for Parameter,  z_hatch

## Information from parameters_master.RData
# t(FishToxTranslator::parameters_master[ 1 ,]) #Uncomment to pull data from package into R

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

# a=0.00, b= 1.256; for juveniles
CRP_LV_SL_to_TL <- function(SL) {
  return(0 + (1.256*SL))
}

# a=0.00, b= 1.118; for juveniles
CRP_LV_FL_to_TL <- function(SL) {
  return(0 + (1.118*SL))
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
