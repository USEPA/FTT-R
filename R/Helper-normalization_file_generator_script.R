##~             ,''''''''''''''.
##~~           /   USEPA FISH   \\
##~   >~',*>  <  TOX TRANSLATOR  )
##~~           \\ v1.0 'Doloris' /
##~             `..............'
##~~
##~  N. Pollesch - pollesch.nathan@epa.gov

## File description:
## This is a helper script that will generate the Normalization-*.R template R script
## for the parameter normalization files based on the parameters that are in the FishToxTranslator::parameters_master .RData file

numofparams<-dim(FishToxTranslator::parameters_master)[1]
for(i in 1:numofparams) {
  pname<-FishToxTranslator::parameters_master[i,2]
  plines<-paste(
    "##~             ,''''''''''''''.
##~~           /   USEPA FISH   \\
##~   >~',*>  <  TOX TRANSLATOR  )
##~~           \\ v1.0 'Doloris' /
##~             `..............'
##~~
##~  N. Pollesch - pollesch.nathan@epa.gov

#### Header Info ####
## Normalization Functions for Parameter, ",pname,"

## Information from parameters_master.RData
# t(FishToxTranslator::parameters_master[",i,",]) #Uncomment to pull data from package into R

#### Derivation Notes: ####

#### Example Normalizations: ####")
  write_lines(plines, path = sprintf("R/Normalization-%s.R", pname))

}
