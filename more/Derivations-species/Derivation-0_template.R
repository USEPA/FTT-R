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

##### >>Enter Sci Name ####
## Notes:
##### >>Enter Com Name ####
## Notes:

##----END SPECIES INFO SECTION----###

#### PARAMETERS ####

#### Growth/Size ####
## General Notes on Growth/Size:

##### z_hatch ####
## Study IDs:
## Notes:

##### z_inf ####
## Study IDs:
## Notes:

##### k_g ####
## Study IDs:
## Notes:

##### var_k_g ####
## Study IDs:
## Notes:

##### var_e_g ####
## Study IDs:
## Notes:

##### dd_g ####
## Study IDs:
## Notes:

##### allo_slope ####
## Study IDs:
## Notes:

##### allo_intercept ####
## Study IDs:
## Notes:

##----END Growth/Size SECTION----##

#### Survival ####
## Notes on survival:

##### s_min ####
## Study IDs:
## Notes:

##### s_max ####
## Study IDs:
## Notes:

##### s_a ####
## Study IDs:
## Notes:

##### s_b ####
## Study IDs:
## Notes:

##----END Survival SECTION----##

#### Reproduction ####
## Notes on reproduction:

##### post_spawning_mortality ####
## Study IDs:
## Notes:

##### z_repro ####
## Study IDs:
## Notes:

##### sex_ratio ####
## Study IDs:
## Notes:

##### fecu_slope ####
## Study IDs:
## Notes:

##### fecu_intercept ####
## Study IDs:
## Notes:

##### hatch_rate ####
## Study IDs:
## Notes:

##### spawn_int ####
## Study IDs:
## Notes:

##### spawns_max_season ####
## Study IDs:
## Notes:

##### repro_start ####
## Study IDs:
## Notes:

##### repro_end ####
## Study IDs:
## Notes:

##----END Reproduction SECTION ----##

##----END PARAMETERS SECTION----##

#### ADDITIONAL INFO ####
## Notes:

##----END ADDITIONAL INFO SECTION ----##
