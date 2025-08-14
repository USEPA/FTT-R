#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Data: Sheepshead Minnow (Cyprinodon variegatus) Life History Parameters
#'
#'
#' @docType data
#'
#' @example data(cyprinodon_variegatus)
#'
#' @format data.frame
#'
#' @keywords datasets
#'
"cyprinodon_variegatus"

# This object was created by importing the life history parameter list file .csv for Fathead minnow and saving it as an RData object
cyprinodon_variegatus<-read.csv("more/Data-generate/Profile-species/Profile-cyprinodon_variegatus.csv")
save(cyprinodon_variegatus, file="data/cyprinodon_variegatus.rda", version=2)
# update notes:
# 01/10/25 switched to rda extension to try to fix errors compiling

