#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#
#' Data: Species Library
#'
#'
#' @docType data
#'
#' @example data(species_library)
#'
#' @format data.frame
#'
#' @keywords datasets
#'
"species_library"

# ## A data.frame with names of included species
#species_library<-read.csv("more/Data-generate/species_library.csv")
#save(species_library, file="data/species_library.rda", version=2)
# ## Update log
# ## 1/10/25 changed extension to rda to try to fix compiling errors
# ## 2/4/25 updated to reflect naming convention switch to reference 'parameter_data' by scientific name as 'genus_species'
# ## 2/4/25 updated to upload species library csv and create data from that instead of creating the data.frame directly
# ## 3/7/25 updated species library to include new species and an additional column called 'reviewed' which will indicate which of these species
# ##        has been sufficiently reviewed for GUI use
# ## 3/10/25 updated to include "temporary" column to be used to distinguish between temporary and native species profile objects
