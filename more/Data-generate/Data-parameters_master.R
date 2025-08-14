#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Data: Fish Toxicity Translator Master Parameter List
#'
#' @docType data
#'
#' @example data(parameters_master)
#'
#' @format data.frame
#'
#' @keywords datasets
#'
"parameters_master"

# ## This object was created by importing the master parameter list file .csv and saving it as an RData object
parameters_master<-read.csv("more/Data-generate/parameters_master.csv")
save(parameters_master, file="data/parameters_master.rda",version=2)
# ## Notes:
# ## 1/10/25 switched to rda extension to try to address compiling errors
# ## 2/4/25 updated naming of .csv that is uploaded to match the rda object name
# ## 3/10/25 added common and scientific names to parameter lists and added in_profile column with T/F entries to be used to create
# ## + new species profile templates with instead of looking for the "baseline" entry.  Also removed is_dd from species profile template
