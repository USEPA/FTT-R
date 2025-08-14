#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Size-dependent Toxicity Modifier
#'
#' Two built in functions to modify survival dercrement based on either a linear interpolation of toxicity effects or a step function for size classes
#'
#' Linear:
#' Linear transition between largest and smallest classes
#' \deqn{ToxModifier(z) =((toxMultU-toxMultL)/(upperBound-lowerBound))*(z-upperBound)+toxMultU}
#'
#' Step:
#' Step function to assign discrete multipliers to size classes
#' \deqn{ToxModifier(z) = \sum_{i=0}^{n}{mVec_i * \textbf{1}_{\zPartition_{i}}(z)}}
#'
#' @param z Size at the beginning of the timestep [float or vector]
#' @param toxModType String to switch between the two current options, "linear", or "step"
#' @param toxMultU Toxicity multiplier at upper bound size 'upperBound' [float]
#' @param toxMultL Toxicity multiplier at lower bound size 'lowerBound' [float]
#' @param upperBound Upper size limit in the model [float]
#' @param lowerBound Lower size limit in the model [float]
#' @param toxModVec Multipliers for each component of 'zPartition' vector [vector]
#' @param zPartition Size partition of the size range.  Uses 'stepfun' from 'stats' package, please see help for additional details on using 'stepfun' [vector]
#' @return Toxicity multiplier as a function of size [float]
#' @export


ToxModifier<-function(z, toxModType, toxMultU, toxMultL, upperBound, lowerBound, zPartition,toxModVec){
  return(switch(toxModType,
                linear = ((toxMultU-toxMultL)/(upperBound-lowerBound))*(z-upperBound)+toxMultU,
                step = stepfun(zPartition,toxModVec)(z)))}


