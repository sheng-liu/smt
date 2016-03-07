# smt help (environment)
#
#
###############################################################################


## -----------------------------------------------------------------------------
## smt Roxygen help
##' @name smt
##' @aliases smt
##' @title smt-simple parameters for single molecule tracking analysis
##' @rdname smt
##' @docType package
##' @description simple analysis on single molecule tracking data using parameters based on mean square displacement (MSD).
## @usage
## smt()

##' @details smt provide a simple analysis on single molecule tracking data using parameters based on mean square displacement (MSD). Currently includes:
##' - duration of the tracks (dwellTime),
##'
##' - square displacement (squareDisp),
##'
##' - mean square displacement as a function of time (msd),
##'
##' - diffusion coefficient (Dcoef) and
##'
##' - emperical cumulative distribution function (eCDF) of MSD over time.

## @seealso

##' @import ggplot2
##' @import dplyr
##' @import reshape2
##' @import gridExtra
##' @importFrom scales cbreaks
##' @importFrom mixtools normalmixEM

## dplyr has masked  intersect, setdiff, setequal, union from base and other packages, try to use importFrom instead of import package
## @importFrom dplyr summarise group_by select %>%
##

smt=function(){}

