## squareDisp-methods
##
##
###############################################################################
##' @name squareDisp
##' @aliases squareDisp
##' @title squareDisp
##' @rdname squareDisp-methods
##' @docType methods
##'
##' @description read output file (tracks/trajecotries) from Diatrack.

##' @usage squareDisp(folder,merge=F)
##' @method # this roxygen directive does not working
##' @param folder Full path to Diatrack output file.
##' @param merge An logical indicate if the output list should be merged into one. Default merge = FALSE, output list is divided by file names.

##' @return
##' \itemize{
##' \item{merge = F} Defult. A list of list of data.frames. First level is a list of file names in Diatrack output folder, second level is a list of data.frames from individual output file.

##' \item{merge = T} A list list of data.frames. First level is the folder name. second level is a list of data.frames from all Diatrack output files merged into one
##' }


## @section Usage : {
## squareDisp(folder,merge=F)
## }

##' @examples
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=squareDisp(folder)
##' str(trackll)

##' @details
##' default merge = FALSE, so the researcher can assay variations between files. Keep both output as two level list is for simplicity of downstream analysis.

##' @import ggplot2
##' @import dplyr
##' @export squareDisp

###############################################################################

##------------------------------------------------------------------------------
## squareDisp

## calculate square displacement of a track/trajectory as a function of time/step
## data.frame has two column, x and y

squareDisp=function(track,dt=1,resolution=0.107){

    # validity check for dt less than track length
    if (dt >(dim(track)[1]-1)){
        stop("\ntrack length:\t",dim(track)[1],
             "\ndt:\t\t",dt,
             "\nTime interval (dt) greater than track length-1\n")
    }

    # store dt-wise tracks into a list
    track.dt=list()
    for (i in 1:dt){

        # divide track into dt-wise-tracks, stored in the form of list
        track.dt[[i]]=track[seq(i,dim(track)[1],dt),]

        # compute square.disp in dt tracks, stored in original data.frame
        x.disp=(track.dt[[i]]$x-lag(track.dt[[i]]$x,n=1))*resolution
        y.disp=(track.dt[[i]]$y-lag(track.dt[[i]]$y,n=1))*resolution
        square.disp=x.disp^2+y.disp^2
        index=rownames(track.dt[[i]])
        track.dt[[i]]=mutate(track.dt[[i]],index,square.disp)

    }

    return(track.dt)

}

##------------------------------------------------------------------------------
## TODO: calculate displacement variance

# variance for each trajectory
# move small steps, or varies hugely

# distribution of displacement, tells how centralized or spread the trajectory is, is a parameter to measure trajectory

# calculate square displacement for all tracks






