
## readDiatrack-methods
##
##
###############################################################################
##' @name readDiatrack
##' @aliases readDiatrack
##' @title readDiatrack
##' @rdname readDiatrack-methods
##' @docType methods
##'
##' @description read output file (tracks/trajecotries) from Diatrack.

##' @usage readDiatrack(folder,merge=F)
##' @method # this roxygen directive does not working
##' @param folder Full path to Diatrack output file.
##' @param merge An logical indicate if the output list should be merged into one. Default merge = FALSE, output list is divided by file names.

##' @return
##' \itemize{
##' \item{merge = F} Defult. A list of list of data.frames. First level is a list of file names in Diatrack output folder, second level is a list of data.frames from individual output file.

##' \item{merge = T} A list list of data.frames. First level is the folder name. second level is a list of data.frames from all Diatrack output files merged into one
##' }


## @section Usage : {
## readDiatrack(folder,merge=F)
## }

##' @examples
##' folder=system.file("extdata",package="smt")
##' trackll=readDiatrack(folder)
##' str(trackll)

##' @details
##' default merge = FALSE, so the researcher can assay variations between files. Keep both output as two level list is for simplicity of downstream analysis.

##' @import ggplot2
##' @import dplyr
##' @export readDiatrack

###############################################################################


readDiatrack=function(folder,merge= F){

    trackll=list()
    track.holder=c()

    # getting a file list of Diatrack files in a directory
    file.list <- list.files(path = folder,pattern = ".txt",full.names = T  )
    file.name = list.files(path=folder, pattern=".txt",full.names=F)
    folder.name = basename(folder)

    if (merge == F){

        # list of list of data.frames,
        # first level list of file names and
        # second level list of data.frames

        for (i in 1:length(file.list)){

            track=.readDiatrack(file=file.list[i])
            trackll[[i]]=track
            names(trackll)[i]=file.name[i]
        }

    }else{

        # list of list of data.frames,
        # first level list of folder names and
        # second level list of data.frames

        for (i in 1:length(file.list)){

            track=.readDiatrack(file=file.list[i])
            # concatenate tracks into one list of data.frames
            track.holder=c(track.holder,track)

        }
        # make the result a list of list with length 1
        trackll[[1]]=track.holder
        names(trackll)[[1]]=folder.name
    }


    return(trackll)
}

##-----------------------------------------------------------------------------
## Note:

## if want to keep the names of each data frame come from, use
## if (merge) do.call(c,trackll)




