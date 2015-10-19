
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

##' @usage readDiatrack(folder,divide=T)
##' @method # this roxygen directive does not working
##' @param folder Full path to Diatrack output file.
##' @param divide An logical indicate if the output list should be divided by file name. Default divide = TRUE.

##' @return
##' \itemize{
##' \item{divide = T} Defult. A list of list of data.frame. First level is a list of file names in Diatrack output folder, second level is a list of data.frames from individual output file.

##' \item{divide = F} A list of data.frames from all Diatrack output files.
##' }


## @section Usage : {
## readDiatrack(folder,divide=T)
## }

##' @examples
##' folder=system.file("extdata",package="smt")
##' trackll=readDiatrack(folder)
##' str(trackll)
##' trackl=readDiatrack(folder,divide = F)
##' str(trackl)

##' @details
##' possibility of a reads been random
##' possibility of peak heights (base coverage) at specific location is esitmated from the average (lambda) coverage of the peak region, 1kb region, 5kb region and 10 kb regions.
##' This is to account for local fluctuations.

##system.file("extdata", "refGene.csv", package="SeqData")
##' @import ggplot2
##' @import dplyr
##' @export readDiatrack

###############################################################################


readDiatrack=function(folder,divide=T){

    folder.track.list=list()

    # getting a file list of Diatrack files in a directory
    file_list <- list.files(path = folder,pattern = ".txt",full.names = T  )
    file_name = list.files(path=folder, pattern=".txt",full.names=F)

    for (i in 1:length(file_list)){

        track=.readDiatrack(file=file_list[i])

        if (divide){
            # list of list of data.frames, with first level is list of file
            # names and second level list of data.frames
            folder.track.list[[i]]=track
            names(folder.track.list)[i]=file_name[i]

        }else{
            # concatenate tracks into one list of data.frames
            folder.track.list=c(folder.track.list,track)
        }
    }

    return(folder.track.list)
}

##-----------------------------------------------------------------------------
## TODO:


