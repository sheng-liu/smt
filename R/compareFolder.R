
## compareFolder-methods
##
##
###############################################################################
##' @name compareFolder
##' @aliases compareFolder
##' @title compareFolder
##' @rdname compareFolder-methods
##' @docType methods
##' @description compare (2 ≤ n ≤ 5) folders with Diatrack output files. merge track files in each folder into one item of a track list. This list can then be fed into other functions for comparison.

##' @usage compareFolder(folder1,folder2,folder3=NULL,folder4=NULL,folder5=NULL)
##' @param folder the path to the folder location.


##' @return
##' \itemize{
##' \item{trackll} A list of tracks, each item of a track list correspond to a folder. This list can then be fed into other functions for comparison.
##' }

##' @examples
##' folder1=system.file("extdata","SWR1",package="smt")
##' folder2=system.file("extdata","HTZ1",package="smt")
##' trackll=compareFolder(folder1,folder2)
##' str(trackll,max.level=1)

##' @import reshape2
##' @export compareFolder
##'
## FUTURE: maybe plot on dt
###############################################################################


compareFolder=function(folder1,folder2,folder3=NULL,folder4=NULL,folder5=NULL){

    # TODO: the number of folder to compare can be extended using ... statement
    folder.list=list(folder1,folder2,folder3,folder4,folder5)

    # remove null folders by subsetting un-null folders
    null.folder=sapply(folder.list,is.null)
    folder.list=folder.list[!null.folder]

    names(folder.list)=sapply(folder.list,basename)

    sample.list=list()

    for (i in 1:length(folder.list)) {
        # i=1
        sample.list[i]=readDiatrack(folder=folder.list[[i]],merg=T)
        cat("\n...\n") # seperator makes ouput clearer
        names(sample.list)[i]=names(folder.list)[i]
    }

    #names(sample.list)=names(folder.list)
    return(sample.list)

}











