
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

##' @usage readDiatrack(folder,merge=F,ab.track=F)
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
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder)
##' str(trackll,max.level=2)

##' @details
##' default merge = FALSE, so the researcher can assay variations between files. Keep both output as two level list is for simplicity of downstream analysis.
##'
##' the absolute coordinates trajectory has moved

##' @import ggplot2
##' @import dplyr
##' @export readDiatrack
##'

###############################################################################

##------------------------------------------------------------------------------
## .readDiatrack
## a function to read one diatrack txt file and returns a list of tracks

.readDiatrack=function(file, interact=F,ab.track=F){

    if (interact==T) {
        file=file.choose()
    }

    file.name=basename(file)
    cat("\nReading Diatrack file: ",file.name,"...\n")

    ## skip the first 'comment line'
    data=read.table(file=file, header=F, skip=1)

    ## read in frame number line (for future use)
    frame.num=data[1,]

    ## remove frame number line for computation
    data=data[-1,]

    ## process the data
    # store coordinates of track in track.list
    track.list=list()
    # store absolute coordinates of track for comparison plots
    ab.track.list=list()


    # select 3 column at a time
    # can use frame number to do this, but this way makes the program more
    # robust with little to non decrease in efficiency
    for (i in 1:(dim(data)[2]/3)) {

        #i=2

        triple=i*3
        track=select(data,(triple-3+1):triple)
        colnames(track)=c("x","y","z")
        track=filter(track,x!=0,y!=0)

        # the [[]] is important, otherwise only x is included
        track.list[[i]]=track

        ## preprocess to fix coordinates from 0 to max
        ## absolute value of trajectory movement

        abTrack=data.frame(x=track$x-min(track$x),
                            y=track$y-min(track$y))
        ab.track.list[[i]]=abTrack

    }

    ## name the tracks

    # frame.id
    frame.num.mx=matrix(frame.num,ncol=3,nrow=length(frame.num)/3,byrow=T)
    frame.id=unlist(frame.num.mx[,1])

    # duration
    duration=table(frame.id)

    # file.id
    file.subname=substr(file.name,
           start=nchar(file.name)-8,
           stop=nchar(file.name)-4)

    file.id=rep(file.subname,length(duration))

    # index
    index=seq(from=1,to=length(duration))

    ## trackID=fileID.frameID.duration.index


     ## create a name vector
    track.name=paste(file.id,frame.id,duration,index,sep=".")

    # name the track
    names(track.list)=track.name
    names(ab.track.list)=track.name

    if (ab.track==T) return(ab.track.list) else return(track.list)

}




##------------------------------------------------------------------------------
## Note:the list can be named, this wil change the read.distrack.folder 's naming
## no need for naming it

readDiatrack=function(folder,merge= F,ab.track=F){

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

            track=.readDiatrack(file=file.list[i],ab.track=ab.track)
            trackll[[i]]=track
            names(trackll)[i]=file.name[i]
        }

    }else{

        # list of list of data.frames,
        # first level list of folder names and
        # second level list of data.frames

        for (i in 1:length(file.list)){

            track=.readDiatrack(file=file.list[i],ab.track=ab.track)
            # concatenate tracks into one list of data.frames
            track.holder=c(track.holder,track)

        }
        # make the result a list of list with length 1
        trackll[[1]]=track.holder
        names(trackll)[[1]]=folder.name


        # can put file name into name of the tracks
        # filename was putinto the name of the tracks
    }


    return(trackll)
}

##-----------------------------------------------------------------------------
## Note:

## if want to keep the names of each data frame come from, use
## if (merge) do.call(c,trackll)




