## helperFunctions
#
################################################################################

##------------------------------------------------------------------------------
## .readDiatrack
## a function to read one diatrack txt file and returns a list of tracks

##' @export .readDiatrack
.readDiatrack=function(file, interact=F){

    if (interact==T) {
        file=file.choose()
    }

    ## skip the first 'comment line'
    data=read.table(file=file, header=F, skip=1)

    ## read in frame number line (for future use)
    frame.num=data[1,]

    ## remove frame number line for computation
    data=data[-1,]

    ## process the data
    track.list=list()


    for (i in 1:(dim(data)[2]/3)) {

        # select 3 column at a time
        # can use frame number to do this, but this way makes the program more
        # robust with little to non decrease in efficiency
        triple=i*3
        track=select(data,(triple-3+1):triple)
        colnames(track)=c("x","y","z")
        track=filter(track,x!=0,y!=0)

        # the [[]] is important, otherwise only x is included
        track.list[[i]]=track

    }


    return(track.list)
}

##------------------------------------------------------------------------------
## TODO:the list can be named, this wil change the read.distrack.folder 's naming
