## helperFunctions
#
################################################################################






##------------------------------------------------------------------------------
## .timeStamp
# add time stamp and file name as a unique signature of the output file
##'@export .timeStamp
.timeStamp=function(filename){

    basename=basename(filename)
    name=unlist(strsplit(basename,split="[.]"))
    fileName=paste(name[1],"-",format(Sys.time(),"%Y%m%d.%H%M%S"),sep="")

}

##------------------------------------------------------------------------------
## .readDiatrack

# validity check for dt less than track length (-1)
.valid=function(dt,track){

    # get track length of all tracks
    tracklen=dim(track)[1]

    if (dt >(tracklen-1)){
        stop("\ntrack length:\t",dim(track)[1],
             "\ndt:\t\t",dt,
             "\nTime interval (dt) greater than track length-1\n")
    }
}


.valid=function(dt,track){

    # get track length of all tracks
    tracklen=dim(track)[1]

    if (dt >(tracklen-1)){
        stop("\ntrack length:\t",dim(track)[1],
             "\ndt:\t\t",dt,
             "\nTime interval (dt) greater than track length-1\n")
    }
}




##------------------------------------------------------------------------------
## filtration

## a function to filter trackll based on specified fitler value (filtration on track length), default 6 frames/steps to Inf

##' @export filtration
filtration=function(trackll,filter=c(min=7,max=Inf)){

    #filter=match.arg(filter)

    # reinforce name
    names(filter)=c("min","max")

    cat("applying filter, min",filter["min"],"  max",filter["max"],"\n")


    track.len=list()
    for (i in 1:length(trackll)){
        track.len[[i]]=sapply(trackll[[i]],function(track){dim(track)[1]})
        trackll[[i]]=trackll[[i]][ track.len[[i]]>=filter["min"] & track.len[[i]]<filter["max"] ]
   }

    return(trackll)
}


# no need for the focus swtich, as one can simply filter on a number that is bigger than the dt he wanted to draw on

##------------------------------------------------------------------------------
## filtration

##' @export tracks.msda2smt
tracks.msda2smt=function(file){

    tracks.file=readMat(file)
    # file.name=basename(file)
    tracks.mat=tracks.file$tracks

    trackl.smt=lapply(tracks.mat, function(x){
        x=data.frame(x)
        x=x[,-1] # remove time column
        x=x/0.107  # change µm to pixel
        colnames(x)=c("x","y")
        x$z=rep(1,times=dim(x)[1])
        return(x)
    })

    trackll.smt=list(trackl.smt)
    names(trackll.smt)=basename(file)
    return(trackll.smt)

}
