## helperFunctions
#
################################################################################




##------------------------------------------------------------------------------
## .dwellTime
## a function to calculate dwell time from a list of data.frame track (trackl). and returns a vector of dwell time.

## nomenclature
## track    data.frame with x,y,z coordinates
## trackl   list of data.frames with x,y,z coordinates, read from one track file
## trackll  list of list of data.frames with x,y,z coordinates, read from multiple track file

.dwellTime=function(trackl,exposure=10){
    sapply(trackl,function(x){dim(x)[1]*exposure})
}


##------------------------------------------------------------------------------
## .timeStamp
# add time stamp and file name as a unique signature of the output file
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


