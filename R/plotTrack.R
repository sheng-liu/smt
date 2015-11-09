
## plotTrack-methods
##
##
###############################################################################
##' @name plotTrack
##' @aliases plotTrack
##' @title plotTrack
##' @rdname plotTrack-methods
##' @docType methods
##' @description Plot track/trajectory from track list. either randomly or specified.

##' @usage plotTrack(trackll,exposure=10,plot=F,output=F)
##' @param trackll Track list output from readDiatrack().
##' @param exposure Exposure time, default = 10ms.
##' @param plot An Logical indicate if plot should be generated. If plot = TRUE, the plot data will also be output.
##' @param output An Logical indicate if output should be generated. 1) dwell time of tracks in the track list output to csv file. Each item in the list will have an individual csv file. 2) Plot PDF and plot data will be saved.


##' @return
##' \itemize{
##' \item{dwell time list} A list of dwell time for every trajectory, separated by file names of the trajectory file in Diatrack file folder. If combined dewell time is intended, use readDiatrack(folder, merge=T) to generate a single length list, then apply this function.
##' \item{PDF} dwell time frequency plot in PDF format, when plot = TRUE.
##' \item{csv} dwell time output in csv format, when output = TRUE.
##' }

##' @examples
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder)
##' plotTrack(trackll,plot=T)

##' @import reshape2
##' @export plotTrack
##' @export .plotTrack
##'@import animation
## FUTURE: maybe plot on dt

## TODO: make the function input as c(min,max)
###############################################################################

.plotTrack=function(ab.trackl,file.name="TrajectoryPlot",resolution=0.107,frame.min=8,frame.max=100,frame.start=1,frame.end=500){

    ab.trackl.res=lapply(ab.trackl,function(x) x*resolution)
    m=max(sapply(ab.trackl.res,max))

    fileName=paste(.timeStamp(file.name),".pdf",sep="")

    pdf(file=fileName)

    par(mfrow=c(2,2))
    len=length(ab.trackl.res)

    frame.end=ifelse(len<frame.end,len,frame.end)

        for (i in frame.start:frame.end){

        p=ab.trackl.res[[i]]
        frame.len=dim(p)[1]
        if (frame.len>frame.min & frame.len<frame.max)
        plot(p$x,p$y,type="l",xlim=c(0,m),ylim=c(0,m),xlab="X (µM)",
             ylab="Y (µM)")
        }
    dev.off()



}


plotTrack=function(ab.trackll,resolution=0.107,frame.min=8,frame.max=100,frame.start=1,frame.end=500){

    file.name=names(ab.trackll)


    for (i in 1:length(file.name)){

        .plotTrack(ab.trackll[[i]],file.name[i],resolution=resolution,frame.min=frame.min,frame.max=frame.max,frame.start=frame.start,frame.end=frame.end)

    }



        # lapply(ab.trackll,function(ab.trackl,file.name){.plotTrack(ab.trackl,file.name,resolution=resolution,frame.min=frame.min,frame.max=frame.max,frame.start=frame.start,frame.end=frame.end)})
    # lapply can only take one input

}








