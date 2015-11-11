
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

##' @usage plotTrack(ab.trackll,resolution=0.107,frame.min=8,frame.max=100,frame.start=1,frame.end=500)
##' @param ab.trackll absolute coordinates for plotting, generated from readDiatrack(folder,ab.track=T).
##' @param resolution ratio of pixel to µM.
##' @param frame.min minimum frame number for plotting.
##' @param frame.max max frame number for plotting.
##' @param frame.start the first frame to plot. Default 1.
##' @param frame.end last frame to plot. Default 500.


##' @return
##' \itemize{

##' \item{PDF} One PDF file with all the frames satisfy the creteria. If trackll has multiple items, it ouptus mutiple PDF files each corresponding to one item.

##' }

##' @examples
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder,ab.track=TRUE)
##' plotTrack(trackll)

## @import reshape2
##' @export plotTrack
## @export .plotTrack
## @import animation
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

        cat("\nOutput track plot...\n")

    }

        # lapply(ab.trackll,function(ab.trackl,file.name){.plotTrack(ab.trackl,file.name,resolution=resolution,frame.min=frame.min,frame.max=frame.max,frame.start=frame.start,frame.end=frame.end)})
    # lapply can only take one input

}








