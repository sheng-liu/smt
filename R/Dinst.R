## Dinst-methods
##
##
###############################################################################
##' @name Dinst
##' @aliases Dinst
##' @title Dinst
##' @rdname Dinst-methods
##' @docType methods
##' @description Caclulate instantaneous diffusion coefficient (Dinst) for
##'   trajecotries.
##'
##' @usage
##' Dinst(trackll,dt=8,resolution=0.107,lag.start=2,lag.end=5,filter=F,binwidth=0.5,plot=c("none","histogram","density","variance"),output=F)
##' @param trackll Track list output from readDiatrack().
##' @param dt Time intervals.
##' @param resolution ratio of pixel to µM.
##' @param lag.start time lag used as start of dt for compute Dinst.
##' @param lag.end Time lag used as end of dt for compute Dinst.
##' @param filter An logical indicate if frames less than specified time interval (< = dt) should be filtered out (i.e. Take only trajectories that have number of frames > dt).
##'
##' @param plot A parameter for plotting. "none" (default), no plot; "histogram", plots histogram with count information, binwidth can be set through parameter binwidt; "density", plots density/frequency; "variance", plots mean and standard deviation of all trajectories, in this mode, rolling window calculaton of Dinst is applied and filter is on.
##'
##' @param binwidth binwidth used for histogram, default 0.5.
##' @param output An Logical indicate if output should be generated. See Values
##'   for detail.

##' @return
##' \itemize{
##' \item{Dinst} A list of Dinst for each file in trackll.
##' \item{PDF} Log.Dinst histogram fitted with density curve, when plot = TRUE.
##' \item{csv} Dinst output in csv format, when output = TRUE.
##' }

##' @examples
##' # compare files
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder)
##' Dinst(trackll,plot="density")
##'
##' # compare folders
##' folder1=system.file("extdata","SWR1",package="smt")
##' folder2=system.file("extdata","HTZ1",package="smt")
##' trackll=compareFolder(folder1,folder2)
##' Dinst(trackll,plot="variance")

##' @export Dinst
###############################################################################

# Dinst
# Diffusion coefficient instentaneoius

Dinst=function(
    trackll,dt=8,resolution=0.107,lag.start=2,lag.end=5,filter=F,binwidth=0.5,plot=c("none","histogram","density","variance"),output=F){

    if (plot=="variance"){
        ## currently set rollingwindow only for variance plot
        cat("\nvariance = TRUE, applying rolling window, filter swtiched on\n")
        rolling.window=T
        filter=T
    }else{
            rolling.window=F
        }

        MSD=msd(trackll,dt=dt,resolution=resolution,filter=filter,summarize=F)


##------------------------------------------------------------------------------
## Dinst, rolling window

    if (rolling.window==T){

        D.inst=Dinst.roll(MSD,dt=dt)
        D.inst.subset=rsquare.filter(D.inst,static=F)
        Log.D.inst=Dinst.log(D.inst.subset,static=F)

    }else{

        D.inst=Dinst.static(MSD)
        D.inst.subset=rsquare.filter(D.inst,static=T)
        Log.D.inst=Dinst.log(D.inst.subset,static=T)

    }

##------------------------------------------------------------------------------
## plot

    plot=match.arg(plot)
    switch(plot,
           variance=plotVariance(Log.D.inst),

           ## needs more work to deal with a list

           # see count inforamtion
           histogram=plotHistogram(Log.D.inst,binwidth = binwidth),

           # plot frequency so it is easier to compare groups
           density=plotDensity(Log.D.inst,binwidth = binwidth)
           # else do nothing
           )


##------------------------------------------------------------------------------
## output

    if (output==T){

        # output csv
        for (i in 1:length(trackll)){
            fileName=paste("Dinst-",.timeStamp(names(MSD)[i]),".csv",sep="")
            write.csv(file=fileName,D.inst[[i]])
        }


    }

    return(D.inst)
}






