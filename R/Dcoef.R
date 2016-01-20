## Dcoef-methods
##
##
###############################################################################
##' @name Dcoef
##' @aliases Dcoef
##' @title Dcoef
##' @rdname Dcoef-methods
##' @docType methods
##' @description Caclulate instantaneous diffusion coefficient (Dcoef) for
##'   trajecotries.
##'
##' @usage
##' Dcoef( trackll,dt=6,filter=c(min=7,max=Inf),resolution=0.107,binwidth=0.5,method=c("static","rolling.window","percentage"),plot=c("none","histogram","density","variance"),output=F,t.interval=0.01)
##' @param trackll Track list output from readDiatrack().
##' @param dt Time intervals.
##' @param resolution ratio of pixel to µM.
##' @param lag.start time lag used as start of dt for compute Dcoef.
##' @param lag.end Time lag used as end of dt for compute Dcoef.
##' @param filter An logical indicate if frames less than specified time interval (< = dt) should be filtered out (i.e. Take only trajectories that have number of frames > dt).
##'
##' @param plot A parameter for plotting. "none" (default), no plot; "histogram", plots histogram with count information, binwidth can be set through parameter binwidt; "density", plots density/frequency; "variance", plots mean and standard deviation of all trajectories, in this mode, rolling window calculaton of Dcoef is applied and filter is on.
##'
##' @param binwidth binwidth used for histogram, default 0.5.
##' @param output An Logical indicate if output should be generated. See Values
##'   for detail.
##' @param t.interval time interval between frames, default 0.010 s (10ms).
##' @return
##' \itemize{
##' \item{Dcoef} A list of Dcoef for each file in trackll.
##' \item{PDF} Log.Dcoef histogram fitted with density curve, when plot = TRUE.
##' \item{csv} Dcoef output in csv format, when output = TRUE.
##' }
##' @details Generic parameters (parameter applied to all methods, such as resolution etc) are set in the function. Method dependent parameters (such as lag.start, lag.end for method = "static"), are stored in profile.csv in PREF folder under extdata. To change preference parameter, can either programably or manually go to folder system.file("extdata","PREF","profile.csv",package="smt"), and change the profile.csv.
##'
##' @examples
##' # compare files
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder)
##' Dcoef(trackll,plot="density")
##'
##' # compare folders
##' folder1=system.file("extdata","SWR1",package="smt")
##' folder2=system.file("extdata","HTZ1",package="smt")
##' trackll=compareFolder(folder1,folder2)
##' Dcoef(trackll,plot="variance"

##' @export Dcoef
###############################################################################

# Dcoef (Diffusion coefficient)


Dcoef=function(
    trackll,dt=6,filter=c(min=7,max=Inf),resolution=0.107,binwidth=0.5,method=c("static","rolling.window","percentage"),plot=c("none","histogram","density","variance"),output=F,t.interval=0.01){


##------------------------------------------------------------------------------
## set corresponding switches

    ## read in preference parameters
    ## these are some method dependent parameters, the generic parameters (parameter applied to all methods) are set in the function

    profile=system.file("extdata","PREF","profile.csv",package="smt")
    PARAM=read.csv(file=profile,header=T,row.names="PARAMETER")
    lag.start=PARAM["lag.start",]
    lag.end=PARAM["lag.end",]
    binwidth=PARAM["binwidth",]

    method=match.arg(method)

    switch(method,
           static={
               cat("\napplying static,lag.start=",
                   lag.start,"\t","lag.end=", lag.end,"\n")
               static=T
               lag.start=lag.start
               lag.end=lag.end

               # calculate MSD
               MSD=msd(trackll,dt=dt,resolution=resolution,
                       filter=filter,summarize=F)

               # calculate Dcoef using rolling window
               D.coef=Dcoef.static(MSD,lag.start=lag.start,lag.end=lag.end,t.interval=t.interval)
           },
           rolling.window={
               cat("\napplying rolling window,
                   filter swtiched on\n")
               static=F
               window.size=4

               # calculate MSD
               MSD=msd(trackll,dt=dt,resolution=resolution,
                       filter=filter,summarize=F)
               # calculate Dcoef using rolling window
               D.coef=Dcoef.roll(MSD,window.size=window.size,t.interval=t.interval)

           },
           percentage={
               cat("\napplying percentage,")
               static=T

               D.coef=Dcoef.perc(trackll,percentage=0.25,weighted=F,
                                 filter=filter, resolution=resolution,
                                 t.interval=t.interval)

           })

#     if (plot=="variance"){
#         ## currently set rollingwindow only for variance plot
#         cat("\nvariance = TRUE, applying rolling window, filter swtiched on\n")
#         rolling.window=T
#         filter=T
#     }else{
#             rolling.window=F
#         }

    # calculate MSD based on swtiches
    # MSD=msd(trackll,dt=dt,resolution=resolution,filter=filter,summarize=F)
    # MSD may need a track specific function, or switch
    ## can be the filter function that needs to be implemented


##------------------------------------------------------------------------------
## call corresponding functions

#     if (rolling.window==T){
#
#         D.coef=Dcoef.roll(MSD,dt=dt)
#         D.coef.subset=rsquare.filter(D.coef,static=F)
#         Log.D.coef=Dcoef.log(D.coef.subset,static=F)
#
#     }else{
#
#         D.coef=Dcoef.static(MSD)
#         D.coef.subset=rsquare.filter(D.coef,static=T)
#         Log.D.coef=Dcoef.log(D.coef.subset,static=T)
#
#     }

    D.coef.subset=rsquare.filter(D.coef,static=static)
    Log.D.coef=Dcoef.log(D.coef.subset,static=static)


##------------------------------------------------------------------------------
## plot

    plot=match.arg(plot)
    switch(plot,
           variance={
               if (method=="static"||method=="percentage"){

                   cat("variance plot for method static and percentage not available for smt v0.2 \n")

#                    cat("variance plot for method static and percentage does not use rsquare filter. \n")
#                    Log.D.coef.nofilter=Dcoef.log(D.coef,static=T)
#                    plotVariance(Log.D.coef.nofilter,method=method)

               }else{ plotVariance(Log.D.coef,method=method)}

              },

           ## needs more work to deal with a list

           # see count inforamtion
           histogram=plotHistogram(Log.D.coef,binwidth = binwidth,method=method),

           # plot frequency so it is easier to compare groups
           density=plotDensity(Log.D.coef,binwidth = binwidth,method=method)
           # else do nothing
           )


##------------------------------------------------------------------------------
## output

    if (output==T){

        # output csv
        for (i in 1:length(trackll)){
            fileName=paste("Dcoef-",.timeStamp(names(MSD)[i]),".csv",sep="")
            write.csv(file=fileName,D.coef[[i]])
        }


    }

    return(D.coef)
}






