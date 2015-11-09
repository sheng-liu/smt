
## dwellTime-methods
##
##
###############################################################################
##' @name dwellTime
##' @aliases dwellTime
##' @title dwellTime
##' @rdname dwellTime-methods
##' @docType methods
##' @description Caclulate dwell time (/residence time) for trajecotries.

##' @usage dwellTime(trackll,exposure=10,plot=F,output=F)
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
##' dwellTime(trackll,plot=T)

##' @import reshape2
##' @export dwellTime
###############################################################################


dwellTime=function(trackll,exposure=10,plot=F,output=F){

    ## compute dwell time
    dwell.time=sapply(trackll,function(x){.dwellTime(x,exposure)})
    file.name=names(trackll)



    ## reshape data for plot
    dwell.time.mlt=melt(dwell.time)

    freqpoly=ggplot(dwell.time.mlt,aes(x=value,color=L1)) + geom_freqpoly(binwidth=exposure)+labs(x="Dwell time (ms)", y="Count")+theme_classic()+ theme(legend.title=element_blank())

    if (plot==T) plot(freqpoly)

    ## output
    if (output==T){

        # output csv
        for (i in 1:length(trackll)){
            fileName=paste("Dwell Time-",.timeStamp(file.name[i]),".csv",sep="")
            write.csv(file=fileName,dwell.time[[i]])
        }

        # output plot
        tStamp.plotName=paste(.timeStamp(file.name[1]),"...",sep="")
        plotName=paste("Dwell Time Plot-",tStamp.plotName,".pdf",sep="")
        ggsave(filename=plotName,plot=freqpoly,width=8,height=4)

        # output plot data
        plotData=ggplot_build(freqpoly)$data
        plotFile=paste("Dwell Time Plot-",tStamp.plotName,".csv",sep="")
        write.csv(file=plotFile,plotData)
    }
    return(dwell.time)
}


##-----------------------------------------------------------------------------
## TODO:




