
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

##' @usage dwellTime(trackll,exposure=10,x.scale=c(min=0,max=250),plot=F,output=F)
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
##' dwellTime(trackll,plot=TRUE)

##' @import reshape2
##' @export dwellTime
###############################################################################


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


dwellTime=function(trackll,exposure=10,x.scale=c(min=0,max=250),plot=F,output=F){

    ## compute dwell time
    dwell.time=sapply(trackll,function(x){.dwellTime(x,exposure)})
    file.name=names(trackll)



    ## reshape data for plot
    dwell.time.mlt=melt(dwell.time)


    histo.plot=ggplot(dwell.time.mlt,
           aes(x=value,group=L1,col=L1,fill=L1))+
        geom_histogram(binwidth=exposure,position="dodge")+
        xlim(x.scale["min"],x.scale["max"])+
        theme_bw()+
        theme(legend.title=element_blank())+
        labs(x="Lifetime of trajectories (ms)", y="Number of trajecotries")

    density.plot=ggplot(dwell.time.mlt,
           aes(x=value,group=L1,col=L1,fill=L1))+
        geom_density(alpha=0.2)+
        xlim(x.scale["min"],x.scale["max"])+
        theme_bw()+
        theme(legend.title=element_blank())+
        labs(x="Lifetime of trajectories (ms)", y="Frequency of trajectories")

    # multiplot(histo.plot,density.plot,cols=1)


    if (plot==T) multiplot(histo.plot,density.plot,cols=1)

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
##

# freqpoly=ggplot(dwell.time.mlt,aes(x=value,color=L1)) + geom_freqpoly(binwidth=exposure)+labs(x="Dwell time (ms)", y="Count")+theme_bw()+ theme(legend.title=element_blank())+xlim(0,200)

#     histodensity=ggplot(dwell.time.mlt,aes(x=value,color=L1,fill=L1))+
#         geom_histogram(binwidth=exposure,position="dodge")+
#         geom_density(aes(y=10*..count..),alpha=0.2)+
#         theme_bw()+
#         theme(legend.title=element_blank())+
#         xlim(0,200)



