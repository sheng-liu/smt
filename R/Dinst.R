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

##' @usage
##'   Dinst(trackll,dt=8,resolution=0.107,lag.start=2,lag.end=5,filter=F,plot=F,output=F)
##' @param trackll Track list output from readDiatrack().
##' @param dt Time intervals.
##' @param resolution ratio of pixel to µM.
##' @param lag.start time lag used as start of dt for compute Dinst.
##' @param lag.end Time lag used as end of dt for compute Dinst.
##' @param filter An logical indicate if frames less than specified time interval (< = dt) should be filtered out (i.e. Take only trajectories that have number of frames > dt).
##' @param plot An Logical indicate if plot should be generated. See Values for
##'   detail.
##' @param output An Logical indicate if output should be generated. See Values
##'   for detail.

##' @return
##' \itemize{
##' \item{Dinst} A list of Dinst for each file in trackll.
##' \item{PDF} Log.Dinst histogram fitted with density curve, when plot = TRUE.
##' \item{csv} Dinst output in csv format, when output = TRUE.
##' }

##' @examples
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder)
##' Dinst(trackll,plot=TRUE)


##' @export Dinst
###############################################################################

# Dinst
# Diffusion coefficient instentaneoius

Dinst=function(
    trackll,dt=8,resolution=0.107,lag.start=2,lag.end=5,filter=F,plot=F,freq=F,binwidth=0.5,output=F){

    # calculate the Mean Square Displacement (MSD) for each trajectory longer
    # than 8 frames
    MSD=msd(trackll,dt=dt,resolution=resolution,filter=filter,summarize=F)

    #  linear fitting of the MSD curves between time dt 2 and 5
    x=lag.start:lag.end
    dstep=lag.end-(lag.start-1)

    # x=2:5 ; x=1:4# coefficient is the same
    D.inst=list()
    r.square=c()
    for (i in 1:length(trackll)){
        D.inst[[i]]=apply(MSD[[i]][lag.start:lag.end,],MARGIN=2,function(y){
            fit=lm(y~x)
            MSDslope=coefficients(fit)[2]/dstep
            MSDcorr=summary(fit)$r.squared
            sc=c(MSDslope,MSDcorr)
            names(sc)=c("slope","corr")
            return(sc)

        })
        # r.squared > 0.8 as quality control
        #r.square[i]=summary(fit)$r.squared
    }




    names(D.inst)=names(MSD)

    # to varify the fit
    # fit=lm(MSD[[1]][2:5,][,1]~x); plot(fit)

    # r.squared >= 0.8 as quality control

    slope=lapply(D.inst,function(x){x[rownames(x)=="slope"]})
    corr=lapply(D.inst,function(x){x[rownames(x)=="corr"]})

    corr.filter=lapply(corr,function(x){x>=0.8})
    D.inst=mapply("[",slope,corr.filter)


    #Log.D.inst=suppressWarnings(lapply(D.inst,log))
    Log.D.inst=lapply(D.inst,log)

    # remove NaN if wanted
    # Log.D.inst=lapply(Log.D.inst, function(x){
    #    x[!is.nan(x)]
    #})


    # plot frequency so it is easier to compare groups


    if (plot==T){

        p=reshape2::melt(Log.D.inst)


        colnames(p)=c("Log.D.inst","file.name")

        # overlay histogram and density plot without changing count as y axies
        Dinst.plot=ggplot(p,aes(x=Log.D.inst,group=file.name,col=file.name))+
            #geom_histogram(aes(y = ..count..,fill=file.name),binwidth=0.2,position="identity")+
            geom_histogram(aes(y = ..count..,fill=file.name),binwidth=binwidth,position="dodge")+

            geom_density(aes(y=0.2*..count..,fill=file.name),alpha=0.2)+
            theme_classic()+
            theme(legend.title=element_blank())


        if (freq==T){

            Dinst.plot=ggplot(p,
                              aes(x=Log.D.inst,group=file.name,col=file.name,fill=file.name))+
                geom_histogram(aes(y = ..density..,fill=file.name),binwidth=binwidth,position="dodge")+
                geom_density(alpha = 0.2)+
                theme_classic()+
                theme(legend.title=element_blank())

        }


        plot(Dinst.plot)

    }

    if (output==T){

        # output csv
        for (i in 1:length(trackll)){
            fileName=paste("Dinst-",.timeStamp(names(MSD)[i]),".csv",sep="")
            write.csv(file=fileName,D.inst[[i]])
        }


    }

    return(D.inst)


}






