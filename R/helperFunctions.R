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

##------------------------------------------------------------------------------
## .
## from Rcookbook
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


##------------------------------------------------------------------------------
## .Dinst.roll

Dinst.roll=function(MSD,dt,window.size=4){

    D.inst=list()
    D.inst.roll=list()
    names.D.inst.roll=c()
    window=1:window.size

    for (i in 1:length(trackll)){

        # zero stands for the first window as subsetting using window+j
        for ( j in 0:(dt-window.size)){
            print(window+j)
            x=window+j
            D.inst.roll[[j+1]]=apply(MSD[[i]][window+j,],
                                     MARGIN=2,
                                     function(y){
                                         fit=lm(y~x)
                                         MSDslope=coefficients(fit)[2]/window.size
                                         MSDcorr=summary(fit)$r.squared
                                         sc=c(MSDslope,MSDcorr)
                                         names(sc)=c("slope","corr")
                                         return(sc)
                                     })
            names.D.inst.roll=c(names.D.inst.roll,
                                paste(as.character(window+j),collapse=" "))
        }

        names(D.inst.roll)=names.D.inst.roll

        D.inst[[i]]=D.inst.roll
        names.D.inst.roll=c()
    }
    names(D.inst)=names(MSD)

    return(D.inst)
}

##------------------------------------------------------------------------------
## rsquare.filter
rsquare.filter=function(D.inst,static=TRUE){

    if (static==T){

        # to varify the fit
        # fit=lm(MSD[[1]][2:5,][,1]~x); plot(fit)

        # r.squared >= 0.8 as quality control
        slope=lapply(D.inst,function(x){x[rownames(x)=="slope"]})
        corr=lapply(D.inst,function(x){x[rownames(x)=="corr"]})

        corr.filter=lapply(corr,function(x){x>=0.8})
        D.inst.subset=mapply("[",slope,corr.filter)


    }else{






    # to varify the fit
    # fit=lm(MSD[[1]][2:5,][,1]~x); plot(fit)

    ## filter
    ## filtration with r.squared
    # r.squared >= 0.8 as quality control

    ## the next two filtration blocks maybe combined to increase efficiency,
    ## however for now the efficiency is secondary, let the logic stand
    ## clear, then improve the efficiency. as many times efficiency is at
    ## the expense of sacrifice clearness of the code, hard to read or
    ## interpretate later.

    D.inst.subset=list()
    for (i in 1:length(D.inst)){

        # r.squared >= 0.8 as quality control
        slope=lapply(D.inst[[i]],function(x){x[rownames(x)=="slope"]})
        corr=lapply(D.inst[[i]],function(x){x[rownames(x)=="corr"]})

        #corr.filter=lapply(corr,function(x){x>=0.8})
        #D.inst.subset[[i]]=mapply("[",slope,corr.filter)


        ## logorithm
        #Log.D.inst[[i]]=lapply(D.inst[[i]],log)
        # remove NaN if wanted
        # Log.D.inst=lapply(Log.D.inst, function(x){
        #    x[!is.nan(x)]
        #})

        ## alternative
        for (m in 1:length(corr)){
            for (n in 1:length(corr[[m]])){
                if (corr[[m]][n]<0.8)
                    slope[[m]][n]=NaN
            }
        }

        D.inst.subset[[i]]=slope

    }
    names(D.inst.subset)=names(D.inst)

    }
    ## filter without losing location information
    ## or filter in the last step, has to be replaced before log
    ## if corr <0.8, replace slope with NaN

    ## alternative
    #     for (i in 1: length(D.inst)){
    #
    #         for (j in 1: length(D.inst[[i]])){
    #
    #             # dim(D.inst[[i]][[j]])[2] is the length of the matrix
    #             for (k in 1:dim(D.inst[[i]][[j]])[2]){
    #
    #
    #                 if (D.inst[[i]][[j]][,k]["corr"]<0.8)
    #                     D.inst[[i]][[j]][,k]["slope"]=NaN
    #
    #             }
    #         }
    #     }

    return(D.inst.subset)
}

##------------------------------------------------------------------------------
## Dinst.log

Dinst.log=function(D.inst.subset,static=T){
    if (static){

        #Log.D.inst=suppressWarnings(lapply(D.inst,log))
        Log.D.inst=lapply(D.inst.subset,log)

        # remove NaN if wanted
        # Log.D.inst=lapply(Log.D.inst, function(x){
        #    x[!is.nan(x)]
        #})
    }else{
        ## logorithm
        Log.D.inst=list()
        for (i in 1:length(D.inst.subset)){
            #Log.D.inst=suppressWarnings(lapply(D.inst,log))
            Log.D.inst[[i]]=lapply(D.inst.subset[[i]],log)

        }
        names(Log.D.inst)=names(D.inst.subset)
        return(Log.D.inst)

    }

}

##------------------------------------------------------------------------------
## Dinst.static
Dinst.static=function(MSD,lag.start=2,lag.end=5){


    #  linear fitting of the MSD curves between time dt 2 and 5
    x=lag.start:lag.end
    dstep=lag.end-(lag.start-1)


    # x=2:5 ; x=1:4 # coefficient is the same
    D.inst=list()
    for (i in 1:length(trackll)){
        D.inst[[i]]=apply(MSD[[i]][lag.start:lag.end,],MARGIN=2,function(y){
            fit=lm(y~x)
            MSDslope=coefficients(fit)[2]/dstep
            MSDcorr=summary(fit)$r.squared
            sc=c(MSDslope,MSDcorr)
            names(sc)=c("slope","corr")
            return(sc)

        })
    }
    names(D.inst)=names(MSD)

    return(D.inst)
}

##------------------------------------------------------------------------------
## plotVariance
plotVariance=function(Log.D.inst){

    ## plot data preparation
    ## plot mean of Log.D.inst of each individual trajectory, against variance of each individual trajectory

    # when the list have same length, it maybe easier to work with when converted into data.frame

    Log.D.inst.df=do.call(rbind.data.frame,Log.D.inst)
    MEAN=data.frame(apply(Log.D.inst.df,1,mean,na.rm=T))

    folder=c()
    for (i in 1:dim(MEAN)[1])
        folder[i]=unlist(strsplit(rownames(MEAN)[i],split ="[.]"))[1]
    MEAN=cbind(MEAN,folder)
    colnames(MEAN)=c("mean","folder")


    SD=data.frame(apply(Log.D.inst.df,1,sd,na.rm=T))
    colnames(SD)=c("sd")

    data=cbind(MEAN,SD)

    # plotting
#     scatter=ggplot(data,aes(x=mean,y=sd,col=folder))+
#         geom_point(alpha=0.8)+
#         theme_classic()+
#         theme(legend.title=element_blank())
#
#     mean.density=ggplot(data,aes(x=mean,col=folder))+
#         geom_density()+
#         theme_classic()+
#         theme(legend.title=element_blank())
#
#     sd.density=ggplot(data,aes(x=sd,col=folder))+
#         geom_density()+
#         theme_classic()+
#         theme(legend.title=element_blank())
#
#     multiplot(scatter,mean.density,sd.density, cols=1)

    # another implementation using gridExtra::grid.arrange
    scatter=ggplot(data,aes(x=mean,y=sd,col=folder))+
        geom_point(alpha=1,shape=21)+
        theme_classic()+
        theme(legend.title=element_blank())+
        theme(legend.position=c(1,1),legend.justification=c(1,1))

    mean.density=ggplot(data,aes(x=mean,fill=folder,col=folder))+
        geom_density(alpha=0.5)+
        theme(legend.title=element_blank())+
        theme(legend.position = "none")

    sd.density=ggplot(data,aes(x=sd,fill=folder,col=folder))+
        coord_flip()+
        geom_density(alpha=0.5)+
        theme(legend.title=element_blank())+
        theme(legend.position = "none")

    empty <- ggplot()+geom_point(aes(1,1), colour="white")+
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()
        )


    grid.arrange(mean.density, empty, scatter, sd.density, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))



    # this would have worked if mapply takes na.rm=T
    # mapply(mean,Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]],na.rm=T)


    # or this one
    # mapply(function(x){mean(x,na.rm=T)},Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]])

    # this means mapply is not taking the elements of each list into one vector, but used them as seperate


    # used alternative, concatanate, then apply
    #             C=mapply("c",Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]])
    #
    #
    #
    #             apply(C,2,mean,na.rm=T)
    #             mapply(mean,a,b)
    #
    #             lapply(Log.D.inst[[1]])
    #
    #         }
    #
    #         mapply(mean,Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]],na.rm=T)
    #         mapply(function(x){mean(x,na.rm=T)},
    #                      Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]])
    #
    #         mapply()
    #
    #         # collapse sublist rolling windowns into uper level list
    #         Log.D.inst=lapply(Log.D.inst,unlist)

}

##------------------------------------------------------------------------------
## plotHistogram
plotHistogram=function(Log.D.inst,binwidth=0.5){
    p=reshape2::melt(Log.D.inst)
    colnames(p)=c("Log.D.inst","file.name")

    # overlay histogram and density plot without changing count as y axies
    Dinst.plot=ggplot(p,aes(x=Log.D.inst,group=file.name,col=file.name))+
        #geom_histogram(aes(y = ..count..,fill=file.name),binwidth=0.2,position="identity")+
        geom_histogram(aes(y = ..count..,fill=file.name),binwidth=binwidth,position="dodge")+

        geom_density(aes(y=0.2*..count..,fill=file.name),alpha=0.2)+
        theme_classic()+
        theme(legend.title=element_blank())
    plot(Dinst.plot)

}
##------------------------------------------------------------------------------
## plotDensity
plotDensity=function(Log.D.inst,binwidth=0.5){
    p=reshape2::melt(Log.D.inst)
    colnames(p)=c("Log.D.inst","file.name")


    Dinst.plot=ggplot(p,
                      aes(x=Log.D.inst,group=file.name,col=file.name,fill=file.name))+
        geom_histogram(aes(y = ..density..,fill=file.name),binwidth=binwidth,position="dodge")+
        geom_density(alpha = 0.2)+
        theme_classic()+
        theme(legend.title=element_blank())

    plot(Dinst.plot)
}

# ggplot(p,
#        aes(x=Log.D.inst))+
#     geom_density()
