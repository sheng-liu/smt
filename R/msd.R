## msd-methods
##
##
###############################################################################
##' @name msd
##' @aliases msd
##' @title msd
##' @rdname msd-methods
##' @docType methods
##'
##' @description calculate mean square displacement for individual trajectory or
##'   summarize on trajectories.

##' @usage msd(trackll,dt=6,resolution=0.107,summarize=F,filter=c(min=7,max=Inf),plot=F,output=F)
##' @param dt Time intervals.
##' @param resolution ratio of pixel to µM.
##' @param trackll Track list output from readDiatrack().
##' @param summarize An logical indicate if MSD should be calculated on
##'   individual trajectories (Default) or summarized on all trajectories.
##'
##' @param filter a vector specifies the minimum and max length of trajecotries to be analyzed. Take only trajectories that has number of frames greater than (>=) min and less than (<) max.
##'
##' @param plot An logical indicate if plot should be generated. See Values for
##'   detail.
##' @param output An logical indicate if output should be generated. See Values
##'   for detail.

##' @return
##' \itemize{
##' \item{SummarizedMSD} MSD summarized over all trajectories as a function of
##' dt.
##'
##' \item{InidvidualMSD} MSD of individual trajectories at specified dt. Row
##' number corresponding to its dt. Notice only the trajectories that satisfies the specified dt is output, trajectories that does not satisfy (i.e.
##' trajectories satisfies 1:(dt-1)) is not output here.
##'
##' \item{StandardError} Standard Error of the sample mean measures the
##' variations of sample mean to underlying mean, it is estimated as
##' SE=SD/sqrt(N).
##'
##'
##' \item{SampleSize} The sample size (number of tracks/trajectories) used for
##' calculating the msd and standard error.
##' }


##' @examples
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder)
##' msd=msd(trackll,dt=8,summarize=TRUE,plot=TRUE)
##' str(msd)
##'
##' ## focus on a group of trajectory by setting filter greater than dt
##' msd=msd(trackll,dt=6,summarize=TRUE,filter=c(7,Inf),plot=TRUE)

##' @details
##' msd() calculate track (/trajectory)'s mean square displacement as a function
##' of time (dt). For a track of N steps, at each dt, there are dt number of
##' sub-trajectory/sub-tracks, mean of dt-wise sub-trajectories/ step-wise sub
##' tracks average subtracks into one number at each dt.
##'
##' the dt number of su-btracks each contains N:N-dt steps. Because minimum step
##' is 1 (N-dt > = 1), so the maxium dt is N-1 (dt < = N-1). As dt increase, the
##' number of steps used to generate that mean decrease with the maxmum dt
##' (dt=N-1) generated from one step.
##'
##' if one wants to focus on a group of trajectory's evolution, he can simply filter on a number that is bigger than the dt he wanted to plot MSD.

##' @import ggplot2
##' @import dplyr
## @import dplyr::select
## @import reshape2::melt
##' @import reshape2
##' @export msd


## TODO:melt implementation
###############################################################################


##------------------------------------------------------------------------------
## msd.track

## calculate msd for tracks (data.frame)
## track, data.frame, xyz
## resolution 107nm=1 pixel

##' @export msd.track
##'
msd.track=function(track,dt=6,resolution=0.107){

    # validity check for dt less than track length
    if (dt >(dim(track)[1]-1)){
        stop("\ntrack length:\t",dim(track)[1],
             "\ndt:\t\t",dt,
             "\nTime interval (dt) greater than track length-1\n")
    }


    # summarize msd for track at all dt
    # note this function calculates only "at" each dt
    msd.dt.track=c()
    for (i in 1:dt){

        #track.sqd=squareDisp(track,dt=i,resolution=resolution)

        # at each dt, there are dt number of sub-trajectory/sub-tracks
        # mean of dt-wise sub-trajectories/ step-wise sub tracks
        # average subtracks into one number at each dt
        #msd.dt.subtrack=sapply(track.sqd,function(x){
        #    mean(x$square.disp,na.rm=T)})
        # msd.dt.track[i]=mean(msd.dt.subtrack)

        # caculate msd for track at specified dt
        track.sqd=squareDisp(track,dt=i,resolution=resolution)

        # pull all the squared displacement at this dt together
        square.disp=do.call(rbind,track.sqd)$square.disp
        square.disp=square.disp[!is.na(square.disp)]
        # get the genuine mean
        msd=mean(square.disp)


        # get the sum of all subtrajectories then average them
#         sum.square.disp=lapply(track.sqd,function(trk){
#           # subsetting list with [["colnames]]
#           sum(trk[["square.disp"]],na.rm=T)})
#         # get the msd
#         msd=mean(do.call(rbind,sum.square.disp))


        # for any N length trajectory, one can have N-1 dt steps
        # the resulting msd at each dt is generated from N:N-1 individual steps
        msd.dt.track=c(msd.dt.track, msd)


    }

    return(msd.dt.track)
}

# msd() calculate trajectory(/track)'s msd as a function of time (dt).
# for a track that has N steps,
# at each dt, there are dt number of sub-trajectory/sub-tracks,
# mean of dt-wise sub-trajectories/ step-wise sub tracks
# average subtracks into one number at each dt

# the dt number of su-btracks each contains N:N-dt steps
# (N-dt > = 1, so dt < = N-1) because minimum step is 1 (N-dt > = 1),
# so the maxium dt is N-1 (dt < = N-1)
# as dt increase, the number of steps used to generate that mean decrease
# with the maxmum dt (dt=N-1) generated from one step


##------------------------------------------------------------------------------
## msd.trackl

## calculate msd.track for trackl (list of data.frame) one level

msd.trackl=function(trackl,dt=6,resolution=0.107){

    # validity check for max track length greater than dt
    track.len=sapply(trackl,function(x) dim(x)[1])
    if (dt>(max(track.len)-1)) {
        stop("\nmax track length:\t",max(track.len),
             "\ndt:\t\t\t",dt,
             "\nTime interval (dt) greater than max track length-1\n")
    }

    # subset trackl for each dt
    msd=list()
    msd.summarized=c()
    std.summarized=c()
    num.tracks=c()

    for (i in 1:dt){

        # select only tracks longer than i
        trackl.dt=trackl[(track.len-1)>=i]

        # double check if tracks exist, calculate msd for trackl
        if (length(trackl.dt)==0){
            stop("no track satisfies dt =",i,"\n")
        }

        cat("\n",length(trackl.dt),"tracks satisfy dt =",i,"\n")

        num.tracks[i]=length(trackl.dt)
        msd.individual=sapply(trackl.dt,function(x){
            msd.track(track=x,dt=i,resolution=resolution)})


        # i=1, sapply returns a vecotr
        # i>1, sapply returns a matrix

        if (i==1){

            N=length(msd.individual)

            msd.summarized[i]=mean(msd.individual,na.rm=T)
            std.summarized[i]=sd(msd.individual,na.rm=T)/sqrt(N)
        }else{

            N=length(msd.individual[i,][!is.na(msd.individual[i,])])
            msd.summarized[i]=mean(msd.individual[i,],na.rm=T)
            std.summarized[i]=sd(msd.individual[i,],na.rm=T)/sqrt(N)
        }

    }

    # output
    msd$InidvidualMSD=msd.individual
    msd$SummarizedMSD=msd.summarized
    msd$StandardError=std.summarized
    msd$NumTracksAtDt=num.tracks

    return(msd)

}


##------------------------------------------------------------------------------
## msd.trackll

msd.trackll=function(trackll,dt=6,resolution=0.107){


    msd.trackll.lst=lapply(trackll,function(x){
        msd=msd.trackl(trackl=x,dt=dt,resolution=resolution)
        cat("\n...\n") # a seperator to make output clearer
        return(msd)
        })

    #     msd.trackll.mx=do.call(rbind,msd.trackll.lst)
    #     msd.trackll.vec=apply(msd.trackll.mx, 2, mean)

    return(msd.trackll.lst)
}



##------------------------------------------------------------------------------
## msd

## pass in a list of tracks (list of data.frame), calculate msd for individual
## tracks over time  and output a or calculate averaged msd for all tracks

# msd=function(trackll,dt=6,resolution=0.107,summarize=F,filter=F,plot=F,output=F){
#
#     # filter out frames has less than dt+1
#
#     if (filter == TRUE){
#         track.len=list()
#         for (i in 1:length(trackll)){
#
#             track.len[[i]]=sapply(trackll[[i]],function(track){dim(track)[1]})
#             # take tracks have dt+1 frames
#             trackll[[i]]=trackll[[i]][track.len[[i]]>dt]
#
#         }
#     }

msd=function(trackll,dt=6,resolution=0.107,summarize=F,filter=c(min=7,max=Inf),plot=F,output=F){

    ## filtration of tracks using filter
    trackll=filtration(trackll,filter=filter)

    MSD=msd.trackll(trackll,dt=dt,resolution=resolution)
    file.name=names(trackll)

    # if summarize
    if (summarize==T){

        # remove the IndividualMSD
        MSD=lapply(MSD,function(x){
            x$InidvidualMSD=NULL
            return(x)
        })

        # restructure list to list of matrix
        m=lapply(MSD,function(x){
            m=do.call(rbind,x)
            d=data.frame(t(m))
        })

        n=do.call(rbind,m)


            if (length(grep("txt",rownames(n)[1]))==0){
                Index=strsplit(rownames(n),"\\.")

            }else{
                Index=strsplit(rownames(n),".txt.")


            }

        Index=do.call(rbind,Index)
        colnames(Index)=c("file.name","dt")

        rownames(n)=NULL
        p=cbind(n,Index)

        # another way
        #         melt(m)
        #         dcast(melt(m),Index~...)

        # change dt from factor to integer/numeric
        # another way as.numeric(levels(x))[x]
        p=transform(p,dt=as.integer(as.character(dt)))
        msd.plot=ggplot(

            # p,aes(x=as.integer(as.character(dt)), not work
            p,aes(x=dt,
                  y=SummarizedMSD,group=file.name,col=file.name))+
            geom_line()+geom_point()+
            geom_errorbar(aes(ymin=SummarizedMSD-StandardError,
                              ymax=SummarizedMSD+StandardError), width=.1)+
            # this makes integer breaks
            scale_x_continuous(breaks=scales::pretty_breaks())+
            labs(x="Time intervals", y="SummarizedMSD (µm^2)")+
            theme_bw()+
            theme(legend.title=element_blank())


        # p,aes(x=1:length(dt),y=SummarizedMSD,group=file.name,col=file.name))
        if (plot==T) plot(msd.plot)

        if (output==T){

            fileName=paste("MSD Summarized-",
                           .timeStamp(file.name[1]),"....csv",sep="")
            cat("\nOutput MSD for summarized trajectories.\n")
            write.csv(file=fileName,p)
        }

    }else{
        # calculate msd for individual MSD at 1:dt and output without summarize,
        # this is useful for diffusion coefficient calculation

        # extract only IndividualMSD from list
        MSD=lapply(MSD,function(x){x$InidvidualMSD})

        if(plot==T){
            p=melt(MSD)

            colnames(p)=c("index","track.num","msd","file.name")

            # note group needs to be on two variable (i.e. track.num and
            # file.name) as track.num starts over again from a new file, use
            # interaction() realize it
            msd.plot=ggplot(p,aes(x=index,y=msd,
                                  group=interaction(file.name,track.num),
                                  col=file.name))+
                geom_line()+
                # this makes integer breaks
                scale_x_continuous(breaks=scales::pretty_breaks())+
                labs(x="Time intervals", y="MSD (µm^2)")+
                theme_bw()+
                theme(legend.title=element_blank())

            plot(msd.plot)
        }
        # output csv
        if (output==T){

                for (i in 1:length(trackll)){
                    fileName=paste("MSD individual-",
                                   .timeStamp(file.name[i]),".csv",sep="")
                    cat("\nOutput MSD for individual trajectories.\n")
                    write.csv(file=fileName,MSD[[i]])
                }

            }


    }
    return(MSD)
}

##------------------------------------------------------------------------------
## msd.vecdt

## a msd function that calculates msd for individual track based on a list (corresponding to tracks list) of vector of dt

##' @export msd.vecdt
msd.vecdt=function(trackll,vecdt=NULL,resolution=0.107,filter=c(min=7,max=Inf),output=F){

    ## filtration of tracks using filter
    trackll=filtration(trackll,filter=filter)


    ## dt is in a list, track is in a list, use i j system maybe better
    # copy trackll's structure
    msd.list=list()
    length(msd.list)=length(trackll)
    names(msd.list)=names(trackll)


    # i folder name level
    for (i in 1:length(trackll)){

        # j data.frame level
        for (j in 1:length(trackll[[i]])){

            cat("\rcalculating MSD for individual tracks...","folder ",i," track ",j)
            msd.list[[i]][[j]]=msd.track(track=trackll[[i]][[j]],
                                         dt=vecdt[[i]][[j]],
                                         resolution=resolution)
        }
    }

    if(output==T){

        p=melt(msd.list)
        p=cbind(rownames(p),p)
        colnames(p)=c("index","msd","track.num","file.name")

        fileName=paste("MSD individual-",
                       .timeStamp("vecdt"),".csv",sep="")
        cat("\nOutput MSD for individual trajectories.\n")
        write.csv(file=fileName,p)

    }

    return(msd.list)

    # this list is of different length, only used for curve fitting

}

#     # need to fill the gap to plot correctly
#     msd.plot=ggplot(p,aes(x=1:length(index),y=msd,
#                           group=interaction(file.name,track.num),
#                           col=file.name))+
#         geom_line()+
#         labs(x="Time intervals (10ms)", y="MSD (µm^2)")+
#         theme_bw()+
#         theme(legend.title=element_blank())





