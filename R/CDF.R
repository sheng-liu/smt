## CDF-methods
##
##
###############################################################################
##' @name CDF
##' @aliases CDF
##' @title CDF
##' @rdname CDF-methods
##' @docType methods
##' @description Plot emperical cummulative probability (eCDF) for
##'   trajecotries.

##' @usage
##'   CDF(trackll,dt)
##' @param trackll Track list output from readDiatrack().
##' @param dt Time intervals.

##' @return
##' \itemize{

##' \item{PDF} eCDF of MSD at specified dt.

##' }

##' @examples
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder)
##' CDF(trackll,dt=8)


##' @export CDF
##'
##'
##'
###############################################################################


# plot histogram, CDF of MSD at 8 time point
# superimpose them on one graph

# calculate MSD for all trajectory at a single dt, plot all those MSD's CDF and histogram

## CDF on MSD
## if one select filter as 2 frames, then it is not filtering out any short tracks

CDF=function(trackll,dt){


    ## get the data, msd at individual trajectories
    MSD=msd(trackll,dt=dt,summarize=F,filter=c(min=2,max=Inf))

    # cdf
    # each row is MSD of individual trajectories at dt
    # calculte cumulative sum of each row
    #msd.dt[[1]][dt,]

    MSD.dt=lapply(MSD,function(x){x[dt,]})


    #cdf=lapply(msd.dt,function(x){cumsum(x[dt,])})


    p=melt(MSD.dt)

    ecdf=ggplot(p,aes(x=value,group=L1,colour=L1))+stat_ecdf()+
        labs(x="MSD",y="eCDF")+
        theme_classic()+
        theme(legend.title=element_blank())

    plot(ecdf)

#     # plot(ecdf(msd.dt[[1]][1,]))
#
#     # change vector to matrix to keep index during melt
#     cdf.mx=lapply(cdf,as.matrix)
#     p=melt(cdf.mx)
#     p$Var2=NULL
#
#     ggplot(p,aes(x=Var1,y=value,group=L1,colour=L1))+
#         geom_line()+geom_point()+
#         labs(xlab="dt",ylab="Cumulative MSD")
#
#     +
#         theme_classic()+
#         theme(legend.title=element_blank())

    return(ecdf)

}

## TODO: should add original cumsum as well, as it is also valid way of comparison
## CDF on Log.Dcoef,  compare with Log.Dcoef histogram
## and Dcoef this way one is not losing the negative values



## CDF on squaredisp.sqrt

## CDF is essentially a way of representing data, like histogram or density
## should/can be added to plot method of each function.

# CDFsquaredisp=function(track,dt=6,resolution=0.107){
#
#     # validity check for dt less than track length
#     if (dt >(dim(track)[1]-1)){
#         stop("\ntrack length:\t",dim(track)[1],
#              "\ndt:\t\t",dt,
#              "\nTime interval (dt) greater than track length-1\n")
#     }
#
#
#     msd.dt.track=c()
#     for (i in 1:dt){
#
#         #track.sqd=squareDisp(track,dt=i,resolution=resolution)
#
#         # at each dt, there are dt number of sub-trajectory/sub-tracks
#         # mean of dt-wise sub-trajectories/ step-wise sub tracks
#         # average subtracks into one number at each dt
#         #msd.dt.subtrack=sapply(track.sqd,function(x){
#         #    mean(x$square.disp,na.rm=T)})
#         # msd.dt.track[i]=mean(msd.dt.subtrack)
#
#         # caculate msd for track at specified dt
#         track.sqd=squareDisp(track,dt=i,resolution=resolution)
#
#         # pull all the squared displacement at this dt together
#         square.disp=do.call(rbind,track.sqd)$square.disp
#         square.disp=square.disp[!is.na(square.disp)]
#         # get the genuine mean
#         msd=mean(square.disp)
#
#
#         # get the sum of all subtrajectories then average them
#         #         sum.square.disp=lapply(track.sqd,function(trk){
#         #           # subsetting list with [["colnames]]
#         #           sum(trk[["square.disp"]],na.rm=T)})
#         #         # get the msd
#         #         msd=mean(do.call(rbind,sum.square.disp))
#
#
#         # for any N length trajectory, one can have N-1 dt steps
#         # the resulting msd at each dt is generated from N:N-1 individual steps
#         msd.dt.track=c(msd.dt.track, msd)
#
#
#     }
#
#     return(msd.dt.track)
#
#
#
# }





















