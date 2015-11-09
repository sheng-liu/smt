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
###############################################################################


# plot histogram, CDF of MSD at 8 time point
# superimpose them on one graph



# calculate MSD for all trajectory at a single dt, plot all those MSD's CDF and histogram


CDF=function(trackll,dt){


    ## get the data, msd at individual trajectories
    MSD=msd(trackll,dt=dt,summarize=F,filter=T)

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















