## Dcoef helpers

## < r^2 > = 2 * d * D * △t = 4 D △t

## < r^2 >, mean square displacement
## d, dimensionality of the problem
## D, diffusion coefficient
## t, time

## D △t = < r^2 > / 4


# divide the values by 2 and 2
# first 2 comes from Einstein Brownian motion equation k = sqrt(2 * D * dT)
# second 2 comes from dimension

##------------------------------------------------------------------------------
## Dcoef.static

## return a list of coefficients
Dcoef.static=function(MSD,lag.start=2,lag.end=5,t.interval=0.010){

    #  linear fitting of the MSD curves between time dt 2 and 5
    x=(lag.start:lag.end)*t.interval
    dimension=2



    # x=2:5 ; x=1:4 # coefficient is the same
    D.inst=list()
    for (i in 1:length(MSD)){
        D.inst[[i]]=apply(MSD[[i]][lag.start:lag.end,],MARGIN=2,function(y){
            fit=lm(y~x)
            MSDslope=coefficients(fit)[2]/2/dimension
            MSDcorr=summary(fit)$r.squared
            sc=c(MSDslope,MSDcorr)
            names(sc)=c("slope","corr")
            return(sc)
        })

    }

    names(D.inst)=names(MSD)

    # change shape of the matrix
    D.inst=sapply(D.inst,function(x){
        x=t(x)
        colnames(x)=c("slope","corr")
        return(x)
    },simplify = F)
    return(D.inst)
}

##------------------------------------------------------------------------------
## .Dcoef.roll

## cant use roll on MSD method = percentage, as its MSD is different length, MSD method = percentage is calculated differently, using msd.vecdt(), instead of msd().

Dcoef.roll=function(MSD,window.size=4,t.interval=0.010){

    D.inst=list()
    D.inst.roll=list()
    names.D.inst.roll=c()
    window=1:window.size
    dt=dim(MSD[[1]])[1]
    dimension=2


    for (i in 1:length(MSD)){

        # zero stands for the first window as subsetting using window+j
        for ( j in 0:(dt-window.size)){
            print(window+j)
            x=window+j
            D.inst.roll[[j+1]]=apply(MSD[[i]][window+j,],
                                     MARGIN=2,
                                     function(y){
                                         x=x*t.interval
                                         fit=lm(y~x)
                                         MSDslope=coefficients(fit)[2]/2/dimension
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


    # change shape of the matrix
    for (i in 1:length(D.inst)){
        for (j in 1:length(D.inst[[i]])){
            D.inst[[i]][[j]]=t(D.inst[[i]][[j]])
        }
    }


    #     for (i in 1:length(Dinst)){
    #         D.inst[[i]]=sapply(D.inst[[i]],function(x){
    #             x=t(x)
    #             colnames(x)=c("slope","corr")
    #         })
    #     }
    #     # this changes into a high level array, need a way to control sapply output format


    return(D.inst)
}

##------------------------------------------------------------------------------
## percentage
## To determine the diffusion constant from a trajectory, a line was fit to MSD(n􏲄t) with n running from 1 to the largest integer less than or equal to L/4 (Saxton, 1997).

Dcoef.perc=function(trackll,percentage=0.25,weighted=F,filter=c(min=5,max=Inf), resolution=0.107,t.interval=0.010){

    ## filtration of tracks using filter
    trackll=filtration(trackll,filter=filter)

    #determine the length of each trajectory N then compute first 25% N's
    #msd, manipulate N before hand, then pass in the N vector to dt

    dimension=2
    N=list()

    for (i in 1: length(trackll)){
        N[[i]]=sapply(trackll[[i]],function(x){dim(x)[1]})
    }
    names(N)=names(trackll)

    # tracks greater than 20 steps are reduced by percentage
    for (i in 1:length(N)){
        # tracks greater than 20 steps are reduced by percentage
        N[[i]][N[[i]]>20]=round(percentage*N[[i]][N[[i]]>20])

        # tracks less than 20 are reduced by 1 dt
        N[[i]][N[[i]]<=20]=N[[i]][N[[i]]<=20]-1
    }


    msd.list=msd.vecdt(trackll,vecdt=N,resolution=resolution,filter=filter,output=F)


    # use first 25% of positions for fitting
    # then divide by 2 and 2 (2D)
    # still not sure why it is for in msd analsyzer, thought it was 4 step

    # dstep=4


    # copy trackll's structure
    D.coef=list()
    length(D.coef)=length(msd.list)
    names(D.coef)=names(msd.list)
    for (i in 1:length(msd.list)){

        for (j in 1:length(msd.list[[i]])){
            y=msd.list[[i]][[j]]

            #len=dim(msd.list[[i]][[j]])[1]
            len=length(msd.list[[i]][[j]])

            x=seq(from=t.interval,to=len*t.interval,by=t.interval)
            #x=1:len

            if (weighted==T){
                w=1:len
                fit=lm(y~x,weights =w )
            }else{
                fit=lm(y~x)
            }

            MSDslope=coefficients(fit)[2]/2/dimension
            MSDcorr=summary(fit)$r.squared
            sc=c(MSDslope,MSDcorr)
            names(sc)=c("slope","corr")
            D.coef[[i]][[j]]=sc
        }

    }

    # this changes format into a matrix
    D.coef=sapply(D.coef,function(x){
        do.call(rbind,x)},simplify=F)

    return(D.coef)

}




# x=list()
# for (i in 1:length(D.coef)){
#     x[[i]]=do.call(rbind,D.coef[i])
# }
#
# dx=sapply(D.coef,function(x){
#     do.call(rbind,x)})
# # this returns a matrix
#
# }
# names(D.inst)=names(MSD)


#     Weights are set to be the number of points (length of trajectory?) averaged to generate the mean square displacement value at the given delay (in this case, it is the 25%). Thus, we give more weight to MSD curves with greater certainty (larger number of elements averaged).

# weights are essentially the lenght of the msd list

#     % - M the weighted mean of MSD for each delay
#     % - STD the weighted standard deviation
#     % - N the number of degrees of freedom in the weighted mean
#     % (see http://en.wikipedia.org/wiki/Weighted_mean)

# plot those coef and get the mean of all

# The only requirement for weights is that the vector supplied must be the same length as the data.

# simplest weights  index of the msd (as it shows how many points is used to generate the msd, steps)

# more sophistacted  1/theta^2 (variance)



# would be nice to have a subsetting method for a smt class, there are so many levels of subsetting, each time it needs a lapply

## instead of trackll, it maybe better to store tracks in data.table, then folders
## instead of (second) list of data.frame, it may worth the effort simply making it a data.frame (data.table) with fourth column as trajectory numbers.

## it makes program (maybe) easier, computation faster

##------------------------------------------------------------------------------
## rsquare.filter
rsquare.filter=function(D.inst,static=TRUE){

    if (static==T){

        # r.squared >= 0.8 as quality control
        #         slope=lapply(D.inst,function(x){x[rownames(x)=="slope"]})
        #         corr=lapply(D.inst,function(x){x[rownames(x)=="corr"]})


        #         slope=lapply(D.inst,function(x){x[colnames(x)=="slope"]})
        #         corr=lapply(D.inst,function(x){x[colnames(x)=="corr"]})

        slope=lapply(D.inst,function(x){x[,"slope"]})
        corr=lapply(D.inst,function(x){x[,"corr"]})

        corr.filter=lapply(corr,function(x){x>=0.8})
        D.inst.subset=mapply("[",slope,corr.filter,SIMPLIFY=F)

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
            slope=lapply(D.inst[[i]],function(x){x[,"slope"]})
            corr=lapply(D.inst[[i]],function(x){x[,"corr"]})

            #corr.filter=lapply(corr,function(x){x>=0.8})
            #D.inst.subset[[i]]=mapply("[",slope,corr.filter)


            ## logorithm
            #Log.D.inst[[i]]=lapply(D.inst[[i]],log)
            # remove NaN if wanted
            # Log.D.inst=lapply(Log.D.inst, function(x){
            #    x[!is.nan(x)]
            #})

            ## alternative  works
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
## Dcoef.log

Dcoef.log=function(D.inst.subset,static=T){
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
