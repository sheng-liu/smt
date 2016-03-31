## fitNormDistr-methods
##
##
###############################################################################
##' @name fitNormDistr
##' @aliases fitNormDistr
##' @title fitNormDistr
##' @rdname fitNormDistr-methods
##' @docType methods
##' @description fit normal distribution to diffusion coefficient caclulated by Dcoef method.
##'
##' @usage
##' fitNormDistr(dcoef,components=2,log.transform=F,output=F)
##' @param dcoef diffusion coefficient calculated from Dcoef().
##' @param components parameter specifying the number of components to fit.
##' @param log.transform logical indicate if log10 transformation is needed, default F.
##' @param output Logical indicaring if output file should be generated.
##' @return
##' \describe{
##' \item{proportions}{The final mixing proportions.}
##' \item{mean}{The final mean parameters.}
##' \item{sd}{The final standard deviations.}
##' \item{loglik}{The log likelihood.}
##' }

##' @examples
##'
##' # compare folders
##' folder1=system.file("extdata","SWR1",package="smt")
##' folder2=system.file("extdata","HTZ1",package="smt")
##' trackll=compareFolder(c(folder1,folder2))
##' dcoef=Dcoef(trackll,dt=6,plot="none",output=F)
##' fitNormDistr(dcoef,components=2,log.transform=F,output=F)

##' @export fitNormDistr
###############################################################################
# fit normal distribution to diffusion coefficient

# library(mixtools)


fitNormDistr=function(dcoef,components=2,log.transform=F,output=F){

    # scale=1e3
    mixmdl.lst=list()
    name=names(dcoef)

    for (i in 1:length(dcoef)){

        data=dcoef[[name[i]]][,"slope"]

        # log transformation
        if (log.transform==T) {
            data=log10(data)
            data=data[!is.na(data)]
            }

        if (components==1){

            # this fit ignores the negative values
            # data=data[data>=0]
            # mixmdl=fitdist(data,"norm")
            # mixmdl=fitdist(data*scale,"norm",method="mle")
            # small values need scaling, mme doesn't and generate the same fit.

            mixmdl=fitdistrplus::fitdist(data,"norm",method="mme")
            print(summary(mixmdl))

            # denscomp(fitg,demp=T) or plot(mixmdl)
            fitdistrplus::denscomp(mixmdl, addlegend=FALSE)
            mixmdl.lst[[i]]=mixmdl

        }else{

            # convergence creteria epsilon = 1e-10 is to filter out false positives
            mixmdl=normalmixEM(data,k=components,maxit=1e4,epsilon = 1e-10)
            print(summary(mixmdl))
            plot(mixmdl,which=2)
            # mixmdl[c("mu","sigma","lambda")]
            mixmdl.lst[[i]]=mixmdl


        }

    }

    names(mixmdl.lst)=name

    # return(mixmdl.lst)

    # abstract result from the fitting model
    if (components==1){

        result.lst=lapply(mixmdl.lst,function(x){
            s=summary(x)
            result=list(mean=s$estimate[1],sd=s$estimate[2])
            if (log.transform) result$mean=10^(result$mean)
            result=do.call(cbind.data.frame,result)
            print(result)
            return(result)
        })

    }else{

        result.lst=lapply(mixmdl.lst,
                         function(x){
                             result=list(proportion=x$lambda,
                                         mean=x$mu,
                                         sd=x$sigma,
                                         log.lik=x$loglik)
                             if (log.transform) result$mean=10^(result$mean)
                             result=do.call(cbind.data.frame,result)
                             return(result)})
    }

    # return(result.lst)
    print(result.lst)

    # output
    if (output==T){

        result.df=do.call(rbind.data.frame,result.lst)
        fileName=paste("FitNormDistr-",
                       .timeStamp(name[1]),"....csv",sep="")
        cat("\nOutput FitNormDistr.\n")
        write.csv(file=fileName,result.df)
    }
    # use invisible() so the user would not be overwhelmed by the numbers
    # while programmers can assign the value and use it
    return(invisible(mixmdl.lst))

}

## TODO
## output csv files of result.lst



