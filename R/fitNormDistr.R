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
##' fitNormDistr(dcoef,components=2,log.transform=T)
##' @param dcoef diffusion coefficient calculated from Dcoef().
##' @param components parameter specifying the number of components to fit.
##' @param log.transform logical indicate if log10 transformation is needed, default F.
##' @return
##' \describe{
##' \item{proportions}{The final mixing proportions.}
##' \item{mean}{The final mean parameters.}
##' \item{sd}{The final standard deviations.}
##' }

##' @examples
##'
##' # compare folders
##' folder1=system.file("extdata","SWR1",package="smt")
##' folder2=system.file("extdata","HTZ1",package="smt")
##' trackll=compareFolder(c(folder1,folder2))
##' dcoef=Dcoef(trackll,dt=6,plot="none",output=F)
##' fitNormDistr(dcoef,components=2,log.transform=T)

##' @export fitNormDistr
###############################################################################
# fit normal distribution to diffusion coefficient

# library(mixtools)


fitNormDistr=function(dcoef,components=2,log.transform=F){

    # scale=1e3
    mixmdl.lst=list()
    name=names(dcoef)

    for (i in 1:length(dcoef)){

        data=dcoef[[name[i]]][,"slope"]

        # log transformation
        if (log.transform) data=log10(data)

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
            return(result)
        })

    }else{

        result.lst=lapply(mixmdl.lst,
                         function(x){
                             result=list(proportions=x$lambda,mean=x$mu,sd=x$sigma)
                             if (log.transform) result$mean=10^(result$mean)
                             result=do.call(cbind.data.frame,result)
                             return(result)})
    }

    # return(result.lst)
    print(result.lst)
    return(mixmdl.lst)

}

## TODO
## output csv files of result.lst



