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
##' fitNormDistr(dcoef,components=2)
##' @param dcoef diffusion coefficient calculated from Dcoef().
##' @param components parameter specifying the number of components to fit.
##'
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
##' dcoef=Dcoef(trackll,dt=6,plot=F,output=F)
##' fitNormDistr(dcoef,components=2)

##' @export fitNormDistr
###############################################################################
# fit normal distribution to diffusion coefficient

# library(mixtools)


fitNormDistr=function(dcoef,components=2){

    mixmdl.lst=list()

    name=names(dcoef)

    for (i in 1:length(dcoef)){

        data=dcoef[[name[i]]][,"slope"]

        mixmdl=normalmixEM(data,k=components)
        plot(mixmdl,which=2)

        mixmdl.lst[[i]]=mixmdl
    }

    names(mixmdl.lst)=name
    # return(mixmdl.lst)

    abstr.lst=lapply(mixmdl.lst,
                     function(x){
                         return(list(proportions=x$lambda,mean=x$mu,sd=x$sigma))})
    return(abstr.lst)
}






#
