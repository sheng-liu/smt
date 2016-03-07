## fitCDF-methods
##
##
###############################################################################
##' @name fitCDF
##' @aliases fitCDF
##' @title fitCDF
##' @rdname fitCDF-methods
##' @docType methods
##' @description Caclulate apparent diffusion coefficient (Dcoef) for
##'   trajecotries by fitting displacementCDF.
##'
##' @usage
##' fitCDF(cdf,
##'        components=c("one","two","three"),
##'        start.value=list(
##'             oneCompFit=list(D=0.5),
##'             twoCompFit=list(D1=0.1,D2=0.5,alpha=0.5),
##'             threeCompFit=list(D1=0.3,D2=0.3,D3=0.3,alpha=0.3,beta=0.5)))
##' @param cdf cdf calculated from displacementCDF().
##' @param components parameter specifying the number of components to fit.Currently support one to three components fit.
##' @param start.value the start value for fitting.
##'
##' @return
##' \itemize{
##' \item{on screen output,} Result and parameters of goodness of the fit.
##' \item{Plot,} fiting plot.
##' }
##' @details calculating Dceof by fitting displacementCDF.
##'
##' @examples
##'
##' # compare folders
##' folder1=system.file("extdata","SWR1",package="smt")
##' folder2=system.file("extdata","HTZ1",package="smt")
##' trackll=compareFolder(c(folder1,folder2))
##' cdf=displacementCDF(trackll,dt=1,plot=F,output=F)
##' fitCDF(cdf,components="two")

##' @import minpack.lm
###############################################################################

## dt needs to be avariable in this equation so it is flexible and has a meaning to its unit um/s

# ------------------------------------------------------------------------------
# one component fit

one.comp.fit=function(r,P,start.value=list(D=0.5),name){
    # with one parameter, D
    p = function(r,D){1 - exp(-r^2/(4*D*0.01))}

    title=paste("One component fit -",name)
    cat("\n\n","==>",title,"\n")

    # fit equation 2 to data P
    ocfit=nls(P ~ p(r,D),start=start.value)
    print(ocfit)

    # plot
    plot(r,P,main=title,cex=0.3)
    curve(p(x,D=coef(ocfit)),add=TRUE,col="red")

    return(ocfit)
}

# ------------------------------------------------------------------------------
# two components fit

two.comp.fit=function(r,P,start.value=list(D1=0.1,D2=0.5,alpha=0.5),name){

    ## equation
    p3 =function(r,D1,D2,alpha){
        1 - (alpha*exp(-r^2/(4*D1*0.01)) + (1-alpha)*exp(-r^2/(4*D2*0.01)))}

    title=paste("Two components fit -",name)
    cat("\n\n","==>",title,"\n")

    ## fitting
    tcfit= nls(P ~ p3(r,D1,D2,alpha),start=start.value)
    print(tcfit)

    ## plotting
    plot(r,P,main=title,cex=0.3)
    curve(p3(x,
             coef(tcfit)["D1"],
             coef(tcfit)["D2"],
             coef(tcfit)["alpha"]),
          add=T,col="red"
    )
}

# ------------------------------------------------------------------------------
# three components fit

three.comp.fit=function(r,P,start.value=list(D1=0.3,D2=0.3,D3=0.3,alpha=0.3,beta=0.5),name){

    ## equation
    p5=function(r,D1,D2,D3,alpha,beta){
        1 - (
            alpha*exp(-r^2/(4*D1*0.01)) +
                beta*exp(-r^2/(4*D2*0.01)) +
                (1-alpha-beta)*exp(-r^2/(4*D3*0.01))
        )}

    title=paste("Three components fit -",name)
    cat("\n\n","==>",title,"\n")

    # try minpack.lm for low or zero noise data
    thcfit=nlsLM(P ~ p5(r,D1,D2,D3,alpha,beta),start=start.value,lower=c(0,0,0,0,0),upper=c(Inf,Inf,Inf,1,1))
    print(coef(thcfit))

    ## plot
    plot(r,P,main=title,cex=0.3)
    curve(p5(x,coef(thcfit)["D1"],coef(thcfit)["D2"],coef(thcfit)["D3"],coef(thcfit)["alpha"],coef(thcfit)["beta"]),add=T,col="red")

}



# ------------------------------------------------------------------------------
# fitCDF
##' @export fitCDF
fitCDF=function(cdf, components=c("one","two","three"),
            start.value=list(
                oneCompFit=list(D=0.5),
                twoCompFit=list(D1=0.1,D2=0.5,alpha=0.5),
                threeCompFit=list(D1=0.3,D2=0.3,D3=0.3,alpha=0.3,beta=0.5))
                      ){

    # use lapply to do it for all folders
    cdf.displacement=cdf$cdf.displacement
    name=names(cdf.displacement)

    method=match.arg(components)


    for (i in 1:length(cdf.displacement)){

        r=cdf.displacement[[i]]$UniqueDisplacement
        P=cdf.displacement[[i]]$CDF


        switch(method,
               one={one.comp.fit(r,P,start.value=start.value$oneCompFit,name[i])},
               two={two.comp.fit(r,P,start=start.value$twoCompFit,name[i])},
               three={three.comp.fit(r,P,start=start.value$threeCompFit,name[i])}
               )

    }

}


# ------------------------------------------------------------------------------
#TODO:

# a better density plot than ggplot2 there, or adjust it to be better /professiona looking
#   plot(density(r),main="distribution of displacement r")

## output files
