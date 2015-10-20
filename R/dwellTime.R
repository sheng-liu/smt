
## dwellTime-methods
##
##
###############################################################################
##' @name dwellTime
##' @aliases dwellTime
##' @title dwellTime
##' @rdname dwellTime-methods
##' @docType methods
##' @description caclulate dwellTime from tracks/trajecotries.

##' @usage findPeaks(bamFile.chip) # this roxygen directive does not working
##' @method findPeaks # this roxygen directive does not working
##' @param bamFie.chip Full path to chip bam file.
##' @param bamFile.control Full path to control bam file.
##' @param obj.chip SeqData object containing information on chip.
##' @param obj.control SeqData object containing information on control.
##' @param description Optional. User can input short desciption for the output file, it will show up in the file name of the output file.
##' @return
##' \itemize{
##' \item{peakTable.csv} A csv file contaning information about the significantly enriched regions, including chromosome,start,end,peakPositions,peakCoverageSums,rpkm.peakCoverageSums,chipPeakHeights,controlPeakHeights,p.values,fdr,foldChange.
##' \item{coverageView slot} A RleViews object containing Views of the peak regions.
##' }
##' @section Usage : {
##' findPeaks(obj.chip=NULL,obj.control=NULL,bamFile.chip=character(0),bamFile.control=character(0),fdr.max=1e-5,foldChange.min=2)
##' }
##' @examples
##' #findPeaks(bamFile.chip)
##' #findPeaks(bamFile.chip,bamFile.control)
##' #findPeaks(obj.chip)
##' #findPeaks(obj.chip,obj.control)
##' #findPeaks(obj.chip,obj.control,bamFile.chip,bamFile.control)
##' # to view coverageView slot
##' # coverageView(obj.chip)[[1]]
##' @details
##' returns the dwell time for every first level of the list, corresponding to each file. If combined dewell time is intended, use readDiatrack(folder, merge=T) to generate a single length list, then apply this function.


###############################################################################


dwellTime=function(trackll,exposure=10,output=F){

    dwell.time=sapply(trackll,function(x){.dwellTime(x,exposure)})




    ## output
    if (output==T){

        file.name=names(trackll)

        for (i in 1:length(trackll)){

            fileName=paste("Dwell Time-",.timeStamp(file.name[i]),".csv",sep="")

            write.csv(file=fileName,dwell.time[[i]])

        }

    }

    return(dwell.time)
}



# dt.list=lapply(trackll,dwellTime)
# dt=do.call(c,dt.list)



##-----------------------------------------------------------------------------
## TODO: Add PDF



# TODO: switch for either TFBS or Histone modification
# TODO: add peakPositions.control for sample swap to remove negtive peaks, or just add a swapper which is better off.
# TODO: add scale factor for libSizeDiff
# TODO: plot enriched.peaks vs unenriched (It's a nice pic, worth doing)

# TODO: when libSize has huge difference, this may leads to loss of information, further normalization using MAnorm package maybe added if needed.

## performance
## chr12 with H3 as control
# $sampling.time
# [1] 91.2

