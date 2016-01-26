
## plotTrack-methods
##
##
###############################################################################
##' @name plotTrack
##' @aliases plotTrack
##' @title plotTrack
##' @rdname plotTrack-methods
##' @docType methods
##' @description Plot track/trajectory from track list. either randomly or specified.

##' @usage plotTrack(ab.trackll,resolution=0.107,frame.min=8,frame.max=100,frame.start=1,frame.end=500)
##'
##' plotTrackFromIndex(index.file, movie.folder=c(folder1,folder2,...),resolution=0.107,frame.min=1,frame.max=100,frame.start=1,frame.end=500)
##'
##' @param ab.trackll absolute coordinates for plotting, generated from readDiatrack(folder,ab.track=T).
##' @param resolution ratio of pixel to µM.
##' @param frame.min minimum frame number for plotting.
##' @param frame.max max frame number for plotting.
##' @param frame.start the first frame to plot. Default 1.
##' @param frame.end last frame to plot. Default 500.
##' @param index.file a csv file that contains index of tracks in the first column. Leave a header line when preparing such a file.
##' @param movie.folder the path to the folder which contains Diatrack output txt files (presumably it is the same folder with movie files).


##' @return
##' \itemize{

##' \item{PDF} One PDF file with all the frames satisfy the creteria. If trackll has multiple items, it ouptus mutiple PDF files each corresponding to one item.

##' }
##' @details plotTrackFromIndex: if user provide a csv file with first column listing the index of trajectories, this program will plot the tracks isted in the csv file. It is useful after manipulating with the output from Dceof, to plot the tracks that of interest to the user (e.g. highest Dcoef). User need to provide the indexFile.csv, and specify the movie folder which contains the movies where specified trajectories are tracked.

##' @examples
##' folder=system.file("extdata","SWR1",package="smt")
##' trackll=readDiatrack(folder,ab.track=TRUE)
##' plotTrack(trackll)
##'
##' ## plot from index file
##' index.file=system.file("extdata","INDEX","indexFile.csv",package="smt")
##' movie.folder=system.file("extdata","SWR1",package="smt")
##' plotTrackFromIndex(index.file=index.file,movie.folder = movie.folder)
##'
##' ## index file contain trajectories from multiple movie folders
##' folder1=system.file("extdata","SWR1",package="smt")
##' folder2=system.file("extdata","HTZ1",package="smt")
##' index.file2=system.file("extdata","INDEX","indexFile2.csv",package="smt")
##' plotTrackFromIndex(index.file=index.file2,movie.folder = c(folder1,folder2))



## @import reshape2
##' @export plotTrack
##' @export .plotTrack
##' @export plotTrackFromIndex
## @import animation
## FUTURE: maybe plot on dt


## TODO: make the function input as c(min,max)
###############################################################################

.plotTrack=function(ab.trackl,file.name="TrajectoryPlot",resolution=0.107,frame.min=8,frame.max=100,frame.start=1,frame.end=500){

    # trackl is just a list of trajectories, with no upper level indicating folder
    ab.trackl.res=lapply(ab.trackl,function(x) x*resolution)
    m=max(sapply(ab.trackl.res,max))


    # add names to each plot
    name=names(ab.trackl)


    fileName=paste(.timeStamp(file.name),".pdf",sep="")

    pdf(file=fileName)

    par(mfrow=c(2,2))
    len=length(ab.trackl.res)

    frame.end=ifelse(len<frame.end,len,frame.end)

        for (i in frame.start:frame.end){

        p=ab.trackl.res[[i]]
        frame.len=dim(p)[1]
        if (frame.len>frame.min & frame.len<frame.max)
        plot(p$x,p$y,type="l",xlim=c(0,m),ylim=c(0,m),xlab="X (µM)",
             ylab="Y (µM)",main=name[[i]])
        }

    # sub = name[[i]]
    dev.off()

    return(ab.trackl.res)

}


plotTrack=function(ab.trackll,resolution=0.107,frame.min=8,frame.max=100,frame.start=1,frame.end=500){

    file.name=names(ab.trackll)


    for (i in 1:length(file.name)){

        # output plot
        cat("\nOutput track plot...\n")

        plot.coords=.plotTrack(ab.trackll[[i]],file.name[i],resolution=resolution,frame.min=frame.min,frame.max=frame.max,frame.start=frame.start,frame.end=frame.end)

        # output csv of the plot
        cat("\nOutput csv file for track plot...\n")

        plot.coords.df=do.call(rbind,plot.coords)
        # melt(plot.coords)


        fileName=paste("Track Coords-",.timeStamp(file.name[i]),".csv",sep="")
        write.csv(file=fileName,plot.coords.df)

    }

    # lapply(ab.trackll,function(ab.trackl,file.name){.plotTrack(ab.trackl,file.name,resolution=resolution,frame.min=frame.min,frame.max=frame.max,frame.start=frame.start,frame.end=frame.end)})
    # lapply can only take one input

}



## plot trajectory according to index
## user need to put the corresponding movie files into a folder

plotTrackFromIndex=function(index.file, movie.folder=c(folder1,folder2,...),resolution=0.107,frame.min=1,frame.max=100,frame.start=1,frame.end=500){

    ## read trajectory index from the index.file
    index.df=read.csv(file=index.file,header=T)
    index=as.character(index.df[,1])


#     # TODO: the number of folder to compare can be extended using ... statement
#     folder.list=list()
#     for (i in 1:length(movie.folder)){
#         folder.list[[i]]=movie.folder[i]
#     }
#
#
#     # remove null folders by subsetting un-null folders
#     null.folder=sapply(folder.list,is.null)
#     folder.list=folder.list[!null.folder]
#
#     names(folder.list)=sapply(folder.list,basename)
#
#     ab.trackll=list()
#
#     for (i in 1:length(folder.list)) {
#
#         ## read in tracks in movie.folder with absolute coords,
#         ## merge them as the input is merged csv files
#         ab.trackll[i]=readDiatrack(folder=folder.list[[i]],merg=T,ab.track=T)
#         cat("\n...\n") # seperator makes ouput clearer
#         names(ab.trackll)[i]=names(folder.list)[i]
#
#
#     }

    ab.trackll=compareFolder(folders=movie.folder,ab.track=T)

    # as it is only for one folder
    # trackl.plot=ab.trackll[[1]][index]

    ## because it is a merged list, use .plotTrack
    ## .plotTrack(trackl.plot,resolution=resolution,frame.min=frame.min,frame.max=frame.max,frame.start=frame.start,frame.end=frame.end)


    # or one can maintain the ab.trackl's structure, which has the folder name
    trackll.plot=lapply(ab.trackll,function(x){x[index]})

    ## remove NA (or combine it into one list)

    trackll.plot.narm=lapply(trackll.plot, function(x) x[!is.na(names(x))])

    ## if the list is length 0, remove it
    ## remove zero length list
    for (i in 1:length(trackll.plot.narm)){

        if (length(trackll.plot.narm[[i]])==0) trackll.plot.narm[[i]]=NULL
    }

    plotTrack(trackll.plot.narm,resolution=resolution,frame.min=frame.min,frame.max=frame.max,frame.start=frame.start,frame.end=frame.end)


}





