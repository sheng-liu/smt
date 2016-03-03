## plotting helpers

##------------------------------------------------------------------------------
## plotHistogram
plotHistogram=function(Log.D.inst,binwidth=0.5, method){
    p=reshape2::melt(Log.D.inst)

    if (method=="static"||method=="percentage"){

        colnames(p)=c("Log.D.inst","file.name")

        # overlay histogram and density plot without changing count as y axies
        Dcoef.plot=ggplot(p,aes(x=Log.D.inst,group=file.name,col=file.name))+
            geom_histogram(aes(y = ..count..,fill=file.name),
                           binwidth=binwidth,position="dodge")+

            geom_density(aes(y=0.5*..count..,fill=file.name),alpha=0.2)+
            theme_bw()+
            theme(legend.title=element_blank())
        plot(Dcoef.plot)
    }else if (method=="rolling.window"){



        colnames(p)=c("Log.D.inst","window.name","file.name")

        facet.plot=ggplot(p,aes(x=Log.D.inst,group=file.name,col=file.name))+
            geom_histogram(aes(y = ..count..,fill=file.name),
                           binwidth=binwidth,position="dodge")+

            geom_density(aes(y=0.5*..count..,fill=file.name),alpha=0.2)+
            theme_bw()+
            theme(legend.title=element_blank())+
            facet_grid(window.name ~ .)

        merged.plot=ggplot(p,aes(x=Log.D.inst,group=file.name,col=file.name))+
            geom_histogram(aes(y = ..count..,fill=file.name),
                           binwidth=binwidth,position="dodge")+
            geom_density(aes(y=0.5*..count..,fill=file.name),alpha=0.2)+
            theme_bw()+
            theme(legend.title=element_blank())

        multiplot(facet.plot,merged.plot,cols=2)

    }
}
## TODO:
## change the 0.5 to binwidth, so it is dynamic, it is not recognized somehow.


##------------------------------------------------------------------------------
## plotDensity
plotDensity=function(Log.D.inst,binwidth=0.5,method){

    p=reshape2::melt(Log.D.inst)


    if (method=="static"||method=="percentage"){
        colnames(p)=c("Log.D.inst","file.name")

        Dcoef.plot=ggplot(p,
                          aes(x=Log.D.inst,group=file.name,
                              col=file.name,fill=file.name))+
            geom_histogram(aes(y = ..density..,fill=file.name),
                           binwidth=binwidth,position="dodge")+
            geom_density(alpha = 0.2)+
            theme_bw()+
            theme(legend.title=element_blank())

        plot(Dcoef.plot)


    }else if (method=="rolling.window"){

        colnames(p)=c("Log.D.inst","window.name","file.name")

        # a perfect case for faceting
        facet.plot=ggplot(p,
               aes(x=Log.D.inst,group=file.name,
                   col=file.name,fill=file.name))+
            geom_density(alpha = 0.2)+
            theme_bw()+
            theme(legend.title=element_blank())+
            facet_grid(window.name ~ .)

        merged.plot=ggplot(p,
               aes(x=Log.D.inst,group=file.name,
                   col=file.name,fill=file.name))+
            geom_density(alpha = 0.2)+
            theme_bw()+
            theme(legend.title=element_blank())

        ## could also add a merged without 1234

        multiplot(facet.plot,merged.plot,cols=2)


    }


}

# ggplot(p,
#        aes(x=Log.D.inst))+
#     geom_density()

##------------------------------------------------------------------------------
## plotVariance
plotVariance=function(Log.D.inst,method){

        cat("generating variance plot \n")

    ## plot data preparation
    ## plot mean of Log.D.inst of each individual trajectory, against variance of each individual trajectory

    # when the list have same length, it maybe easier to work with when converted into data.frame

    Log.D.inst.df=do.call(rbind.data.frame,Log.D.inst)
    MEAN=data.frame(apply(Log.D.inst.df,1,mean,na.rm=T))

    folder=c()
    for (i in 1:dim(MEAN)[1])
        folder[i]=unlist(strsplit(rownames(MEAN)[i],split ="[.]"))[1]
    MEAN=cbind(MEAN,folder)
    colnames(MEAN)=c("mean","folder")


    SD=data.frame(apply(Log.D.inst.df,1,sd,na.rm=T))
    colnames(SD)=c("sd")

    data=cbind(MEAN,SD)

    # plotting
    #     scatter=ggplot(data,aes(x=mean,y=sd,col=folder))+
    #         geom_point(alpha=0.8)+
    #         theme_bw()+
    #         theme(legend.title=element_blank())
    #
    #     mean.density=ggplot(data,aes(x=mean,col=folder))+
    #         geom_density()+
    #         theme_bw()+
    #         theme(legend.title=element_blank())
    #
    #     sd.density=ggplot(data,aes(x=sd,col=folder))+
    #         geom_density()+
    #         theme_bw()+
    #         theme(legend.title=element_blank())
    #
    #     multiplot(scatter,mean.density,sd.density, cols=1)

    # another implementation using gridExtra::grid.arrange
    scatter=ggplot(data,aes(x=mean,y=sd,col=folder))+
        geom_point(alpha=1,shape=21)+
        theme_bw()+
        theme(legend.title=element_blank())+
        theme(legend.position=c(1,1),legend.justification=c(1,1))

    mean.density=ggplot(data,aes(x=mean,fill=folder,col=folder))+
        geom_density(alpha=0.5)+
        theme(legend.title=element_blank())+
        theme(legend.position = "none")

    sd.density=ggplot(data,aes(x=sd,fill=folder,col=folder))+
        coord_flip()+
        geom_density(alpha=0.5)+
        theme(legend.title=element_blank())+
        theme(legend.position = "none")

    empty <- ggplot()+geom_point(aes(1,1), colour="white")+
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()
        )


    grid.arrange(mean.density, empty, scatter, sd.density, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))



    # this would have worked if mapply takes na.rm=T
    # mapply(mean,Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]],na.rm=T)


    # or this one
    # mapply(function(x){mean(x,na.rm=T)},Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]])

    # this means mapply is not taking the elements of each list into one vector, but used them as seperate


    # used alternative, concatanate, then apply
    #             C=mapply("c",Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]])
    #
    #
    #
    #             apply(C,2,mean,na.rm=T)
    #             mapply(mean,a,b)
    #
    #             lapply(Log.D.inst[[1]])
    #
    #         }
    #
    #         mapply(mean,Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]],na.rm=T)
    #         mapply(function(x){mean(x,na.rm=T)},
    #                      Log.D.inst[[1]][[1]],Log.D.inst[[1]][[2]])
    #
    #         mapply()
    #
    #         # collapse sublist rolling windowns into uper level list
    #         Log.D.inst=lapply(Log.D.inst,unlist)

}



##------------------------------------------------------------------------------
## .
## from Rcookbook
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

