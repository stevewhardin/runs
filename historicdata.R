plotdata <- function(allruns)
{
#   #2009
  print("2009")
  allruns2009 <- allruns[ which(year(allruns$date) == 2009),]
  qts <- quantile(allruns2009$nresponse_time, probs=c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .90), na.rm = TRUE)

  #qts <- quantile(allruns2009$nresponse_time, probs=c(.0, .90), na.rm = TRUE)
  print(qts)
  ranges <- table(cut(allruns2009$nresponse_time, seq(from= 0, to = 38, by = 1 )))
  print(ranges)
  desc <- describe(allruns2009$nresponse_time)
  print(desc)
  #print html
  #print(xtable(ranges), type="html")
  #print(xtable(qts), type="html")
  #print(xtable(desc), type="html")
  plot2009 <- ggplot(allruns2009, aes(x=nresponse_time))
  plot2009 <- plot2009 + geom_histogram(fill="white", colour="black") + geom_vline(xintercept=qts[1], colour="red") + geom_vline(xintercept=qts[10], colour="red") +
    ggtitle("2009 Runs") + xlab("Response Time in Minutes") + ylab("Number of Runs")
ggsave(file="2009.png")

  #2010
  print("2010")
  allruns2010 <- allruns[ which(year(allruns$date) == 2010),]
  qts <- quantile(allruns2010$nresponse_time, probs=c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .90), na.rm = TRUE)
  
  print(qts)
  ranges <- table(cut(allruns2010$nresponse_time, seq(from= 0, to = 38, by = 1 )))
  print(ranges)
  desc <- describe(allruns2010$nresponse_time)
  print(desc)
  #print html
 # print(xtable(ranges), type="html")
 #print(xtable(qts), type="html")
  #print(xtable(desc), type="html")
  plot2010 <- ggplot(allruns2010, aes(x=nresponse_time))
  plot2010 <- plot2010 + geom_histogram(fill="white", colour="black") + geom_vline(xintercept=qts[1], colour="red") + geom_vline(xintercept=qts[10], colour="red") +
    ggtitle("2010 Runs") + xlab("Response Time in Minutes") + ylab("Number of Runs")
ggsave(file="2010.png")
 
 #2011
 print("2011")
 allruns2011 <- allruns[ which(year(allruns$date) == 2011),]
 qts <- quantile(allruns2011$nresponse_time, probs=c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .90), na.rm = TRUE)
 
 print(qts)
 ranges <- table(cut(allruns2011$nresponse_time, seq(from= 0, to = 38, by = 1 )))
 print(ranges)
 desc <- describe(allruns2011$nresponse_time)
 print(desc)
 #print html
 # print(xtable(ranges), type="html")
 #print(xtable(qts), type="html")
 #print(xtable(desc), type="html")
 plot2011 <- ggplot(allruns2011, aes(x=nresponse_time))
 plot2011 <- plot2011 + geom_histogram(fill="white", colour="black") + geom_vline(xintercept=qts[1], colour="red") + geom_vline(xintercept=qts[10], colour="red") +
   ggtitle("2011 Runs") + xlab("Response Time in Minutes") + ylab("Number of Runs")
ggsave("2011.png") 

 #2012
 print("2012")
 allruns2012 <- allruns[ which(year(allruns$date) == 2012),]
 qts <- quantile(allruns2012$nresponse_time, probs=c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .90), na.rm = TRUE)
 
 print(qts)
 ranges <- table(cut(allruns2012$nresponse_time, seq(from= 0, to = 38, by = 1 )))
 print(ranges)
 desc <- describe(allruns2012$nresponse_time)
 print(desc)
 #print html
 # print(xtable(ranges), type="html")
 #print(xtable(qts), type="html")
 #print(xtable(desc), type="html")
 plot2012 <- ggplot(allruns2012, aes(x=nresponse_time))
 plot2012 <- plot2012 + geom_histogram(fill="white", colour="black") + geom_vline(xintercept=qts[1], colour="red") + geom_vline(xintercept=qts[10], colour="red") +
   ggtitle("2012 Runs") + xlab("Response Time in Minutes") + ylab("Number of Runs")
ggsave("2012.png")

 #2013
 print("2013")
 allruns2013 <- allruns[ which(year(allruns$date) == 2013),]
 qts <- quantile(allruns2013$nresponse_time, probs=c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .90), na.rm = TRUE)
 
 print(qts)
 ranges <- table(cut(allruns2013$nresponse_time, seq(from= 0, to = 38, by = 1 )))
 print(ranges)
 desc <- describe(allruns2013$nresponse_time)
 print(desc)
 plot2013 <- ggplot(allruns2013, aes(x=nresponse_time))
 plot2013 <- plot2013 + geom_histogram(fill="white", colour="black") + geom_vline(xintercept=qts[1], colour="red") + geom_vline(xintercept=qts[10], colour="red") +
   ggtitle("2013 Runs") + xlab("Response Time in Minutes") + ylab("Number of Runs")
ggsave("2013.png") 
 
# png("histograms.png")
#  #grid.arrange(plot2009, plot2010, ncol=2)
# multiplot(plot2009, plot2010, cols=2)
# dev.off()


  
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
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