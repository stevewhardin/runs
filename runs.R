#R script to analyze basic fire department run data
#Uses Plotly for some of the graphs
#Steve Hardin 

library(psych)
library(car)
library(ggplot2)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(grid)
library(gridExtra)

working_dir = "/Users/stevehardin/Dropbox/work/r/runs"
setwd(working_dir)
data_file="/Users/stevehardin/Dropbox/work/r/runs/data/runs.csv"
data_fileitypes="/Users/stevehardin/Dropbox/work/r/runs/data/incidenttypes2013.csv"
runs <- read.table(data_file, header=T, sep=",")
incidenttypes <- read.table(data_fileitypes, header=T, sep=",")
#Merge subset incident types in order to simplyfy
allruns <- merge(runs, incidenttypes, by="incident_type")
allruns$date <- parse_date_time(as.character(allruns$alarm_time), "%Y%m%d %H%M%S")
allruns$nresponse_time <- as.numeric(as.character(allruns$response_time))
allruns$dayofweek <- wday(allruns$date, label = TRUE)

#sink("runs.txt", append=FALSE, split=FALSE)
#Plot histograms of historic data
source("historicdata.R")
plotdata(allruns)
#end historic data
print("alldata")
describe(allruns$nresponse_time)
qts <- quantile(allruns$nresponse_time, probs=c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .90), na.rm = TRUE)
qts
gsummary <- as.data.frame(table(allruns$dayofweek, allruns$sub_description))
#as.data.frame.matrix will keep dataframe intact without frequency
gsubset <- subset(gsummary, Freq > 0, select=c(Var1, Var2, Freq))
bubbleplot <- ggplot(data = gsubset, aes(x=Var1, y=Var2, size=Freq)) +
  geom_point() + scale_size_continuous(range = c(3, 10)) + 
  ggtitle("2009 to 2013 Runs by Subgroup") + xlab("Number of Runs") + ylab("Type of Run")
ggsave(file="bubbleplot.png")


#rt <- as.numeric(as.character(allruns$response_time))

#Bad form
#allruns$nresponse_time <- as.numeric(as.character(allruns$response_time))
# timeranges <- table(cut(rt, breaks=c(0,1,2,3,4,5,6,7,8,9,10)))
# timeranges
#html output for documentation
#print(xtable(timeranges), type="html")
timeranges <- table(cut(allruns$nresponse_time, seq(from= 0, to = 38, by = 1 )))
timeranges
# #html output for documentation
# print(xtable(upperranges), type="html")
# 
# print(xtable(desc), type="html")


#qts <- quantile(rt, probs=c(.0, .90), na.rm = TRUE)
#hist(at)
#abline(v=qts[1],col="red") 
#abline(v=qts[2],col="red")
#p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#p + geom_vline(xintercept = 5)
#qts <- quantile(rt, probs=c(.0, .90), na.rm = TRUE)
#dotchart(wday(allruns$date),labels=row.names(allruns$description), cex=7, main= "Incident by day", xlab = "incident description")
#table(wday(allruns$date, label = TRUE), allruns$description)
#cast(allruns, wday(date) ~ description)
#ddplquantile(at, na.rm = TRUE)
#Build Heatmap
alarmhour <-hour(allruns$date)
dayofweek <- wday(allruns$date, label = TRUE)
prop.table(table(alarmhour))
dtbl <- table(alarmhour,dayofweek)

dimnames(dtbl) <- list(NULL , NULL)

#Plot a heatmap

days <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
hrs <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')

#simple plotly heatmap
#data <- list(
 # x = days,
#  y = hrs,
#  z = dtbl, type = 'heatmap')
 

#Fancy plot
scl <- brewer.pal(9,'YlOrBr')
data <- list(
  x = days,
  y = hrs,
  z = dtbl,
  scl= list(
    c(0,"rgb(128, 0, 38)"),
    c(0.125,"rgb(189, 0, 38)"),
    c(0.25,"rgb(227, 26, 28)"),
    c(0.375,"rgb(252, 78, 42)"),
    c(0.5,"rgb(253, 141, 60)"),
    c(0.625,"rgb(254, 178, 76)"),
    c(0.75,"rgb(254, 217, 118)"),
    c(0.875,"rgb(255, 237, 160)"),
    c(1,"rgb(255, 255, 204)")
  ),
  type = 'heatmap')
layout <- list(
  #  barmode = 'overlay', 
  # bargap = 0.25, 
  #  bargroupgap = 0.3, 
  # bardir = 'v', 
  title = 'Heatmap  Number of Runs per Day of Week',
  xaxis = list(title='Day of Week'),
  yaxis = list(title='Time of Day in 24hrs'))

p1 <- plotly(username="stevewhardin", key="xxx")

response1 <- p1$plotly(data, kwargs=list(layout=layout))

# url and filename
url1 <- response1$url
filename1 <- response1$filename1

url1
# Runtime allrun plot
rtplot <- list(
  x = allruns$nresponse_time,
  type = 'histogramx',
  name = 'control',
  marker = list(  
  #               color = 'fuchsia', 
   #               opacity = 0.75,
                  line = list(
                    color = 'grey',
                    width = 0)),
  autobinx = FALSE,
  xbins = list(
    start = 1,
    end = 38,
    size = 0.2,
    histnorm = 'count')) 

layout <- list(
#  barmode = 'overlay', 
 # bargap = 0.25, 
#  bargroupgap = 0.3, 
 # bardir = 'v', 
  title = 'Fire Response Times',
  xaxis = list(title='Response Time in Minutes'),
  yaxis = list(title='Count'))

p2<- plotly(username="stevewhardin", key="xxxx")
response2 <- p2$plotly(rtplot, kwargs=list(layout=layout))
url2 <- response2$url
filename2 <- response2$filename

url2
#sink()

