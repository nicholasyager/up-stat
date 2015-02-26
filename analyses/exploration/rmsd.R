# RMSD Patter Analysis
# 2015-02-23


# Functions --------------------------------------------------------------------
rmsd <- function(vector1, vector2) {
  deviation = (vector1 - vector2)^2
  numNA = length(which(is.na(deviation)))
  return(sqrt(sum(deviation,na.rm=T)/(length(deviation)-numNA)))
}

# Data preparation -------------------------------------------------------------
traffic <- read.csv("UrbanAnalytics2015.csv", sep=" ")
traffic$DateTime <- strptime(traffic$DateTime, "%m/%d/%y %H:%M")

# Time-based filtering ---------------------------------------------------------
stepsPerDay <- 1440/5
days <- 31
steps = days * stepsPerDay

dayList<-c("Sun.","Mon.","Tues.","Wed.","Thurs.","Fri.","Sat.")

# RMSD for each day week to week -----------------------------------------------

## Make templates
templates <- list()
for (day in 1:7) {
  correction = correctionList[day]
  startIndex <- week*stepsPerDay + (day-1)*stepsPerDay
  endIndex   <- week*stepsPerDay + (day)*stepsPerDay -1
  series <- newTraffic$Delay[startIndex:endIndex]
  templates <- c(templates, list(series))
}



days <- traffic$DateTime[nrow(traffic)]$yday + 356 - traffic$DateTime[1]$yday
weeks = floor(days/7)
rmsds <- matrix(0, nrow=weeks-1, ncol=7)
for (wday in 0:6) {

  ## For each weekday, compile a matrix of every week's version and compare
  weeklySubset = traffic[which(traffic$DateTime$wday == wday),]
  weeklyMatrix <- matrix(NA,ncol=288, nrow=weeks)
  for (index in 1:nrow(weeklySubset)) {
    dayNumber = 1+  weeklySubset$DateTime[index]$yday + 356 *(weeklySubset$DateTime[index]$year - 113)  - weeklySubset$DateTime[1]$yday
    week = 1 + floor(dayNumber/7)
    minute = 1 + (weeklySubset$DateTime[index]$min + (60*weeklySubset$DateTime[index]$hour))/5

    weeklyMatrix[week, minute] = weeklySubset$Volume[index]
    print(paste(week, minute,weeklySubset$Volume[index]))

  }
  for (week in 1:(weeks-1)) {
    rmsds[week, wday+1] <- rmsd(weeklyMatrix[1,],weeklyMatrix[week + 1,])
  }
}

## Simple Plotting -------------------------------------------------------------
matplot(rmsds,type="l",
        lty=1,lwd=2,col=rainbow(7))
legend("topleft",legend=dayList,fill=rainbow(7),
       cex=0.75)

## Plotting --------------------------------------------------------------------

require(ggplot2)
require(reshape2)
ggplot(melt(rmsds), aes(Var1,Var2, fill=value)) +
  scale_fill_gradientn(colours=c("white","blue","orange"))+
  xlab("Week")+
  ylab("Day")+
  geom_raster() +
  theme( panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank()) +
  scale_x_discrete(breaks = seq(1, 31, 1), labels = seq(2,32,1))+
  scale_y_discrete(breaks=c("1","2","3","4","5","6","7"), labels=dayList, limits=c(1,2,3,4,5,6,7))


