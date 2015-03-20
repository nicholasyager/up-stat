# cor Pattern Analysis
# 2015-02-23

rm(list=ls())

# Data preparation -------------------------------------------------------------
traffic <- read.csv("UrbanAnalytics2015.csv", sep=" ")
traffic$DateTime <- strptime(traffic$DateTime, "%m/%d/%y %H:%M")

# Time-based filtering ---------------------------------------------------------
stepsPerDay <- 1440/5
days <- 31
steps = days * stepsPerDay

dayList<-c("Sun.","Mon.","Tues.","Wed.","Thurs.","Fri.","Sat.")

# cor for each day week to week -----------------------------------------------

days <- traffic$DateTime[nrow(traffic)]$yday + 356 - traffic$DateTime[1]$yday
weeks = floor(days/7)
cors <- matrix(0, nrow=weeks, ncol=7)
matrices <- list()
templates <- list()
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
  template <- apply(weeklyMatrix, 2, median, na.rm=T)
  for (week in 1:weeks) {
    cors[week, wday+1] <- cor(template,weeklyMatrix[week,], use="na.or.complete")
  }
  matrices <- c(matrices, list(weeklyMatrix))
  templates <- c(templates, list(template))
}

## Simple Plotting -------------------------------------------------------------
matplot(cors,type="l",
        lty=1,lwd=2,col=rainbow(7))
legend("topleft",legend=dayList,fill=rainbow(7),
       cex=0.75)

## Plotting --------------------------------------------------------------------

cors[6, 1] <- NA

require(ggplot2)
require(reshape2)
ggplot(melt(cors), aes(Var1,Var2, fill=value)) +
  scale_fill_gradientn(colours=c("orange","blue","white"))+
  xlab("Week")+
  ylab("Day")+
  geom_raster() +
  theme( panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank()) +
  scale_x_discrete(breaks = seq(1, 32, 1), labels = seq(1,32,1))+
  scale_y_discrete(breaks=c("1","2","3","4","5","6","7"), labels=dayList, limits=c(1,2,3,4,5,6,7))

## Plot the abnormailties ------------------------------------------------------

cor_table <- melt(cors)
indices <- order(cor_table$value, decreasing = F)

for ( index in indices[1:10]) {
    wday = cor_table$Var2[index]
    week = cor_table$Var1[index]
    titleString = paste(wday, week)
    matplot(cbind(templates[[wday]], t(matrices[[wday]])[,week]),type="l",lty=1,
            ylab="Traffic Volume (cars/hour)",xaxt="n", xlab="Hour",
            main=titleString)
    axis(1, seq(0,288,12), seq(0, 24, 1))
}

## Pairwise cor ---------------------------------------------------------------
firstDays <- 1:(7*32)
secondDays <- 1:(7*32)

dayToIndices <- function(day) {
  week <- ceiling(day/7)
  day <- day %% 7
  if (day == 0) {day = 7}
  return(c(day=day, week=week))
}
results <- matrix(NA, ncol=length(firstDays), nrow=length(firstDays))
for (firstDay in firstDays) {
  secondDays <- secondDays[-which(secondDays == firstDay)]
  print(length(secondDays))
  for (secondDay in secondDays){
    firstIndices <- dayToIndices(firstDay)
    secondIndices <- dayToIndices(secondDay)
    corValue <- cor(matrices[[firstIndices["day"]]][firstIndices["week"],],
                      matrices[[secondIndices["day"]]][secondIndices["week"],])
    results[firstDay, secondDay] <- corValue
    print(paste(firstDay, secondDay, corValue))
  }
}
ggplot(melt(results), aes(Var1,Var2, fill=value)) +
  scale_fill_gradientn(colours=c("white","orange","blue"),name="cor")+
  xlab("Day")+
  ylab("Day")+
  geom_raster() +
  theme( panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank())

sums <- rowSums(results,na.rm=T) + colSums(results,na.rm=T)
barplot(sums)

calendar <- matrix(sums, ncol=7, byrow=T)
ggplot(melt(calendar), aes(Var1,Var2, fill=value)) +
  scale_fill_gradientn(colours=rainbow(10), name="Sum of cors")+
  xlab("Week")+
  ylab("Day")+
  geom_raster() +
  theme( panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank()) +
  scale_x_discrete(breaks = seq(1, 31, 1), labels = seq(2,32,1))+
  scale_y_discrete(breaks=c("1","2","3","4","5","6","7"), labels=dayList, limits=c(1,2,3,4,5,6,7))
