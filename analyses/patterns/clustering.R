rm(list=ls())
traffic <- read.csv("UrbanAnalytics2015.csv", sep=" ")
traffic$DateTime <- strptime(traffic$DateTime, "%m/%d/%y %H:%M")

bp <- boxplot(traffic[1:4])
# Remove Outliers
for (col in 1:4) {
  traffic[which(traffic[,col] >= bp$stats[5,col]),col] <- NA
}

# Time-based filtering ---------------------------------------------------------
stepsPerDay <- 1440/5
days <- 31
steps = days * stepsPerDay

dayList<-c("Sun.","Mon.","Tues.","Wed.","Thurs.","Fri.","Sat.")

# Bin the volume each day  -----------------------------------------------------

days <- traffic$DateTime[nrow(traffic)]$yday + 356 - traffic$DateTime[1]$yday
weeks = floor(days/7)
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
  matrices <- c(matrices, list(weeklyMatrix))
  templates <- c(templates, list(template))
}

# Data aggregation
allDays <- matrix(NA, ncol=288, nrow=32*7)
dayIndex = 1
for (week in 1:32) {
  for (wday in 1:7) {
    allDays[dayIndex,] <- matrices[[wday]][week,]
    dayIndex = dayIndex + 1
  }
}
# Examine only hourly rates
days <- allDays[,seq(1,288,2)]

cluster <- clara(days, k=3, samples=100)
plot(cluster)
matplot(t(days), type="l", col=cluster$clustering)

classificationMatrix <- matrix(cluster$clustering, ncol=7, nrow=32, byrow=T)
ggplot(melt(classificationMatrix), aes(Var1,Var2, fill=value)) +
  scale_fill_gradientn(colours=c("gray","orange","blue"),name="RMSD")+
  xlab("Day")+
  ylab("Day")+
  geom_raster() +
  theme( panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank())


require("cluster")
clusters<-c(2,4,2,2,2,2,5)
for (index in 1:7) {
  cluster <- clara(matrices[[index]],k=clusters[index])
  print(cluster$silinfo$avg.width)
  matplot(t(matrices[[index]]), type="l", col=cluster$clustering, lty=1)
}