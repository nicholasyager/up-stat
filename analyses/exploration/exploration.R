
# Data preparation -------------------------------------------------------------
traffic <- read.csv("UrbanAnalytics2015.csv", sep=" ")
traffic$DateTime <- strptime(traffic$DateTime, "%m/%d/%y %H:%M")

# Time-based filtering ---------------------------------------------------------
stepsPerDay <- 1440/5
days <- round(365/2)
steps = days * stepsPerDay

dayList<-c("Sun.","Mon.","Tues.","Wed.","Thurs.","Fri.","Sat.")

# Relationship between traffic flow volume and traffic stop volume -------------
plot(traffic$Volume[1:steps], traffic$Stops[1:steps])
model <- lm(traffic$Stops[1:steps]~traffic$Volume[1:steps])
summary(model)
abline(model, col='red')

# Relationship between traffic flow volume and delay ---------------------------
plot(traffic$Volume[1:steps], traffic$Delay[1:steps])
model <- lm(traffic$Delay[1:steps]~traffic$Volume[1:steps])
summary(model)
abline(model, col='red')

# Averages over a day
dailyInformation <- matrix(0, nrow=7, ncol=stepsPerDay-1)
dailyCounts <- matrix(0, nrow=7, ncol=stepsPerDay-1)
for (index in 1:nrow(traffic)) {
  wDay = 1 + traffic$DateTime[index]$wday
  minute = (traffic$DateTime[index]$min + (traffic$DateTime[index]$hour * 60)) / 5
  dailyInformation[wDay, minute] = dailyInformation[wDay, minute] + traffic$Volume[index]
  dailyCounts[wDay, minute] = dailyCounts[wDay, minute] + 1
}

dailyInformation = dailyInformation / dailyCounts
image(dailyInformation,col=rainbow(100))

require(ggplot2)
require(reshape2)

ggplot(melt(dailyInformation), aes(Var1,Var2, fill=value)) +
  scale_fill_gradientn(colours=rainbow(50))+
  xlab("Day of the Week")+
  ylab("Time (Hours)")+
  geom_raster() +
  theme( panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank()) +
  scale_y_discrete(breaks = seq(1, 288, 12), labels = seq(0,23,1)) +
  scale_x_discrete(breaks=c("1","2","3","4","5","6","7"), labels=dayList, limits=c(1,2,3,4,5,6,7))
