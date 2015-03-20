# Trends
# Three Degrees of Freedom
rm(list=ls())

# Functions --------------------------------------------------------------------

# dayToIndices
#
# Convert a day in the data set into a day and week index coordinate.
#
# Accepts:
#   day - An integer day that represents the day of sampling.
#
# Returns:
#   day - The index for the day for a list of day matricies.
#   week- The week index for the day matrix.

dayToIndices <- function(day) {
    week <- ceiling(day/7)
    day <- day %% 7
    if (day == 0) {day = 7}
    return(c(day=day, week=week))
}

# Data preparation -------------------------------------------------------------
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

# Total traffic for each day ---------------------------------------------------
results <- matrix(0, nrow=32)
for (week in 1:32) {
    weekData <- numeric(7)
    for(wday in 1:7){
        # Divide by 12 because each element is vehicles/hour per 5 minute segment
        totalTraffic <- sum(matrices[[wday]][week,],na.rm=T)/12
        weekData[wday] <- totalTraffic
    }
    results[week,1] <- median(weekData, na.rm=T)
}

# Remove holidays
holidays <- matrix(0, ncol=2, nrow=2)
holidays[1,] <- c(10, results[10])
holidays[2,] <- c(7, results[7])
results[c(10, 7)] <- NA

# Plot
plot(results[1:nrow(results),],
     ylab="Median Daily Traffic",
     xlab="Week", ylim=c(3000, 6000))
points(holidays, pch="X")

# Regress regress! -
week <- 1:32
trafficVolume <- results[1:32,]
model <- lm(trafficVolume~week)
summary(model)
abline(model, col="red")

cat(paste("The traffic volume is growing at",model$coefficients[2],"vehicles per week."))

# Bin the delay each day  -----------------------------------------------------

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

        weeklyMatrix[week, minute] = weeklySubset$Delay[index]

    }
    template <- apply(weeklyMatrix, 2, median, na.rm=T)
    matrices <- c(matrices, list(weeklyMatrix))
    templates <- c(templates, list(template))
}


# Total traffic for each day ---------------------------------------------------
delayResults <- matrix(0, nrow=32)
for (week in 1:32) {
    weekData <- numeric(7)
    for(wday in 1:7){
        # Divide by 12 because each element is vehicles/hour per 5 minute segment
        totalTraffic <- median(matrices[[wday]][week,],na.rm=T)
        weekData[wday] <- totalTraffic
    }
    delayResults[week,1] <- median(weekData, na.rm=T)
}

# Remove holidays
holidays <- matrix(0, ncol=2, nrow=2)
holidays[1,] <- c(10, delayResults[10])
holidays[2,] <- c(7, delayResults[7])
delayResults[c(10, 7)] <- NA

# Plot
plot(delayResults[1:nrow(delayResults),],
     ylab="Median Traffic Delay (Seconds)",
     xlab="Week", ylim=c(65, 130))
points(holidays, pch="X")

# Regress regress! -------------------------------------------------------------
week <- 1:32
trafficDelay <- delayResults[1:32,]
model <- lm(trafficDelay~week)
abline(model, col="red")
summary(model)

cat(paste("The delay time is increasing at",model$coefficients[2],"seconds per week."))
