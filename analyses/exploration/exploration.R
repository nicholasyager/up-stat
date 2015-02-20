
# Data preparation -------------------------------------------------------------
traffic <- read.csv("UrbanAnalytics2015.csv", sep=" ")
traffic$DateTime <- strptime(traffic$DateTime, "%m/%d/%y %H:%M")

# Time-based filtering ---------------------------------------------------------
stepsPerDay <- 1440/5
days <- 31
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

# FFT Analysis------------------------------------------------------------------
# Perform a fast foruier transformation of the data to locate prominant frequencies
stepsPerDay <- 1440/5
days <- 1
steps = days * stepsPerDay
subset <- traffic[1:steps,]
d.Delay <- diff(traffic$Delay)[1:steps]
plot(x=traffic$DateTime[1:steps], y=d.Delay,type="l")

spectrum(d.Delay)
dft <- fft(d.Delay)
m <- floor(steps/2) # Number of frequencies
dft <- dft[2:(m+1)]
f <- (2*pi/steps)*(1:m) # vector of m Fourier frequencies
y.p <- (1/(2*pi*steps))*(Mod(dft))^2
plot(x=f, y=y.p,type="l")

# Delay over time --------------------------------------------------------------
par(pch=19, col="black")
stepsPerDay <- 1440/5
days <- 7
steps = days * stepsPerDay
delay <- traffic$Delay[1:steps]
date  <- as.numeric(traffic$DateTime[1:steps]) - min(as.numeric(traffic$DateTime[1:steps]))

plot(x=date, y=delay,
     ylab="Average Wait Time (minutes)",
     col="black", type="l")

xc <- cos(2*pi*date/date[stepsPerDay])
xs <- sin(2*pi*date/date[stepsPerDay])
fit.lm <- lm(delay~xc+xs+xc2+xs2)
summary(fit.lm)
pred <- predict(fit.lm, newdata=data.frame(date=date))
lines(date,pred, col="blue")


# Factor analysis --------------------------------------------------------------
# Let's identify the factors affecting the data.
# Let's find how many factors to extract

#install.packages("nFactors")
library(nFactors)

ev <- eigen(cor(traffic[,1:4])) # Get eigenvectors of the correlation matrix
ap <- parallel(subject=nrow(traffic), var=ncol(traffic),
               rep=100, cent=0.05)
nS <- nScree(x=ev$values, ap$eigen$qevpea)
plotnScree(nS)

# This will perform maximum likelihood factor analysis
fit <- factanal(traffic[,1:4], 1, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- fit$loadings[,1]
plot(load,type="n") # set up plot
text(load,labels=names(traffic[,1:4]),cex=.7) # add variable names

# Principal Component Analysis -------------------------------------------------
# Pricipal Components Analysis
# entering raw data and extracting PCs
# from the correlation matrix
fit <- princomp(traffic[,1:4], cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)
