# Traffic Analysis
# 2015-02-19

# Here we can determine some of the characteristics of the traffic using real
# traffic terms or parameters.

# Read the data ----------------------------------------------------------------
traffic <- read.csv("UrbanAnalytics2015.csv", sep=" ")
traffic$DateTime <- strptime(traffic$DateTime, "%m/%d/%y %H:%M")

# Traffic Density --------------------------------------------------------------
# Density is defined as the number of vehicles per unit length of the roadway.
# The two important densities are Kc, the critical density, and Kj, the jam
# density. The maximum density achievable under free flow is Kc.

# Density is described as q/v, where q is flow rate (volume), and v is average
# velocity

density = traffic$Volume/traffic$Speed

# Trim out non-finite values
density[which(!is.finite(density))]=NA

plot(x=traffic$DateTime, y=density)

# Density and flow -------------------------------------------------------------
# There is a non-linear relationship between the flow vs the density.
plot(x=density, y=traffic$Volume, pch=19, cex=0.1,
     xlab="Density (Vehicles/Mile)", ylab="Flow (Vehicles/hour)")
