# OMPD data analysis
## Created 2024-06
## Lara Munro


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/OMPD/transformedData/"



# Part 1: Read YSI EXO file -----------------------------------------------

exoname <- paste(dataloc, "EXO/EXO_OMPD_2024-6-14-to-2024-6-21.csv", sep = "")
exodat <- read.csv(exoname)
head(exodat)

# Convert date and time columns to datetime
exodat$datetime <- as.POSIXct(paste(exodat$Date, exodat$Time), format = "%m/%d/%Y %H:%M:%S")


# Plot EXO data

# Conductivity
plot(exodat$datetime, exodat$Cond.uscm, type = "l", xlab = "date",
     ylab = "conductivity (uS/cm)")

# fDOM
plot(exodat$datetime, exodat$fDOM.RFU, type = "l", xlab = "date",
     ylab = "fDOM (RFU)")

# fDOM
plot(exodat$datetime, exodat$fDOM.QSU, type = "l", xlab = "date",
     ylab = "fDOM (QSU)")

# DO percent sat
plot(exodat$datetime, exodat$ODO.percSat, type = "l", xlab = "date",
     ylab = "DO (% saturation)")

# DO mg/l
plot(exodat$datetime, exodat$ODO.mgl, type = "l", xlab = "date",
     ylab = "DO (mg/l)")

# specific condctance
plot(exodat$datetime, exodat$SpCond.uscm, type = "l", xlab = "date",
     ylab = "Specific conductance (uS/cm)")

# turbidity
plot(exodat$datetime, exodat$Turbidity.FNU, type = "l", xlab = "date",
     ylab = "Turbidity (FNU)")

# pH
plot(exodat$datetime, exodat$pH, type = "l", xlab = "date",
     ylab = "pH")

# Temperature
plot(exodat$datetime, exodat$Temp.C, type = "l", xlab = "date",
     ylab = "Temperature (C)")

# Depth
plot(exodat$datetime, exodat$Depth.m, type = "l", xlab = "date",
     ylab = "Depth (m)")
