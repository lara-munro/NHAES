# CLGB.Ag EXO data 

# Author: Lara Munro
# Date: August 2025

# Script aggregates conductivity measurements from the HOBO data logger at CLGB.Ag into annual data tables
# Data files need to be converted into .CSVs in HOBOWare
# Loops are set up for data tables exported into EST (GMT -5.00) and for the logger withe the serial number: 20625421
# If the loops do not work, check time zones and logger serial number

# Set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"

# 2024 --------------------------------------------------------------------
exoname <- paste(dataloc, "CLGB.AG/data/fdom/y2024/intermediateData/EXO_CLGBag_2024-05-02-to-2024-10-04.csv", sep = "")
exodat <- read.csv(exoname)
head(exodat)
exodat$DateTime.EST <- as.POSIXct(paste(exodat$Date, exodat$Time), format = "%m/%d/%Y %H:%M:%S")
exodat$DateTime.EST <- round_date(exodat$DateTime.EST, "15 minutes")
exodat <- subset(exodat, select = c("DateTime.EST", "Site.Name",  "Pressure.psi", 
                                    "Cond.uScm", "SpCond.uScm", "Temp.C",
                                    "fDOM.QSU", 
                                    "ODO.perc", "ODO.mgL", 
                                    "Turbidity.FNU",
                                    "pH"))
exodat$serialNb <- "24C104113"


exoname <- paste(dataloc, "CLGB.AG/data/fdom/y2024/intermediateData/EXO_CLGBag_2024-10-04-to-2024-12-04.csv", sep = "")
exodat2 <- read.csv(exoname)
head(exodat2)
exodat2$DateTime.EST <- as.POSIXct(paste(exodat2$Date, exodat2$Time), format = "%m/%d/%Y %H:%M:%S")
exodat2$DateTime.EST <- round_date(exodat2$DateTime.EST, "15 minutes")
exodat2 <- subset(exodat2, select = c("DateTime.EST", "Site.Name",  "Pressure.psi", 
                                    "Cond.uScm", "SpCond.uScm", "Temp.C",
                                    "fDOM.QSU", 
                                    "ODO.perc", "ODO.mgL", 
                                    "Turbidity.FNU",
                                    "pH"))


exodat2$serialNb <- "13C100184"
exodat <- rbind(exodat, exodat2)

exodat$DateTime.EST <- as.character(format(exodat$DateTime.EST))
write.csv(cond, paste0(dataloc, "CLGB.AG/data/fdom/y2024/finalData/CLGBag_fdom_2024.csv"), row.names = FALSE)
