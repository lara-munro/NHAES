# Aggregate CLGB.Ag data for 2022-2023
# Author: Lara Munro
# Date: April 2025

# Script aggregates all "final data" into a single data table
# Some data for 2022 and 2023 were aggregated into a single "final data" file in early 2024, this script uses those files from the y2023 folders


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"

# 1. Read in raw data -----------------------------------------------------

# Discharge
disloc <- paste0(dataloc, "CLGB.AG/data/stage/y2022/finalData/CLGB.AG_2022_DISCHARGE.csv")
dis <- read.csv(disloc)
dis$DateTime <- as.POSIXct(dis$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
dis2 <- dis[c("DateTime", "Q.m3s", "Q.m3sQF")]

disloc <- paste0(dataloc, "CLGB.AG/data/stage/y2023/finalData/CLGB.AG_2023_DISCHARGE.csv")
dis <- read.csv(disloc)
dis$DateTime <- as.POSIXct(dis$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
dis3 <- dis[c("DateTime", "Q.m3s", "Q.m3sQF")]

dat <- rbind(dis2, dis3)

# Conductivity
condloc <- paste0(dataloc, "CLGB.AG/data/conductivity/y2023/finalData/CLGBag_COND_2022-2023.csv")
cond <- read.csv(condloc)
cond$DateTime <- as.POSIXct(cond$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")

# SUNA
sunaloc <- paste0(dataloc, "CLGB.AG/data/no3/y2023/finalData/CLGBag_SUNA_2022-2023.csv")
suna <- read.csv(sunaloc)
suna$DateTime <- as.POSIXct(suna$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
suna2 <- suna[c("DateTime", "no3.mgl")]

# Turner (fdom)
fdomloc <- paste0(dataloc, "CLGB.AG/data/fdom/y2023/finalData/CLGBag_Turner_2022-2023.csv")
fdom <- read.csv(fdomloc)
fdom$DateTime <- as.POSIXct(fdom$DateTime, format = "%Y-%m-%d %H:%M:%S")
fdom2 <- fdom[c("DateTime", "Turbidity.NTU", "CDOM.ppb", "Temp.C"),]

# Chemistry
chemloc <- paste0(dataloc, "CLGB.AG/data/lab/intermediateData/chem_data_2025_04_10.csv")
chemdat <- read.csv(chemloc)
chemdat$DateTime <- as.POSIXct(chemdat$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
clgbag.chem <- chemdat[which(chemdat$site == "CLGB.AG"),]

# Merge data

dat <- merge(dis2, suna2, by = "DateTime", all.x = TRUE)
dat <- merge(dat, cond, by = "DateTime", all.x = TRUE)
dat <- merge(dat, fdom2, by = "DateTime", all.x = TRUE)
dat <- merge(dat, clgbag.chem, by = "DateTime", all.x = TRUE)

# Write csv with the data
dat2 <- dat
dat2$DateTime <- as.character(format(dat2$DateTime))

write.csv(dat2, paste0(dataloc, "CLGB.AG/data/aggregated data/CLGB.AG_allDATA_2022-2023.csv"), row.names = FALSE)

dat$NO3.mgL.x[dat$NO3.mgL.x < 0] <- NA

