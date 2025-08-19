# CLGB.Ag plots for 2024
# Date: April 2025
# Author: Lara Munro


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"

# 1. Read in raw data -----------------------------------------------------

# Discharge
disloc <- paste0(dataloc, "CLGB.AG/data/stage/y2024/finalData/CLGB.AG_2024_DISCHARGE.csv")
dis <- read.csv(disloc)
dis$DateTime <- as.POSIXct(dis$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
dis2 <- dis[c("DateTime.EST", "Q.m3s", "Q.m3sQF")]

# Conductivity
condloc <- paste0(dataloc, "CLGB.AG/data/conductivity/y2024/finalData/CLGB.AG_CONDUCTIVITY_2024.csv")
cond <- read.csv(condloc)
cond$DateTime <- as.POSIXct(cond$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")

# SUNA
sunaloc <- paste0(dataloc, "CLGB.AG/data/no3/y2024/finalData/SUNA_legible_FINAL2024.csv")
suna <- read.csv(sunaloc)
suna$DateTime.EST <- as.POSIXct(suna$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
suna2 <- suna[c("DateTime.EST", "NO3.mgL")]

# EXO
exoloc <- paste0(dataloc, "CLGB.AG/data/fdom/y2024/finalData/CLGB.AG_EXO_2024.csv")
exo <- read.csv(exoloc)
exo$DateTime <- as.POSIXct(paste(exo$Date, exo$Time), format = "%m/%d/%Y %H:%M:%S")
exo$DateTime <- round_date(exo$DateTime, "15 minutes")

# Chemistry
chemloc <- paste0(dataloc, "CLGB.AG/data/lab/intermediateData/chem_data_2025_04_03.csv")
chemdat <- read.csv(chemloc)
chemdat$DateTime <- as.POSIXct(chemdat$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
clgbag.chem <- chemdat[which(chemdat$site == "CLGB.AG"),]

# Merge data

dat <- merge(dis2, suna2, by = "DateTime.EST", all.x = TRUE)
dat <- merge(dat, cond, by = "DateTime", all.x = TRUE)
dat <- merge(dat, exo, by = "DateTime", all.x = TRUE)
dat <- merge(dat, clgbag.chem, by = "DateTime", all.x = TRUE)

# Write csv with the data
dat2 <- dat
dat2$DateTime <- as.character(format(dat2$DateTime))

write.csv(dat2, paste0(dataloc, "CLGB.AG/data/aggregated data/CLGB.AG_allDATA_2024.csv"), row.names = FALSE)

dat$NO3.mgL.x[dat$NO3.mgL.x < 0] <- NA


# 2. Plot data ------------------------------------------------------------


# Chemistry vs sensors ----------------------------------------------------

# NO3 grab sample vs SUNA
plot(dat$NO3.mgL.y, dat$NO3.mgL.x, pch = 19, 
     xlab = "NO3 - grab sample (mg/L)", 
     ylab = "NO3 - sensor (mg/L)")
abline(a = 0, b = 1, lty = 2, col = "blue")

no3rel <- lm(dat$NO3.mgL.y ~ dat$NO3.mgL.x)
summary(no3rel)

# TSS vs turbidity
plot(dat$tss.mgL, dat$Turbidity.FNU, pch = 19,
     xlab = "TSS - grab sample (mg/L)",
     ylab = "Turbidity - EXO (FNU)",
     ylim = c(0, 60))
abline(a = 0, b = 1, lty = 2, col = "blue")
tssrel <- lm(dat$Turbidity.FNU ~ dat$tss.mgL)
summary(tssrel)


# fDOM vs DOC


# DO YSI vs EXO
plot(dat$DO.mgl, dat$ODO.mgL, pch = 19, 
     xlab = "DO - YSI (mg/L)",
     ylab = "DO - EXO (mg/L)")
abline(a = 0, b = 1, lty = 2, col = "blue")
dorel <- lm(dat$DO.mgl ~ dat$ODO.mgL)
summary(dorel)


plot(dat$DO.percent, dat$ODO.perc, pch = 19, 
     xlab = "DO - YSI (% saturation)",
     ylab = "DO - EXO (% saturation)")
abline(a = 0, b = 1, lty = 2, col = "blue")
dorel2 <- lm(dat$DO.percent ~ dat$ODO.perc)
summary(dorel2)

# Temperature YSI vs sensors & sensors vs sensors
plot(dat$temperature.C, dat$temp.C, pch = 19,
     xlab = "temperature - YSI (C)",
     ylab = "temperature - conductivity HOBO (C)")
abline(a = 0, b = 1, lty = 2, col = "blue")

plot(dat$temperature.C, dat$Temp.C, pch = 19,
     xlab = "temperature - YSI (C)",
     ylab = "temperature - EXO (C)")
abline(a = 0, b = 1, lty = 2, col = "blue")

plot(dat$temp.C, dat$Temp.C, pch = 19,
     xlab = "temperature - conductivity HOBO (C)",
     ylab = "temperature - EXO (C)")
abline(a = 0, b = 1, lty = 2, col = "blue")

# Conductivity YSI vs sensors & sensor vs sensor
plot(dat$conductivity.uscm, dat$cond.uScm, pch = 19,
     xlab = "conductivity - YSI (uS/cm)",
     ylab = "conductivity - conductivity HOBO (uS/cm)")
abline(a = 0, b = 1, lty = 2, col = "blue")

plot(dat$conductivity.uscm, dat$Cond.uScm, pch = 19,
     xlab = "conductivity - YSI (uS/cm)",
     ylab = "conductivity - EXO (uS/cm)")
abline(a = 0, b = 1, lty = 2, col = "blue")

plot(dat$cond.uScm, dat$Cond.uScm, pch = 19,
     xlab = "conductivity - conductivity HOBO (uS/cm)",
     ylab = "conductivity - EXO (uS/cm)")
abline(a = 0, b = 1, lty = 2, col = "blue")



# c-Q relationships/plots -------------------------------------------------

dat2 <- dat[-c(1:11178),]
dat2$NO3.mgL.x[dat2$Q.m3s < 0.0001] <- NA
dat2$Q.m3s[dat2$Q.m3s < 0.0001] <- NA

plot(dat2$Q.m3s, dat2$NO3.mgL.x, pch = 19,
     log = "xy",
     cex.lab = 1.5,
     cex.axis = 1.5,
     ylab = "NO3-N (mg/l)",
     xlab = expression(paste("Discharge (m"^"3", "/s)")),
     main = "Nitrate concentrations as a function of discharge for College Brook agriculture reach")

par(mar = c(4.5, 5, 2, 2))
plot(dat2$DateTime, dat2$Q.m3s, type = "l",
     log = "y",
     xlab = "Date",
     ylab = expression(paste("Discharge (m"^"3","/s)")),
     col = "darkslateblue",
     cex.lab = 1.5,
     cex.axis = 1.5,
     yaxt = "n"
)
axis(side = 2, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 


# temperature and discharge
par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(dat$DateTime, dat$temp.C, type = "l",
     xlab = "Date",
     ylab = "Temperature (C)",
     col = "pink",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dat$DateTime, dat$Q.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)

# Conductivity and discharge

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(dat$DateTime, dat$cond.uScm, type = "l",
     xlab = "Date",
     ylab = "Specific conductance (uS/cm)",
     col = "goldenrod",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dat$DateTime, dat$Q.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)

# Turbidity and discharge
par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(dat2$DateTime, dat2$Turbidity.FNU, type = "l",
     xlab = "Date",
     ylab = "Turbidity (FNU)",
     col = "brown",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dat2$DateTime, dat2$Q.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axTicks(side = 4)
axis(side = 4, at = c(0.01, 0.1, 1, 2), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)


# fDOM and discharge

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(dat2$DateTime, dat2$fDOM.QSU, type = "l",
     xlab = "Date",
     ylab = "fDOM (QSU)",
     col = "forestgreen",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dat2$DateTime, dat2$Q.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axTicks(side = 4)
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)

# Nitrate and discharge
par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(dat2$DateTime, dat2$NO3.mgL.x, type = "l",
     xlab = "Date",
     ylab = "Nitrate (mg/l)",
     col = "darkred",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dat2$DateTime, dat2$Q.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1, 2), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)


# DO and discharge
par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(dat2$DateTime, dat2$ODO.perc, type = "l",
     xlab = "Date",
     ylab = "Dissolved oxygen (%)",
     col = "orange",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dat2$DateTime, dat2$Q.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1, 2), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)


# pH and discharge
par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(dat2$DateTime, dat2$pH, type = "l",
     xlab = "Date",
     ylab = "pH",
     col = "grey",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dat2$DateTime, dat2$Q.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1, 2), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)



# Storm events ------------------------------------------------------------

# October 14 2024 storm
dat3 <- dat2[dat2$DateTime > "2024-10-14" & dat2$DateTime < "2024-10-16", ]

plot(dat3$DateTime, dat3$Q.m3s, type = "l",     
     log = "y",
     col = "darkslateblue",
     cex.lab = 1.5,
     cex.axis = 1.5,
     xlab = "Date",
     ylab = expression(paste("Discharge (m"^"3", "/s)")),
     main = "Discharge at the College Brook agriculture reach during a storm on October 14 2024",
     lwd = 2)

plot(dat3$Q.m3s, dat3$NO3.mgL.x, pch = 19,
     log = "xy",
     cex.lab = 1.5,
     cex.axis = 1.5,
     ylab = "NO3-N (mg/l)",
     xlab = expression(paste("Discharge (m"^"3", "/s)")),
     main = "Nitrate concentrations as a function of discharge for College Brook agriculture reach on October 14 2024")



