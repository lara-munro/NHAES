# OMPD data analysis
## Created 2024-06
## Lara Munro


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations

#dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/CLGB.AG/data/transformedData/"
dataloc <- "C:/Users/laram/OneDrive - USNH/OMPD/transformedData/"

# Part 1: Read YSI EXO file -----------------------------------------------

exoname <- paste(dataloc, "y2024/EXO/EXO_OMPD_2024-06-13-to-2024-12-04.csv", sep = "")
exodat <- read.csv(exoname)
head(exodat)

# Convert date and time columns to datetime
exodat$datetime <- as.POSIXct(paste(exodat$Date, exodat$Time), format = "%m/%d/%Y %H:%M:%S")
exodat$datetime <- round_date(exodat$datetime, "15 minutes")


# Plot EXO data

# Conductivity
plot(exodat$datetime, exodat$Cond.uscm, type = "l", xlab = "date",
     col = "goldenrod",
     ylab = "conductivity (uS/cm)")

# fDOM
plot(exodat$datetime, exodat$fDOM.RFU, type = "l", xlab = "date",
     ylab = "fDOM (RFU)")

# fDOM
plot(exodat$datetime, exodat$fDOM.QSU, type = "l", xlab = "date",
     col = "forestgreen",
     ylab = "fDOM (QSU)")

# DO percent sat
plot(exodat$datetime, exodat$ODO.perc, type = "l", xlab = "date",
     col = "orange",
     ylab = "DO (% saturation)")

# DO mg/l
plot(exodat$datetime, exodat$ODO.mgl, type = "l", xlab = "date",
     ylab = "DO (mg/l)")
#     ylim = c(0, 10))

# specific condctance
plot(exodat$datetime, exodat$SpCond.uscm, type = "l", xlab = "date",
     col = "goldenrod",
     ylab = "Specific conductance (uS/cm)")
#     ylim = c(200, 600))

# pH
plot(exodat$datetime, exodat$pH, type = "l", xlab = "date",
     col = "grey",
     ylab = "pH")
#     ylim = c(200, 600))

# Temperature
plot(exodat$datetime, exodat$Temp.C, type = "l", xlab = "date",
     col = "pink",
     ylab = "Temperature (C)")
#     ylim = c(10, 30))

# Depth
plot(exodat$datetime, exodat$Depth.m, type = "l", xlab = "date",
     ylab = "Depth (m)")

exodat <- subset(exodat, Turbidity.FNU<25)
# turbidity
plot(exodat$datetime, exodat$Turbidity.FNU, type = "l", xlab = "date",
     col = "brown",
     ylab = "Turbidity (FNU)")
#     ylim = c(0, 50))



# 2. SUNA -----------------------------------------------------------------

sunaloc <- paste0(ompdloc, "/transformedData/y2024/SUNA/SUNA_legible_FINAL2024.csv")
suna <- read.csv(sunaloc)
suna$DateTime <- as.POSIXct(suna$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
suna.ompd <- suna[c("DateTime", "NO3.mgL")]

plot(suna.ompd$DateTime, suna.ompd$NO3.mgL, type = "l",
     xlab = "Date",
     ylab = "Nitrate (mg/l)",
     col = "darkred",
     cex.lab = 1.5,
     cex.axis = 1.5
)


# Depth and water quality data --------------------------------------------

# temperature and discharge
par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(exodat$datetime, exodat$Temp.C, type = "l",
     xlab = "Date",
     ylab = "Temperature (C)",
     col = "pink",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(exodat$datetime, exodat$Depth.m, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Depth (m)")), side = 4, line = 3, cex = 1.5)


# Discharge ---------------------------------------------------------------

# Download data from USGS !!

disloc <- paste0(ompdloc, "transformedData/y2024/Oyster_USGS_2024-01-to-2025-01.csv")
ompdQ <- read.csv(disloc)

ompdQ$datetime <- as.POSIXct(ompdQ$DateTime, tz = "EST5EDT", format = "%Y-%m-%d %H:%M")

ompdQ$Q.m3s <- 0.028316847 * ompdQ$Q.cfs

plot(ompdQ$datetime, ompdQ$Q.m3s, type = "l")



# Merge all data from OMPD ------------------------------------------------

ompddat <- merge(ompdQ, suna.ompd, by.x = "datetime", by.y = "DateTime", all.x = TRUE)
ompddat <- merge(ompddat, exodat, by = "datetime", all.x = TRUE)

ompddat2 <- ompddat[ompddat$Q.m3s < 2, ]
ompddat2 <- ompddat2[ompddat$NO3.mgL > 0.005, ]
plot(ompddat2$Q.m3s, ompddat2$NO3.mgL, pch = 19, 
     log = "xy",
     cex.lab = 1.5,
     cex.axis = 1.5,
     ylab = "NO3-N (mg/l)",
     xlab = expression(paste("Discharge (m"^"3", "/s)")),
     main = "Nitrate concentrations as a function of discharge for the Mill Pond dam")


# OMPD storm event 2024 --------------------------------------------------

# October 14 2024 storm
ompddat3 <- ompddat[ompddat$datetime > "2024-10-14" & ompddat$datetime < "2024-10-18", ]

plot(ompddat3$datetime, ompddat3$Q.m3s, type = "l",
     log = "y",
     cex.lab = 1.5,
     cex.axis = 1.5,
     col = "darkslateblue",
     xlab = "Date",
     ylab = expression(paste("Discharge (m"^"3", "/s)")),
     main = "Discharge at the Mill Pond dam during a storm on October 14 2024")

plot(ompddat3$Q.m3s, ompddat3$NO3.mgL, pch = 19,
     log = "xy",
     cex.lab = 1.5,
     cex.axis = 1.5,
     ylab = "NO3-N (mg/l)",
     xlab = expression(paste("Discharge (m"^"3", "/s)")),
     main = "Nitrate concentrations as a function of discharge for the Mill Pond Dam on October 14 2024")


