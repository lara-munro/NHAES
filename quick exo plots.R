# Part 1: Read YSI EXO file -----------------------------------------------
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/CLGB.AG/data/"

exoname <- paste(dataloc, "transformedData/y2024/EXO/EXO_CLGBag_2024-05-02-to-2024-10-04.csv", sep = "")
exodat <- read.csv(exoname)
head(exodat)

exoname <- paste(dataloc, "transformedData/y2024/EXO/EXO_CLGBag_2024-10-04-to-2024-12-04.csv", sep = "")
exodat2 <- read.csv(exoname)
head(exodat2)

exodat <- rbind(exodat, exodat2)

write.csv(exodat, file = paste0(dataloc, "transformedData/y2024/EXO/CLGB.AG_EXO_2024.csv"), row.names = F)

# Convert date and time columns to datetime
exodat$datetime <- as.POSIXct(paste(exodat$Date, exodat$Time), format = "%m/%d/%Y %H:%M:%S")


# Plot EXO data

# Conductivity
plot(exodat$datetime, exodat$Cond.uScm, type = "l", xlab = "date",
     ylab = "conductivity (uS/cm)")

# fDOM
plot(exodat$datetime, exodat$fDOM.RFU, type = "l", xlab = "date",
     ylab = "fDOM (RFU)")

# fDOM
plot(exodat$datetime, exodat$fDOM.QSU, type = "l", xlab = "date",
     ylab = "fDOM (QSU)")

# DO percent sat
plot(exodat$datetime, exodat$ODO.perc, type = "l", xlab = "date",
     ylab = "DO (% saturation)")

# DO mg/l
plot(exodat$datetime, exodat$ODO.mgL, type = "l", xlab = "date",
     ylab = "DO (mg/l)")

# specific condctance
plot(exodat$datetime, exodat$SpCond.uScm, type = "l", xlab = "date",
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
