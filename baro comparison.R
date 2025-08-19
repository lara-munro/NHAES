# Compare barmetric pressure from WHB and CLGB
# Author: Lara Munro
# Date: August 2025

# Script aggregates and compares barometric pressure readings from the barometer at Wednesday Hill Brook (maintained by Jody Potter in WQAL) 
# and the barometer deployed at CLGB.Ag. 
# The goal of this script is to determine whether there is an offset between the instruments and locations

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data location
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"

partbaroloc <- paste(dataloc, "WHB Barometric/", sep = "")
baro2025 <- read.csv(paste(partbaroloc, "annual/WHB_atm_Pressure2025.csv", sep = ""))
baro2025$DateTime <- as.POSIXct(baro2025$DateTime, tz = "EST",
                                format = "%Y-%m-%d %H:%M")
baro2025 <- na.omit(baro2025)

part5A <- read.csv(paste0(partbaroloc, "baro_CLGB_2025_05-to_2025_07_14.csv"), skip = 1)
head(part5A)
part5A <- part5A[1:4]
part5A <- part5A %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.clgb.kpa = Abs.Pres..kPa..LGR.S.N..22254013..SEN.S.N..22254013.,
         temp.clgb.C = Temp...C..LGR.S.N..22254013..SEN.S.N..22254013.)
part5A$DateTime <- as.POSIXct(part5A$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part5A <- part5A[3:5]


part5B <- read.csv(paste0(partbaroloc, "WHB_B_20250602to20250709.csv"), skip = 1)
head(part5B)
part5B <- part5B[1:4]
part5B <- part5B %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.whb.kpa = Abs.Pres..kPa..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.whb.C = Temp...C..LGR.S.N..10012043..SEN.S.N..10012043.)
part5B$DateTime <- as.POSIXct(part5B$datetime, tz = "EST",
                              format = "%m/%d/%y %I:%M:%S %p")
part5B <- part5B[3:5]

barocomp <- merge(part5A, part5B, by = "DateTime")

barocomp$diff <- barocomp$pres.whb.kpa - barocomp$pres.clgb.kpa

plot(barocomp$DateTime, barocomp$diff, type = "l", 
     xlab = "Date 2025",
     ylab = "Difference in barometric pressure (kPa)")

barocomp$diff.perc <- barocomp$pres.clgb.kpa*100/barocomp$pres.whb.kpa

plot(barocomp$DateTime, barocomp$diff.perc, type = "l", 
     xlab = "Date 2025",
     ylab = "Difference in barometric pressure (%)")


barocomp$density.kgm3 = (999.83952 + (16.945176 * barocomp$temp.clgb.C) - 
                      (7.9870401e-03 * barocomp$temp.clgb.C**2) - 
                      (46.170461e-06 * barocomp$temp.clgb.C**3) + 
                      (105.56302e-09 * barocomp$temp.clgb.C**4) - 
                      (280.54253e-12 * barocomp$temp.clgb.C**5)) /
  (1 + 16.879850e-03 * barocomp$temp.clgb.C) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
barocomp$density.lbft <- 0.0624279606 * barocomp$density.kgm3


# Convert pressure to density-dependent fluid depth array

barocomp$stagediff.m = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * barocomp$diff) / barocomp$density.lbft

barocomp[sapply(barocomp, is.infinite)] <- NA

plot(barocomp$DateTime, (barocomp$stagediff.m*100), type = "l",
     xlab = "Date",
     ylab = "Equivalent stage difference (cm)")

ggplot(barocomp, aes(x = DateTime))+
  geom_line(aes(y = pres.whb.kpa, col = "WHB"))+
  geom_line(aes(y = pres.clgb.kpa, col = "CLGB"))+
  ylab("Barometric pressure (kPa)")+
  xlab("Date")+
  theme_classic()


