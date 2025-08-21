# CLGB.Ag 2022 data

library(tidyverse)
library(ggplot2)

# Set data location
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# 1. Read barometric data -------------------------------------------------

# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
baroloc <- paste(dataloc, "WHB Barometric/annual/atm_Pressure2025.csv", sep = "")
barodat <- read.csv(baroloc)
barodat$DateTime <- as.POSIXct(barodat$DateTime, tz = "EST",
                           format = "%Y-%m-%d %H:%M")
barodat <- barodat %>%
  rename(DateTime.EST = DateTime,
         pres.baro.kpa = pres.kpa,
         temp.baro.C = temp.C)
barodat <- na.omit(barodat)

# 2. Read silver body HOBO ------------------------------------------------

# Read tables from 2025 for silver body HOBO
staloc <- paste0(dataloc, "CLGB.AG/data/stage/y2025/rawData/")
flist <- list.files(staloc, pattern = "*_silver.csv")
flist

# Read the first stage file
dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.water.C = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta$DateTime.EST <- as.POSIXct(sta$DateTime.EST, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)
sta$stageDevice <- "silver HOBO"
sta$stageSerialNb <- "1303192"

# Read the second stage file
dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.water.C = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                               format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "silver HOBO"
sta2$stageSerialNb <- "1303192"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Read the third stage file
dfloc <- paste0(staloc, flist[3])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.water.C = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "silver HOBO"
sta2$stageSerialNb <- "1303192"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Read the fourth stage file
dfloc <- paste0(staloc, flist[4])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.water.C = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "silver HOBO"
sta2$stageSerialNb <- "1303192"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)
sta <- sta[!duplicated(sta),]

# Merge baro and HOBO data
dat <- merge(barodat, sta, by = "DateTime.EST", all = FALSE)
dat <- dat[!duplicated(dat),]

# Convert pressure to depth --------------------------------------------

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0

# water pressure
dat$pres.diff.kpa <- dat$pres.water.kpa - dat$pres.baro.kpa


# Fluid density calculation (from Reese LeVea)
dat$density.kgm3 = (999.83952 + (16.945176 * dat$temp.water.C) - 
                        (7.9870401e-03 * dat$temp.water.C**2) - 
                        (46.170461e-06 * dat$temp.water.C**3) + 
                        (105.56302e-09 * dat$temp.water.C**4) - 
                        (280.54253e-12 * dat$temp.water.C**5)) /
  (1 + 16.879850e-03 * dat$temp.water.C) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
dat$density.lbft <- 0.0624279606 * dat$density.kgm3


# Convert pressure to density-dependent fluid depth array

dat$stage.raw.m = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * dat$pres.diff.kpa) / dat$density.lbft
dat$stage.raw.m[(dat$stage.raw.m<0.1)] <- NA
dat[sapply(dat, is.infinite)] <- NA
dat <- subset(dat, select = -(density.lbft))

# Remove data after change in barometric pressure
dat <- dat[which(dat$DateTime.EST <= as.POSIXct("2025-06-02 9:00", tz = "EST")),]

# Export data ----------------------------------------------------------
dat2 <- dat

dat$DateTime.EST <- as.character(format(dat$DateTime.EST))
#write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2025_waterDepths.csv"), row.names = F)
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/stage/y2022/finaldata/CLGB.AG_2025_waterDepths.csv"), row.names = F)
dat <- dat2


# Corrected stage -----------------------------------------------------------

offset2025A <- 0.067912854 # 2024-09-27 to 2025-06-03 Old atmospheric pressure
offsetchangedate <- as.POSIXct("2025-06-02 09:00")

offset2025B <- 0.021153303 # 2025-06-03 new barometer


for (i in 1:nrow(dat)){
  if (dat$DateTime.EST[i] < offsetchangedate){
    dat$stage.correction.factor.m[i] = offset2025A
  } else {
    dat$stage.correction.factor.m[i] = offset2025B
  }
}

dat$stage.corrected.m <- dat$stage.raw.m + dat$stage.correction.factor.m


# Discharge ---------------------------------------------------------------

for (i in 1:nrow(dat)){
  if(!is.na(dat$stage.corrected.m[i])){
    if(dat$stage.corrected.m[i] < 0.51){
      dat$Q.m3s[i] <- 0
      dat$Q.m3sQF[i] <- 0
    } else if (dat$stage.corrected.m[i] < 0.74){
      dat$Q.m3s[i] <- 10.037 * dat$stage.corrected.m [i]**16.081
      dat$Q.m3sQF[i] <- 0
    } else {
      dat$Q.m3s[i] <- 0.2281 * dat$stage.corrected.m [i]**4.2561
      if(dat$stage.corrected.m [i] < 0.91){
        dat$Q.m3sQF[i] <- 0
      } else {
        dat$Q.m3sQF[i] <- 1
      }
    }
  } else{
    dat$Q.m3s[i] <- NA
    dat$Q.m3sQF[i] <- 1
  }
}

plot(dat$DateTime.EST, dat$Q.m3s, type = "l",
     ylab = "Q (m3/s)", xlab = "Date 2025")
plot(dat$DateTime.EST, dat$Q.m3s, type = "l", log = "y",
     ylab = "Q (m3/s)", xlab = "Date 2025")

dat2 <- dat
dat$DateTime.EST <- as.character(format(dat$DateTime.EST))
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2025_DISCHARGE.csv"), row.names = F)
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/stage/y2025/finalData/CLGB.AG_2025_DISCHARGE.csv"), row.names = F)
dat <- dat2


# Import Q measurements ---------------------------------------------------
dat <- read.csv(paste0(dataloc,"/CLGB.AG/data/stage/y2025/finalData/CLGB.AG_2025_DISCHARGE.csv"))
dat$DateTime.EST <- as.POSIXct(dat$DateTime.EST, tz = "EST",
                               format = "%Y-%m-%d %H:%M:%S")

qmeas <- read.csv(paste0(dataloc, "CLGB.AG/data/ratingCurve/discharge_measurements.csv"))
qmeas$DateTime.EST <- as.POSIXct(paste(qmeas$date, qmeas$time), tz = "EST",
                                 format = "%Y-%m-%d %H:%M")
qmeas <- subset(qmeas, select =c("DateTime.EST", "measured.Q.m3s"))
dat <- merge(dat, qmeas, by = "DateTime.EST", all.x = TRUE, all.y = FALSE)

dat$Q.m3s[dat$Q.m3s < 1e-10] <- NA
ggplot(dat, aes(x=DateTime.EST)) +
  geom_line(aes(y = Q.m3s))+
  geom_point(aes(y = measured.Q.m3s, color = "measured Q"))+
  scale_x_datetime(name = "Date 2025") +
  scale_y_log10( name = "Discharge (m3/s)", limits = c(0.0001, 1))+
  theme_classic()+
  theme(legend.title = element_blank())
  


