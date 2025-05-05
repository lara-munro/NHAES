# CLGB.Ag depth aggregation
# Aggregate pressure and depth data for CLGB.Ag from 2 HOBOs and 1 YSI EXO
# Done for 2024 because of absent HOBOs for part of the year and switch between 2 HOBOs and stilling wells

# Creator: Lara Munro
# Date: December 2 2024


# Load libraries
library(tidyverse)

# Parts: 
# 1. Read barometric data
# 2. Read silver body HOBO 
# 3. Read black body HOBO
# 4. Read EXO
# 5. Convert pressure to depth
# 6. Calculate depth offsets
# 7. Export data

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# 1. Read barometric data -------------------------------------------------

# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
baroloc <- paste(dataloc, "WHB Barometric/annual/WHB_atm_Pressure2024.csv", sep = "")
dat <- read.csv(baroloc)
dat$DateTime <- as.POSIXct(dat$DateTime, tz = "EST",
                                format = "%Y-%m-%d %H:%M")
dat <- dat %>%
  rename(pres.kpa.baro = pres.kpa,
         temp.C.baro = temp.C)
dat <- na.omit(dat)

# 2. Read silver body HOBO ------------------------------------------------

# Read tables from 2024 for silver body HOBO
staloc <- paste0(dataloc, "CLGB.AG/data/rawData/y2024/Stage/")
flist <- list.files(staloc, pattern = "*_silver.csv")
flist

# Read the first stage file
dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.psi.silver = Abs.Pres..psi..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.C.silver = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta$DateTime <- as.POSIXct(sta$DateTime, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)

# Read the second stage file
dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.psi.silver = Abs.Pres..psi..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.C.silver = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

# Combine black body data into single data frame
sta <- rbind(sta, sta2)

# Read the third stage file
dfloc <- paste0(staloc, flist[3])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.psi.silver = Abs.Pres..psi..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.C.silver = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

# Combine black body data into single data frame
sta <- rbind(sta, sta2)

# Merge baro and silver HOBO data
dat <- merge(dat, sta, by = "DateTime", all = TRUE)

# 3. Read black body HOBO -------------------------------------------------

# Read tables from 2024 for black body HOBO
staloc <- paste0(dataloc, "CLGB.AG/data/rawData/y2024/Stage/")
flist <- list.files(staloc, pattern = "*_black.csv")
flist

dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa.black = Abs.Pres..kPa..LGR.S.N..22063078..SEN.S.N..22063078.,
         temp.C.black = Temp...C..LGR.S.N..22063078..SEN.S.N..22063078.)
sta$DateTime <- as.POSIXct(sta$DateTime, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)

# read second black body file
dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa.black = Abs.Pres..kPa..LGR.S.N..22063078_duplicate..SEN.S.N..22063078.,
         temp.C.black = Temp...C..LGR.S.N..22063078_duplicate..SEN.S.N..22063078.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

# Combine black body data into single data frame
sta <- rbind(sta, sta2)

# read third black body file
dfloc <- paste0(staloc, flist[3])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa.black = Abs.Pres..kPa..LGR.S.N..22063078..SEN.S.N..22063078.,
         temp.C.black = Temp...C..LGR.S.N..22063078..SEN.S.N..22063078.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

# Combine black body data into single data frame
sta <- rbind(sta, sta2)

# read fourth black body file
dfloc <- paste0(staloc, flist[4])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa.black = Abs.Pres..kPa..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.C.black = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

# Combine black body data into single data frame
sta <- rbind(sta, sta2)

# Remove duplicated rows
sta <- sta[!duplicated(sta),]

# Merge baro/silver HOBO and black HOBO data
dat <- merge(dat, sta, by = "DateTime", all = TRUE)

# 4. Read EXO  ------------------------------------------------------------

exoname <- paste(dataloc, "CLGB.AG/data/transformedData/y2024/EXO/EXO_CLGBag_2024-05-02-to-2024-09-25.csv", sep = "")
exodat <- read.csv(exoname)

# Convert date and time columns to datetime
exodat$DateTime <- as.POSIXct(paste(exodat$Date, exodat$Time), format = "%m/%d/%Y %H:%M:%S")

head(exodat)
exodat <- exodat[c("DateTime", "Pressure.psi", "Temp.C")]


exoname <- paste(dataloc, "CLGB.AG/data/transformedData/y2024/EXO/EXO_CLGBag_2024-10-04-to-2024-12-04.csv", sep = "")
exodat2 <- read.csv(exoname)
exodat2$DateTime <- as.POSIXct(paste(exodat2$Date, exodat2$Time), format = "%m/%d/%Y %H:%M:%S")
head(exodat2)
exodat2 <- exodat2[c("DateTime", "Pressure.psi", "Temp.C")]

# Combine all EXO data into single data frame
exodat <- rbind(exodat, exodat2)

# Remove duplicated rows
exodat <- exodat[!duplicated(exodat),]

# Round datetime to nearest 15 minutes
exodat$DateTime <- round_date(exodat$DateTime, "15 mins")

exodat <- exodat %>%
  rename(pres.psi.exo = Pressure.psi,
         temp.C.exo = Temp.C)

# Merge baro/silver/black HOBO and EXO data
dat <- merge(dat, exodat, by = "DateTime", all = TRUE)


# 5. Convert pressure to depth --------------------------------------------

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0


# Convert psi to kPa (where needed)
dat$pres.kpa.silver <- dat$pres.psi.silver/KPA_TO_PSI
dat$pres.kpa.exo <- dat$pres.psi.exo/KPA_TO_PSI

# water pressure
dat$watPres.kPa.silver <- dat$pres.kpa.silver - dat$pres.kpa.baro
dat$watPres.kPa.black <- dat$pres.kpa.black - dat$pres.kpa.baro
dat$watPres.kPa.exo <- dat$pres.kpa.exo - dat$pres.kpa.baro


# Fluid density calculation (from Reese LeVea)
dat$density.silver = (999.83952 + (16.945176 * dat$temp.C.silver) - 
                   (7.9870401e-03 * dat$temp.C.silver**2) - 
                   (46.170461e-06 * dat$temp.C.silver**3) + 
                   (105.56302e-09 * dat$temp.C.silver**4) - 
                   (280.54253e-12 * dat$temp.C.silver**5)) /
  (1 + 16.879850e-03 * dat$temp.C.silver) # density in kg/m3

dat$density.black = (999.83952 + (16.945176 * dat$temp.C.black) - 
                        (7.9870401e-03 * dat$temp.C.black**2) - 
                        (46.170461e-06 * dat$temp.C.black**3) + 
                        (105.56302e-09 * dat$temp.C.black**4) - 
                        (280.54253e-12 * dat$temp.C.black**5)) /
  (1 + 16.879850e-03 * dat$temp.C.black) # density in kg/m3

dat$density.exo = (999.83952 + (16.945176 * dat$temp.C.exo) - 
                        (7.9870401e-03 * dat$temp.C.exo**2) - 
                        (46.170461e-06 * dat$temp.C.exo**3) + 
                        (105.56302e-09 * dat$temp.C.exo**4) - 
                        (280.54253e-12 * dat$temp.C.exo**5)) /
  (1 + 16.879850e-03 * dat$temp.C.exo) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
dat$density.lbft.silver <- 0.0624279606 * dat$density.silver
dat$density.lbft.black <- 0.0624279606 * dat$density.black
dat$density.lbft.exo <- 0.0624279606 * dat$density.exo


# Convert pressure to density-dependent fluid depth array

dat$depth.m.silver = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * dat$watPres.kPa.silver) / dat$density.lbft.silver
dat$depth.m.black = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * dat$watPres.kPa.black) / dat$density.lbft.black
dat$depth.m.exo = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * dat$watPres.kPa.exo) / dat$density.lbft.exo

dat[sapply(dat, is.infinite)] <- NA

# 6. Calculate depth offsets ----------------------------------------------

# Offsets from each sensor to visual stage (get from rating curve file)
vis.silver = 0.067988753 # offset from the silver HOBO changes from 2022 to 2024. This offset is only from 2024
vis.black = 0.217437035
vis.exo = 0.318362188

# Remove exo depths from when exo was out of the water

dat$depth.m.exo[dat$depth.m.exo < 0.1] <- NA

dat$depth.silver.standard.m <- dat$depth.m.silver + vis.silver
dat$depth.black.standard.m <- dat$depth.m.black + vis.black
dat$depth.exo.standard.m <- dat$depth.m.exo + vis.exo


# 7. Export data ----------------------------------------------------------

write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2024_waterDepths_FINAL.csv"), row.names = F)



# 8. Make plots -----------------------------------------------------------

plot(dat$DateTime, dat$depth.m.black)
fart <- lm(dat$depth.m.exo ~ dat$depth.m.black)
plot(dat$depth.m.black, dat$depth.m.exo, pch = 19, ylim = c(-0.1, 0.5),
     ylab = "EXO depth (m)",
     xlab = "black body HOBO depth (m)")
abline(a = fart$coefficients[1], b = fart$coefficients[2], col = "red", lwd = 3)

fart <- lm(dat$pres.kpa.black ~ dat$pres.kpa.silver)
plot(dat$pres.kpa.silver, dat$pres.kpa.black, pch = 19, 
     ylab = "black body HOBO pressure (kPa)",
     xlab = "silver body pressure (kPa)",
     ylim = c(104, 106))
abline(a = fart$coefficients[1], b = fart$coefficients[2], col = "red", lwd = 3)

fart <- lm(dat$pres.kpa.exo ~ dat$pres.kpa.black)
plot(dat$pres.kpa.black, dat$pres.kpa.exo, pch = 19, 
     ylab = "EXO pressure (kPa)",
     xlab = "black body pressure (kPa)",
     ylim = c(100, 107))
abline(a = fart$coefficients[1], b = fart$coefficients[2], col = "red")


plot(dat$DateTime, dat$depth.m.black, type = "l")

library(ggplot2)
ggplot(data = dat, aes(x = DateTime))+
  geom_line(aes(y = depth.m.black), color = "black")+
  geom_line(aes(y = depth.m.silver), color = "grey")+
  geom_line(aes(y = depth.m.exo), color = "blue")+
  theme_classic()+
  #ylim(0, 0.75)+
  scale_x_datetime(limits = c(as.POSIXct("2024-05-01"), as.POSIXct("2024-12-01")))+
  scale_y_log10()+
  ylab("Depth (m)")


ggplot(data = dat, aes(x = DateTime))+
  geom_line(aes(y = depth.black.standard.m, color = "Black HOBO"))+
  geom_line(aes(y = depth.silver.standard.m, color = "Silver HOBO"))+
  geom_line(aes(y = depth.exo.standard.m, color = "EXO"))+
  theme_classic()+
  scale_x_datetime(limits = c(as.POSIXct("2024-05-01"), as.POSIXct("2024-12-01")))+
 # scale_y_log10()+
  ylab("Standardized depth (m)")+
  scale_color_manual(values = c("black", "blue","grey" ))+
  labs(color = "Sensor")


# 9. Calculate discharge --------------------------------------------------

for (i in 1:nrow(dat)){
  if(!is.na(dat$depth.black.standard.m[i])){
    if(dat$depth.black.standard.m[i] < 0.74){
      dat$Q.black.m3s[i] <- 4.0443 * dat$depth.black.standard.m[i]**14.187
    } else {
      dat$Q.black.m3s[i] <- 0.1723 * dat$depth.black.standard.m[i]**3.1365
    }
  } else{
    dat$Q.black.m3s[i] <- NA
  }
  if(!is.na(dat$depth.silver.standard.m[i])){
    if(dat$depth.silver.standard.m[i] < 0.74){
      dat$Q.silver.m3s[i] <- 4.0443 * dat$depth.silver.standard.m[i]^14.187
    } else{
      dat$Q.silver.m3s[i] <- 0.1723 * dat$depth.silver.standard.m[i]^3.1365
    }
  } else {
    dat$Q.silver.m3s[i] <- NA
  }
  
  if(!is.na(dat$depth.exo.standard.m[i])){
    if(dat$depth.exo.standard.m[i] < 0.74){
      dat$Q.exo.m3s[i] <- 4.0443 * dat$depth.exo.standard.m[i]^14.187
    } else {
      dat$Q.exo.m3s[i] <- 0.1723 * dat$depth.exo.standard.m[i]^3.1365
    }
  } else {
    dat$Q.exo.m3s[i] <- NA
  }
  
}

for (i in 1:nrow(dat)){
  if (dat$depth.black.standard.m[i] > 0.95 | is.na(dat$depth.black.standard.m[i])){
    dat$Q.black.m3s.QF[i] <- 1
  } else {
    dat$Q.black.m3s.QF[i] <- 0
  }
  
  if (dat$depth.silver.standard.m[i] > 0.95  | is.na(dat$depth.silver.standard.m[i])){
    dat$Q.silver.m3s.QF[i] <- 1
  } else {
    dat$Q.silver.m3s.QF[i] <- 0
  }
  
  if (dat$depth.exo.standard.m[i] > 0.95  | is.na(dat$depth.exo.standard.m[i])){
    dat$Q.exo.m3s.QF[i] <- 1
  } else {
    dat$Q.exo.m3s.QF[i] <- 0
  }
}

# Choose the best Q value to include (silver > black > exo)

for (i in 1:nrow(dat)){
  if (!is.na(dat$Q.silver.m3s[i])){
    dat$Q.m3s[i] <- dat$Q.silver.m3s[i]
    dat$Q.m3s.QF[i] <- dat$Q.silver.m3s.QF[i]
    dat$Q.source[i] <- "silver HOBO"
  } else if (!is.na(dat$Q.black.m3s[i])){
    dat$Q.m3s[i] <- dat$Q.black.m3s[i]
    dat$Q.m3s.QF[i] <- dat$Q.black.m3s.QF[i] 
    dat$Q.source[i] <- "black HOBO"
  } else {
    dat$Q.m3s[i] <- dat$Q.exo.m3s[i]
    dat$Q.m3s.QF[i] <- dat$Q.exo.m3s.QF[i] 
    dat$Q.source[i] <- "EXO"
  }
  
}

plot(dat$DateTime, dat$Q.m3s, type = "l", log = "y")

dat2 <- dat[-c(1:11178),]

plot(dat2$DateTime, dat2$Q.m3s, type = "l", log = "y",
     ylab = expression(paste("Discharge (m"^"3","/s)")),
     xlab = "date")

dat2 <- dat

dat$DateTime <- as.character(format(dat$DateTime))
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2024_DISCHARGE.csv"), row.names = F)


# 9.1 Calculate discharge for 2022 ----------------------------------------

dat2022 <- read.csv(paste0(dataloc, "CLGB.AG/data/ratingCurve/CLGB.AG_2022_waterDepths.csv"))
dat2022$DateTime <- as.POSIXct(dat2022$DateTime, tz = "EST",
                           format = "%Y-%m-%d %H:%M")
offset2022 <- -0.034760685

dat2022$depth.standard.m <- dat2022$depth.m.new + offset2022

for (i in 1:nrow(dat2022)){
  if(!is.na(dat2022$depth.standard.m[i])){
    if(dat2022$depth.standard.m[i] < 0.74){
      dat2022$Q.m3s[i] <- 4.0443 * dat2022$depth.standard.m[i]**14.187
      dat2022$Q.m3sQF[i] <- 0
    } else {
      dat2022$Q.m3s[i] <- 0.1723 * dat2022$depth.standard.m[i]**3.1365
      if(dat2022$depth.standard.m[i] < 0.95){
        dat2022$Q.m3sQF[i] <- 0
      } else {
        dat2022$Q.m3sQF[i] <- 1
      }
    }
  } else{
    dat2022$Q.m3s[i] <- NA
    dat2022$Q.m3sQF[i] <- 1
  }
}

dat2022$DateTime <- as.character(format(dat2022$DateTime))
write.csv(dat2022, paste0(dataloc,"/CLGB.AG/data/stage/y2022/finalData/CLGB.AG_2022_DISCHARGE.csv"), row.names = F)


# 9.2 calculate discharge for 2023 ----------------------------------------

dat2023 <- read.csv(paste0(dataloc, "CLGB.AG/data/ratingCurve/CLGB.AG_2023_waterDepths.csv"))
dat2023$DateTime <- as.POSIXct(dat2023$DateTime, tz = "EST",
                               format = "%Y-%m-%d %H:%M")
offset2023A <- -0.034760685 # 2022 to 2023-06-01
offset2023B <- -0.077386304 # 2023-06-01 10:45 to 2023-10-06

offsetchangedate <- as.POSIXct("2023-06-01 10:45")

for (i in 1:nrow(dat2023)){
  if (dat2023$DateTime[i] < offsetchangedate){
    dat2023$depth.standard.m[i] <- dat2023$depth.m[i] + offset2023A
  } else{
    dat2023$depth.standard.m[i] <- dat2023$depth.m[i] + offset2023B
  }
}


for (i in 1:nrow(dat2023)){
  if(!is.na(dat2023$depth.standard.m[i])){
    if(dat2023$depth.standard.m[i] < 0.74){
      dat2023$Q.m3s[i] <- 4.0443 * dat2023$depth.standard.m[i]**14.187
      dat2023$Q.m3sQF[i] <- 0
    } else {
      dat2023$Q.m3s[i] <- 0.1723 * dat2023$depth.standard.m[i]**3.1365
      if(dat2023$depth.standard.m[i] < 0.95){
        dat2023$Q.m3sQF[i] <- 0
      } else {
        dat2023$Q.m3sQF[i] <- 1
      }
    }
  } else{
    dat2023$Q.m3s[i] <- NA
    dat2023$Q.m3sQF[i] <- 1
  }
}

dat2023$DateTime <- as.character(format(dat2023$DateTime))
write.csv(dat2023, paste0(dataloc,"/CLGB.AG/data/stage/y2023/finalData/CLGB.AG_2023_DISCHARGE.csv"), row.names = F)





