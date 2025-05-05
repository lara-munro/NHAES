# Sensor data validation


# Date: January 2025
# Author: Lara Munro


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations
clgbloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"
ompdloc <- "C:/Users/laram/OneDrive - USNH/OMPD/"

# Chemistry (YSI & grab samples)
chemloc <- paste0(clgbloc, "CLGB.AG/data/lab/intermediateData/chem_data_2025_04_10.csv")
chemdat <- read.csv(chemloc)
chemdat <- chemdat[!is.na(chemdat$WQAL.ID),]
chemdat <- chemdat[!is.na(chemdat$DateTime),]
chemdat$DateTime <- as.POSIXct(chemdat$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
chemdat$DON.mgl <- chemdat$TDN.mgl - (chemdat$NO3.mgL * 14.007/62.0049) - (chemdat$NH4.ugL * 14.007/(18.04 * 1000))
chemdat$DON.mgl[chemdat$DON < 0] <- NA
chem.clgb <- subset(chemdat, site == "CLGB.AG")
chem.ompd <- subset(chemdat, site == "OMPD")


# 1. CLGB.Ag --------------------------------------------------------------

# SUNA at CLGB.Ag
sunaloc <- paste0(clgbloc, "CLGB.Ag/data/no3/y2023/finaldata/CLGBag_SUNA_2022-2023.csv")
suna <- read.csv(sunaloc)
suna$DateTime <- as.POSIXct(suna$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M")
suna$no3.suna.mgL <- suna$no3.mgl
suna.clgb2023 <- suna[c("DateTime", "no3.suna.mgL")]

sunaloc <- paste0(clgbloc, "CLGB.Ag/data/no3/y2024/finaldata/SUNA_legible_FINAL2024.csv")
suna <- read.csv(sunaloc)
suna$DateTime <- as.POSIXct(suna$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M")
suna$no3.suna.mgL <- suna$NO3.mgL
suna.clgb2024 <- suna[c("DateTime", "no3.suna.mgL")]

suna.clgb <- rbind(suna.clgb2023, suna.clgb2024)

# merge chemistry data with suna

dat.clgb <- merge(chem.clgb, suna.clgb, by = "DateTime", all.x = TRUE, all.y = FALSE)

# Remove NO3 concentrations below 0
dat.clgb$no3.suna.mgL[dat.clgb$no3.suna.mgL < 0] <- NA

# EXO at CLGB.Ag
exoloc <- paste0(clgbloc, "CLGB.Ag/data/fdom/y2024/finaldata/CLGB.AG_EXO_2024.csv")
exo <- read.csv(exoloc)
exo$DateTime <- as.POSIXct(paste(exo$Date, exo$Time), format = "%m/%d/%Y %H:%M:%S")
exo$DateTime <- round_date(exo$DateTime, "15 minutes")
exo <- exo[c("DateTime", "Cond.uScm", "fDOM.QSU", "ODO.perc", "ODO.mgL",
             "SpCond.uScm", "Turbidity.FNU", "Temp.C")]
dat.clgb <- merge(dat.clgb, exo, by = "DateTime", all.x = TRUE)

# Turner data from 2023-2022
turnerloc <- paste0(clgbloc, "CLGB.AG/data/fdom/y2023/finaldata/CLGBag_Turner_2022-2023.csv")
turner <- read.csv(turnerloc)
turner$DateTime <- as.POSIXct(turner$DateTime, format = "%Y-%m-%d %H:%M:%S")
turner <- turner[c("DateTime", "Turbidity.NTU", "CDOM.ppb", "Temp.C")]

dat.clgb <- merge(dat.clgb, turner, by = "DateTime", all.x = TRUE)

# Plot NO3 data
fart <- lm(dat.clgb$no3.suna.mgL~ dat.clgb$NO3.mgL)
summary(fart)
plot(dat.clgb$NO3.mgL, dat.clgb$no3.suna.mgL, pch = 19,
     ylim = c(0, 0.3),
     xlim = c(0, 0.3),
     xlab = "Grab sample NO3 (mg/L)",
     ylab = "SUNA NO3 (mg/L)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")
text(dat.clgb$NO3.mgL, dat.clgb$no3.suna.mgL,
     labels = round_date(dat.clgb$DateTime, "day"), pos = 4)


# Plot fdom vs DOC
fart <- lm(dat.clgb$fDOM.QSU~ dat.clgb$TOC.mgl)
summary(fart)
plot(dat.clgb$TOC.mgl, dat.clgb$fDOM.QSU, pch = 19,
     xlab = "Grab sample DOC (mg/L)",
     ylab = "YSI EXO fDOM (QSU)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])

# Plot fdom vs DON
fart <- lm(dat.clgb$fDOM.QSU~ dat.clgb$DON.mgl)
summary(fart)
plot(dat.clgb$DON.mgl, dat.clgb$fDOM.QSU, pch = 19,
     xlab = "Grab sample DON (mg/L)",
     ylab = "YSI EXO fDOM (QSU)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])

# Turner data (only 4 points)
plot(dat.clgb$DON.mgl, dat.clgb$CDOM.ppb, pch = 19)

# Turbidity vs TSS
fart <- lm(dat.clgb$Turbidity.FNU~ dat.clgb$tss.mgL)
summary(fart)
plot(dat.clgb$tss.mgL, dat.clgb$Turbidity.FNU, pch = 19,
     xlab = "TSS (mg/l)",
     ylab = "EXO Turbidity (FNU)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])

# EXO DO vs YSI DO (% saturation)
fart <- lm(dat.clgb$ODO.perc~ dat.clgb$DO.percent)
summary(fart)
plot(dat.clgb$DO.percent, dat.clgb$ODO.perc, pch = 19,
     xlab = "YSI handheld DO (% saturation)",
     ylab = "EXO DO (% saturation)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")

# conductivity
fart <- lm(dat.clgb$Cond.uScm~ dat.clgb$conductivity.uscm)
summary(fart)
plot(dat.clgb$conductivity.uscm, dat.clgb$Cond.uScm, pch = 19,
     xlab = "YSI handheld conductivity (uS/cm)",
     ylab = "EXO conductivity (uS/cm)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")

# specific conductivity
fart <- lm(dat.clgb$SpCond.uScm~ dat.clgb$SPC.uscm)
summary(fart)
plot(dat.clgb$SPC.uscm, dat.clgb$SpCond.uScm, pch = 19,
     xlab = "YSI handheld specific conductivity (uS/cm)",
     ylab = "EXO specific conductivity (uS/cm)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")

# temperature
fart <- lm(dat.clgb$Temp.C.x~ dat.clgb$temperature.C)
summary(fart)
plot(dat.clgb$temperature.C, dat.clgb$Temp.C.x, pch = 19,
     ylim = c(0, 30),
     xlim = c(0, 30),
     xlab = "YSI handheld temperature (C)",
     ylab = "EXO temperature (C)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")

# 2. OMPD -----------------------------------------------------------------


# SUNA at OMPD
sunaloc <- paste0(ompdloc, "no3/y2024/finaldata/SUNA_legible_FINAL2024.csv")
suna <- read.csv(sunaloc)
suna$DateTime <- as.POSIXct(suna$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
suna$no3.suna.mgl <- suna$NO3.mgL
suna.ompd <- suna[c("DateTime", "no3.suna.mgl")]

dat.ompd <- merge(chem.ompd, suna.ompd, by = "DateTime", all.x = TRUE)

# EXO at OMPD
exoname <- paste(ompdloc, "fdom/y2024/finalData/EXO_OMPD_2024-06-13-to-2024-12-04.csv", sep = "")
exodat <- read.csv(exoname)
head(exodat)

# Convert date and time columns to datetime
exodat$DateTime <- as.POSIXct(paste(exodat$Date, exodat$Time), format = "%m/%d/%Y %H:%M:%S")
exodat$DateTime <- round_date(exodat$DateTime, "15 minutes")
exodat <- exodat[c("DateTime", "Cond.uscm", "fDOM.QSU", "ODO.percSat", "ODO.mgl",
             "SpCond.uscm", "Turbidity.FNU", "Temp.C")]

dat.ompd <- merge(dat.ompd, exodat, by = "DateTime", all.x = TRUE)

# Plot NO3 data
fart <- lm(dat.ompd$no3.suna.mgl~ dat.ompd$NO3.mgL)
summary(fart)
plot(dat.ompd$NO3.mgL, dat.ompd$no3.suna.mgl, pch = 19,
     ylim = c(0, 0.3),
     xlim = c(0, 0.3),
     xlab = "Grab sample NO3 (mg/L)",
     ylab = "SUNA NO3 (mg/L)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")
text(dat.ompd$NO3.mgL.x, dat.ompd$NO3.mgL.y,
     labels = round_date(dat.ompd$DateTime, "day"), pos = 4)

# Plot fdom vs DOC
fart <- lm(dat.ompd$fDOM.QSU~ dat.ompd$TOC.mgl)
summary(fart)
plot(dat.ompd$TOC.mgl, dat.ompd$fDOM.QSU, pch = 19,
     xlab = "Grab sample DOC (mg/L)",
     ylab = "YSI EXO fDOM (QSU)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])

# Plot fdom vs DON
fart <- lm(dat.ompd$fDOM.QSU~ dat.ompd$DON.mgl)
summary(fart)
plot(dat.ompd$DON.mgl, dat.ompd$fDOM.QSU, pch = 19,
     xlab = "Grab sample DON (mg/L)",
     ylab = "YSI EXO fDOM (QSU)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])

# Turbidity vs TSS
fart <- lm(dat.ompd$Turbidity.FNU ~ dat.ompd$tss.mgL)
summary(fart)
plot(dat.ompd$tss.mgL, dat.ompd$Turbidity.FNU, pch = 19,
     xlab = "TSS (mg/l)",
     ylab = "EXO Turbidity (FNU)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
text(dat.ompd$tss.mgL, dat.ompd$Turbidity.FNU,
     labels = round_date(dat.ompd$DateTime, "day"), pos = 4)


# EXO DO vs YSI DO (% saturation)
fart <- lm(dat.ompd$ODO.perc ~ dat.ompd$DO.percent)
summary(fart)
plot(dat.ompd$DO.percent, dat.ompd$ODO.perc, pch = 19,
     xlab = "YSI handheld DO (% saturation)",
     ylab = "EXO DO (% saturation)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")

# conductivity
fart <- lm(dat.ompd$Cond.uscm~ dat.ompd$conductivity.uscm)
summary(fart)
plot(dat.ompd$conductivity.uscm, dat.ompd$Cond.uscm, pch = 19,
     xlab = "YSI handheld conductivity (uS/cm)",
     ylab = "EXO conductivity (uS/cm)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")

# specific conductivity
fart <- lm(dat.ompd$SpCond.uscm~ dat.ompd$SPC.uscm)
summary(fart)
plot(dat.ompd$SPC.uscm, dat.ompd$SpCond.uscm, pch = 19,
     xlab = "YSI handheld specific conductivity (uS/cm)",
     ylab = "EXO specific conductivity (uS/cm)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")

# temperature
fart <- lm(dat.ompd$Temp.C~ dat.ompd$temperature.C)
summary(fart)
plot(dat.ompd$temperature.C, dat.ompd$Temp.C, pch = 19,
     ylim = c(0, 30),
     xlim = c(0, 30),
     xlab = "YSI handheld temperature (C)",
     ylab = "EXO temperature (C)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")


# 3. Grab series over time ------------------------------------------------
# TSS plot
ggplot(data = chemdat, aes(DateTime, tss.mgL, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("TSS (mg/l)")+
  scale_color_brewer(palette = "Set1")

# TDN plot
ggplot(data = chemdat, aes(DateTime, TDN.mgl, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("TDN (mg/l)")+
  scale_color_brewer(palette = "Set1")

# NO3 plot
ggplot(data = chemdat, aes(DateTime, NO3.mgL, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("NO3 (mg/l)")+
  scale_color_brewer(palette = "Set1")

# NH4 plot
ggplot(data = chemdat, aes(DateTime, NH4.ugL, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("NH4 (ug/l)")+
  scale_color_brewer(palette = "Set1")

# DON plot
ggplot(data = chemdat, aes(DateTime, DON.mgl, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("DON (mg/l)")+
  scale_color_brewer(palette = "Set1")

# DOC plot
ggplot(data = chemdat, aes(DateTime, TOC.mgl, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("DOC (mg/l)")+
  scale_color_brewer(palette = "Set1")

# PO4 plot
ggplot(data = chemdat, aes(DateTime, PO4.ugL, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("PO4 (ug/l)")+
  scale_color_brewer(palette = "Set1")


# Cl plot
ggplot(data = chemdat, aes(DateTime, Cl.mgL, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("Cl (mg/l)")+
  scale_color_brewer(palette = "Set1")

# Mg plot
ggplot(data = chemdat, aes(DateTime, Mg.mgL, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("Mg (mg/l)")+
  scale_color_brewer(palette = "Set1")

# Ca plot
ggplot(data = chemdat, aes(DateTime, Ca.mgL, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("Ca (mg/l)")+
  scale_color_brewer(palette = "Set1")

# K plot
ggplot(data = chemdat, aes(DateTime, K.mgL, color = site))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ylab("K (mg/l)")+
  scale_color_brewer(palette = "Set1")
