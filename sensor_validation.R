# Sensor data validation
# Date: January 2025
# Author: Lara Munro

# Script creates regression plots between sensor data and grab samples
# Script analyses data from CLGB.Ag, OMPD, and CLGB.Upper

# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations
clgbloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"
ompdloc <- "C:/Users/laram/OneDrive - USNH/OMPD/"

# Chemistry (YSI & grab samples)
chemloc <- paste0(clgbloc, "CLGB.AG/data/lab/intermediateData/chem_data_2025_02_23.csv")
chemdat <- read.csv(chemloc)
chemdat <- chemdat[!is.na(chemdat$WQAL.ID),]
chemdat <- chemdat[!is.na(chemdat$DateTime),]
chemdat$DateTime <- as.POSIXct(chemdat$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
chemdat$DON.mgl <- chemdat$TDN.mgl - (chemdat$NO3.mgL * 14.007/62.0049) - (chemdat$NH4.ugL * 14.007/(18.04 * 1000))
chemdat$DON.mgl[chemdat$DON < 0] <- NA
chem.clgb <- subset(chemdat, site == "CLGB.AG")
chem.ompd <- subset(chemdat, site == "OMPD")

chem.no3 <- read.csv(paste0(clgbloc, "CLGB.AG/data/lab/intermediateData/NO3data_quick.csv"))


# 1. CLGB.Ag --------------------------------------------------------------

# SUNA at CLGB.Ag
sunaloc <- paste0(clgbloc, "CLGB.Ag/data/no3/y2023/finaldata/CLGBag_SUNA_2022-2023.csv")
suna <- read.csv(sunaloc)
suna$DateTime <- as.POSIXct(suna$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M")
suna$no3.suna.mgl <- suna$no3.mgl
suna.clgb2023 <- suna[c("DateTime", "no3.suna.mgl")]

sunaloc <- paste0(clgbloc, "CLGB.Ag/data/no3/y2024/finaldata/SUNA_legible_FINAL2024.csv")
suna <- read.csv(sunaloc)
suna$DateTime <- as.POSIXct(suna$DateTime, tz = "EST", format = "%Y-%m-%d %H:%M")
suna$no3.suna.mgl <- suna$NO3.mgL
suna.clgb2024 <- suna[c("DateTime", "no3.suna.mgl")]

suna.clgb <- rbind(suna.clgb2023, suna.clgb2024)

# merge chemistry data with suna

dat.clgb <- merge(clgbag.chem, suna.clgb, by = "DateTime", all = TRUE)

# Remove NO3 concentrations below 0
dat.clgb$no3.suna.mgl[dat.clgb$no3.suna.mgl < 0] <- NA

# SCAN at CLGB.Ag
scanloc <- paste0(clgbloc, "CLGB.Ag/data/no3/y2025/intermediateData/no3_2025-04-24-to-2025-05-02_clgb_ag.csv") 
scan.clgbagA <- read.csv(scanloc)
scan.clgbagA$DateTime <- as.POSIXct(scan.clgbagA$Timestamp)
scan.clgbagA$DateTime <- round_date(scan.clgbagA$DateTime, "15 minutes")
scan.clgbagA <- rename(scan.clgbagA,
                      no3.scan.mgl = no3.mgl,
                      turb.scan.ftu = turbidity.ftu,
                      temp.scan.C = temperature.C)
scan.clgbagA <- scan.clgbagA[c("DateTime", "temp.scan.C", "turb.scan.ftu", "no3.scan.mgl")]


scanloc <- paste0(clgbloc, "CLGB.Ag/data/no3/y2025/rawData/25063800_parameter_20250502T194500.csv")
scan.clgbag <- read.csv(scanloc)
scan.clgbag$DateTime <- as.POSIXct(scan.clgbag$Timestamp, format = "%Y-%m-%dT%H:%M:%S-0400")
scan.clgbag <- rename(scan.clgbag,
                       no3.scan.mgl = NO3eq,
                      turb.scan.ftu = Turbidity,
                      temp.scan.C = Temperature)
scan.clgbag <- scan.clgbag[c("DateTime", "temp.scan.C", "turb.scan.ftu", "no3.scan.mgl")]

scan.clgbag <- rbind(scan.clgbagA, scan.clgbag)

# merge SCAN data with the rest
dat.clgb <- merge(dat.clgb, scan.clgbag, by = "DateTime", all = TRUE)

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
fart <- lm((dat.clgb$no3.suna.mgl)~ dat.clgb$WNO3)
summary(fart)
plot(dat.clgb$WNO3, (dat.clgb$no3.suna.mgl), pch = 19,
     ylim = c(0, 0.3),
     xlim = c(0, 0.3),
     xlab = "Grab sample NO3 - N (mg/L)",
     ylab = "SUNA NO3 - N (mg/L)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")
#abline(a = 0, b = (62/14), col = "blue")
text(dat.clgb$WNO3, dat.clgb$no3.suna.mgL,
     labels = round_date(dat.clgb$DateTime, "day"), pos = 4)

# Plot NO3 time series vs grab samples
ggplot(data = dat.clgb, aes(x = DateTime))+
  geom_line(aes(y = (no3.suna.mgl)))+
  geom_line(aes(y = (no3.scan.mgl * 14/62)))+
  geom_point(aes(y = WNO3), col = "red")+
  xlim(limits = (as.POSIXct(c("2023-05-15", "2023-12-31")))) +
  ylim(limits= c(0, 0.5))+
  labs(x = "Date 2023", y = "NO3-N (mg/l)")+
  ggtitle("CLGB.Ag 2023")+
  theme_classic()

# plot time series for SCAN and grab samples
dat.clgb2025 <- dat.clgb[dat.clgb$DateTime >= "2025-04-22",]
  
ggplot(data = dat.clgb2025, aes(x = DateTime))+
  geom_line(aes(y = (no3.scan.mgl * 14/62)))+
  geom_point(aes(y = WNO3), col = "red")+
  labs(x = "Date 2025", y = "NO3-N (mg/l)")+
  ggtitle("CLGB.Ag 2025")+
  theme_classic()

fart <- lm((dat.clgb$no3.scan.mgl*14/62)~ dat.clgb$WNO3)
summary(fart)
plot(dat.clgb$WNO3, (dat.clgb$no3.scan.mgl*14/62), pch = 19,
#     ylim = c(0, 0.3),
#     xlim = c(0, 0.3),
     xlab = "Grab sample NO3 - N (mg/L)",
     ylab = "SUNA NO3 - N (mg/L)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")
#abline(a = 0, b = (62/14), col = "blue")
text(dat.clgb$WNO3, (dat.clgb$no3.scan.mgL*14/62),
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

dat.ompd <- merge(ompd.chem, suna.ompd, by = "DateTime", all = TRUE)

# Remove NO3 concentrations below 0
dat.ompd$no3.suna.mgl[dat.ompd$no3.suna.mgl < 0] <- NA

# SCAN at OMPD
scanloc <- paste0(ompdloc, "no3/y2025/intermediateData/no3_2025-04-24-to-2025-05-02_ompd.csv") 
scan.ompdA <- read.csv(scanloc)
scan.ompdA$DateTime <- as.POSIXct(scan.ompdA$Timestamp)
scan.ompdA$DateTime <- round_date(scan.ompdA$DateTime, "15 minutes")
scan.ompdA <- rename(scan.ompdA,
                       no3.scan.mgl = no3.mgl,
                       turb.scan.ftu = turbidity.ftu,
                       temp.scan.C = temp.c)
scan.ompdA <- scan.ompdA[c("DateTime", "temp.scan.C", "turb.scan.ftu", "no3.scan.mgl")]


scanloc <- paste0(ompdloc, "no3/y2025/rawData/25063801_parameter_20250502T184500.csv")
scan.ompd <- read.csv(scanloc)
scan.ompd$DateTime <- as.POSIXct(scan.ompd$Timestamp, format = "%Y-%m-%dT%H:%M:%S-0400")
scan.ompd <- rename(scan.ompd,
                      no3.scan.mgl = NO3eq,
                      turb.scan.ftu = Turbidity,
                      temp.scan.C = Temperature)
scan.ompd$no3.scan.mgl[scan.ompd$Flags.NO3eq != ""] <- NA
scan.ompd <- scan.ompd[c("DateTime", "temp.scan.C", "turb.scan.ftu", "no3.scan.mgl")]

scan.ompd <- rbind(scan.ompd, scan.ompd)

# merge SCAN data with the rest
dat.ompd <- merge(dat.ompd, scan.ompd, by = "DateTime", all = TRUE)


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

# SCAN at OMPD



# Plot NO3 data
fart <- lm(dat.ompd$no3.suna.mgl~ dat.ompd$WNO3)
summary(fart)
plot(dat.ompd$WNO3, dat.ompd$no3.suna.mgl, pch = 19,
     ylim = c(0, 0.1),
     xlim = c(0, 0.1),
     xlab = "Grab sample NO3-N (mg/L)",
     ylab = "SUNA NO3-N (mg/L)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")
text(dat.ompd$WNO3, dat.ompd$no3.suna.mgl,
     labels = round_date(dat.ompd$DateTime, "day"), pos = 4)

fart <- lm((dat.ompd$no3.scan.mgl*14/62)~ dat.ompd$WNO3)
summary(fart)
plot(dat.ompd$WNO3, (dat.ompd$no3.scan.mgl*14/62), pch = 19,
     ylim = c(0, 0.5),
     xlim = c(0, 0.5),
     xlab = "Grab sample NO3-N (mg/L)",
     ylab = "SCAN NO3-N (mg/L)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")
text(dat.ompd$WNO3, (dat.ompd$no3.scan.mgl*14/62),
     labels = round_date(dat.ompd$DateTime, "day"), pos = 4)


# Plot NO3 time series vs grab samples
ggplot(data = dat.ompd, aes(x = DateTime))+
  geom_line(aes(y = no3.suna.mgl))+
  geom_line(aes(y = (no3.scan.mgl*14/62)))+
  geom_point(aes(y = WNO3), col = "red")+
  labs( x = "Date 2024", y = "NO3-N (mg/l)")+
  ggtitle("OMPD 2024")+
  xlim(limits = (as.POSIXct(c("2024-09-01", "2024-12-31")))) +
  theme_classic()

plot(dat.ompd$WNO3, dat.ompd$no3.scan.mgl*14/62, pch = 19)

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


# 3. CLGB.UP --------------------------------------------------------------

# SCAN at CLGB.Ag
scanloc <- paste0(clgbloc, "CLGB.UP/no3/y2025/intermediateData/no3_2025-04-24-to-2025-05-02_clgb_up.csv") 
scan.clgbupA <- read.csv(scanloc)
scan.clgbupA$DateTime <- as.POSIXct(scan.clgbupA$Timestamp)
scan.clgbupA$DateTime <- round_date(scan.clgbupA$DateTime, "15 minutes")
scan.clgbupA <- rename(scan.clgbupA,
                       no3.scan.mgl = no3.mgl,
                       turb.scan.ftu = turbidity.ftu,
                       temp.scan.C = temperature.C)
scan.clgbupA <- scan.clgbupA[c("DateTime", "temp.scan.C", "turb.scan.ftu", "no3.scan.mgl")]


scanloc <- paste0(clgbloc, "CLGB.UP/no3/y2025/intermediateData/no3_2025-05-02-to-2025-06-06_clgb_up.csv")
scan.clgbup <- read.csv(scanloc)
scan.clgbup$DateTime <- as.POSIXct(scan.clgbup$Timestamp, format = "%Y-%m-%dT%H:%M:%S-0400")
scan.clgbup <- rename(scan.clgbup,
                      no3.scan.mgl = NO3eq,
                      turb.scan.ftu = Turbidity,
                      temp.scan.C = Temperature)
scan.clgbup <- scan.clgbup[c("DateTime", "temp.scan.C", "turb.scan.ftu", "no3.scan.mgl")]

scan.clgbup <- rbind(scan.clgbupA, scan.clgbup)

# merge SCAN data with the rest
dat.clgbup <- merge(clgbup.chem, scan.clgbup, by = "DateTime", all = TRUE)

# Plot NO3 data
fart <- lm((dat.clgbup$no3.scan.mgl*14/62)~ dat.clgbup$WNO3)
summary(fart)
plot(dat.clgbup$WNO3, (dat.clgbup$no3.scan.mgl*14/62), pch = 19,
     xlab = "Grab sample NO3-N (mg/L)",
     ylab = "SUNA NO3-N (mg/L)")
abline(a = fart$coefficients[1], b = fart$coefficients[2])
abline(a = 0, b = 1, col = "red")
text(dat.clgbup$WNO3, (dat.clgbup$no3.scan.mgl*14/62),
     labels = round_date(dat.clgbup$DateTime, "day"), pos = 3)


# Plot NO3 time series vs grab samples
ggplot(data = dat.clgbup, aes(x = DateTime))+
  geom_line(aes(y = (no3.scan.mgl*14/62)))+
  geom_point(aes(y = WNO3), col = "red")+
  labs( x = "Date 2025", y = "NO3-N (mg/l)")+
  ggtitle("CLGB.Upper 2025")+
  xlim(limits = (as.POSIXct(c("2025-05-01", "2025-06-30")))) +
  theme_classic()



# 4. Grab series over time ------------------------------------------------
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
