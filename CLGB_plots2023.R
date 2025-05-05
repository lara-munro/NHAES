# Create a summary sheet of AES data
## Created 2023-12
## Lara Munro; updated from Chris Whitney


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/CLGB.AG/data/"


# Part 1: Read in raw data ------------------------------------------------

# Read stage data
disloc <- paste(dataloc, "transformedData/CLGBag_DISCHARGE_2022-2023.csv", sep = "")
discharge <- read.csv(disloc)

discharge$DateTime <- as.POSIXct(discharge$DateTime, tz = "EST",
                           format = "%Y-%m-%d %H:%M:%S")



for (i in 1:nrow(discharge)){
  if (!is.na(discharge$discharge.m3s[i])){
    if (discharge$discharge.m3s[i] < 0.001){
      discharge$discharge.m3s[i] <- NA
    }
  }
}

# Read conductivity data
condloc <- paste(dataloc, "transformedData/CLGBag_COND_2022-2023.csv", sep = "")
cond <- read.csv(condloc)
cond$DateTime <- as.POSIXct(cond$DateTime, tz = "EST",
                                 format = "%Y-%m-%d %H:%M:%S")


# Read Turner data
turnerloc <- paste(dataloc, "transformedData/CLGBag_Turner_2022-2023.csv", sep = "")
turn <- read.csv(turnerloc)
turn$DateTime <- as.POSIXct(turn$DateTime, tz = "EST",
                                 format = "%Y-%m-%d %H:%M:%S")


# Read SUNA data
sunaloc <- paste(dataloc, "transformedData/CLGBag_SUNA_2022-2023.csv", sep = "")
SUNA <- read.csv(sunaloc)
SUNA$DateTime <- as.POSIXct(SUNA$DateTime, tz = "EST",
                                 format = "%Y-%m-%d %H:%M")


# Merge data
dat <- merge(discharge, cond, by = "DateTime", all = TRUE)
dat <- merge(dat, turn, by = "DateTime", all = TRUE)
dat <- merge(dat, SUNA, by = "DateTime", all = TRUE)
#dat$DateTime <- as.POSIXct(dat$DateTime, tz = "EST",
#                           format = "%Y-%m-%d %H:%M:%S")


# Create a file to save
datsave <- subset(dat, select = c("DateTime", "TempWat.C.stage", "discharge.m3s", 
                                  "Cond.uScm", "TempWat.C.cond",
                                  "Rhodamine.ppb", "Turbidity.NTU", "CDOM.ppb", "Depth.m", "Temp.C",
                                  "Nitrate_mg", "SUNA_Error", "SUNA_Flag"
                                  ))

# Rename columns to match previous names
datsave <- datsave %>%
  rename(Temp.C_Stage = TempWat.C.stage,
         Discharge.m3s_NEW_Stage = discharge.m3s,
         Temp.C_Cond = TempWat.C.cond,
         SpCond.uScm_Cond = Cond.uScm,
         Rhodamine.ppb_Turner = Rhodamine.ppb,
         Turbidity.NTU_Turner = Turbidity.NTU,
         CDOM.ppb_Turner = CDOM.ppb,
         Depth.m_Turner = Depth.m,
         Temp.C_Turner = Temp.C
         )

col_order <- c("DateTime", "Temp.C_Stage", "Discharge.m3s_NEW_Stage", "Temp.C_Cond",
               "SpCond.uScm_Cond", "Nitrate_mg", "SUNA_Error", "SUNA_Flag",
               "Rhodamine.ppb_Turner", "Turbidity.NTU_Turner", "CDOM.ppb_Turner",
               "Depth.m_Turner", "Temp.C_Turner")
datsave <- datsave[, col_order]
datsave$DateTime <- as.character(format(datsave$DateTime))

savename <- paste(dataloc, "CLGB_AG_", Sys.Date(), ".csv", sep = "")
write.csv(file = savename, datsave, row.names = FALSE)


# Read TSS data
# Update TSS manually from Exvel file to csv
tssloc <- paste(dataloc, "rawData/Lab_Field/CLGB_AG_TSS.csv", sep = "")
tss <- read.csv(tssloc)
tss$Date <- as.POSIXct(tss$Date, tz = "EST", format = "%m/%d/%Y")
tssSimp <- subset(tss, select = c("Date", "TSS.mgl"))

# Merge sensor data with TSS
dat <- merge(dat, tssSimp, by.x = "DateTime", by.y = "Date", all.x = TRUE)


# Part 2: Plot data -------------------------------------------------------

# Discharge alone
dis2 <- discharge
dis2$DateTime <- as.POSIXct(dis2$DateTime, tz = "EST",
                                 format = "%Y-%m-%d %H:%M:%S")
dis2 <- na.omit(dis2)

par(mar = c(4.5, 5, 2, 2))
plot(dis2$DateTime, dis2$discharge.m3s, type = "l",
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
plot(dis2$DateTime, dis2$TempWat.C.stage, type = "l",
     xlab = "Date",
     ylab = "Temperature (C)",
     col = "pink",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dis2$DateTime, dis2$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)



# Conductivity and discharge
# Create a table with only the relevant data
condQ <- subset(dat, select = c("DateTime", "Cond.uScm", "discharge.m3s"))
condQ[condQ < 0] <- NA
condQ <- na.omit(condQ)

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(condQ$DateTime, condQ$Cond.uScm, type = "l",
     xlab = "Date",
     ylab = "Specific conductance (uS/cm)",
     col = "goldenrod",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(condQ$DateTime, condQ$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)


# Turbidity and discharge 2022
turbid <- subset(dat, select = c("DateTime", "Turbidity.NTU", "discharge.m3s"))
turbid2022 <- turbid[turbid$DateTime >= "2022-01-01" & turbid$DateTime <= "2022-12-31", ]
turbid2022 <- na.omit(turbid2022)

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(turbid2022$DateTime, turbid2022$Turbidity.NTU, type = "l",
     xlab = "Date 2022",
     ylab = "Turbidity (NTU)",
     col = "brown",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(turbid2022$DateTime, turbid2022$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axTicks(side = 4)
axis(side = 4, at = c(0.01, 0.1, 1, 2), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)

# Turbidity and discharge 2023 - only 2 weeks of data and it looks weird
turbid <- subset(dat, select = c("DateTime", "Turbidity.NTU", "discharge.m3s"))
turbid2023 <- turbid[turbid$DateTime >= "2023-01-01" & turbid$DateTime <= "2023-12-31", ]
turbid2023 <- na.omit(turbid2023)

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(turbid2023$DateTime, turbid2023$Turbidity.NTU, type = "l",
     xlab = "Date 2023",
     ylab = "Turbidity (NTU)",
     col = "brown",
     cex.lab = 1.5,
     ylim = c(0, 100)
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(turbid2023$DateTime, turbid2023$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axTicks(side = 4)
axis(side = 4, at = c(0.01, 0.1, 1, 2)) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)


# CDOM and discharge
# Create a table with only the relevant data
cdomQ <- subset(dat, select = c("DateTime", "CDOM.ppb", "discharge.m3s"))
cdomQ2022 <- cdomQ[cdomQ$DateTime >= "2022-01-01" & cdomQ$DateTime <= "2022-12-31", ]
cdomQ2022 <- na.omit(cdomQ2022)

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(cdomQ2022$DateTime, cdomQ2022$CDOM.ppb, type = "l",
     xlab = "Date 2022",
     ylab = "CDOM (ppb)",
     col = "forestgreen",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(cdomQ2022$DateTime, cdomQ2022$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axTicks(side = 4)
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)

cdomQ2023 <- cdomQ[cdomQ$DateTime >= "2023-01-01" & cdomQ$DateTime <= "2023-12-31", ]
cdomQ2023 <- na.omit(cdomQ2023)

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(cdomQ2023$DateTime, cdomQ2023$CDOM.ppb, type = "l",
     xlab = "Date 2023",
     ylab = "CDOM (ppb)",
     col = "forestgreen",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(cdomQ2023$DateTime, cdomQ2023$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axTicks(side = 4)
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)



# Nitrate and discharge
no3Q <- subset(dat, select = c("DateTime", "Nitrate_mg", "discharge.m3s"))
no3Q2022 <- no3Q[no3Q$DateTime >= "2022-01-01" & no3Q$DateTime <= "2022-12-31", ]
no3Q2022[no3Q2022 < 0] <- NA
no3Q2022 <- na.omit(no3Q2022)

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(no3Q2022$DateTime, no3Q2022$Nitrate_mg, type = "l",
     xlab = "Date 2022",
     ylab = "Nitrate (mg/l)",
     col = "darkred",
     cex.lab = 1.5,
     ylim = c(0, 0.6),
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(no3Q2022$DateTime, no3Q2022$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1, 2), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)

# Nitrate and discharge 2023
no3Q2023 <- no3Q[no3Q$DateTime >= "2023-01-01" & no3Q$DateTime <= "2023-12-31", ]
no3Q2023[no3Q2023 < 0] <- NA
head(no3Q2023)
tail(no3Q2023)
no3Q2023 <- no3Q[no3Q$DateTime >= "2023-05-27" & no3Q$DateTime <= "2023-08-04", ]
#no3Q2023 <- na.omit(no3Q2023)

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(no3Q2023$DateTime, no3Q2023$Nitrate_mg, type = "l",
     xlab = "Date 2023",
     ylab = "Nitrate (mg/l)",
     col = "darkred",
     cex.lab = 1.5,
#     ylim = c(0, 0.6),
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(no3Q2023$DateTime, no3Q2023$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1, 2), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)



# TSS and turbidity -------------------------------------------------------

# Read Turner data
turnerloc <- paste(dataloc, "transformedData/CLGBag_Turner_2022-2023.csv", sep = "")
turn <- read.csv(turnerloc)
turn$DateTime <- as.POSIXct(turn$DateTime, tz = "EST",
                            format = "%Y-%m-%d %H:%M:%S")
# Calculate daily mean turbidity
turn2 <- turn %>%
  mutate(date = floor_date(DateTime, unit = "day")) %>%
  group_by(date) %>%
  summarize( turbidity= mean(Turbidity.NTU))


# Read TSS data
tssloc <- paste(dataloc, "rawData/Lab_Field/CLGB_AG_TSS.csv", sep = "")
tss <- read.csv(tssloc)
tss$Date <- as.POSIXct(tss$Date, tz = "EST", format = "%m/%d/%Y")
tssSimp <- subset(tss, select = c("Date", "TSS.mgl"))

# Merge TSS and turbidity data
tssturb <- merge(tss, turn2, by.x = "Date", by.y = "date", all.x = TRUE)

plot(tssturb$TSS.mgl, tssturb$turbidity, pch = 19,
     xlab = "TSS (mg/l)",
     ylab = "Turbidity (NTU)")

# TSS at all sites --------------------------------------------------------

# Read all sites TSS data
alltssloc <- paste(dataloc, "rawData/Lab_Field/all_sites_TSS.csv", sep = "")
alltss <- read.csv(alltssloc)

boxplot(alltss$TSS.mg.l~alltss$Site, 
        xlab = "site",
        ylab = "TSS (mg/l)",
        col = "dodgerblue")


# GGplot plots (garbage) -------------------------------------------------------

# Discharge alone

ggplot(CLGBAGall, aes(x = DateTime))+
  geom_line(aes(y = Discharge.m3s_NEW_Stage), col = "darkslateblue")+
  scale_y_continuous(trans = "log10", name = "Discharge (m3/s)")+
  scale_x_datetime(name = "Date 2022") +
  theme_classic()+
  theme(
    axis.title.y = element_text(color = "darkslateblue", size=15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.text.x = element_text(size = 15)
  ) 


ggplot(dat, aes(x = DateTime))+
  geom_line(aes(y = discharge.m3s), col = "darkslateblue")+
  scale_y_continuous(trans = "log10", name = "Discharge (m3/s)")+
  scale_x_datetime(name = "Date 2022", 
                   limits = as.POSIXct(c("2022-08-08 12:00:00", "2023-08-08 12:00:00"), format = "%Y-%m-%d %H:%M:%S"))+
  theme_classic()+
  theme(
    axis.title.y = element_text(color = "darkslateblue", size=15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.text.x = element_text(size = 15)
  ) 



# Conductivity and discharge

condQ <- subset(dat, select = c("DateTime", "Cond.uScm", "discharge.m3s"))
#fart <- na.omit(condQ)
condQ[condQ < 0] <- NA

coeff <-  1000


ggplot(condQ, aes(x=DateTime)) +
  geom_line(aes(y = Cond.uScm), col = "goldenrod")+
  geom_line(aes(y = discharge.m3s*coeff), col = "darkslateblue") +
  scale_y_continuous(sec.axis = sec_axis(~./coeff,
                                         name = "Discharge (m3/s)")
  ) +
  scale_y_continuous(sec.axis = sec_axis(trans = "log10", name = "Discharge (m3/s)"), limits = c(0, 3000)) +
  scale_x_datetime(name = "Date",
                   limits = as.POSIXct(c("2022-08-08 12:00:00", "2023-08-08 12:00:00"), format = "%Y-%m-%d %H:%M:%S")
  )+
  theme_classic()+
  labs(y = "Specific conductance (uS/cm)")+
  theme(
    axis.title.y = element_text(color = "goldenrod", size=15),
    axis.text.y = element_text(size = 15),
    axis.title.y.right = element_text(color = "darkslateblue", size=15),
    axis.text.y.right = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.text.x = element_text(size = 15)
  ) 



ggplot(condQ, aes(x=DateTime)) +
  geom_line(aes(y = Cond.uScm), col = "goldenrod")+
  geom_line(aes(y = discharge.m3s*coeff), col = "darkslateblue") +
  scale_y_continuous(
    name = "Conductivity (uS/cm)",
    sec.axis = sec_axis(~./coeff,
                        name = "Discharge (m3/s)")
  ) +
  scale_y_continuous(sec.axis = sec_axis(trans = "log10", name = "Discharge (m3/s)")) +
  scale_x_datetime(name = "Date") +
  theme_classic()+
  labs(y = "Specific conductance (uS/cm)")+
  theme(
    axis.title.y = element_text(color = "goldenrod", size=15),
    axis.text.y = element_text(size = 15),
    axis.title.y.right = element_text(color = "darkslateblue", size=15),
    axis.text.y.right = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.text.x = element_text(size = 15)
  ) 


# Turbidity and discharge
dat2022 <- dat[dat$DateTime >= "2022-01-01" & dat$DateTime <= "2022-12-31", ]
dat2023 <- dat[dat$DateTime >= "2023-01-01" & dat$DateTime <= "2023-12-31", ]

coeff <-  100

ggplot(dat2023, aes(x=DateTime))+
  geom_line(aes(y = Turbidity.NTU), col = "red")+
  geom_line(aes(y = (discharge.m3s)*coeff), col = "darkslateblue") +
  scale_y_continuous(
    name = "Turbidity (NTU)",
    sec.axis = sec_axis(~./coeff, name = "Discharge (m3/s)")) +
  scale_x_datetime(name = "Date") +
  theme_classic()+
  theme(axis.title.y = element_text(color = "red", size=15),
        axis.text.y = element_text(size = 15),
        axis.title.y.right = element_text(color = "darkslateblue", size=15),
        axis.text.y.right = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15)
  ) +
  labs(title = "CLGB.Ag turbidity and discharge in 2023")


# CDOM and discharge

# Create a table with only the relevant data
cdomQ <- subset(CLGBAGall, select = c("DateTime", "CDOM.ppb_Turner", "Discharge.m3s_NEW_Stage"))
cdomQ[cdomQ < 0] <- NA

coeff <-  100

ggplot(cdomQ, aes(x=DateTime)) +
  geom_line(aes(y = CDOM.ppb_Turner), col = "darkgreen")+
  geom_line(aes(y = Discharge.m3s_NEW_Stage*coeff), col = "darkslateblue") +
  scale_y_continuous(
    name = "CDOM (ppb)",
    sec.axis = sec_axis(~log10(./coeff), name = "Discharge (m3/s)")
  ) +
  scale_y_continuous(sec.axis = sec_axis(trans = "log10", name = "Discharge (m3/s)", breaks = seq(0, 0.1, 1))) +
  scale_x_datetime(name = "Date") +
  theme_classic()+
  labs(y = "CDOM (ppb)")+
  theme(
    axis.title.y = element_text(color = "darkgreen", size=15),
    axis.text.y = element_text(size = 15),
    axis.title.y.right = element_text(color = "darkslateblue", size=15),
    axis.text.y.right = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.x = element_text(size = 15)
  ) +
  labs(title = "CLGB.Ag CDOM and discharge in 2022")

# Nitrate and discharge
no3Q <- subset(CLGBAGall, select = c("DateTime", "Nitrate_mg", "Discharge.m3s_NEW_Stage"))
no3Q[no3Q < 0] <- NA


ggplot(no3Q, aes(x=DateTime)) +
  geom_line(aes(y = Nitrate_mg), col = "orange")+
  geom_line(aes(y = Discharge.m3s_NEW_Stage*1000), col = "darkslateblue") +
  scale_y_continuous(
    name = "Nitrate (mg/l)",
    sec.axis = sec_axis(~./1000, name = "Discharge (m3/s)", trans = "log10")
  ) +
  scale_x_datetime(name = "Date") +
  theme_classic()+
  theme(
    axis.title.y = element_text(color = "orange", size=15),
    axis.text.y = element_text(size = 15),
    axis.title.y.right = element_text(color = "darkslateblue", size=15),
    axis.text.y.right = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.x = element_text(size = 15)
  ) +
  labs(title = "CLGB.Ag Nitrate and discharge in 2022")






