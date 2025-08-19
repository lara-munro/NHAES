# Aggregate chemistry data 
# Date: January 2025
# Author: Lara Munro

# Set up R space

# Load libraries
library(tidyverse)
library(lubridate)
library(reshape2)

# Set data location

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/CLGB.AG/data/"

csvloc <- paste0(dataloc, "lab/")

# Load sample ID list  
sampleID <- read.csv(paste0(csvloc, "AES_sample_list.csv"))

# create DateTime column for entire dataframe
dat <- sampleID

dat$DateTime <- as.POSIXct(paste(dat$date, dat$time.est), format = "%Y-%m-%d %H:%M")

dat <- subset(dat, select = c(site, DateTime, WQAL.ID))

# YSI data ----------------------------------------------------------------
ysidat <- read.csv(paste0(dataloc, "/field/allSitesYSI.csv"))
ysidat$DateTime <- as.POSIXct(paste(ysidat$date, ysidat$time.est), format = "%Y-%m-%d %H:%M")
ysidat <- subset(ysidat, select = c("site", "DateTime", "temperature.C", "DO.percent", "DO.mgl", "SPC.uscm",
                                     "conductivity.uscm", "stage.cm"))

dat <- merge(dat, ysidat, by = c("site", "DateTime"), all = TRUE)

# remove samples that either have no WQAL ID or Date
dat <- dat[!is.na(dat$DateTime),]
dat <- dat[!is.na(dat$WQAL.ID),]

# TSS data ----------------------------------------------------------------
tssdat <-  read.csv(paste0(csvloc, "intermediateData/TSS.csv"))
tssdat$DateTime <- as.POSIXct(paste(tssdat$date, tssdat$time.est), format = "%Y-%m-%d %H:%M")
tssdat <- subset(tssdat, select = c("site", "DateTime", "tss.mgL"))

dat <- merge(dat, tssdat, by = c("site", "DateTime"), all.x = TRUE)

# IC data -----------------------------------------------------------------

iclist <- list.files(paste0(csvloc, "rawData/IC/"), pattern = "_IC.csv")
icnb <- length(iclist)
icdat <- read.csv(paste0(csvloc , "rawData/IC/", iclist[1]))

for (i in 2:icnb){
  fart <- read.csv(paste0(csvloc, "rawData/IC/", iclist[i]))
  icdat <-  rbind(icdat, fart)
}
dat <- merge(dat, icdat, by.x = "WQAL.ID", by.y = "sampleID", all.x = TRUE)

# NH4 data ----------------------------------------------------------------

nh4list <- list.files(paste0(csvloc, "rawData/NH4/"), pattern = "_NH4.csv")
nh4nb <- length(nh4list)
nh4dat <- read.csv(paste0(csvloc, "rawData/NH4/", nh4list[1]))

for (i in 2:nh4nb){
  fart <- read.csv(paste0(csvloc, "rawData/NH4/", nh4list[i]))
  nh4dat <-  rbind(nh4dat, fart)
}
dat <- merge(dat, nh4dat, by.x = "WQAL.ID", by.y = "SampleID", all.x = TRUE, all.y = FALSE)

# PO4 data ----------------------------------------------------------------

po4list <- list.files(paste0(csvloc, "rawData/PO4/"), pattern = "_PO4.csv")
po4nb <- length(po4list)
po4dat <- read.csv(paste0(csvloc, "rawData/PO4/", po4list[1]))

for (i in 2:po4nb){
  fart <- read.csv(paste0(csvloc, "rawData/PO4/", po4list[i]))
  po4dat <-  rbind(po4dat, fart)
}
dat <- merge(dat, po4dat, by.x = "WQAL.ID", by.y = "sampleID", all.x = TRUE)


# TOC data ----------------------------------------------------------------

tocdat <- read.csv(paste0(csvloc, "rawData/TOC-TN/TOC_all_data.csv"))
# Merge TOC data with other chemistry data
dat <- merge(dat, tocdat, by.x = "WQAL.ID", by.y = "sampleID", all.x = TRUE)


#  NO3 data ---------------------------------------------------------------

no3dat <- read.csv(paste0(csvloc, "intermediateData/NO3data_quick.csv"))
dat <- merge(dat, no3dat, by.x = "WQAL.ID", by.y = "SampleID", all = TRUE)


# Save data frame as its own item (could be rewritten in other scripts)
chemdat <- dat

# save file

write.csv(chemdat, paste0(dataloc, "lab/intermediateData/chem_data_2025_06_26.csv"), row.names = FALSE)

# Subset CLGB.Ag data to compare with sensor
clgbag.chem <- chemdat[which(chemdat$site == "CLGB.AG"),]

# Subset OMPD data to compare with sensor
ompd.chem <- chemdat[which(chemdat$site == "OMPD"),]

# Subset CLGB.Upper data to compare with sensor
clgbup.chem <- chemdat[which(chemdat$site == "CLGB.Upper"),]

# Plot data ---------------------------------------------------------------

# Plot NO3 for IC vs Smartchem readings

chemsub <- rbind(clgbag.chem, clgbup.chem)
chemsub <- rbind(chemsub, ompd.chem)

ggplot(chemdat, aes(x = NO3.mgL, y = WNO3))+
  geom_point()+
  xlim(0, 0.2)+
  ylim(0, 0.2)+
  geom_abline(intercept = 0, slope = 1)+
#  geom_smooth(method = "lm")+
  labs(x = "NO3-N from the IC (mg/l)", y = "NO3-N from the Smartchem (mg/l)")+
  theme_classic()


boxplot(dat$NO3.mgL ~ dat$site,
        ylab = "NO3 (mg/l)",
        xlab = "Site")
boxplot(dat$NH4.ugL ~ dat$site,
        ylab = "NH4 (ug/l)",
        xlab = "Site")

boxplot(dat$temperature.C ~ dat$site, ylim = c(-10, 30),
        ylab = "Temperature (C)",
        xlab = "Site")
boxplot(dat$DO.percent ~ dat$site,
        ylab = "Dissolved oxygen (%)",
        xlab = "Site")
boxplot(dat$SPC.uscm ~ dat$site,
        ylab = "Specific conductivity (uS/cm)",
        xlab = "Site")
boxplot(dat$tss.mgL ~ dat$site,
        ylab = "Total suspended solids (mg/l)",
        xlab = "Site")

boxplot(dat$Cl.mgL ~ dat$site,
        ylab = "Cl (mg/l)",
        xlab = "Site")
boxplot(dat$SO4.mgL ~ dat$site,
        ylab = "SO4 (mg/l)",
        xlab = "Site")
boxplot(dat$Na.mgL ~ dat$site,
        ylab = "Na (mg/l)",
        xlab = "Site")
boxplot(dat$K.mgL ~ dat$site,
        ylab = "K (mg/l)",
        xlab = "Site")
boxplot(dat$Ca.mgL ~ dat$site,
        ylab = "Ca (mg/l)",
        xlab = "Site")
boxplot(dat$Mg.mgL ~ dat$site,
        ylab = "Mg (mg/l)",
        xlab = "Site")
boxplot(dat$PO4.ugL ~ dat$site,
        ylab = "PO4 (ug/l)",
        xlab = "Site")

boxplot(dat$TOC.mgl ~ dat$site,
        ylab = "TOC (mg/l)",
        xlab = "Site")
boxplot(dat$TDN.mgl ~ dat$site,
        ylab = "TDN (mg/l)",
        xlab = "Site")
