# Barometric pressure aggregation

# HOBO data aggregation
# Script aggregates HOBO stage data for 2025 for CLGB.Ag and CLGB.Up
# Script also aggregates barometric data from WHB and the local barometer installed at CLGB.Ag
# Script also combines raw stage pressure with barometric pressure to calculate raw water depth (this is also done in individual stage/discharge scripts)
# This is NOT where you calculate discharge or include the offsets with the visual stage


# Load relevant libraries
library(tidyverse)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# Aggregate 2024 barometric data -----------------------------------------------
#Aggregate barometric data (not necessarily needed)
partbaroloc <- paste(dataloc, "Barometer/", sep = "")
baro2024 <- read.csv(paste(partbaroloc, "annual/WHB_atm_Pressure2024.csv", sep = ""))
baro2024$DateTime <- as.POSIXct(baro2024$DateTime, tz = "EST",
                                format = "%Y-%m-%d %H:%M")
baro2024 <- na.omit(baro2024)
baro2024 <- baro2024[3:5]

part1 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20240301to20240507.csv"), skip = 1)
part1$DateTime <- as.POSIXct(part1$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part1 <- part1[c(3:4, 9)]

part2 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20240507to20240626.csv"), skip = 1)
part2 <- part2 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.C = Temp...C..LGR.S.N..10012043..SEN.S.N..10012043.)
part2$DateTime <- as.POSIXct(part2$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part2 <- part2[c(3:4, 9)]

part3 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20240626to20240825.csv"), skip = 1)
part3 <- part3[1:4]
part3 <- part3 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part3$pres.kpa <- 6.89476 * part3$pres.psi
part3$temp.C <- (part3$temp.F - 32) *5/9
part3$DateTime <- as.POSIXct(part3$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")

part3 <- part3[5:7]

part4 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20240815to20240924.csv"), skip = 1)
part4 <- part4[1:4]
part4 <- part4 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part4$pres.kpa <- 6.89476 * part4$pres.psi
part4$temp.C <- (part4$temp.F - 32) *5/9
part4$DateTime <- as.POSIXct(part4$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part4 <- part4[5:7]

baro2024 <- rbind(baro2024, part4)
baro2024 <- baro2024[!duplicated(baro2024),]


part5 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20240924to20241105.csv"), skip = 1)
part5 <- part5[1:4]
part5 <- part5 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part5$pres.kpa <- 6.89476 * part5$pres.psi
part5$temp.C <- (part5$temp.F - 32) *5/9
part5$DateTime <- as.POSIXct(part5$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part5 <- part5[5:7]


baro2024 <- rbind(baro2024, part5)
baro2024 <- baro2024[!duplicated(baro2024),]

part6 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20241105to20241219.csv"), skip = 1)
head(part6)
part6 <- part6[1:4]
part6 <- part6 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part6$pres.kpa <- 6.89476 * part6$pres.psi
part6$temp.C <- (part6$temp.F - 32) *5/9
part6$DateTime <- as.POSIXct(part6$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part6 <- part6[5:7]

baro2024 <- rbind(baro2024, part6)
baro2024 <- baro2024[!duplicated(baro2024),]


part7 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20241219to20250204.csv"), skip = 1)
head(part7)
part7 <- part7[1:4]
part7 <- part7 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part7$pres.kpa <- 6.89476 * part7$pres.psi
part7$temp.C <- (part7$temp.F - 32) *5/9
part7$DateTime <- as.POSIXct(part7$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part7 <- part7[5:7]

baro2024 <- rbind(baro2024, part7)
baro2024 <- baro2024[!duplicated(baro2024),]


baro2024$DateTime <- as.character(format(baro2024$DateTime))
write.csv(baro2024, paste0(partbaroloc, "annual/WHB_atm_Pressure2024.csv"), row.names = F)


# Aggregate 2025 barometric pressure --------------------------------------

partbaroloc <- paste(dataloc, "Barometer/", sep = "")
baro2025 <- read.csv(paste(partbaroloc, "annual/WHB_atm_Pressure2025.csv", sep = ""))
baro2025$DateTime <- as.POSIXct(baro2025$DateTime, tz = "EST",
                                format = "%Y-%m-%d %H:%M")
baro2025 <- na.omit(baro2025)
#baro2025 <- baro2025[3:5]

part1 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20241219to20250204.csv"), skip = 1)
head(part1)
part1 <- part1[1:4]
part1 <- part1 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part1$pres.kpa <- 6.89476 * part1$pres.psi
part1$temp.C <- (part1$temp.F - 32) *5/9
part1$DateTime <- as.POSIXct(part1$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part1 <- part1[5:7]
part1$serialnb <- 10012043

baro2025 <- rbind(baro2025, part1)
baro2025 <- baro2025[!duplicated(baro2025),]

part2 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20250204to20250325.csv"), skip = 1)
head(part2)
part2 <- part2[1:4]
part2 <- part2 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part2$pres.kpa <- 6.89476 * part2$pres.psi
part2$temp.C <- (part2$temp.F - 32) *5/9
part2$DateTime <- as.POSIXct(part2$datetime, tz = "EST",
                             format = "%m/%d/%Y %H:%M")
part2 <- part2[5:7]
part2$serialnb <- 10012043

baro2025 <- rbind(baro2025, part2)
baro2025 <- baro2025[!duplicated(baro2025),]


part3 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20250325to20250501.csv"), skip = 1)
head(part3)
part3 <- part3[1:4]
part3 <- part3 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.C = Temp...C..LGR.S.N..10012043..SEN.S.N..10012043.)
part3$DateTime <- as.POSIXct(part3$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part3 <- part3[3:5]
part3$serialnb <- 10012043

baro2025 <- rbind(baro2025, part3)
baro2025 <- baro2025[!duplicated(baro2025),]

part4 <- read.csv(paste0(partbaroloc, "WHB barometer/WHB_B_20250501to20250602.csv"), skip = 1)
head(part4)
part4 <- part4[1:4]
part4 <- part4 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.C = Temp...C..LGR.S.N..10012043..SEN.S.N..10012043.)
part4$DateTime <- as.POSIXct(part4$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part4 <- part4[3:5]
part4$serialnb <- 10012043

baro2025 <- rbind(baro2025, part4)
baro2025 <- baro2025[!duplicated(baro2025),]

baro2025B <- baro2025
baro2025$DateTime <- as.character(format(baro2025$DateTime))
write.csv(baro2025, paste0(partbaroloc, "annual/atm_Pressure2025.csv"), row.names = F)
baro2025 <- baro2025B

# Read baro data from local logger (not WHB)


part5 <- read.csv(paste0(partbaroloc, "baro_CLGB_2025_05-to_2025_07_14.csv"), skip = 1)
head(part5)
part5 <- part5[1:4]
part5 <- part5 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..22254013..SEN.S.N..22254013.,
         temp.C = Temp...C..LGR.S.N..22254013..SEN.S.N..22254013.)
part5$DateTime <- as.POSIXct(part5$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part5 <- part5[3:5]
part5$serialnb <- 22254013

baro2025 <- rbind(baro2025, part5)
baro2025 <- baro2025[!duplicated(baro2025),]

part6 <- read.csv(paste0(partbaroloc, "baro_CLGB_2025_07_14-to_2025_08_29.csv"), skip = 1)
head(part6)
part6 <- part6[1:4]
part6 <- part6 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..22254013..SEN.S.N..22254013.,
         temp.C = Temp...C..LGR.S.N..22254013..SEN.S.N..22254013.)
part6$DateTime <- as.POSIXct(part6$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part6 <- part6[3:5]
part6$serialnb <- 22254013

baro2025 <- rbind(baro2025, part6)
baro2025 <- baro2025[!duplicated(baro2025),]

baro2025$DateTime <- as.character(format(baro2025$DateTime))
write.csv(baro2025, paste0(partbaroloc, "annual/atm_Pressure2025.csv"), row.names = F)
