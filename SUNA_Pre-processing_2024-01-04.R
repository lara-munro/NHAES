#SUNA datafile aggregation
#Christopher Whitney
#Updated 2016-09-20 to QA/QC raw data based on Fit RMSE > 0.05 and include internal 
#                 temperature and relative humidity in final output file
#Updated 2016-08-18 to include processing of SUNA data collected with Campbell datalogger
#Created 2016-07-06 using code from multiple previous versions of SUNA processing scripts

#Read in raw data (SUNA or Campbell), lines 26-36
#Process raw data from SUNA, lines 38-63
#Process semi-pre-processed data from Campbell, lines 65-70
#Initial QA/QC, lines 73-77
#Export SUNA file, lines 79-84

# Load libraries
library(tidyr)
library(tidyverse)

#Read in raw data files
#Individual .CSV files downloaded from SUNA should be together in a single directory
#within the working directory
#i.e. "~wd/SUNA/CSV/" *Note that for SMD in 2016 there is both /Up and /Out folders

# Titus nutrient addition loc
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/Titus-TASCC 2025/CLGB.16/NO3/rawData"

#OMPD loc
#dataloc <-  "C:/Users/laram/OneDrive - USNH/OMPD/rawData/y2024/SUNA/"

# CLGB.Ag loc
dataloc <-  "C:/Users/laram/OneDrive - USNH/CLGB/CLGB.AG/data/no3/y2024/rawData/SUNAV2/DAT/"
suna_list <- list.files(path=dataloc, pattern = "*.CSV", 
                        full = TRUE)

SUNA_Raw<-do.call(rbind, lapply(suna_list,
                                read.csv, skip=14, header=FALSE))

# Filter SUNA_Raw for light frames only

# For CLGB.Ag SUNA
#LF <- SUNA_Raw %>% filter(V1 == "SATSLF0419")

# For OMPD SUNA & nutrient addition SUNA
LF <- SUNA_Raw %>% filter(V1 == "SATSLF0310")

# Select SUNA-Raw columns and change column names
LF <-
  LF %>% select(c(1:8, 268:275, 281)) %>% setNames(
    c(
      "SerialNumber",
      "Date",
      "Time_Decimal",
      "NO3.uM",
      "NO3.mgL",
      "Abs.254",
      "Abs.350",
      "Br.mgL",
      "Internal.Temp.C",
      "Spec_Temp.C",
      "Lamp_Temp.C",
      "Lamp_Time.sec",
      "Rel_Humidity.pct",
      "Main_Voltage.V",
      "Lamp_Voltage.V",
      "Internal_Voltage.V",
      "RMSE"
    )
  )

## Create DateTime column

LF <-
  LF %>% mutate(
    Time = as.POSIXct(LF$Time_Decimal * 3600, origin = '1970-01-01', "UTC") %>% format("%H:%M"),
    Date = strptime(Date, format = "%Y%j") %>% as.Date(Date),
    DateTime = paste(Date, Time) %>% as.POSIXct(format = "%Y-%m-%d %H:%M", tz = "UTC")
  )

## Convert from UTC to EST (This works but there was a Lubridate method too)                     

attr(LF$DateTime, "tzone") <- "EST"

## Aggregate data to 15-minute intervals

SUNA <-
  LF %>% select("DateTime", "NO3.uM":"RMSE") %>% group_by(DateTime) %>%
  summarize(across(everything(), mean))

## Flag negative values and measurements with high RMSE

SUNA <- SUNA %>% mutate(Flag = ifelse(SUNA$NO3.uM < 0 | SUNA$NO3.mgL < 0, 1,
                                      ifelse(SUNA$RMSE > 0.005, 2, NA)))


# Plot nitrate data
SUNA <- SUNA[-1,]

plot(SUNA$DateTime, SUNA$NO3.mgL, type = "l", xlab = "Date", ylab = "NO3 (mg/l)", pch = ".",
     main = "CLGB.16", xaxt = "n")
#     ylim = c(0, 2))
axis.POSIXct(side = 1, x = SUNA$DateTime,
             at = seq(from = round(SUNA$DateTime[1], "day"),
                      to = SUNA$DateTime[1] + ceiling(difftime(tail(SUNA$DateTime, 1), head(SUNA$DateTime, 1), 
                                                   units = "day")),
                      by = "1 day"),
             las = 2)

# remove negative values
sunasub <- SUNA[(is.na(SUNA$Flag)),]

plot(sunasub$DateTime, sunasub$NO3.mgL, type = "l", xlab = "Date", ylab = "NO3 (mg/l)", pch = ".")


## Write data to file

#CLGB.Ag

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/CLGB.AG/data/"
sunaSaveloc <- paste(dataloc, "no3/y2024/finalData/SUNA_legible_FINAL2024.csv", sep = "")

#OMPD
dataloc <- "C:/Users/laram/OneDrive - USNH/OMPD/"
sunaSaveloc <- paste(dataloc, "no3/y2024/finalData/SUNA_legible_FINAL2024.csv", sep = "")

# CLGB.16 nutrient addition
sunaSaveloc <- "C:/Users/laram/OneDrive - USNH/CLGB/Titus-TASCC 2025/CLGB.16/NO3/CLGB.16_no3.csv"

SUNA$DateTime <- as.character(format(SUNA$DateTime))
write.csv(file = sunaSaveloc, SUNA, row.names = F)


