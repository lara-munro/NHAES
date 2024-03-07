## CLGB_AG Data Analysis
## Created 2023-11-07
## Lara Munro; updated from Chris Whitney


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations

dataloc <- "C:/Users/laram/Documents/UNH/AES stuff/Data/"


# Part 1: Read in raw data ------------------------------------------------

## Stage
filename <- paste(dataloc, "Stage/CLGB_AG_Stage_NEW_2022-08-08-to-2022-11-28.csv", sep = "")
stage <- read.csv(filename,
                  header = TRUE, stringsAsFactors = FALSE)
# Convert datetime into the same format as other data
stage <- stage %>% mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M", tz = "EST"))
# Rename the columns so that they indicate where the data comes from 
stage <- stage %>% rename_with(~paste0(.x, "_stage"), !DateTime)

## Conductivity

filename <- paste(dataloc, "Conductivity/CLGB_AG_Cond_2022-05-03-to-2022-11-28.csv", sep = "")
cond <- read.csv(filename,
                 header = TRUE, stringsAsFactors = FALSE)
# Convert datetime into the same format as other data
cond <- cond %>% mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M", tz = "EST"))
# Rename the columns so that they indicate where the data comes from 
cond <- cond %>% rename_with(~paste0(.x, "_cond"), !DateTime)

## SUNA

filename <- paste(dataloc, "SUNA/SUNA_WQual_2022-06-06-to-2022-11-28.csv", sep = "")
SUNA <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

# Convert datetime into the same format as other data
SUNA <- SUNA %>% mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
                        DateTime = as.POSIXct(round(as.numeric(TIMESTAMP)/900)*900, origin = '1970-01-01',
                                              tz = "EST"), .before = Year,
                        TIMESTAMP = NULL, RECORD = NULL)
# Only keep the most relevant information
SUNA <- SUNA %>% select(DateTime, Nitrate_mg, SUNA_Error)
# Adjust the error codes so that they are consistent
SUNA$SUNA_Error <- as.numeric(SUNA$SUNA_Error)
SUNA <- SUNA %>% mutate(SUNA_Flag = ifelse(SUNA_Error > 0.05, 1, 0))
#SUNA <- SUNA %>% rename_with(~paste0(.x, "_SUNA"), !DateTime)


## Turner

filename <- paste(dataloc, "Turner/CLGB_AG_Turner_2022-06-10-to-2022-11-28.csv", sep = "")
Turner <- read.csv(filename,header = TRUE, stringsAsFactors = FALSE)

# Convert datetime into the same format as other data
Turner <- Turner %>% mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M", tz = "EST"))
# Rename the columns so that they indicate where the data comes from 
Turner <- Turner %>% rename_with(~paste0(.x, "_Turner"), !DateTime)


## Pease MET data (for extracting Baro pressure)

filename <- paste(dataloc, "Baro/Pease_2022.csv", sep = "")
Pease <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

# Convert datetime into the same format as other data
Pease <- Pease %>% filter(SOURCE == 7) %>% mutate(DATE = gsub(x = DATE, pattern = "T", replacement = " "),
                                                  DateTime = as.POSIXct(DATE, format = "%Y-%m-%d %H:%M:%S"))
# Convert pressure into kPa
Baro <- Pease %>% mutate(Baro.kPa = as.numeric(gsub(x = HourlyStationPressure, pattern = "s", replacement = "")) * 3.38639) %>%
  select("DateTime", "Baro.kPa")

# Convert datetime into the same format as other data
Baro <- Baro %>% mutate(DateTime15 = as.POSIXct(round(as.numeric(DateTime)/900)*900,origin='1970-01-01',tz="EST"))
dateseq <- data.frame(DateTime15 = seq(from = Baro$DateTime15[1], to = Baro[nrow(Baro),]$DateTime15, by = 15*60))
Baro2 <- left_join(dateseq, Baro[,c("DateTime15", "Baro.kPa")], by = "DateTime15")

# Rename pressure columns and save a file with only the relevant data
Baro3 <- data.frame(approx(Baro2$DateTime15, Baro2$Baro.kPa, xout = Baro2$DateTime15, rule = 2, method = "linear", ties = "mean"))
names(Baro3) <- c("DateTime", "Baro.kPa")

write.csv(Baro3, file = "Data/Baro/Pease_Interp_2022-11-19.csv", row.names = FALSE)


## Join all data

CLGB_AG <- list(stage, cond, SUNA, Turner) %>% reduce(left_join, by = "DateTime")

CLGB_AG <- CLGB_AG %>% select("DateTime", "Temp.C_stage", "Discharge.m3s_NEW_stage", "Temp.C_cond":"Temp.C_Turner")

savename <- paste(dataloc, "CLGB_AG_", Sys.Date(), ".csv", sep = "")
write.csv(file = savename, CLGB_AG, row.names = FALSE)



# Part 2: c-Q plots -------------------------------------------------------

#C~Q Plots
Storm <- CLGB_AG %>% slice(2186:2240) %>% filter(SUNA_Error < 0.005) %>% select("DateTime", "Discharge.m3s_NEW_stage", "SpCond.uScm_cond", "Nitrate_mg") %>% mutate(Event = "August") %>%
  rbind(CLGB_AG %>% slice(9153:9310) %>% filter(SUNA_Error < 0.005) %>% select("DateTime", "Discharge.m3s_NEW_stage", "SpCond.uScm_cond", "Nitrate_mg") %>% mutate(Event = "November"))

Storm$Limb<-NA
Storm[0:14,]$Limb <- "Rising"
Storm[15:55,]$Limb <- "Falling"
Storm[56:78,]$Limb <- "Rising"
Storm[79:201,]$Limb <- "Falling"

Storm <- rbind(Storm[c(1:89, NA, 90:nrow(Storm)),])
Storm[90,]$DateTime <- as.POSIXct('2022-11-12 05:30:00', origin = '1970-01-01')



Events <- ggplot(data = Storm)+
  geom_line(aes(x = DateTime, y = Nitrate_mg, color = Limb))+
  #geom_line(aes(x = DateTime, y = Discharge.m3s_NEW_stage*1000, color = Limb))+
  #geom_line(aes(x = DateTime, y = SpCond.uScm_cond/1000, color = Limb))+
  facet_wrap(~Event, scales = "free", nrow = 2)+
  theme_bw()
Events


NO3Q <- ggplot(data = Storm)+
  geom_point(aes(x = Discharge.m3s_NEW_stage, y = Nitrate_mg, color = Limb))+
  geom_line(aes(x = Discharge.m3s_NEW_stage, y = Nitrate_mg, group = Limb, color = Limb))+
  labs(x = expression(bold(Discharge~(m^"3"~s^"-1"))), y = expression(bold(NO["3"]~(mg~L^"-1"))))+
  facet_wrap(~Event, scales = "free", nrow = 2)+
  theme_bw()
NO3Q

ggsave(file = paste0("Plots/NO3Q_", Sys.Date(), ".png"), NO3Q)

condQ <- ggplot(data = Storm)+
  geom_point(aes(x = Discharge.m3s_NEW_stage, y = SpCond.uScm_cond, color = Limb))+
  geom_line(aes(x = Discharge.m3s_NEW_stage, y = SpCond.uScm_cond, group = Limb, color = Limb))+
  labs(x = expression(bold(Discharge~(m^"3"~s^"-1"))), y = expression(bold(Sp.~Conductance~(mu*S~cm^"-1"))))+
  facet_wrap(~Event, scales = "free", nrow = 2)+
  theme_bw()
condQ

ggsave(file = paste0("Plots/SpCondQ_", Sys.Date(), ".png"), condQ)








# Part 3: Diagnostics plots -----------------------------------------------

## Diagnostic plots

TurnerLong <- Turner %>% pivot_longer(cols = "Rhodamine.ppb_Turner":"Temp.C_Turner")

CDOM <- ggplot(data = TurnerLong[TurnerLong$name == "CDOM.ppb_Turner",])+
  geom_line(aes(x=DateTime, y = value))+
  #facet_wrap(~name, scales = "free_y")+
  labs(x = expression(bold(DateTime)), y = expression(bold(CDOM~(ppb))))+
  theme_bw()
CDOM

ggsave(file = paste0("Plots/CDOM_", Sys.Date(), ".png"), CDOM)

Turb <- ggplot(data = TurnerLong[TurnerLong$name == "Turbidity.NTU_Turner",])+
  geom_line(aes(x=DateTime, y = value))+
  #facet_wrap(~name, scales = "free_y")+
  labs(x = expression(bold(DateTime)), y = expression(bold(Turbidity~(NTU))))+
  theme_bw()
Turb

ggsave(file = paste0("Plots/Turbidity_", Sys.Date(), ".png"), Turb)


Turner <- ggplot(data = TurnerLong)+
  geom_line(aes(x=DateTime, y = value))+
  facet_wrap(~name, scales = "free_y")+
  #labs(x = expression(bold(DateTime)), y = expression(bold(CDOM~(ppb))))+
  theme_bw()
Turner

ggsave(file = paste0("Plots/Turner_", Sys.Date(), ".png"), Turner)


NO3 <- ggplot(data = SUNA[SUNA$SUNA_Error < 0.005,])+
  geom_line(aes(x=DateTime, y = Nitrate_mg))+
  
  theme_bw()
NO3

ggsave(file = paste0("Plots/NO3_", Sys.Date(), ".png"), NO3)


stageHeight <- ggplot(data=stage)+
  geom_line(aes(x=DateTime, y = Depth.m_stage))+
  labs(x = expression(bold(DateTime)), y = expression(bold(stage~Height~(m))))+
  theme_bw()
stageHeight

ggsave(file = paste0("Plots/stage_", Sys.Date(), ".png"), stageHeight)


condPlot <- ggplot(data=cond)+
  geom_line(aes(x=DateTime, y = SpCond.uScm_cond))+
  labs(x = expression(bold(DateTime)), y = expression(bold(Sp.~Cond~(uS~cm^-1))))+
  theme_bw()
condPlot

ggsave(file = paste0("Plots/Cond_", Sys.Date(), ".png"), condPlot)



tmp <- ggplot()+
  geom_line(data=SUNA[SUNA$SUNA_Error < 0.005,], aes(x = DateTime, y = Nitrate_mg, color = "NO3"))+
  geom_line(data=cond, aes(x=DateTime, y = SpCond.uScm_cond/1000, color = "cond"))+
  scale_y_continuous(sec.axis = sec_axis(~ . *1000))+
  theme_bw()

tmp

ggsave(file = paste0("Plots/tmp_", Sys.Date(), ".png"), tmp)






