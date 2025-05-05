# Compare SCAN NO3 data from the 3 sites during their first week


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations
dataloc <- "C:/Users/laram/OneDrive - USNH/"

# load no3 data

ompdno3 <- read.csv(paste0(dataloc, "OMPD/no3/y2025/intermediateData/no3_2025-04-24-to-2025-05-02_ompd.csv"))
ompdno3$DateTime <- as.POSIXct(ompdno3$Timestamp)
ompdno3 <- ompdno3 %>%
  rename(no3.ompd = no3.mgl,
         turb.ompd = turbidity.ftu,
         temp.ompd = temp.c)
ompdno3 <- ompdno3[-1]

upno3 <- read.csv(paste0(dataloc, "CLGB/CLGB.UP/no3/y2025/intermediateData/no3_2025-04-24-to-2025-05-02_clgb_up.csv"))
upno3$DateTime <- as.POSIXct(upno3$Timestamp)
upno3 <- upno3 %>%
  rename(no3.up = no3.mgl,
         turb.up = turbidity.ftu,
         temp.up = temperature.C)
upno3 <- upno3[-1]

agno3 <- read.csv(paste0(dataloc, "CLGB/CLGB.AG/data/no3/y2025/intermediateData/no3_2025-04-24-to-2025-05-02_clgb_ag.csv"))
agno3$DateTime <- as.POSIXct(agno3$Timestamp)
agno3 <- agno3 %>%
  rename(no3.ag = no3.mgl,
         turb.ag = turbidity.ftu,
         temp.ag = temperature.C)
agno3 <- agno3[-1]

no3dat <- merge(ompdno3, upno3, by = "DateTime", all = TRUE)
no3dat <- merge(no3dat, agno3, by = "DateTime", all = TRUE)

# Plot the three values

ggplot(no3dat, aes(x = DateTime))+
  geom_line(aes(y = no3.ompd, color = "OMPD"), linewidth = 0.7)+
  geom_line(aes(y = no3.up, color = "CLGB.Upper"), linewidth = 0.7)+
  geom_line(aes(y = no3.ag, color = "CLGB.Ag"), linewidth = 0.7)+
  theme_classic()+
  ylab("NO3 (mg/l)")

ggplot(no3dat, aes(x = DateTime))+
  geom_line(aes(y = turb.ompd, color = "OMPD"), linewidth = 0.7)+
  geom_line(aes(y = turb.up, color = "CLGB.Upper"), linewidth = 0.7)+
  geom_line(aes(y = turb.ag, color = "CLGB.Ag"), linewidth = 0.7)+
  theme_classic()+
  ylab("Turbidity (FTU)")+
  ylim(c(0,50))

ggplot(no3dat, aes(x = DateTime))+
  geom_line(aes(y = temp.ompd, color = "OMPD"), linewidth = 0.7)+
  geom_line(aes(y = temp.up, color = "CLGB.Upper"), linewidth = 0.7)+
  geom_line(aes(y = temp.ag, color = "CLGB.Ag"), linewidth = 0.7)+
  theme_classic()+
  ylab("Temperature (C)")
  
# Read OMPD Q data (Q data is provisional)

ompd.Q <- read.csv(paste0(dataloc, "OMPD/discharge/y2025/rawData/Q_2025-04-23-to-2025-05-02.csv"))
ompd.Q$DateTime <- as.POSIXct(ompd.Q$datetime)

ompd.Q <- subset(ompd.Q, select = c("DateTime", "discharge.fts"))
ompd.Q$m3s <- ompd.Q$discharge.fts * 0.0283168

ompddat <- merge(ompd.Q, ompdno3, by = "DateTime")

plot(ompddat$m3s, ompddat$no3.ompd, pch = 19, log = "xy",
     xlab = "Q (m3/s)", ylab = "NO3 (mg/l)",
     main = "c-Q OMPD April 24-May 2 2025")

plot(ompddat$DateTime, ompddat$m3s, pch = 19, log = "y",
     xlab = "Date", ylab = "Q (m3/s)",
     main = "Q and OMPD April 24-May 2 2025")
