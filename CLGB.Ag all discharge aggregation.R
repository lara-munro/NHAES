# Aggregate Q data from 2022-2025


dat2022 <- read.csv(paste0(dataloc,"/CLGB.AG/data/stage/y2022/finalData/CLGB.AG_2022_DISCHARGE.csv"))
dat2023 <- read.csv(paste0(dataloc,"/CLGB.AG/data/stage/y2023/finalData/CLGB.AG_2023_DISCHARGE.csv"))
dat2024 <- read.csv(paste0(dataloc,"/CLGB.AG/data/stage/y2024/finalData/CLGB.AG_2024_DISCHARGE.csv"))
dat2025 <- read.csv(paste0(dataloc,"/CLGB.AG/data/stage/y2025/finalData/CLGB.AG_2025_DISCHARGE.csv"))

dat.all <- rbind(dat2022, dat2023)
dat.all <- rbind(dat.all, dat2024)
dat.all <- rbind(dat.all, dat2025)
dat.all <- dat.all[!duplicated(dat.all),]

write.csv(dat.all, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2022-2025_DISCHARGE.csv"), row.names = F)

dat.all$DateTime.EST <- as.POSIXct(dat.all$DateTime, tz = "EST",
                              format = "%Y-%m-%d %H:%M")

plot(dat.all$DateTime.EST, dat.all$Q.m3s, type = "l", log = "y",
     ylim = c(1E-5, 2),
     ylab = "Discharge (m3/s)", xlab = "Date")

qmeas <- read.csv(paste0(dataloc, "CLGB.AG/data/ratingCurve/discharge_measurements.csv"))
qmeas$DateTime.EST <- as.POSIXct(paste(qmeas$date, qmeas$time), tz = "EST",
                                 format = "%Y-%m-%d %H:%M")
qmeas <- subset(qmeas, select =c("DateTime.EST", "measured.Q.m3s"))
dat.all <- merge(dat.all, qmeas, by = "DateTime.EST", all.x = TRUE, all.y = FALSE)
dat.all$Q.m3s[dat.all$Q.m3s < 1e-10] <- NA


ggplot(dat.all, aes(x=DateTime.EST)) +
  geom_line(aes(y = Q.m3s))+
  geom_point(aes(y = measured.Q.m3s,  color = "measured Q"))+
  scale_x_datetime(name = "Date") +
  scale_y_log10(limits = c(0.0001, 1))+
  ylab("Discharge (m3/s)")+
#  ylim(c(0.0001, 0.5))+
  theme_classic()

