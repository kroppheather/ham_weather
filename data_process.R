#####################################################
#### Processing of weather station               ####
#####################################################

# organizes data into a csv with accompanying metadata file

############### libraries and directories ----
library(dplyr)
library(lubridate)
library(ggplot2)

dirData <- "G:/My Drive/research/projects/Data/campus_weather/ham_weather"
dirOut <- "G:/My Drive/research/projects/Data/campus_weather/ham_weather_out"

############### Organization of old data -----

# original data
weather1 <- read.csv(paste0(dirData,"/z6-10463(z6-10463)-1705432897/z6-10463(z6-10463)-Configuration 1-1705432897.2769604.csv"),
                    skip=3, header=FALSE)

colnames(weather1) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                       "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                       "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")

dateF1 <- mdy_hms(weather1$Date)
weather1$doy <- yday(dateF1)
weather1$hour <- hour(dateF1)
weather1$year <- year(dateF1)

# data around the soil sensor getting plugged in
weather2 <- read.csv(paste0(dirData,"/z6-10463(z6-10463)-1705433977/z6-10463(z6-10463)-Configuration 2-1705433977.5088146.csv"),
                     skip=3, header=FALSE)
colnames(weather2) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                        "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                        "SensorTemp","VPD","SWC","SoilTemp", "EC", "BatPct","BatVolt","RefPr","LogTemp")

dateF2 <- mdy_hms(weather2$Date)
weather2$doy <- yday(dateF2)
weather2$hour <- hour(dateF2)
weather2$year <- year(dateF2)

# rearrange weather 1 to match new configuration

weather1R <- weather1[,1:16]
weather1R$SWC <- rep(NA, nrow(weather1R))
weather1R$SoilTemp <- rep(NA, nrow(weather1R))
weather1R$EC <- rep(NA, nrow(weather1R))
weather1R <- cbind(weather1R, weather1[,17:23])

# bind weather 
weatherT1 <- rbind(weather1R, weather2)

# last day checks 
weatherCheck1 <- weatherT1 %>%
  filter(doy == 264 & year == 2023)
weatherCheck1 <- weatherT1 %>%
  filter(doy == 263 & year == 2023)

# read in rest of fall
weather3 <- read.csv(paste0(dirData,"/z6-10463(z6-10463)-1705432949/z6-10463(z6-10463)-Configuration 1-1705432949.5836954.csv"),
                     skip=3, header=FALSE)
colnames(weather3) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                        "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                        "SensorTemp","VPD","SWC","SoilTemp", "EC", "BatPct","BatVolt","RefPr","LogTemp")

dateF3 <- mdy_hms(weather3$Date)
weather3$doy <- yday(dateF3)
weather3$hour <- hour(dateF3)
weather3$year <- year(dateF3)

weather3 <- weather3 %>%
  filter(doy != 264)

weatherT2 <- rbind(weatherT1, weather3)
weatherT2copy <- weatherT2
weatherT2copy$dateF <-  mdy_hms(weatherT2copy$Date)
ggplot(weatherT2copy, aes(dateF, AirTemp))+
  geom_line()

ggplot(weatherT2copy, aes(dateF, Precip))+
  geom_line()

weather4 <- read.csv(paste0(dirData,"/z6-10463(z6-10463)-1705432981/z6-10463(z6-10463)-Configuration 1-1705432981.9531858.csv"),
                     skip=3, header=FALSE)
colnames(weather4) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                        "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                        "SensorTemp","VPD","SWC","SoilTemp", "EC", "BatPct","BatVolt","RefPr","LogTemp")

dateF4 <- mdy_hms(weather4$Date)
weather4$doy <- yday(dateF4)
weather4$hour <- hour(dateF4)
weather4$year <- year(dateF4)

weather4 <- weather4 %>%
  filter(doy != 348)

weatherT3 <- rbind(weatherT2, weather4)

# read in column names and units
colNamesDF <- read.csv(paste0(dirData,"/z6-10463(z6-10463)-1705432981/z6-10463(z6-10463)-Configuration 1-1705432981.9531858.csv"),
                      nrows=3, header=FALSE)
units <- unname(unlist(colNamesDF[3,])) 

colMeta <- data.frame(col_names= c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                        "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                        "SensorTemp","VPD","SWC","SoilTemp", "EC", "BatPct","BatVolt","RefPr","LogTemp"),
                      col_units = units)

############### Script for monthly addition of new data ----
weather5 <- read.csv(paste0(dirData,"/z6-10463(z6-10463)-1713299543/z6-10463(z6-10463)-Configuration 1-1713299544.0092711.csv"),
                     skip=3, header=FALSE)
colnames(weather5) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                        "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                        "SensorTemp","VPD","SWC","SoilTemp", "EC", "BatPct","BatVolt","RefPr","LogTemp")

dateF5 <- mdy_hms(weather5$Date)
weather5$doy <- yday(dateF5)
weather5$hour <- hour(dateF5)
weather5$year <- year(dateF5)

weather5 <- weather5 %>%
  filter(doy != 15)

weatherT4 <- rbind(weatherT3, weather5)

# end date June 16

weather6 <- read.csv(paste0(dirData,"/z6-10463(z6-10463)-1718648015/z6-10463(z6-10463)-Configuration 1-1718648015.935887.csv"),
                     skip=3, header=FALSE)
colnames(weather6) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                        "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                        "SensorTemp","VPD","SWC","SoilTemp", "EC", "BatPct","BatVolt","RefPr","LogTemp")

dateF6 <- mdy_hms(weather6$Date)
weather6$doy <- yday(dateF6)
weather6$hour <- hour(dateF6)
weather6$year <- year(dateF6)

weather6 <- weather6 %>%
  filter(doy != 106)

weatherT5 <- rbind(weatherT4, weather6)


############### Data flags:
# create a precipitation flag:
# precip flag values:
# debris indicates an excessive build up of debris renders precip values unreliable
# freeze indicates that temps are at below 2 degrees C or today or in the last week
# and the user should exercise caution around precipitation values to ensure that
# there is no accumulation of frozen precipitation. 

# enter name of current data frame
weatherToFlag <- weatherT5

# freeze flag 
freezeFlag <- rep(NA,7)
for(i in 8:nrow(weatherToFlag)){
  freezeFlag[i] <- ifelse(length(which(weatherToFlag$AirTemp[(i-6):i] <=2)) > 0,
                          1,0)
                          
}

Precipflag <- ifelse(freezeFlag == 1, "Freeze", NA) 

                     
# flag for recorded excessive debris in weather station
weatherToFlag$PrecipFlag <- ifelse(weatherToFlag$doy >= 121 & weatherToFlag$doy <= 188 & weatherToFlag$year == 2021,  "Debris", Precipflag)
 

# reorganize 

weatherOut <- cbind(weatherToFlag[,1],weatherToFlag[,24:26],
                    weatherToFlag[,2:3], weatherToFlag[,27],
                    weatherToFlag[,4:23])

colnames(weatherOut)[1] <- "Date"
colnames(weatherOut)[7] <- "PrecipFlag"



# metadata out
PFlag <- data.frame(col_names = c("PrecipFlag"),
                    col_units = c("Freeze = freezing warning, Debris= blocked bucket"))
colInfo <- rbind(colMeta[1:3,], PFlag, colMeta[4:23,])

write.csv(weatherOut, paste0(dirOut,"/v1.3/Atmos41_weather.csv"), row.names=FALSE)
write.csv(colInfo, paste0(dirOut,"/v1.3/Atmos41_metadata_columns.csv"), row.names=FALSE)


weatherOut$dateF <-  mdy_hms(weatherOut$Date)
ggplot(weatherOut, aes(dateF, SolRad))+
  geom_line()
