#####################################################
#### Processing of Weather Station Data ####
#####################################################

### When adding new data to Atmos41_weather dataset ###
# 1. add csv to list of weather files
# 2. adjust version number of output

# Load required libraries
library(dplyr)
library(lubridate)

# Directories
dirData <- "G:/My Drive/research/projects/Data/campus_weather/ham_weather"
dirOut <- "G:/My Drive/research/projects/Data/campus_weather/ham_weather_out"

### Function to Read and Process CSV Files  ###

process_weather_data <- function(file_path) {
  # Read CSV file (skip metadata rows)
  weather_data <- read.csv(file_path, skip = 3, header = FALSE)
  
  # Define column names
  cols <- c("Date", "SolRad", "Precip", "LightningAct", "LightningDist", "WindDir", 
                   "WindSpeed", "GustSpeed", "AirTemp", "VaporPr", "AtmosPr", "XLevel", 
                   "YLevel", "MaxPrecip", "SensorTemp", "VPD", "BatPct", "BatVolt", 
                   "RefPr", "LogTemp", "SWC", "SoilTemp", "EC")
  
  # Assign column names
  colnames(weather_data) <- cols
  
  # Convert date column
  weather_data$dateF <- mdy_hms(weather_data$Date)
  
  # Add additional time columns
  weather_data <- weather_data %>%
    mutate(doy = yday(dateF),
           hour = hour(dateF),
           year = year(dateF))
  
  return(weather_data)
}

### Load and Combine All Weather Data ###

# List of weather files
weather_files <- list(
  list(file = "/v1.6/Atmos41_weather.csv", is_new = FALSE),  # Existing dataset
  list(file = "new.csv", is_new = TRUE)               # New dataset
)

# Read and merge datasets 
weather_list <- lapply(weather_files, function(x) {
  data <- process_weather_data(paste0(dirData, "/", x$file))
  
  # If new dataset, remove first day
  if (x$is_new) {
    first_day <- min(data$doy, na.rm = TRUE)  # Get earliest day in new dataset
    first_year <- min(data$year, na.rm = TRUE)  # Get corresponding year
    data <- data %>% filter(!(doy == first_day & year == first_year))  # Remove first day
  }
  
  return(data)
})

# Combine all weather data into one dataset
weather_data_combined <- bind_rows(weather_list)

### Data flags ###
# create a precipitation flag:
# precip flag values:
# debris indicates an excessive build up of debris renders precip values unreliable
# freeze indicates that temps are at below 2 degrees C or today or in the last week
# and the user should exercise caution around precipitation values to ensure that
# there is no accumulation of frozen precipitation. 

# enter name of current data frame
weatherToFlag <- weather_data_combined

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

PFlag <- data.frame(col_names = c("PrecipFlag"),
                    col_units = c("Freeze = freezing warning, Debris= blocked bucket"))
colInfo <- rbind(colMeta[1:3,], PFlag, colMeta[4:23,])

### Save Data ###

write.csv(weatherOut, paste0(dirOut,"/v1.7/Atmos41_weather.csv"), row.names=FALSE)
write.csv(colInfo, paste0(dirOut,"/v1.7/Atmos41_metadata_columns.csv"), row.names=FALSE)


weatherOut$dateF <-  mdy_hms(weatherOut$Date)
ggplot(weatherOut, aes(dateF, SolRad))+
  geom_line()
