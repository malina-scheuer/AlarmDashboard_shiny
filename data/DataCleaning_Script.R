# ---------- Packages --------------------------------------------------------------------

library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)

# ---------- Import data --------------------------------------------------------------------

# load dataset
data_raw <- get(load("data/data_raw.Rdata")) 
remove(data_tbl)

# ---------- Rename & manipulate variables --------------------------------------------------------------------

# Rename
data <- data_raw %>% 
  dplyr::rename(
               Bed = Bettname,
               Alarmtype = Alarmfarbe,
               Device = Alarmgruppe, # or Alarm
               Shift = Schicht,
               Alarm_generated = Situation) 
  
# Seperate date & time
data <- data %>% 
  mutate(Alarm = as.factor(Alarm)) %>% 
  mutate(Time = as_datetime(TrueTime),
         Date = as.Date(Time),
         dummytime = floor_date(Time, unit = "minutes")) %>%
  select(-c(Zeit, TrueTime))
  
# ---------- Filter dataset (reduce size) --------------------------------------------------------------------

# only keep generated alarms
data <- data %>%
  filter(Alarm_generated == "generiert") 

# ---------- Create seperate variables for devices, colours & shifts -----------------------------------------

# Alarm colours
  data$Yellow_alarms = NA
  data$Yellow_alarms[data$Alarmtype == "gelb"] = 1
  
  data$Red_alarms = NA
  data$Red_alarms[data$Alarmtype == "rot"] = 1
  
  data$Blue_alarms = NA
  data$Blue_alarms[data$Alarmtype == "blau"] = 1

# Devices
  data$IBP = NA
  data$IBP[data$Device == "IBP"] = 1
  
  data$Ventilator = NA
  data$Ventilator[data$Device == "Ventilator"] = 1
    
  data$ECG = NA
  data$ECG[data$Device == "ECG"] = 1
  
  data$SpO2 = NA
  data$SpO2[data$Device == "SpO2"] = 1
  
  data$NIBP = NA
  data$NIBP[data$Device == "NIBP"] = 1
  
  data$Temperature = NA
  data$Temperature[data$Device == "Temperature"] = 1
  
  data$ICP = NA
  data$ICP[data$Device == "ICP"] = 1
  
  data$Thermodilution = NA
  data$Thermodilution[data$Device == "Thermodilution"] = 1
  
  data$Technical_failure = NA
  data$Technical_failure[data$Device == "Technical failure"] = 1

# Shift
  data$Afternoon = NA
  data$Afternoon[data$Shift == "Spät"] = 1

  #data$'Afternoon-Night' = NA
  #data$'Afternoon-Night'[data$Shift == "SpätNacht"] = 1

  data$Night = NA
  data$Night[data$Shift == "Nacht"] = 1
  
  #data$'Night-Morning' = NA
  #data$'Night-Morning'[data$Shift == "NachtFrüh"] = 1
  
  data$Morning = NA
  data$Morning[data$Shift == "Früh"] = 1
  
  #data$'Morning-Afternoon' = NA
  #data$'Morning-Afternoon'[data$Shift == "FrühSpät"] = 1

# Alarm generated
  data$Alarm = NA
  data$Alarm[data$Alarm_generated == "generiert"] = 1
  
# ---------- Ten Min Index for Alarmflood calculation ----------------------------------

# Add ten min index
data <- data %>%
  ungroup() %>%
  mutate(TenMinuteIndex = cut(strptime(Time, format = "%Y-%m-%d %H:%M:%S"),
                              breaks = "10 mins", labels = FALSE))

# ---------- Finalize dataset --------------------------------------------------------
  
# select & arrange columns
data <- data %>%
  select(c(Bed, Date, Time, 
           Alarm,
           Alarmtype, Yellow_alarms, Red_alarms, Blue_alarms,
           Device, IBP,Ventilator,ECG,SpO2,NIBP,Temperature,ICP,Thermodilution,
           Technical_failure,
           Shift, Morning, Afternoon, Night,
           TenMinuteIndex)) 

# ------------------------------------- EXPORT -----------------------------------------------------------

save(data, file = "data/data_clean.Rdata")

