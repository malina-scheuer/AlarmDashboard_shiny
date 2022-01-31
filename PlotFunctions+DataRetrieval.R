# ---------- Packages --------------------------------------------------------------------

# required packages
library(dplyr)
library(stringr)
library(ggplot2)

# ---------- Load data --------------------------------------------------------------------

load("data/data_clean.Rdata") # as data

# ---------- Remove 2 months (Only keep Sept, Oct & Nov data) ----------------------------------------------------

data <- data %>%
  filter(Date > '2019-09-01')

# ---------- Aggregate data (Summarize alarms per day) --------------------------------------------------------------------

# Summarizing alarms per day
data_days <- data %>%
  group_by(Date) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE) %>%
  dplyr::rename(Alarms_total = Alarm)

Beds_grouped <- data %>%
  group_by(Date) %>%
  summarise(Beds_total = length(unique(Bed))) 

  data_days <- right_join(data_days,Beds_grouped,by='Date')

  remove(Beds_grouped)

# Calculating alarms per bed
data_days$Alarms_per_bed <- round(data_days$Alarms_total/data_days$Beds_total)

# Calculating alarm floods
data$TenMinuteIndex = as.factor(data$TenMinuteIndex)

Alarmfluten <- data %>%
  group_by(Date, TenMinuteIndex) %>%
  summarise(Alarmfloods = n()) %>%
  filter(Alarmfloods >= 10) %>%
  group_by(Date) %>%
  summarise(Alarmfloods = n()) %>%
  select(Alarmfloods)

  data_days <- cbind(data_days, Alarmfluten)

  remove(Alarmfluten)

# ---------- Rename variables for plotting -------------------------------------

# Renaming of device variable names
data_days <- data_days %>%
  dplyr::rename(
    ac_Yellow_alarms = Yellow_alarms,
    ac_Red_alarms = Red_alarms,
    d_IBP = IBP,
    d_Beatmung = Ventilator,
    d_EKG = ECG,
    d_SpO2 = SpO2,
    d_NIBP = NIBP,
    d_Temperatur = Temperature,
    d_ICP = ICP,
    s_2_Afternoon = Afternoon,
    s_3_Night = Night,
    s_1_Morning = Morning)

# Changing device factor levels (English to German)
data$Device <- recode_factor(data$Device, 
                             "ECG" = 'EKG',
                             "Ventilator" = 'Beatmung',
                             "Temperature" = "Temperatur")

# ---------- Plot style --------------------------------------------------------------------

##### Plot theme

set_plot_theme <- function(){
  alarm_theme <- theme(
    text = element_text(size = 16, vjust = 0, family ="Helvetica-Bold"),
    plot.title = element_text(size = 17, face="bold", hjust = 0, family ="Helvetica",
                              margin = margin(b=10)),
    axis.title = element_blank(),
    axis.text.y = element_text(size=12, family="Helvetica"),
    axis.text.x = element_text(size=12, family="Helvetica")
  )
  
  theme_set(alarm_theme)
}

set_plot_theme()

# ---------- Plot functions --------------------------------------------------------------------

#------ Alarms over time

alarms_overtime <- function(data,start,end,textsize){
  
  plot <- data %>%
    filter(Date >= start & Date <= end) %>%
    select(Date, Alarmfloods, Technical_failure, Alarms_per_bed) 
  
  max_yaxis = max(plot$Alarms_per_bed) + max(plot$Alarms_per_bed)*0.1
  
  # plot
  ggplot(plot, aes(x=Date)) + 
    #geom_bar(position="stack", stat="identity", width=0.7) +
    geom_line(aes(y = Alarms_per_bed,colour = "Alarme pro Bett", linetype="Alarme pro Bett"),size=1.3) +
    geom_line(aes(y = Alarmfloods, colour = "Alarmfluten", linetype="Alarmfluten"),size=1.3) +
    geom_line(aes(y = Technical_failure, colour = "Techn. Störungen", linetype="Techn. Störungen"),size=1.3) +
    
    scale_color_manual("Variabler", breaks = c("Alarme pro Bett","Alarmfluten","Techn. Störungen"),  
                       values = c("Alarme pro Bett" = "darkgoldenrod2","Alarmfluten" = "red", "Techn. Störungen"="navy")) + 
    scale_linetype_manual("Variabler",values=c("Alarme pro Bett"=1,"Alarmfluten"=5,"Techn. Störungen"=3))+
    
    scale_x_date(date_labels="%d.%b", date_breaks  ="1 day") +
    scale_y_continuous(limits=c(0, max_yaxis), 
                       breaks  = seq(0,max_yaxis, by = 25))+
    
    ylab("Tägliche Anzahl an Alarmen\n") + 
    xlab("\nAusgewählte Daten") +
    
    theme(
      axis.text.x = element_text(size=textsize, family="Helvetica"),
      axis.text.y = element_text(size=textsize, family="Helvetica"),
      axis.title = element_text(size = textsize, vjust = 0, family ="Helvetica-Bold"),
      legend.text = element_text(size=textsize),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.direction="horizontal",
      legend.key.width = unit(1.7, 'cm'),
      legend.background=element_blank(), 
      legend.key=element_blank())
}

alarms_overtime(data_days, start = '2019-09-15',end = '2019-09-25',12)

#------ Alarms per device

plot_d <- function(data,startswith,start,end,colour,textsize){
  
  plot <- data %>%
    filter(Date >= start & Date <= end) %>%
    select(starts_with(startswith)) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    t %>% as.data.frame() %>%
    dplyr::rename("n" = "V1") %>%
    tibble::rownames_to_column()
  
  plot$rowname <- str_remove(plot$rowname, startswith)
  plot$rowname <- str_replace(plot$rowname, "_"," ")
  
  max_axis = (max(plot$n)+max(plot$n)*0.1)
  
  # plot
  ggplot(plot, aes(x=factor(rowname), y=n)) + 
    geom_bar(position="stack", stat="identity", width=0.7, fill=colour) +
    geom_text(aes(label = n), color = "black", size = 4.5, vjust = -0.7) + #position = position_stack(
    #coord_flip() +
    
    scale_y_continuous(limits=c(0, max_axis))+

    theme(
      axis.text.x = element_text(size=textsize, family="Helvetica"),
      axis.text.y = element_text(size=textsize),
      legend.text = element_text(size=textsize))
}

#plot_d(data = data_days, startswith = "d_", start = '2019-09-15',end='2019-09-16',"#4682B4", textsize = 13)

#------ Alarms per colour (including device filter)

plot_ac_fil <- function(data,device,start,end,colour,textsize){
  
  plot <- data %>%
    filter(Date >= start & Date <= end) %>%
    filter(Device == device) %>%
    group_by(Alarmtype) %>% 
    summarise(n = length(Alarmtype))
  
  # include blue if there is blue alarms
    colend = nrow(plot)
  
  # rename factor
  plot$Alarmtype <- recode_factor(plot$Alarmtype, 
                                "rot" = 'Rot (kritisch)',
                                "gelb" = 'Gelb (unkritisch)')
     
  # plot
  ggplot(plot, aes(x=Alarmtype, y=n)) + 
    geom_bar(position="stack", stat="identity", width=0.7, fill=colour[0:colend]) +
    geom_text(aes(label = n), color = "white", size = 5, position = position_stack(vjust = 0.5)) +

    ylab("Anzahl an Alarmen\n") +
    
  theme(
    axis.text.x = element_text(size=textsize, family="Helvetica"),
    axis.text.y = element_text(size=textsize))
}

#plot_ac_fil(data=data,device="IBP",start = '2019-09-15',end = '2019-09-16',list('#F0BA1A','#D82E3F','#3581D8'),20) #yel,re,bl

#------ Alarms per shift (including device filter)

plot_s_fil <- function(data,device,start,end,colour,textsize){
  
  plot<-data[!(data$Shift=="NachtFrüh" | data$Shift=="FrühSpät" | data$Shift=="SpätNacht"),]  # filter Shifts
  
  plot <- plot %>%
    filter(Date >= start & Date <= end) %>%
    filter(Device == device) %>%
    group_by(Shift) %>% 
    summarise(n = length(Shift))
  
  # reorder factor levels (Shifts)
  plot$Shift = factor(plot$Shift, c('Früh', 'Spät', 'Nacht'))
  plot$Shift <- recode_factor(plot$Shift, 
                                "Früh" = 'Frühschicht', 
                                "Spät" = 'Spätschicht',
                                "Nacht" = 'Nachtschicht')

  # include blue if there is blue alarms
  colend = nrow(plot)
  
  # plot
  ggplot(plot, aes(x=Shift, y=n)) + 
    geom_bar(position="stack", stat="identity", width=0.7, fill=colour[0:colend]) +
    geom_text(aes(label = n), color = "white", size = 5, position = position_stack(vjust = 0.5)) +
    
    theme(
      axis.text.x = element_text(size=textsize, family="Helvetica"),
      axis.text.y = element_text(size=textsize),
    )
}

#plot_s_fil(data, device="IBP", start = '2019-09-15',end = '2019-09-16',c("#074274","#5195cf","#3c75a6"),14)
