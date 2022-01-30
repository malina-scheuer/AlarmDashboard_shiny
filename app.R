# required packages
library(shiny)
require(shinydashboard)

require(ggplot2)
require(dplyr)
require(stringr)

# load data & plot functions
source("PlotFunctions+DataRetrieval.R")

# icons source: glyphicon (https://www.glyphicons.com/sets/basic/)

#################### SERVER ################################ 

# server functions
server <- function(input, output) { 
  
  ########### KPI boxes ####################
  
  # Total Amount of alarms (box)
  output$Alarms_total <- renderValueBox({
    valueBox(
      "Alarme",
      value = tags$p({
        Range <- data_days %>% filter(data_days >= as.character(input$startdate) & data_days <= as.character(input$enddate))
        sum(Range$Alarms_total, na.rm = TRUE) },
        style = "font-size: 80%;"),
      icon = icon("bell", lib = "glyphicon"), #https://getbootstrap.com/docs/3.4/components/#glyphicons
      color = "light-blue")
  })
  
  # Total Alarm floods (box)
  output$Alarmfloods <- renderValueBox({
    valueBox(
      "Alarmfluten",
      #value = tags$p({alarmfloods(data, input$startdate, input$enddate)},
      value = tags$p({
        Range <- data_days %>% filter(data_days >= as.character(input$startdate) & data_days <= as.character(input$enddate))
        round(sum(Range$Alarmfloods, na.rm = TRUE))},
        style = "font-size: 80%;"),
      icon = icon("arrow-thin-up", lib = "glyphicon"),
      color = "red")
  })
  
  # Total Technical failures (box)
  output$TechnicalFailures <- renderValueBox({
    valueBox(
      "Technische Störungen",
      value = tags$p({
        Range <- data_days %>% filter(data_days >= as.character(input$startdate) & data_days <= as.character(input$enddate))
        round(sum(Range$Technical_failure, na.rm = TRUE))},
        style = "font-size: 80%;"),
      icon = icon("wrench", lib = "glyphicon"),
      color = "navy")
  })
  
  # Avg Alarms per day (box)
  output$Alarms_per_day <- renderValueBox({
    valueBox(
      "Alarme pro Tag",
      value = tags$p({
        Range <- data_days %>% filter(data_days >= as.character(input$startdate) & data_days <= as.character(input$enddate))
        round(sum(Range$Alarms_total)/nrow(Range))},
      style = "font-size: 80%;"),
      icon = icon("bell", lib = "glyphicon"), #https://getbootstrap.com/docs/3.4/components/#glyphicons
      color = "light-blue")
  })
  
  # Avg Alarms per bed (box)
  output$Alarms_per_bed <- renderValueBox({
    valueBox(
      "Alarme pro Bett",
      value = tags$p({
        Range <- data_days %>% filter(data_days >= as.character(input$startdate) & data_days <= as.character(input$enddate))
        round(mean(Range$Alarms_per_bed, na.rm = TRUE))},
        style = "font-size: 80%;"),
      icon = icon("bed", lib = "glyphicon"),
      color = "yellow")
  })
  
  ########### Alarm plots ############### 

  # Alarms over time (plot)
  output$alarms_overtime <- renderPlot(
    alarms_overtime(data_days,input$startdate,input$enddate,input$textsize) 
  )
  
  # Alarms per device (plot)
  output$plot_d <- renderPlot(
    plot_d(data_days,"d_",input$startdate,input$enddate,"#4682B4",input$textsize) 
  )
  
  # Alarms per colour (plot)
  output$plot_ac_fil <- renderPlot(
    plot_ac_fil(data,input$device,input$startdate,input$enddate,list('#F0BA1A','#D82E3F','#3581D8'),input$textsize)
  )
  
  # Alarms per shift (plot)
  output$plot_s_fil <- renderPlot(
    plot_s_fil(data,input$device,input$startdate,input$enddate,c("#074274","#5195cf","#3c75a6"),input$textsize) 
  )
}

# ===================== UI =================================== 

# ======== HEADER ===========
header <- dashboardHeader(title = "ICU Alarm Dashboard")  

# ======== SIDEBAR ==========
sidebar <- dashboardSidebar(
  sidebarPanel(width = 16, id="sidebar"
  
  ,dateInput(
      inputId = "startdate",
      label = "Beginn:",
      value = min(data_days$Date),
      min = min(data_days$Date),
      max = max(data_days$Date),
      format = "dd-mm-yyyy", startview = "month", weekstart = 0,
      language = "de", 
      autoclose = TRUE,
      width="100%")
  
  ,dateInput(
      inputId = "enddate",
      label = "Ende:",
      value = min(data_days$Date)+7,
      min = min(data_days$Date),
      max = max(data_days$Date),
      format = "dd-mm-yyyy", startview = "month", weekstart = 0,
      language = "de",
      autoclose = TRUE,
      width="100%")

  ,sliderInput(
      inputId = "textsize",
      label = "Schriftgröße der Achsen:",
      min = 5, max = 19, value = 12, step = 1, round = FALSE,
      sep = "us", ticks = TRUE, animate = FALSE)
  
  ,style = "background-color: #222d32;border-color: #222d32;")
  
  ,tags$style(HTML(".datepicker {z-index:99999 !important;}"))
)

# ============ BODY ============================ 

# ==== KPI Boxes 1 (totals) ==============

KPItitle1 <- h4("Gesamtzahl\n")

  KPIs1 <- fluidRow(
    valueBoxOutput("Alarms_total", width=3)
    ,valueBoxOutput("Alarmfloods",width=3)
    ,valueBoxOutput("TechnicalFailures", width=3)
    ,style = "background-color: #ecf0f5;" #"height:200px;
  )

# ==== KPI Boxes 2 (averagges) ==============

  KPItitle2 <- h4("Durchschnittswerte\n")

  KPIs2 <- fluidRow(
    valueBoxOutput("Alarms_per_day", width=3)
    ,valueBoxOutput("Alarms_per_bed", width=3)
    ,style = "background-color: #ecf0f5;" #"height:200px;
  )

# ==== Alarm plots ==========================

# Alarm overview (plot)
overviewplot <- box(
  title = "Alarme im Zeitverlauf", status = "primary",
  plotOutput("alarms_overtime",height=390),
  width=250
)

# Alarms per device (plot)
deviceplot <- box(
  title = "Alarme pro Gerät", status = "primary",
  plotOutput("plot_d",height=300),
  width=250
)

# Select device (input field)
selplot <- box(
  tabItem(tabName="device",
          selectInput(
            inputId = "device",
            label = "Gerät auswählen:",
            choices =  list("EKG","IBP","ICP","NIBP","SpO2","Temperatur","Beatmung"), #unique(data$Device)
            width="100%"
          )
  ),
  
    # Alarms per colour (plot)
    box(
      title = "Alarme nach Kritikalität", status = "primary",
      plotOutput("plot_ac_fil",height=290)),
    
    # Alarms per shift (plot)
    box(
      title = "Alarme pro Schicht", status = "primary",
      plotOutput("plot_s_fil",height=290)),
    
    width=250
)

# ==== Putting together the body ==========================

body <- dashboardBody(
  KPItitle1, KPIs1
  ,KPItitle2, KPIs2 
  ,overviewplot
  ,deviceplot
  ,selplot)

# ===== Complete the UI ===================================

# complete UI
ui <- fluidPage(
  
  tags$style("h4 {
    font-size: 0.2;
             }"),
  
  dashboardPage(
  title = 'This is my Page title', header,sidebar, body,skin='black'
                )
)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

shinyApp(ui, server)
