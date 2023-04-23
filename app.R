library(shiny)
library(patchwork)
library(fpp3)
options(digits=2)
library(tseries)
library(readxl)
library(dplyr)
library(pacman)
library(tsibble)
library(tidyverse)

p_load(data.table,lubridate,collapse,ggplot2,fixest,stringr,modelsummary,eeptools)

directory <- read_excel('C:/Users/sabad/OneDrive/Documents/GitHub/UberMovementDataAnalysis/data/directory.xlsx')

print(directory)

sf <- fread('C://Users//sabad//OneDrive//Documents//GitHub//UberMovementDataAnalysis//data//formatted_sanfrancisco.csv')

#print(head(sf))
#print(getwd())

cities <- setNames(directory$Path,directory$City)



ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("city_name", "City", choices = cities,width = "100%")
    )
  ),
  fluidRow(
    column(2,selectInput("y","Y-axis",c("mean_travel_time","trip_count")),
           selectInput("color","Color",c("None","Weekday"))),
    column(6, plotOutput("time_series"))
  )
)



server <- function(input, output, session) {
  path <- reactive(input$city_name)
  
  output$time_series <- renderPlot({
    f <- file.path(path())
    city_data <- fread(f)
    #print(head(city_data))
    setDT(city_data)
    
    if (input$y=="mean_travel_time") {
      avg_times <- city_data[,mean_travel_time:= mean(MeanTravelTimeSeconds),by=Date]
      avg_times <- avg_times %>% select(Date,mean_travel_time) %>% distinct()
      avg_times <- avg_times %>% mutate(day=weekdays(Date))
      avg_times <- slice(avg_times,1:(n()-60))
      
      summary <- reactive(avg_times)
    
      if (input$color=="None") {
        summary() %>%
          ggplot()+
          geom_point(aes(x=Date,y=mean_travel_time),color="cornflowerblue", size = 2, alpha=.8)+
          labs(title="Time plot of mean travel duration")
      } else {
        summary() %>%
          ggplot()+
          geom_point(aes(x=Date,y=mean_travel_time),color=day, size = 2, alpha=.8)+
          labs(title="Time plot of mean travel duration colored by Weekdays")
      }
  
    } else {
      trip_count <- city_data[,count_trips:= .N,by=Date] 
      trip_count <- trip_count %>% select(Date,count_trips) %>% distinct()
      trip_count <- trip_count %>% mutate(day=weekdays(Date))
      trip_count <- slice(trip_count,1:(n()-60))
      
      summary <- reactive(trip_count)
      
      if (input$color=="None") { 
        summary() %>%
          ggplot()+
            geom_point(aes(x=Date,y=count_trips),color="cornflowerblue", size = 2, alpha=.8)+
            labs(labs = "Time plot of trip counts")
      } else {
        summary() %>%
          ggplot()+
            geom_point(aes(x=Date,y=count_trips),color=day, size = 2, alpha=.8)+
            labs(labs = "Time plot of trip counts colored by Weekdays")
      }
    }
  },res = 96
  )
  
 
}
shinyApp(ui, server)

