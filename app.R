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

cities <- setNames(directory$Path,directory$City)



ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("city_name", "City", choices = cities,width = "100%")
    )
  ),
  fluidRow(
    column(12, tableOutput("check"))
  )
)


server <- function(input, output, session) {
  path <- reactive(input$city_name)
  
  output$check <- renderText({
    read_csv(paste0("'",path(),"'"))
  })
  #path <- renderText({
  #  city()
    #fread((directory %>% filter(City==input$city_name))$Path)
  #})
  #output$time_series <- renderTable(
  #  summary(path)
  #)
}
shinyApp(ui, server)

