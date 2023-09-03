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
library(feasts)
library(hash)
library(fable.prophet)
#library(aws.signature)

#install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
#library(aws.s3)

#print(packageVersion("aws.s3"))

#Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAQJJEF7ZOA7H2EQ3F","AWS_SECRET_ACCESS_KEY" = "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",
#           "AWS_DEFAULT_REGION" = "ap-south-1"
#)


p_load(data.table,lubridate,collapse,ggplot2,fixest,stringr,modelsummary,eeptools)

directory <- read_excel('C:/Users/sabad/OneDrive/Documents/GitHub/UberMovementDataAnalysis/directory.xlsx')

#directory <- 
#  save_object("s3://juyeebucket/directory.xlsx") %>% read_excel()


cities <- setNames(directory$Path,directory$City)

linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("city_name", "City", choices = cities,width = "100%")
    )
  ),
  fluidRow(
    column(2,selectInput("y","Y-axis",c("mean_travel_time","trip_count")),
           selectInput("color","Color",c("None","Weekday")),
           selectInput("seasonality","Seasonality",c("Yearly Seasonality","Weekly Seasonality")),
           selectInput("moving_average","Moving Average",c("7-MA","30-MA"))),
    column(8, plotOutput("time_series"))
  ),
  fluidRow(
    column(4,plotOutput("histogram")),
    column(4,plotOutput("correlation_plot")),
    column(4,plotOutput("weekly_stats"))
  ),
  fluidRow(
    column(6,plotOutput("facet_plot")),
    column(6,plotOutput("ma_plot"))
  ),
  fluidRow(
    column(6,plotOutput("seasonality")),
    column(6,plotOutput("acf_plot"))
  ),
  fluidRow(
    column(2,h2("STL Decomposition"),
           selectInput("data_transformation","Data Transformation",c("Original","Log Transform")),
           numericInput("trend_window","Trend Window",31),
           textInput("seasonal_window","Seasonal Window","periodic"),
           selectInput("robust","Robust",c("TRUE","FALSE"))),
    column(6,plotOutput("decomposition")),
    column(4,h4("STL features"),tableOutput("stl_features"))
  ),
  fluidRow(
    column(2,h2("STL Forecasting"),
           selectInput("data","Data Type",c("Weekly Data","Daily data")),
           selectInput("data_transformation1","Data Transformation",c("Original","Log Transform")),
           numericInput("trend_window1","Trend Window",31),
           numericInput("train_percent","Training Percentage",90),
           sliderInput("forecast","Forecast",min=1,max=400,value=90)),
    column(5,plotOutput("stl_training")),
    column(5,plotOutput("stl_forecasting"))
    #column(2,plotOutput("stl_accuracy"))
  ),
  
  fluidRow(
    column(2,h2("ARIMA modelling"),
           selectInput("arima_data","Data Type",c("Weekly Data","Daily data")),
           selectInput("arima_data_transformation","Data Transformation",c("Original","Log Transform")),
           numericInput("seasonal_lag","Seasonal Lag",52)),
    column(2,h2(""),
           h4("Stationarity and Differencing Statistics"),
           tableOutput("arima_differencing_table"),
           tableOutput("arima_double_differencing_table"),
           tableOutput("kpss_test")),
    column(4,plotOutput("differenced_plot")),
    column(4,plotOutput("residual_plot"))
    
  ),
  
  fluidRow(
    column(2,h2("ARIMA forecasting"),
           textInput("pdq","pdq","000"),
           textInput("seasonal_pdq","PDQ","000"),
           sliderInput("arima_forecast","Forecast",min=1,max=400,value=90)),
    column(6,plotOutput("arima_forecasting")),
    column(2,h4("ARIMA Performance metrics"),tableOutput("arima_metrics"))
  ),
  
  fluidRow(
    column(2,h2("Prophet Forecasting"),
           selectInput("prophet_data","Data Type",c("Weekly Data","Daily data")),
           selectInput("prophet_data_transformation","Data Transformation",c("Original","Log Transform")),
           numericInput("prophet_train_percent","Training Percentage",90),
           sliderInput("prophet_forecast","Forecast",min=1,max=400,value=90)
    ),
    column(5,plotOutput("prophet_decomposition")),
    column(5,plotOutput("prophet_residuals"))
  ),
  
  fluidRow(
    column(4,plotOutput("prophet_train_prediction")),
    column(2,h4("Accuracy metrics"),tableOutput("prophet_accuracy")),
    column(6,plotOutput("prophet_forecasting"))
  )
  
)



server <- function(input, output, session) {
  path <- reactive(input$city_name)
  
  city_data <- reactive({
    
    #Code for importing from local directory
    f <- file.path(path())
    city_data <- fread(f)
    setDT(city_data)
    
    #Code for importing from AWS S3
    #city_data_obj <- save_object(path())
    
    #city_data <- city_data_obj %>%
    #  data.table::fread()
    
    #print(head(city_data))
  })
  
  avg_times <- reactive({
    city_data <- city_data()
    
    avg_travel_times <- city_data[,mean_travel_time:= mean(MeanTravelTimeSeconds),by=Date]
    avg_travel_times <- avg_travel_times %>% select(Date,mean_travel_time) %>% distinct()
    avg_travel_times <- avg_travel_times %>% mutate(day=weekdays(Date))
    avg_travel_times <- slice(avg_travel_times,1:(n()-60))
    
  })
  
  trip_count <- reactive({
    city_data <- city_data()
    
    trip_counts <- city_data[,count_trips:= .N,by=Date] 
    trip_counts <- trip_counts %>% select(Date,count_trips) %>% distinct()
    trip_counts <- trip_counts %>% mutate(day=weekdays(Date))
    trip_counts <- slice(trip_counts,1:(n()-60))
  })
  
  avg_times_ts <- reactive({
    avg_times <-  avg_times() %>% mutate(Date_parsed = date(Date)) %>% select(-Date)
    avg_times_ts <- as_tsibble(avg_times,index=Date_parsed)
    avg_times_ts <- fill_gaps(avg_times_ts,mean_travel_time = mean(mean_travel_time))
  })
  
  
  trip_count_ts <- reactive({
    trip_count <-  trip_count() %>% mutate(Date_parsed = date(Date)) %>% select(-Date)
    trip_count_ts <- tsibble(trip_count,index=Date_parsed)
    trip_count_ts <- fill_gaps(trip_count_ts,count_trips = mean(count_trips))
  })
  
  avg_times_week <- reactive({
    avg_times <-  avg_times() %>% mutate(Date_parsed = date(Date)) %>% select(-Date)
    
    avg_times <- avg_times %>% mutate(week=yearweek(Date_parsed))
    
    avg_times_week <- avg_times %>% group_by(week) %>% summarise(mean_travel_time = mean(mean_travel_time))
    
    avg_times_week <- as_tsibble(avg_times_week,index=week)
    
    avg_times_week <- fill_gaps(avg_times_week,mean_travel_time = mean(mean_travel_time))
  })
  
  trip_count_week <- reactive({
    trip_count <-  trip_count() %>% mutate(Date_parsed = date(Date)) %>% select(-Date)
    trip_count <- trip_count %>% mutate(week=yearweek(Date_parsed))
    
    trip_count_week <- trip_count %>% group_by(week) %>% summarise(count_trips = mean(count_trips))
    
    trip_count_week <- as_tsibble(trip_count_week,index=week)
    
    trip_count_week <- fill_gaps(trip_count_week,mean_travel_time = mean(mean_travel_time))
  })
  
  arima_differencing <- reactive({
    
    if(input$y=="mean_travel_time") {
      
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          
          nsdiffs_val <- avg_times_week() %>% features(mean_travel_time,unitroot_nsdiffs)
          
        } else {
          nsdiffs_val <- avg_times_week() %>% features(log(mean_travel_time),unitroot_nsdiffs)
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          
          nsdiffs_val <- avg_times_ts() %>% features(mean_travel_time,unitroot_nsdiffs)
          
        } else {
          nsdiffs_val <- avg_times_ts() %>% features(log(mean_travel_time),unitroot_nsdiffs)
        }
      }
      
    } else {
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          
          nsdiffs_val <- trip_count_week() %>% features(count_trips,unitroot_nsdiffs)
          
        } else {
          nsdiffs_val <- trip_count_week() %>% features(log(count_trips),unitroot_nsdiffs)
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          
          nsdiffs_val <- trip_count_ts() %>% features(count_trips,unitroot_nsdiffs)
          
        } else {
          nsdiffs_val<- trip_count_ts() %>% features(log(count_trips),unitroot_nsdiffs)
        }
      }
    }
    
    
  })
  
  arima_double_differencing <- reactive({
    nsdiffs_value <- as.numeric(arima_differencing())
    
    
    if(input$y=="mean_travel_time") {
      
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          
          
          ndiffs_value <-  avg_times_week() %>% mutate(seasonal_differenced_travel_time=difference(mean_travel_time,input$seasonal_lag)) %>% 
            features(seasonal_differenced_travel_time,unitroot_ndiffs)
          
        } else {
          
          ndiffs_value <- avg_times_week() %>% mutate(seasonal_differenced_travel_time=difference(log(mean_travel_time),input$seasonal_lag)) %>% 
            features(seasonal_differenced_travel_time,unitroot_ndiffs)
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          
          ndiffs_value <-  avg_times_ts() %>% mutate(seasonal_differenced_travel_time=difference(mean_travel_time,input$seasonal_lag)) %>% 
            features(seasonal_differenced_travel_time,unitroot_ndiffs)
          
        } else {
          ndiffs_value <- avg_times_ts()%>% mutate(seasonal_differenced_travel_time=difference(log(mean_travel_time),input$seasonal_lag)) %>% 
            features(seasonal_differenced_travel_time,unitroot_ndiffs)
        }
      }
      
    } else {
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          
          ndiffs_value <-  trip_count_week() %>% mutate(seasonal_differenced_no_trips=difference(count_trips,input$seasonal_lag)) %>% 
            features(seasonal_differenced_no_trips,unitroot_ndiffs)
          
        } else {
          ndiffs_value <- trip_count_week()%>% mutate(seasonal_differenced_no_trips=difference(log(count_trips),input$seasonal_lag)) %>% 
            features(seasonal_differenced_no_trips,unitroot_ndiffs)
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          
          ndiffs_value <-  trip_count_ts() %>% mutate(seasonal_differenced_no_trips=difference(count_trips,input$seasonal_lag)) %>% 
            features(seasonal_differenced_no_trips,unitroot_ndiffs)
          
        } else {
          ndiffs_value <- trip_count_ts()%>% mutate(seasonal_differenced_no_trips=difference(log(count_trips),input$seasonal_lag)) %>% 
            features(seasonal_differenced_no_trips,unitroot_ndiffs)
        }
      }
    }
    
    
  })
  
  prophet_model <- reactive({
    
    
    if(input$y=="mean_travel_time") {
      if(input$prophet_data=="Weekly Data") {
        
        if(input$prophet_data_transformation=="Original") {  
          
          fit_ph_dcmp <- avg_times_week() %>% model(
            prophet(mean_travel_time ~
                      season(period = "month", order = 5) +
                      season(period="year",order=10))
          )
          
        } else {
          fit_ph_dcmp <- avg_times_week() %>% model(
            prophet(log(mean_travel_time) ~
                      season(period = "month", order = 5) +
                      season(period="year",order=10))
          )
        }
        
      } else {
        
        if(input$prophet_data_transformation=="Original") {  
          
          fit_ph_dcmp <- avg_times_ts() %>% model(
            prophet(mean_travel_time ~
                      season(period = "month", order = 5) +
                      season(period="year",order=10))
          )
          
        } else {
          fit_ph_dcmp <- avg_times_ts() %>% model(
            prophet(log(mean_travel_time) ~
                      season(period = "month", order = 5) +
                      season(period="year",order=10))
          )
        }
        
      }
      
      
    }
    else {
      
      if(input$prophet_data=="Weekly Data") {
        if(input$prophet_data_transformation=="Original") {  
          
          fit_ph_dcmp <- trip_count_week() %>% model(
            prophet(count_trips ~
                      season(period = "month", order = 5) +
                      season(period="year",order=10))
          )
          
        } else {
          
          fit_ph_dcmp <- trip_count_week() %>% model(
            prophet(log(count_trips) ~
                      season(period = "month", order = 5) +
                      season(period="year",order=10))
          )
          
        }
        
        
      } else {
        
        if(input$prophet_data_transformation=="Original") {  
          
          fit_ph_dcmp <- trip_count_ts() %>% model(
            prophet(count_trips ~
                      season(period = "month", order = 5) +
                      season(period="year",order=10))
          )
          
        } else {
          
          fit_ph_dcmp <- trip_count_ts() %>% model(
            prophet(log(count_trips) ~
                      season(period = "month", order = 5) +
                      season(period="year",order=10))
          )
          
        }
        
      }
      
      
    }
  })
  
  training_data <- reactive({
    
    if(input$y=="mean_travel_time") {
      
      if(input$data=="Weekly Data") {
        
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(avg_times_week()))
        train <- slice(avg_times_week(),1:(n()-test_rows))
        train <- as_tsibble(train,index=week)
        train <- fill_gaps(train,mean_travel_time = mean(mean_travel_time))
        
      } else {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(avg_times_ts()))
        train <- slice(avg_times_ts(),1:(n()-test_rows))
        train <- as_tsibble(train,index=Date_parsed)
        train <- fill_gaps(train,mean_travel_time = mean(mean_travel_time))
        
      }
    }
    else {
      if(input$data=="Weekly Data") {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(trip_count_week()))
        train <- slice(trip_count_week(),1:(n()-test_rows))
        train <- as_tsibble(train,index=week)
        train <- fill_gaps(train,count_trips = mean(count_trips))
      } else {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(trip_count_ts()))
        train <- slice(trip_count_ts(),1:(n()-test_rows))
        train <- as_tsibble(train,index=Date_parsed)
        train <- fill_gaps(train,count_trips = mean(count_trips))
      }
    }
    
  })
  
  
  prophet_train_model <- reactive({
    if(input$y=="mean_travel_time") {
      if(input$prophet_data_transformation=="Original") { 
        fit_ph_dcmp <- training_data() %>% model(
          prophet(mean_travel_time ~
                    season(period = "month", order = 5) +
                    season(period="year",order=10))
        )
      } else {
        fit_ph_dcmp <- training_data() %>% model(
          prophet(log(mean_travel_time) ~
                    season(period = "month", order = 5) +
                    season(period="year",order=10))
        )
      }
      
    } else {
      if(input$prophet_data_transformation=="Original") { 
        fit_ph_dcmp <- training_data() %>% model(
          prophet(count_trips ~
                    season(period = "month", order = 5) +
                    season(period="year",order=10))
        )
      } else {
        fit_ph_dcmp <- training_data() %>% model(
          prophet(log(count_trips) ~
                    season(period = "month", order = 5) +
                    season(period="year",order=10))
        )
      }
      
    }
    
  })
  
  
  
  output$time_series <- renderPlot({
    
    
    if (input$y=="mean_travel_time") {
      
      if (input$color=="None") {
        avg_times() %>%
          ggplot()+
          geom_point(aes(x=Date,y=mean_travel_time),color="cornflowerblue", size = 2, alpha=.8)+
          labs(title="Time plot of mean travel duration",y="Mean travel time")
      } else {
        avg_times() %>%
          ggplot()+
          geom_point(aes(x=Date,y=mean_travel_time,color=day), size = 2, alpha=.8)+
          labs(title="Time plot of mean travel duration colored by Weekdays",y="Mean travel time")
      }
      
    } else {
      
      if (input$color=="None") { 
        trip_count() %>%
          ggplot()+
          geom_point(aes(x=Date,y=count_trips),color="cornflowerblue", size = 2, alpha=.8)+
          labs(labs = "Time plot of trip counts",y="Number of trips")
      } else {
        trip_count() %>%
          ggplot()+
          geom_point(aes(x=Date,y=count_trips,color=day), size = 2, alpha=.8)+
          labs(labs = "Time plot of trip counts colored by Weekdays",y="Number of trips")
      }
    }
  },res = 96
  )
  
  output$summary_data <- renderTable({
    if (input$y=="mean_travel_time") {
      summary(avg_times())
    } else {
      summary(trip_count())
    }
    
  })
  
  output$correlation_plot <- renderPlot({
    merged_data <- reactive(merge(avg_times(),trip_count(),by="Date"))
    merged_data() %>%
      ggplot() +
      geom_point(aes(x=count_trips,y=mean_travel_time),color="steelblue") +
      labs(x="Number of Destinations",y="Mean Travel Time (in seconds)",title="Correlation between travel time and number of destinations")
  }, res=100)
  
  output$histogram <- renderPlot({
    if (input$y=="mean_travel_time") {
      hist(avg_times()$mean_travel_time)
    } else {
      hist(trip_count()$count_trips)
    }
  }, res = 96)
  
  output$facet_plot <- renderPlot({
    if (input$y=="mean_travel_time") {
      avg_times() %>%
        ggplot()+
        geom_point(aes(x=Date,y=mean_travel_time),color="cornflowerblue", size = 2, alpha=.8)+
        facet_wrap(~day, scales="free_y") +
        labs(title="Time plot of mean trip duration on a given day faceted out by weekdays",y="Mean Travel Time")
    } else {
      trip_count() %>%
        ggplot()+
        geom_point(aes(x=Date,y=count_trips),color="cornflowerblue", size = 2, alpha=.8)+
        facet_wrap(~day, scales="free_y") +
        labs(title="Time plot of trip counts on a given day faceted out by weekdays",y="Number of Trips")
    }
  }, res=100)
  
  output$weekly_stats <- renderPlot({
    week_stats <- city_data() %>% group_by(DayOfWeek) %>% summarise(count_trips = n())
    ggplot(week_stats,aes(x=DayOfWeek,y=count_trips,fill=DayOfWeek,group=DayOfWeek))+
      geom_bar(stat="identity")+
      labs(title="Number of trips on each day of the week",x="Day of the Week",y="Number of trips")
  }, res=96)
  
  output$seasonality <- renderPlot({
    if (input$seasonality=="Yearly Seasonality") {
      if (input$y=="mean_travel_time") {
        avg_times_ts() %>% 
          gg_season(mean_travel_time) + labs(y="Mean Travel Time",x="Month",title="Yearly seasonal plot of mean travel times")
      } else {
        trip_count_ts() %>% 
          gg_season(count_trips) + labs(y="Number of Trips",x="Month",title="Yearly seasonal plot of number of destination zones")
      }
    } else {
      if (input$y=="mean_travel_time") {
        avg_times_ts() %>% 
          gg_season(mean_travel_time,period="week") + labs(y="Mean Travel Time",x="Day",title="Seasonal plot showing weekly seasonal patterns of mean travel times")
      } else {
        trip_count_ts() %>% 
          gg_season(count_trips,period="week") + labs(y="Number of Trips",x="Day",title="Seasonal plot showing weekly seasonal patterns of number of trips")
      }
    }
  }, res=100)
  
  output$lag_plot <- renderPlot({
    if(input$y=="mean_travel_time") {
      avg_times_ts() %>% gg_lag(mean_travel_time,geom="point")+
        labs(x="lag(Mean Travel Time)",title="Lag Plot of Mean Travel times")
    } else {
      trip_count_ts() %>% gg_lag(count_trips,geom="point")+
        labs(x="lag(Mean Travel Time)",title="Lag Plot of Number of Trips")
    }
  }, res=100)
  
  output$acf_plot <- renderPlot({
    if(input$y=="mean_travel_time") {
      avg_times_ts() %>% ACF(mean_travel_time,lag_max=52) %>% autoplot() +
        labs(title="ACF Plot of Mean Travel Times")
    } else {
      trip_count_ts() %>% ACF(count_trips,lag_max=52) %>% autoplot() +
        labs(title="ACF Plot of Number of Trips")
    }
  }, res=100)
  
  output$ma_plot <- renderPlot({
    if(input$y=="mean_travel_time") {
      if(input$moving_average=="7-MA") {
        avg_times_ma <- avg_times_ts() %>% mutate(
          `7-MA` = slider::slide_dbl(mean_travel_time, mean,
                                     .before = 4, .after = 3, .complete = TRUE)
        )
        ggplot(avg_times_ma) +
          geom_line(aes(x=Date_parsed,y = `7-MA`), colour = "#D55E00") +
          labs(y = "Mean Travel Time",
               title = "7 day moving average of travel time in seconds") +
          guides(colour = guide_legend(title = "series"))
      } else {
        avg_times_ma <- avg_times_ts() %>% mutate(
          `30-MA` = slider::slide_dbl(mean_travel_time, mean,
                                      .before = 15, .after = 14, .complete = TRUE)
        )
        ggplot(avg_times_ma) +
          geom_line(aes(x=Date_parsed,y = `30-MA`), colour = "#D55E00") +
          labs(y = "Mean Travel Time",
               title = "30 day moving average of travel time in seconds") +
          guides(colour = guide_legend(title = "series"))
      }
    } else {
      if(input$moving_average=="7-MA") {
        trip_counts_ma <- trip_count_ts() %>% mutate(
          `7-MA` = slider::slide_dbl(count_trips, mean,
                                     .before = 4, .after = 3, .complete = TRUE)
        )
        ggplot(trip_counts_ma) +
          geom_line(aes(x=Date_parsed,y = `7-MA`), colour = "#D55E00") +
          labs(y = "Number of trips",
               title = "7 day moving average of number of trips") +
          guides(colour = guide_legend(title = "series"))
      } else {
        trip_counts_ma <- trip_count_ts() %>% mutate(
          `30-MA` = slider::slide_dbl(count_trips, mean,
                                      .before = 15, .after = 14, .complete = TRUE)
        )
        ggplot(trip_counts_ma) +
          geom_line(aes(x=Date_parsed,y = `30-MA`), colour = "#D55E00") +
          labs(y = "Number of trips",
               title = "30 day moving average of number of trips") +
          guides(colour = guide_legend(title = "series"))
      }
    }
  }, res=100)
  
  output$decomposition <- renderPlot({
    if(input$y=="mean_travel_time") {
      
      if(input$data_transformation=="Original") {
        
        if(class(input$seasonal_window)=="character" & input$seasonal_window=="periodic") {
          
          dcmp <- avg_times_ts() %>% model(stl=STL(mean_travel_time ~ trend(window=input$trend_window) +
                                                     season(window="periodic"),
                                                   robust=input$robust)) 
          components(dcmp) %>% autoplot() + labs(title="STL decomposition of travel time",x="Date")
          
        } else if(class(input$seasonal_window)=="character") {
          
          dcmp <- avg_times_ts() %>% model(stl=STL(mean_travel_time ~ trend(window=input$trend_window) +
                                                     season(window=strtoi(input$seasonal_window)),
                                                   robust=input$robust)) 
          components(dcmp) %>% autoplot() + labs(title="STL decomposition of travel time",x="Date")
          
        } else {
          h2("You have provided the wrong seasonal window input")
        }
        
      } else {
        
        if(class(input$seasonal_window)=="character" & input$seasonal_window=="periodic") {
          
          dcmp <- avg_times_ts() %>% model(stl=STL(log(mean_travel_time) ~ trend(window=input$trend_window) +
                                                     season(window="periodic"),
                                                   robust=input$robust)) 
          components(dcmp) %>% autoplot() + labs(title="STL decomposition of travel time",x="Date")
          
        } else if(class(input$seasonal_window)=="character") {
          
          dcmp <- avg_times_ts() %>% model(stl=STL(log(mean_travel_time) ~ trend(window=input$trend_window) +
                                                     season(window=strtoi(input$seasonal_window)),
                                                   robust=input$robust)) 
          components(dcmp) %>% autoplot() + labs(title="STL decomposition of travel time",x="Date")
          
        } else {
          h2("You have provided the wrong seasonal window input")
        }
        
      }
      
    } else {
      if(input$data_transformation=="Original") {
        
        if(class(input$seasonal_window)=="character" & input$seasonal_window=="periodic") {
          
          dcmp <- trip_count_ts() %>% model(stl=STL(count_trips ~ trend(window=input$trend_window) +
                                                      season(window="periodic"),
                                                    robust=input$robust)) 
          components(dcmp) %>% autoplot() + labs(title="STL decomposition of number of trips",x="Date")
          
        } else if(class(input$seasonal_window)=="character") {
          
          dcmp <- trip_count_ts() %>% model(stl=STL(count_trips ~ trend(window=input$trend_window) +
                                                      season(window=strtoi(input$seasonal_window)),
                                                    robust=input$robust)) 
          components(dcmp) %>% autoplot() + labs(title="STL decomposition of number of trips",x="Date")
          
        } else {
          h2("You have provided the wrong seasonal window input")
        }
        
      } else {
        
        if(class(input$seasonal_window)=="character" & input$seasonal_window=="periodic") {
          
          dcmp <- trip_count_ts() %>% model(stl=STL(log(count_trips) ~ trend(window=input$trend_window) +
                                                      season(window="periodic"),
                                                    robust=input$robust)) 
          components(dcmp) %>% autoplot() + labs(title="STL decomposition of number of trips",x="Date")
          
        } else if(class(input$seasonal_window)=="character") {
          
          dcmp <- trip_count_ts() %>% model(stl=STL(log(count_trips) ~ trend(window=input$trend_window) +
                                                      season(window=strtoi(input$seasonal_window)),
                                                    robust=input$robust)) 
          components(dcmp) %>% autoplot() + labs(title="STL decomposition of number of trips",x="Date")
          
        } else {
          h2("You have provided the wrong seasonal window input")
        }
        
      }
    }
  }, res=100)
  
  output$stl_features <- renderTable({
    if(input$y=="mean_travel_time") {
      
      avg_times_ts() %>% features(mean_travel_time,feat_stl) %>% pivot_longer(everything(),names_to="features",values_to="value")
    } else {
      trip_count_ts() %>% features(count_trips,feat_stl) %>% pivot_longer(everything(),names_to="features",values_to="value")
    }
  })
  
  
  output$stl_forecasting <- renderPlot({
    if(input$y=="mean_travel_time") {
      if(input$data=="Weekly Data") {
        
        if(input$data_transformation1=="Original") { 
          dcmp <- avg_times_week() %>% model(stlf=decomposition_model(STL(mean_travel_time ~ trend(window=input$trend_window1),
                                                                          robust=input$robust),
                                                                      SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=input$forecast) %>% autoplot(avg_times_week()) + labs(title="STL forecasting of weekly mean travel time",x="Date")
        } else {
          dcmp <- avg_times_week() %>% model(stlf=decomposition_model(STL(log(mean_travel_time) ~ trend(window=input$trend_window1),
                                                                          robust=input$robust),
                                                                      SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=input$forecast) %>% autoplot(avg_times_week()) + labs(title="STL forecasting of weekly mean travel time",x="Date")
          
          #dcmp %>% gg_tsresiduals()
          
        }
        
      } else {
        if(input$data_transformation1=="Original") {  
          
          dcmp <- avg_times_ts() %>% model(stlf=decomposition_model(STL(mean_travel_time ~ trend(window=input$trend_window1),
                                                                        robust=input$robust),
                                                                    SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=input$forecast) %>% autoplot(avg_times_ts())  + labs(title="STL forecasting of daily mean travel time",x="Date")
          
        } else {
          dcmp <- avg_times_ts() %>% model(stlf=decomposition_model(STL(log(mean_travel_time) ~ trend(window=input$trend_window1),
                                                                        robust=input$robust),
                                                                    SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=input$forecast) %>% autoplot(avg_times_ts())  + labs(title="STL forecasting of daily mean travel time",x="Date")
        }
        
      }
      
    } else {
      
      if(input$data=="Weekly Data") {
        
        if(input$data_transformation1=="Original") { 
          dcmp <- trip_count_week() %>% model(stlf=decomposition_model(STL(count_trips ~ trend(window=input$trend_window1),
                                                                           robust=input$robust),
                                                                       SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=input$forecast) %>% autoplot(trip_count_week())
        } else {
          dcmp <- trip_count_week() %>% model(stlf=decomposition_model(STL(log(count_trips) ~ trend(window=input$trend_window1),
                                                                           robust=input$robust),
                                                                       SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=input$forecast) %>% autoplot(trip_count_week())
          
        }
        
      } else {
        if(input$data_transformation1=="Original") {  
          
          dcmp <- trip_count_ts() %>% model(stlf=decomposition_model(STL(count_trips ~ trend(window=input$trend_window1),
                                                                         robust=input$robust),
                                                                     SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=input$forecast) %>% autoplot(trip_count_ts())
          
        } else {
          dcmp <- trip_count_ts() %>% model(stlf=decomposition_model(STL(log(count_trips) ~ trend(window=input$trend_window1),
                                                                         robust=input$robust),
                                                                     SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=input$forecast) %>% autoplot(trip_count_ts())
        }
        
      }
      
    }
  }, res=100)
  
  output$stl_training <- renderPlot({
    
    if(input$y=="mean_travel_time") {
      
      
      if(input$data=="Weekly Data") {
        
        test_rows = as.integer(((100-input$train_percent)/100)*nrow(avg_times_week()))
        #print(nrow(avg_times_week()))
        train <- slice(avg_times_week(),1:(n()-test_rows))
        #train <- as_tsibble(train,index=Date_parsed)
        train <- fill_gaps(train,mean_travel_time = mean(mean_travel_time))
        
        
        if(input$data_transformation1=="Original") { 
          dcmp <- train %>% model(stlf=decomposition_model(STL(mean_travel_time ~ trend(window=input$trend_window1),
                                                               robust=input$robust),
                                                           SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=test_rows) %>% autoplot(avg_times_week(),level=NULL,color="pink") + labs(title="STL forecasting of weekly mean travel time",x="Date")
          
        } else {
          dcmp <- train %>% model(stlf=decomposition_model(STL(log(mean_travel_time) ~ trend(window=input$trend_window1),
                                                               robust=input$robust),
                                                           SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=test_rows) %>% autoplot(avg_times_week(),level=NULL,color="pink") + labs(title="STL forecasting of weekly mean travel time",x="Date")
          
          
        }
        
      } else {
        test_rows = as.integer(((100-input$train_percent)/100)*nrow(avg_times_ts()))
        train <- slice(avg_times_ts(),1:(n()-test_rows))
        #train <- as_tsibble(train,index=Date_parsed)
        train <- fill_gaps(train,mean_travel_time = mean(mean_travel_time))
        
        if(input$data_transformation1=="Original") {  
          
          dcmp <- train %>% model(stlf=decomposition_model(STL(mean_travel_time ~ trend(window=input$trend_window1),
                                                               robust=input$robust),
                                                           SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=test_rows) %>% autoplot(avg_times_ts(),level=NULL,color="pink")  + labs(title="STL forecasting of daily mean travel time",x="Date")
          
        } else {
          dcmp <- train %>% model(stlf=decomposition_model(STL(log(mean_travel_time) ~ trend(window=input$trend_window1),
                                                               robust=input$robust),
                                                           SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=test_rows) %>% autoplot(avg_times_ts(),level=NULL,color="pink")  + labs(title="STL forecasting of daily mean travel time",x="Date")
        }
        
      }
      
    } else {
      
      if(input$data=="Weekly Data") {
        
        test_rows = as.integer(((100-input$train_percent)/100)*nrow(trip_count_week()))
        #print(nrow(avg_times_week()))
        train <- slice(trip_count_week(),1:(n()-test_rows))
        #train <- as_tsibble(train,index=Date_parsed)
        train <- fill_gaps(train,count_trips = mean(count_trips))
        
        if(input$data_transformation1=="Original") { 
          dcmp <- train %>% model(stlf=decomposition_model(STL(count_trips ~ trend(window=input$trend_window1),
                                                               robust=input$robust),
                                                           SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=test_rows) %>% autoplot(trip_count_week(),level=NULL,color="pink")  + labs(title="STL forecasting of count of trips",x="Date")
          
        } else {
          dcmp <- train %>% model(stlf=decomposition_model(STL(log(count_trips) ~ trend(window=input$trend_window1),
                                                               robust=input$robust),
                                                           SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=test_rows) %>% autoplot(trip_count_week(),level=NULL,color="pink")  + labs(title="STL forecasting of count of trips",x="Date")
          
        }
        
        
        
      } else {
        test_rows = as.integer(((100-input$train_percent)/100)*nrow(avg_times_ts()))
        train <- slice(avg_times_ts(),1:(n()-test_rows))
        #train <- as_tsibble(train,index=Date_parsed)
        train <- fill_gaps(train,mean_travel_time = mean(mean_travel_time))
        
        if(input$data_transformation1=="Original") {  
          
          dcmp <- train %>% model(stlf=decomposition_model(STL(count_trips ~ trend(window=input$trend_window1),
                                                               robust=input$robust),
                                                           SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=test_rows) %>% autoplot(trip_count_ts(),level=NULL,color="pink") + labs(title="STL forecasting of count of trips",x="Date")
          
        } else {
          dcmp <- train %>% model(stlf=decomposition_model(STL(log(count_trips) ~ trend(window=input$trend_window1),
                                                               robust=input$robust),
                                                           SNAIVE(season_adjust)))
          
          dcmp %>% forecast(h=test_rows) %>% autoplot(trip_count_ts()) + labs(title="STL forecasting of count of trips",x="Date")
        }
        
      }
      
    }
    
    
  }, res=100)
  
  
  output$stl_accuracy <- renderTable({
    
    if(input$y=="mean_travel_time") {
      
      if(input$data=="Weekly Data") {
        accuracy(train_fc_obj(),avg_times_week()) %>% pivot_longer(everything(),names_to="features",values_to="value")
      } else {
        
      }
      
      #train_fc <- train_dcmp %>% forecast(h=60)
      #accuracy(train_fc,avg_times_ts)
    } else {
      
    }
  })
  
  output$arima_differencing_table <- renderTable({
    arima_differencing()
  })
  
  output$arima_double_differencing_table <- renderTable({
    arima_double_differencing()
  })
  
  output$kpss_test <- renderTable({
    nsdiffs_val <- as.numeric(arima_differencing())
    ndiffs_val <- as.numeric(arima_double_differencing())
    
  
    if(input$y=="mean_travel_time") {
      
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          
          if(nsdiffs_val>=1) {
            
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(difference(mean_travel_time,input$seasonal_lag),ndiffs_val)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            } else {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,input$seasonal_lag)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            }
            
          } else {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,ndiffs_val)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            } else {
              avg_times_week() %>%
                features(mean_travel_time,unitroot_kpss)
            }
          }
          
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(difference(log(mean_travel_time),input$seasonal_lag),ndiffs_val)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            } else {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),input$seasonal_lag)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),ndiffs_val)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            } else {
              avg_times_week() %>%
                features(log(mean_travel_time),unitroot_kpss)
            }
          }
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(difference(mean_travel_time,input$seasonal_lag),ndiffs_val)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            } else {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,input$seasonal_lag)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,ndiffs_val)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            } else {
              avg_times_ts() %>%
                features(mean_travel_time,unitroot_kpss)
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(difference(log(mean_travel_time),input$seasonal_lag)),ndiffs_val) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            } else {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),input$seasonal_lag)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),ndiffs_val)) %>%
                features(doubly_diff_travel_time,unitroot_kpss)
            } else {
              avg_times_ts() %>%
                features(log(mean_travel_time),unitroot_kpss)
            }
          }
        }
      }
      
    } else {
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(difference(count_trips,input$seasonal_lag),ndiffs_val)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            } else {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(count_trips,input$seasonal_lag)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(count_trips,ndiffs_val)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            } else {
              trip_count_week() %>%
                features(count_trips,unitroot_kpss)
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(difference(log(count_trips),input$seasonal_lag)),ndiffs_val) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            } else {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),input$seasonal_lag)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),input$seasonal_lag)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
              
            } else {
              trip_count_week() %>%
                features(log(count_trips),unitroot_kpss)
            }
            
          }
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(difference(count_trips,input$seasonal_lag),ndiffs_val)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            } else {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(count_trips,input$seasonal_lag)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            }
          } else {
            if(ndiffs_val>=1) {
              
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(count_trips,ndiffs_val)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            } else {
              trip_count_ts()  %>%
                features(count_trips,unitroot_kpss)
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(difference(log(count_trips),input$seasonal_lag),ndiffs_val)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            } else {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),input$seasonal_lag)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),ndiffs_val)) %>%
                features(doubly_diff_no_trips,unitroot_kpss)
            } else {
              trip_count_ts()  %>%
                features(log(count_trips),unitroot_kpss)
            }
          }
        }
      }
    }
    
  })
  
  
  output$differenced_plot <- renderPlot({
    nsdiffs_val <- as.numeric(arima_differencing())
    ndiffs_val <- as.numeric(arima_double_differencing())
    
    
    if(input$y=="mean_travel_time") {
      
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          
          if(nsdiffs_val>=1) {
            
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(difference(mean_travel_time,input$seasonal_lag),ndiffs_val)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            } else {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,input$seasonal_lag)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            }
            
          } else {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,ndiffs_val)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            } else {
              avg_times_week() %>%
                autoplot(mean_travel_time) + labs(title="Differenced plot of travel time (No differencing applied in this case)")
            }
          }
          
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(difference(log(mean_travel_time),input$seasonal_lag),ndiffs_val)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            } else {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),input$seasonal_lag)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),ndiffs_val)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            } else {
              avg_times_week() %>%
                autoplot(log(mean_travel_time)) + labs(title="Differenced plot of travel time (No differencing applied in this case)")
            }
          }
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(difference(mean_travel_time,input$seasonal_lag),ndiffs_val)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            } else {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,input$seasonal_lag)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,ndiffs_val)) %>%
                autoplot(doubly_diff_travel_time) + labs(title="Differenced plot of travel time")
            } else {
              avg_times_ts() %>%
                autoplot(mean_travel_time) + labs(title="Differenced plot of travel time (No differencing applied in this case)")
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(difference(log(mean_travel_time),input$seasonal_lag)),ndiffs_val) %>%
                autoplot(doubly_diff_travel_time)  + labs(title="Differenced plot of travel time")
            } else {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),input$seasonal_lag)) %>%
                autoplot(doubly_diff_travel_time)  + labs(title="Differenced plot of travel time")
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),ndiffs_val)) %>%
                autoplot(doubly_diff_travel_time)  + labs(title="Differenced plot of travel time")
            } else {
              avg_times_ts() %>%
                autoplot(log(mean_travel_time))  + labs(title="Differenced plot of travel time (No differencing applied in this case)")
            }
          }
        }
      }
      
    } else {
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(difference(count_trips,input$seasonal_lag),ndiffs_val)) %>%
                autoplot(doubly_diff_no_trips)  + labs(title="Differenced plot of trip count")
            } else {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(count_trips,input$seasonal_lag)) %>%
                autoplot(doubly_diff_no_trips)  + labs(title="Differenced plot of trip count")
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(count_trips,ndiffs_val)) %>%
                autoplot(doubly_diff_no_trips)  + labs(title="Differenced plot of trip count")
            } else {
              trip_count_week() %>%
                autoplot(count_trips)  + labs(title="Differenced plot of trip count (No differencing applied in this case)")
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(difference(log(count_trips),input$seasonal_lag)),ndiffs_val) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
            } else {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),input$seasonal_lag)) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),ndiffs_val)) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
              
            } else {
              trip_count_week() %>%
                autoplot(log(count_trips)) + labs(title="Differenced plot of trip count (No differencing applied in this case)")
            }
            
          }
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(difference(count_trips,input$seasonal_lag),ndiffs_val)) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
            } else {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(count_trips,input$seasonal_lag)) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
            }
          } else {
            if(ndiffs_val>=1) {
              
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(count_trips,ndiffs_val)) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
            } else {
              trip_count_ts()  %>%
                autoplot(count_trips) + labs(title="Differenced plot of trip count (No differencing applied in this case)")
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(difference(log(count_trips),input$seasonal_lag),ndiffs_val)) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
            } else {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),input$seasonal_lag)) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),ndiffs_val)) %>%
                autoplot(doubly_diff_no_trips) + labs(title="Differenced plot of trip count")
            } else {
              trip_count_ts()  %>%
                autoplot(log(count_trips)) + labs(title="Differenced plot of trip count (No differencing applied in this case)")
            }
          }
        }
      }
    }
    
  }, res=100)
  
  
  output$residual_plot <- renderPlot({
    nsdiffs_val <- as.numeric(arima_differencing())
    ndiffs_val <- as.numeric(arima_double_differencing())
    

    
    if(input$y=="mean_travel_time") {
      
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          
          if(nsdiffs_val>=1) {
            
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(difference(mean_travel_time,input$seasonal_lag),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of weekly travel time")
            } else {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,input$seasonal_lag)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of weekly travel time")
            }
            
          } else {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial") + labs(title="Residual plot of weekly travel time")
            } else {
              avg_times_week() %>%
                gg_tsdisplay(mean_travel_time,plot_type = "partial") + labs(title="Residual plot of weekly travel time")
            }
          }
          
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(difference(log(mean_travel_time),input$seasonal_lag),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of weekly travel time")
            } else {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),input$seasonal_lag)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of weekly travel time")
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_week() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial") + labs(title="Residual plot of weekly travel time")
            } else {
              avg_times_week() %>%
                gg_tsdisplay(log(mean_travel_time),plot_type = "partial") + labs(title="Residual plot of weekly travel time")
            }
          }
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(difference(mean_travel_time,input$seasonal_lag),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of daily travel time")
            } else {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,input$seasonal_lag)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of daily travel time") 
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(mean_travel_time,ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial") + labs(title="Residual plot of daily travel time")
            } else {
              avg_times_ts() %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial") + labs(title="Residual plot of daily travel time")
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(difference(log(mean_travel_time),input$seasonal_lag)),ndiffs_val) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of daily travel time")
            } else {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),input$seasonal_lag)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of daily travel time")
            }
          } else {
            if(ndiffs_val>=1) {
              avg_times_ts() %>% mutate(doubly_diff_travel_time = difference(log(mean_travel_time),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_travel_time,plot_type = "partial") + labs(title="Residual plot of daily travel time")
            } else {
              avg_times_ts() %>%
                gg_tsdisplay(log(mean_travel_time),plot_type = "partial") + labs(title="Residual plot of daily travel time")
            }
          }
        }
      }
      
    } else {
      if(input$arima_data=="Weekly Data") {
        
        if(input$arima_data_transformation=="Original") {  
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(difference(count_trips,input$seasonal_lag),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of weekly trip count")
            } else {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(count_trips,input$seasonal_lag)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of weekly trip count")
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(count_trips,ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial") + labs(title="Residual plot of weekly trip count")
            } else {
              trip_count_week() %>%
                gg_tsdisplay(count_trips,plot_type = "partial") + labs(title="Residual plot of weekly trip count")
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(difference(log(count_trips),input$seasonal_lag)),ndiffs_val) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of weekly trip count")
            } else {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),input$seasonal_lag)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of weekly trip count")
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_week() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial") + labs(title="Residual plot of weekly trip count")
              
            } else {
              trip_count_week() %>%
                gg_tsdisplay(log(count_trips),plot_type = "partial") + labs(title="Residual plot of weekly trip count")
            }
            
          }
        }
        
      } else {
        if(input$arima_data_transformation=="Original") {  
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(difference(count_trips,input$seasonal_lag),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of daily trip count")
            } else {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(count_trips,input$seasonal_lag)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of daily trip count")
            }
          } else {
            if(ndiffs_val>=1) {
              
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(count_trips,ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial") + labs(title="Residual plot of daily trip count")
            } else {
              trip_count_ts()  %>%
                gg_tsdisplay(count_trips,plot_type = "partial") + labs(title="Residual plot of weekly trip count")
            }
          }
          
        } else {
          if(nsdiffs_val>=1) {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(difference(log(count_trips),input$seasonal_lag),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of daily trip count")
            } else {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),input$seasonal_lag)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial",lag=input$seasonal_lag) + labs(title="Residual plot of daily trip count")
            }
          } else {
            if(ndiffs_val>=1) {
              trip_count_ts() %>% mutate(doubly_diff_no_trips = difference(log(count_trips),ndiffs_val)) %>%
                gg_tsdisplay(doubly_diff_no_trips,plot_type = "partial") + labs(title="Residual plot of daily trip count")
            } else {
              trip_count_ts()  %>%
                gg_tsdisplay(log(count_trips),plot_type = "partial") + labs(title="Residual plot of daily trip count")
            }
          }
        }
      }
    }
    
  }, res=100)
  
  arima_avg_times_week <- reactive({
    pdq_params <- as.list(strsplit(input$pdq, '')[[1]])
    spdq_params <- as.list(strsplit(input$seasonal_pdq, '')[[1]])
    
    p <- as.integer(pdq_params[[1]])
    d <- as.integer(pdq_params[[2]])
    q<- as.integer(pdq_params[[3]])
    
    s_p <- as.integer(spdq_params[[1]])
    s_d <- as.integer(spdq_params[[2]])
    s_q <- as.integer(spdq_params[[3]])
    #as.integer(lst[[1]])
    
    if(input$arima_data_transformation=="Original") {  
      
      fit <- avg_times_week() %>%
        model(arima_customized = ARIMA(mean_travel_time ~ pdq(p,d,q) + PDQ(s_p,s_d,s_q))
              #auto = ARIMA(mean_travel_time, stepwise = FALSE, approx = FALSE))
        )
      
      
    } else {
      fit <- avg_times_week() %>%
        model(arima_customized = ARIMA(log(mean_travel_time) ~ pdq(p,d,q) + PDQ(s_p,s_d,s_q))
             # auto = ARIMA(log(mean_travel_time), stepwise = FALSE, approx = FALSE)
             )
      
    }
    
  })
  
  arima_avg_times_day <- reactive({
    pdq_params <- as.list(strsplit(input$pdq, '')[[1]])
    spdq_params <- as.list(strsplit(input$seasonal_pdq, '')[[1]])
    
    p <- as.integer(pdq_params[[1]])
    d <- as.integer(pdq_params[[2]])
    q<- as.integer(pdq_params[[3]])
    
    s_p <- as.integer(spdq_params[[1]])
    s_d <- as.integer(spdq_params[[2]])
    s_q <- as.integer(spdq_params[[3]])
    
    if(input$arima_data_transformation=="Original") {  
      
      fit <- avg_times_ts() %>%
        model(arima_customized = ARIMA(mean_travel_time ~ pdq(p,d,q) + PDQ(s_p,s_d,s_q)),
              auto = ARIMA(mean_travel_time, stepwise = FALSE, approx = FALSE))
      
    } else {
      fit <- avg_times_ts() %>%
        model(arima_customized = ARIMA(log(mean_travel_time) ~ pdq(p,d,q) + PDQ(s_p,s_d,s_q)),
              auto = ARIMA(log(mean_travel_time), stepwise = FALSE, approx = FALSE))
      
    }
  })
  
  arima_trip_count_week <- reactive({
    
    pdq_params <- as.list(strsplit(input$pdq, '')[[1]])
    spdq_params <- as.list(strsplit(input$seasonal_pdq, '')[[1]])
    
    p <- as.integer(pdq_params[[1]])
    d <- as.integer(pdq_params[[2]])
    q<- as.integer(pdq_params[[3]])
    
    s_p <- as.integer(spdq_params[[1]])
    s_d <- as.integer(spdq_params[[2]])
    s_q <- as.integer(spdq_params[[3]])
    
    if(input$arima_data_transformation=="Original") {  
      
      fit <- trip_count_week() %>%
        model(arima_customized = ARIMA(count_trips ~ pdq(p,d,q) + PDQ(s_p,s_d,s_q)),
              auto = ARIMA(count_trips, stepwise = FALSE, approx = FALSE))
      
    } else {
      fit <- trip_count_week() %>%
        model(arima_customized = ARIMA(log(count_trips) ~ pdq(p,d,q) + PDQ(s_p,s_d,s_q)),
              auto = ARIMA(log(count_trips), stepwise = FALSE, approx = FALSE))
      
    }
  })
  
  arima_trip_count_ts <- reactive({
    
    pdq_params <- as.list(strsplit(input$pdq, '')[[1]])
    spdq_params <- as.list(strsplit(input$seasonal_pdq, '')[[1]])
    
    p <- as.integer(pdq_params[[1]])
    d <- as.integer(pdq_params[[2]])
    q<- as.integer(pdq_params[[3]])
    
    s_p <- as.integer(spdq_params[[1]])
    s_d <- as.integer(spdq_params[[2]])
    s_q <- as.integer(spdq_params[[3]])
    
    if(input$arima_data_transformation=="Original") {  
      
      fit <- trip_count_ts() %>%
        model(arima_customized = ARIMA(count_trips ~ pdq(p,d,q) + PDQ(s_p,s_d,s_q)),
              auto = ARIMA(count_trips, stepwise = FALSE, approx = FALSE))
      
    } else {
      fit <- trip_count_ts() %>%
        model(arima_customized = ARIMA(log(count_trips) ~ pdq(p,d,q) + PDQ(s_p,s_d,s_q)),
              auto = ARIMA(log(count_trips), stepwise = FALSE, approx = FALSE,parallel=TRUE))
      
    }
  })
  
  
  output$arima_forecasting <- renderPlot({
    
    if(input$pdq=="000" & input$seasonal_pdq=="000") {
      h4("ARIMA forecasting plot will be displayed here once you enter parameters.")
    } 
    
    else {
      if(input$y=="mean_travel_time") {
        if(input$arima_data=="Weekly Data") {
          
          #print("Computing...")
          
          forecast(arima_avg_times_week(),h=input$arima_forecast) %>%
            filter(.model=='arima_customized') %>% autoplot(avg_times_week()) +
            labs(title = "Forecasting mean travel times for weekly data",
                 y="Mean travel time")
          
          
        } else {
          forecast(arima_avg_times_day(),h=input$arima_forecast) %>%
            filter(.model=='arima_customized') %>% autoplot(avg_times_ts()) +
            labs(title = "Forecasting mean travel times for daily data",
                 y="Mean travel time")
          
        }
        
        
      }
      else {
        
        if(input$arima_data=="Weekly Data") {
          
          forecast(arima_trip_count_week(),h=input$arima_forecast) %>%
            filter(.model=='arima_customized') %>% autoplot(trip_count_week()) +
            labs(title = "Forecasting trip counts for weekly data",
                 y="Mean travel time")
          
          
        } else {
          forecast(arima_trip_count_ts(),h=input$arima_forecast) %>%
            filter(.model=='arima_customized') %>% autoplot(trip_count_ts()) +
            labs(title = "Forecasting trip counts for daily data",
                 y="Mean travel time")
          
        }
        
        
      }
    }
    
    
  }, res=100)
  
  output$arima_metrics <- renderTable({
    #print("arima forecasting table")
    
    if(input$pdq=="000" & input$seasonal_pdq=="000") {
      print("Performance metrics will be displayed here once you enter parameters.")
    } 
    
    else {
      if(input$y=="mean_travel_time") {
        if(input$arima_data=="Weekly Data") {
          
          #print("Computing...")
          
          glance(arima_avg_times_week()) %>% arrange(AICc) %>% select(.model:BIC) %>%
            pivot_longer(!c(.model),names_to="features",values_to="value") %>%
            select(features,value)
          
        } else {
          glance(arima_avg_times_day()) %>% arrange(AICc) %>% select(.model:BIC) %>%
            pivot_longer(!c(.model),names_to="features",values_to="value") %>%
            select(features,value)
          
        }
        
        
      }
      else {
        
        if(input$arima_data=="Weekly Data") {
          
          glance(arima_trip_count_week()) %>% arrange(AICc) %>% select(.model:BIC) %>%
            pivot_longer(!c(.model),names_to="features",values_to="value") %>%
            select(features,value)
          
          
        } else {
          glance(arima_trip_count_ts()) %>% arrange(AICc) %>% select(.model:BIC) %>%
            pivot_longer(!c(.model),names_to="features",values_to="value") %>%
            select(features,value)
          
        }
        
        
      }
    }
    
    
  }, res=100)
  
  output$prophet_decomposition <- renderPlot({
    #print(summary(prophet_model()))
    prophet_model() %>% 
      components() %>%
      autoplot() 
  }, res=100)
  
  output$prophet_residuals <- renderPlot({
    prophet_model() %>% 
      gg_tsresiduals() + labs(title="Residual diagnostics of Prophet model")
    
  }, res=100)
  
  
  
  output$prophet_forecasting <- renderPlot({
    if(input$y=="mean_travel_time") {
      if(input$prophet_data == "Weekly Data") {
        fc_ph <- prophet_model() %>% forecast(h=input$prophet_forecast) 
        fc_ph %>% autoplot(avg_times_week()) + 
          labs(y="Mean Travel Time", title="Forecasting mean travel time for weekly data")
      } else {
        fc_ph <- prophet_model() %>% forecast(h=input$prophet_forecast) 
        fc_ph %>% autoplot(avg_times_ts()) +
          labs(y="Mean Travel Time", title="Forecasting mean travel time for daily data")
      }
    }
    else {
      if(input$prophet_data == "Weekly Data") {
        fc_ph <- prophet_model() %>% forecast(h=input$prophet_forecast) 
        fc_ph %>% autoplot(trip_count_week()) +
          labs(y="Number of Trips", title="Forecasting number of trips for weekly data")
      } else {
        fc_ph <- prophet_model() %>% forecast(h=input$prophet_forecast) 
        fc_ph %>% autoplot(trip_count_ts()) +
          labs(y="Number of Trips", title="Forecasting number of trips for weekly data")
      }
    }
    
  }, res=100)
  
  
  output$prophet_train_prediction <- renderPlot({
    #test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(avg_times_week()))
    if(input$y=="mean_travel_time") {
      if(input$prophet_data == "Weekly Data") {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(avg_times_week()))
        prophet_train_model() %>% forecast(h=test_rows) %>% 
          autoplot(avg_times_week()) + labs(y="Mean Travel Time", title="Training the Prophet Model")
      }
      else {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(avg_times_ts()))
        print(test_rows)
        prophet_train_model() %>% forecast(h=test_rows) %>% 
          autoplot(avg_times_ts()) + labs(y="Mean Travel Time", title="Training the Prophet Model")
      }
      
    } else {
      if(input$prophet_data == "Weekly Data") {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(trip_count_week()))
        prophet_train_model() %>% forecast(h=test_rows) %>% 
          autoplot(trip_count_week()) + labs(y="Number of Trips", title="Training the Prophet Model")
      }
      else {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(trip_count_ts()))
        prophet_train_model() %>% forecast(h=test_rows) %>% 
          autoplot(trip_count_ts()) + labs(y="Number of Trips", title="Training the Prophet Model")
      }
    }
  }, res=100)
  
  output$prophet_accuracy <- renderTable({
    if(input$y=="mean_travel_time") {
      if(input$prophet_data == "Weekly Data") {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(avg_times_week()))
        metrics <- prophet_train_model() %>% forecast(h=test_rows) %>% 
          accuracy(avg_times_week()) 
        
        metrics %>% pivot_longer(!c(.model,.type),names_to="features",values_to="value") %>%
          select(features,value)
      }
      else {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(avg_times_ts()))
        metrics <- prophet_train_model() %>% forecast(h=test_rows) %>% 
          accuracy(avg_times_ts())
        
        metrics %>% pivot_longer(!c(.model,.type),names_to="features",values_to="value") %>%
          select(features,value)
      }
      
    } else {
      if(input$prophet_data == "Weekly Data") {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(trip_count_week()))
        metrics <- prophet_train_model() %>% forecast(h=test_rows) %>% 
          accuracy(trip_count_week())
        metrics %>% pivot_longer(!c(.model,.type),names_to="features",values_to="value") %>%
          select(features,value)
      }
      else {
        test_rows = as.integer(((100-input$prophet_train_percent)/100)*nrow(trip_count_ts()))
        metrics <- prophet_train_model() %>% forecast(h=test_rows) %>% 
          accuracy(trip_count_ts())
        metrics %>% pivot_longer(!c(.model,.type),names_to="features",values_to="value") %>%
          select(features,value)
      }
    }
  })
  
  
}

shinyApp(ui, server)

