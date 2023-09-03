# UberTravelDataAnalysis

In this project, I developed a dashboard to analyze traffic flow and urban mobility of Uber trips in North American cities to forecast future Uber demand and optimize trip durations. The data that is used is Uber movement data that provides a centralized and comprehensive source for downloading uber trips related data across various cities of the world. The project is built using R and the dashboard is deployed through Shiny. The data is dynamically ingested from AWS S3.

The scope of this project includes analyzing mean travel times in a day (averaged out by travel times from the central point of the city to all the other destination that was covered under Uber trips that day), and analyzing count of trips in a day (aggregated count of destination zones that occurred in a day. The objective is to understand how these two factors affect the demand of Uber in a given city and improve the revenue model through these usage statistics. A secondary aim is to monitor traffic states of urban road network which can be used by city planners to address urban transportation challenges.

The features of the dashboard include:

1. Users can select a city based on their choice to analyze the city’s travel time and destination zones covered by Uber on a daily basis.
2. Visualize various features of the time series data such as trends, seasonalities, autocorrelation plots, moving averages, lags, etc. to understand demand, profit distribution and improvement areas to increase revenue.
3. Perform traffic/trip demand forecasting through various models such as STL decomposition, ARIMA, ETL, Dynamic Harmonic Regression & Prophet and customize the hyperparameters of each of these models. Additional feature to perform boostrapping or bagging to improve forecast results.
4. Users can also train data by setting training data percentage, check residual and accuracy diagnostics, perform unit root tests to find out order of differencing and stationarity of the data and also set how many values they want to forecast.
5. Check model performance statistics through RMSE, MAE, Portmanteau tests and information criterias such as Akaike’s Information Criterion (AIC) & Bayesian Information Criterion (BIC).


The validation metrics chosen namely: Root Mean Square Error (RMSE), Mean absolute percentage error (MAPE) and  Autocorrelation of errors at lag 1 (ACF1) proved to be very valuable in distinguishing between the various models and select the best model. It was observed that Prophet shows an averge of 20% decrease in RMSE, and a 15% decrease in MAPE compared to other models. I concluded that advanced forecasting methods such as Prophet along with performance enhancement measures gives good forecast accuracy.
