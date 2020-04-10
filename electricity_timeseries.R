# setting up shop 
library(tidyverse)
library(ggplot2)
library(rjson)
library(jsonlite)
library(eia)

# load API key
eia_api_key <- readtext::readtext("eia_api_key.txt") %>%
  .$text
eia_set_key(eia_api_key)

# load dataset
# API call allows for output to be in JSON or XML; picked JSON for ease 
# UPDATE: GOD DAMN IT I FOUND A SIMPLER WAY AFTER KILLING A TON OF BRAINCELLS TRYING TO UNNEST A JSON | EIA has a package to do this

## hourly demand

lower48_hourly_demand <- paste("http://api.eia.gov/series/?series_id=EBA.US48-ALL.D.H&api_key=", eia_api_key, sep = "") %>%
  fromJSON() 
lower48_hourly_demand_list <- lower48_hourly_demand$series %>%
  select(data)%>%
  .[[1]] 
date_time <- lower48_hourly_demand_list[[1]][,1]
demand <- lower48_hourly_demand_list[[1]][,2]
lower48_hourly_demand_df <-  cbind(date_time, demand) %>%
  data.frame(stringsAsFactors = F) %>%
  mutate(date = as_date(ymd(substr(date_time, 1, 8))),
         hour = as.numeric(substr(date_time, 10, 11)))

## hourly generation
lower48_hourly_generation <- paste("http://api.eia.gov/series/?series_id=EBA.US48-ALL.NG.H&api_key=", eia_api_key, sep = "") %>%
  fromJSON()
lower48_hourly_generation_list <- lower48_hourly_generation$series %>%
  select(data)%>%
  .[[1]] 
date_time <- lower48_hourly_generation_list[[1]][,1]
demand <- lower48_hourly_generation_list[[1]][,2]
lower48_hourly_generation_df <-  cbind(date_time, demand) %>%
  data.frame(stringsAsFactors = F) %>%
  mutate(date = as_date(ymd(substr(date_time, 1, 8))),
         hour = as.numeric(substr(date_time, 10, 11)))

## day ahead demand forecast
lower48_hourly_forecast <- paste("http://api.eia.gov/series/?series_id=EBA.US48-ALL.DF.H&api_key=", eia_api_key, sep = "") %>%
  fromJSON()
lower48_hourly_forecast_list <- lower48_hourly_forecast$series %>%
  select(data)%>%
  .[[1]] 
date_time <- lower48_hourly_forecast_list[[1]][,1]
demand <- lower48_hourly_forecast_list[[1]][,2]
lower48_hourly_forecast_df <-  cbind(date_time, demand) %>%
  data.frame(stringsAsFactors = F) %>%
  mutate(date = as_date(ymd(substr(date_time, 1, 8))),
         hour = as.numeric(substr(date_time, 10, 11)))

## all generation
all_electricity_generation_monthly <- eia_series(id = "TOTAL.ELETPUS.M")
all_electricity_generation_monthly_df <- unnest(all_electricity_generation_monthly, cols="data")
all_electricity_generation_monthly_df <- all_electricity_generation_monthly_df %>%
  select(series_id, units, value, date, year, month)
## operating data
