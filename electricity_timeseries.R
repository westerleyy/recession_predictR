# setting up shop 
library(tidyverse)
library(ggplot2)
library(rjson)
library(jsonlite)
library(eia)
library(ggfortify)
library(fredr)
library(ggpmisc)

# load API key
eia_api_key <- readtext::readtext("eia_api_key.txt") %>%
  .$text
fred_api_key <- readtext::readtext("fred_api_key.txt") %>%
  .$text
eia_set_key(eia_api_key)
fredr_set_key(fred_api_key)

# Force updating 
# eia_clear_cache()

## get yield curve
yield_curve <- fredr(series_id = "T10Y3M", observation_start=as.Date('2015-07-02'))


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

## daily generation - only for complete days
lower48_daily_generation <- lower48_hourly_generation_df[6:(nrow(lower48_hourly_generation_df)-19),] %>%
  group_by(date) %>%
  summarize(daily_generation = sum(as.numeric(demand)),
            daily_generation_terawatt = daily_generation/1000000)

### plot time series
ggplot() +
  geom_line(data = lower48_daily_generation, aes(date, daily_generation_terawatt), color = "#80b1d3",size = 1) +
  geom_line(data = yield_curve, aes(date, value), color = "#8dd3c7", size = 1) 

yield_elec_df <- inner_join(lower48_daily_generation, yield_curve, "date")

ggplot(yield_elec_df, aes(daily_generation_terawatt, value)) + 
  geom_point()

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

## electricity retail sales to industrial
## last updated 3/26
electricity_sales_industrial_monthly <- eia_series(id = "TOTAL.ESICBUS.M")
electricity_sales_industrial_monthly_df <- unnest(all_electricity_generation_monthly, cols = "data") %>%
  select(series_id, units, value, date, year, month)

## fossil fuels industrial sector 
ff_industrial_sector <- eia_series(id = "TOTAL.FFICBUS.M", tidy = T)
ff_industrial_sector_df <- unnest(ff_industrial_sector, cols = "data") %>%
  select(series_id, units, value, date, year, month)

## crude oil and nat gas rigs in operation 
total_rigs <- eia_series(id = "TOTAL.OGNRPUS.M")
total_rigs_df <- unnest(total_rigs, cols = "data") %>%
  select(series_id, units, value, date, year, month)
