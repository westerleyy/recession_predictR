# setting up shop 
library(tidyverse)
library(ggplot2)
library(rjson)
library(jsonlite)
library(eia)
library(ggfortify)
library(fredr)
library(ggpmisc)
library(zoo)
library(plotly)

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

## business tendenc
business_tendenc <- fredr(series_id = "BSCICP03USM665S", observation_start = as.Date("1973-01-01"))

## finding local peaks where m is the number of points on either side of the peak 
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

distance_from_peak <- function(current_position, peaks_vec, time_series_value){
  distance <- peaks_vec-current_position
  closest <- min(distance)
  distance_pct <- (time_series_value[current_position] - time_series_value[closest])/time_series_value[closest]
  distance_pct <- ifelse(is.null(distance_pct) == T, 0, distance_pct)
  distance_pct
}

business_local_peaks <- find_peaks(business_tendenc$value)
distance_peak <- sapply(1:dim(business_tendenc)[1], function(x){
  distance_pct <- distance_from_peak(x, business_local_peaks, business_tendenc$value)
  distance_pct
})

business_tendencies <- cbind(business_tendenc, distance_peak)
business_tendencies[is.na(business_tendencies)] <- 0
write.csv(business_tendencies, "business_tendencies.csv")

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


## all generation
## last updated 3/26
all_electricity_generation_monthly <- eia_series(id = "TOTAL.ELETPUS.M")
all_electricity_generation_monthly_df <- unnest(all_electricity_generation_monthly, cols="data")
all_electricity_generation_monthly_df <- all_electricity_generation_monthly_df %>%
  select(series_id, units, value, date, year, month)

all_electricity_generation_monthly_df <- all_electricity_generation_monthly_df %>%
  select(series_id, value, date, year, month) %>%
  mutate(terawatt_value = value/1000, 
         units = "Terawatthours",
         date = as.yearmon(unique(substr(date, 1,7))))

## hourly generation
lower48_hourly_generation <- eia_series(id = "EBA.US48-ALL.NG.H")
lower48_hourly_generation_df <- unnest(lower48_hourly_generation, cols = "data") %>%
  select(series_id, units, value, date, year, month)

## monthly generation from hourly

lower48_monthly_generation_df <- lower48_hourly_generation_df %>%
  group_by(year, month) %>%
  summarise(value = (sum(value))/1000000,
            units = "Terawatthours",
            date = unique(substr(date, 1,7)))
lower48_monthly_generation_df$date <- as.yearmon(lower48_monthly_generation_df$date)

# monthly plot from hourly data and all 48 states (three months late)
ggplotly(ggplot() + 
  geom_line(lower48_monthly_generation_df, mapping = aes(date, value)) + 
  geom_line(all_electricity_generation_monthly_df, mapping = aes(date, terawatt_value), alpha = 0.2))

# intersecting all states with lower 48 | then calculate prop lower48 to extrapolate retroactively
intersecting_months <- inner_join(lower48_monthly_generation_df, all_electricity_generation_monthly_df, by = "date")
colnames(intersecting_months)[3] <- "lower48_terawatt" 
intersecting_months$proportion <- intersecting_months$lower48_terawatt/intersecting_months$terawatt_value
lower48_prop <- fivenum(intersecting_months$proportion)[3]
all_electricity_generation_monthly_extrapolated_df <- all_electricity_generation_monthly_df %>%
  mutate(lower48_terawatt = terawatt_value*lower48_prop) %>%
  filter(date < "Jul 2015") %>%
  select(year, month, lower48_terawatt, units, date)
colnames(all_electricity_generation_monthly_extrapolated_df)[3] <- "value"
lower48_monthly_generation_extrapolated_df <- bind_rows(all_electricity_generation_monthly_extrapolated_df, lower48_monthly_generation_df)

write.csv(lower48_monthly_generation_extrapolated_df, "monthly_electricity.csv")


## daily generation from hourly
## remove day one and today bec. likely to be incomplete
## daily plot

lower48_daily_generation_df <- lower48_hourly_generation_df %>%
  mutate(day = substr(date, 1, 10)) %>%
  group_by(day) %>%
  summarise(value = sum(value)) %>%
  mutate(day = as.Date(day, format = "%Y-%m-%d"))
lower48_daily_generation_df <- lower48_daily_generation_df[2:(dim(lower48_daily_generation_df)[1]-1),]
ggplot(lower48_daily_generation_df, aes(x = day, y = value)) + 
  geom_line()



# ### plot time series
# ggplot() +
#   geom_line(data = lower48_daily_generation, aes(date, daily_generation_terawatt), color = "#80b1d3",size = 1) +
#   geom_line(data = yield_curve, aes(date, value), color = "#8dd3c7", size = 1) 
# 
# yield_elec_df <- inner_join(lower48_daily_generation, yield_curve, "date")
# 
# ggplot(yield_elec_df, aes(daily_generation_terawatt, value)) + 
#   geom_point()
# 
# ## day ahead demand forecast
# lower48_hourly_forecast <- paste("http://api.eia.gov/series/?series_id=EBA.US48-ALL.DF.H&api_key=", eia_api_key, sep = "") %>%
#   fromJSON()
# lower48_hourly_forecast_list <- lower48_hourly_forecast$series %>%
#   select(data)%>%
#   .[[1]] 
# date_time <- lower48_hourly_forecast_list[[1]][,1]
# demand <- lower48_hourly_forecast_list[[1]][,2]
# lower48_hourly_forecast_df <-  cbind(date_time, demand) %>%
#   data.frame(stringsAsFactors = F) %>%
#   mutate(date = as_date(ymd(substr(date_time, 1, 8))),
#          hour = as.numeric(substr(date_time, 10, 11)))





## operating data

## electricity retail sales to industrial
## last updated 3/26
electricity_sales_industrial_monthly <- eia_series(id = "TOTAL.ESICBUS.M")
electricity_sales_industrial_monthly_df <- unnest(all_electricity_generation_monthly, cols = "data") %>%
  select(series_id, units, value, date, year, month)

ggplot(electricity_sales_industrial_monthly_df, aes(x = date, y = value)) + 
  geom_line()

## fossil fuels industrial sector 
ff_industrial_sector <- eia_series(id = "TOTAL.FFICBUS.M", tidy = T)
ff_industrial_sector_df <- unnest(ff_industrial_sector, cols = "data") %>%
  select(series_id, units, value, date, year, month)

ggplot(ff_industrial_sector_df, aes(x = date, y = value)) + 
  geom_line()

## crude oil and nat gas rigs in operation 
total_rigs <- eia_series(id = "TOTAL.OGNRPUS.M")
total_rigs_df <- unnest(total_rigs, cols = "data") %>%
  select(series_id, units, value, date, year, month)
total_rigs_df <- total_rigs_df %>%
  mutate(change_first_order = round((value - lag(value, 1))/lag(value,1),4),
         change_second_order = ifelse(lag(change_first_order,1) == 0, 0, (change_first_order - lag(change_first_order, 1))/lag(change_first_order,1)))

ggplot(total_rigs_df, aes(x = date, y = value)) + 
  geom_line() + 
  labs(title = "Number of crude oil and natural gas rigs in US, monthly")

ggplot(total_rigs_df, aes(x = date, y = change_first_order)) + 
  geom_line() + 
  labs(title = "Rate of Change in monthly rigs count")

ggplot(total_rigs_df, aes(x = date, y = change_second_order)) + 
  geom_line() + 
  labs(title = "d2y/dx2 in monthly rigs count")

write.csv(total_rigs_df, "monthly_rigs_count.csv")
