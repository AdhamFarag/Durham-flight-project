library(tidyverse)
library(lubridate)
library(ggpubr)
library(rvest)
rm(list = ls())

raw_data = NULL
for( i in 1:17){
  tmp = readxl::read_xlsx( "data/UofT Data Set.xlsx", skip = 1, sheet = i, 
                           col_names =  paste( "X", 1:12, sep="" ) ) %>% 
    mutate( Instructor_ID = i,
            PPL = X1,
            X1 = replace( X1, !str_detect(X1, "Student"), NA ),
            PPL = zoo::na.locf( PPL ),
            X1 = zoo::na.locf(X1) )
  raw_data = bind_rows( raw_data, tmp )
}
rm(tmp,i)

names( raw_data  ) = c( "Student", "Year", "Month", "Day", "Aircraft", "LF_dual", 
                        "LF_solo", "Instrument_AC",  "Instrument_Sim", "CC_dual", "CC_solo", "Exercises",
                        "Instructor_ID", "Licence")

head(raw_data)

raw_data %>%
  filter( !is.na(Year), Year != "Year",
          Year >= 2016, Year <= 2020,) %>% 
  mutate_at( .vars = c(2:4), .funs = as.integer ) %>% 
  mutate_at( .vars = c(6:11), .funs = as.numeric ) %>% 
  mutate( Aircraft = str_to_upper(Aircraft),
          Aircraft = replace( Aircraft, str_detect(Aircraft, "GROUND"), "GROUND"),
          Aircraft = replace_na( Aircraft, "NA"),
          Aircraft = replace(Aircraft, Aircraft=="C152", "C-152"),
          Month = replace(Month, Month==111, 11),
          Other = ifelse( str_detect(Aircraft,"GROUND|NA"), -1, NA ),
          Student_ID = as.numeric( factor( paste( Student, Instructor_ID) ) ), 
          Session_ID = row_number() ) %>% 
  gather( key = "Training_Type", value = "Duration", 6:11, Other) %>% 
  filter( !is.na(Duration) ) %>% 
  mutate( Duration  = na_if(Duration, -1),
          Aircraft = na_if(Aircraft, "NA")) %>% 
  select( Instructor_ID, Student_ID, Session_ID, Year, Month, Day, 
          Aircraft, Duration, Training_Type, Exercises, Licence ) -> clean_data

getSeason <- function(month) {
  ifelse(month >= 3 & month <= 5, "Spring",
         ifelse(month >= 6 & month <= 8, "Summer",
                ifelse(month >= 9 & month <= 11, "Fall", "Winter")))
}

clean_data_processed = clean_data %>% 
  distinct( Session_ID, .keep_all = T) %>% 
  # split the exercises string into a "list" column w str_split()
  mutate( Exercises = str_split(Exercises, ",") ) %>%  
  # and expand list contents into multiple rows w/ unnest()
  unnest(Exercises) %>%
  # remove invalid exercises
  mutate(Exercises = as.integer(Exercises)) %>%
  filter(Exercises >= 1 & Exercises <= 30) %>%
  distinct_all() %>%
  mutate(
    Season = getSeason(Month), 
    Date = make_date(Year, Month, Day), 
    COVID = Year >= 2020
  )

# reading in the fuel per gallon price
webpage <- read_html("https://www.indexmundi.com/commodities/?commodity=jet-fuel&months=240&currency=cad")
tbls <- html_nodes(webpage, "table") %>%
  html_table(fill = TRUE)
fuel_prices <- as.data.frame(tbls[2])
colnames(fuel_prices) <- c("Month_Year", "Price","Change")
clean_data_processed$Month_Words <- month.abb[clean_data_processed$Month]
clean_data_processed$Month_Year <- (str_c(clean_data_processed$Month_Words, clean_data_processed$Year, sep = " "))

clean_data_processed <- left_join(clean_data_processed,fuel_prices,by="Month_Year",copy = TRUE)
clean_data_processed <- select(clean_data_processed, -c("Month_Year","Change","Month_Words"))
colnames(clean_data_processed)[colnames(clean_data_processed) == 'Price'] <- 'Fuel Price Per Gallon'

process_weather <- function(data) {
  data %>% 
    select(
      "date",
      "min_windchill",
      "avg_relative_humidity",
      "avg_dew_point",
      "avg_pressure_sea",
      "avg_pressure_station",
      "avg_visibility",
      "avg_health_index",
      "precipitation",
      "avg_cloud_cover_4",
      "avg_temperature"
    ) %>%
    filter(date <"2021-01-01")
}

climate_data <- read_csv('data/weatherstats_oshawa_daily.csv') %>% process_weather()

# ----- BEGIN NEW CODE -----
exercises_model <- clean_data %>% 
  filter(Aircraft != "GROUND") %>%
  # remove trailing comma
  mutate(Exercises = str_replace(Exercises, ",$", "")) %>% 
  # remove leading comma
  mutate(Exercises = str_replace(Exercises, "^,", "")) %>%
  # remove duplicate comma
  mutate(Exercises = str_replace(Exercises, ",,", ",")) %>%
  group_by(Session_ID, Exercises, Year, Month, Day, Aircraft) %>% 
  # can have multiple training types, therefore get total duration per session
  summarise(Total_Duration = sum(Duration, na.rm = T)) %>%
  mutate(
    Season = getSeason(Month), 
    Date = make_date(Year, Month, Day),
    Month_Words = month.abb[Month]
  ) %>% 
  mutate(Month_Year = (str_c(Month_Words, Year, sep = " "))) %>%
  left_join(climate_data, by=c("Date" = "date")) %>%
  left_join(fuel_prices, by="Month_Year") %>%
  mutate(`Fuel Price Per Gallon` = Price) %>%
  select(-c(Month_Year, Month_Words, Change, Price))
# ----- END NEW CODE -----

clean_data_processed <- left_join(clean_data_processed, climate_data, by=c("Date" = "date"), copy=True)