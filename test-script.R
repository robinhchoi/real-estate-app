library(Quandl)
library(dplyr)
library(ggplot2)
library(plotly)
library(highcharter)

setwd("C:/Users/rchoi/Documents/R Shiny Apps/real-estate-app")
Quandl.api_key("_Mm3p1L8mpQEDCPtxfpH")

area_category_cities <- read.csv("area-category-cities.csv", header = TRUE)
area_category_counties <- read.csv("area-category-counties.csv", header = TRUE)
area_category_metros <- read.csv("area-category-metros.csv", header = TRUE)
area_category_neighborhoods <- read.csv("area-category-neighborhoods.csv", header = TRUE)
area_category_states <- read.csv("area-category-states.csv", header = TRUE)

area_category_states$AREA <- sapply(area_category_states$AREA, tolower)
area_category_cities$AREA <- sapply(area_category_cities$AREA, tolower)
area_category_cities$STATE <- sapply(area_category_cities$STATE, tolower)

# make this object oriented and make these functions methods within app(), and create method called run()

state_analysis <- function() {
  state <- tolower(as.character(readline(prompt="Correctly enter the full name of the state: ")))
  if (!(state %in% area_category_states$AREA)) {
    state_analysis()
  }
  zillow_state_code <- area_category_states$CODE[match(state, area_category_states$AREA)]
  quandl_query <- paste0("ZILLOW/S", zillow_state_code, "_ZHVIAH")
  data <- Quandl(quandl_query)
  p <- plot_ly(data, x=~Date, y=~Value, type="scatter", mode="lines")
  p
}

city_analysis <- function() {
  city <- tolower(as.character(readline(prompt="Correctly enter the full name of the city: ")))
  if (!(city %in% area_category_cities$AREA)) {
    city_analysis()
  }
  city_subset <- area_category_cities %>% filter(AREA == city)
  state <- tolower(as.character(readline(prompt="Enter the state abbreviation code: ")))
  if (!(state %in% city_subset$STATE)) {
    print("Could not find data for city")
    break
  }
  
  zillow_city_code <- area_category_cities$CODE[match(city, area_category_cities$AREA)]
  quandl_query <- paste0("ZILLOW/C", zillow_city_code, "_ZHVIAH")
  data <- Quandl(quandl_query)
  p <- plot_ly(data, x=~Date, y=~Value, type="scatter", mode="lines")
  p
}

zip_analysis <- function() {
  
}

app <- function() {
  user_input <- tolower(as.character(readline(prompt="Would you like to analyze by state, city, or ZIP? ")))
  if (user_input == "state") {
    state_analysis()
  } else if (user_input == "city") {
    city_analysis()
  } else if (user_input == "zip") {
    ## function to analyze by zip
  } else {
    area_type()
  }
}

app()

