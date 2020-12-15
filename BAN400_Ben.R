#===============================================================================
#                BAN400 - TERM PAPER PART ONE: DATA VISUALIZATION              #
#===============================================================================

# Loading libraries 
library(foreign)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(gtrendsR)
library(reshape2)
library(covid19.analytics)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(countrycode)
library(fhidata)
library(prophet)
library(StanHeaders)
library(quantmod)
library(readxl)
library(PxWebApiData)
library(flexdashboard)


# ------------------------------------------------------------------------------
# Loading and plotting COVID-19 data 
# ------------------------------------------------------------------------------

# Loading COVID-19 data for Norway 
cases_covid <- covid19.data(case = "ts-confirmed") %>%
  filter(Country.Region == "Norway") %>%
  gather(date, confirmed_cases) %>%
  slice(5:n()) %>%
  as_tibble() %>%
  mutate(date = ymd(date),
         confirmed_cases = as.numeric(confirmed_cases),
         new_cases = confirmed_cases - lag(confirmed_cases,
                                           n = 1,
                                           default = first(confirmed_cases)))


# Plotting the COVID-19 data

# 1: New daily cases 
daily_cases <-
  ggplot(cases_covid, aes(x = date, y = new_cases)) +
  geom_ma(color = "indianred4",
          ma_fun = SMA,
          n = 7,
          size = 0.5,
          linetype = 1) +
  theme_minimal() +
  labs(x = NULL,
       y = "Number of infected",
       title = "New daily cases of COVID-19 in Norway",
       subtitle = "Data retrieved from CSSE at JHU, moving average = 7") +
  scale_x_date(date_labels = "%b",date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, max(cases_covid$new_cases), 200)) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40",
                                     size = 10,
                                     face = "italic"))

# Show plot
daily_cases


# 2: Total confirmed cases 
total_cases <-
  ggplot(cases_covid, aes(x = date, y = confirmed_cases)) + 
  geom_area(color = "lightsteelblue4",
            fill = "lightsteelblue",
            alpha = 0.4) +
  theme_minimal() +
  labs(x = NULL, y = "Number of infected",
       title ="Total confirmed cases of COVID-19 in Norway",
       subtitle = "Data retrieved from CSSE at JHU") + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, max(cases_covid$confirmed_cases), 5000)) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40",
                                     size = 10,
                                     face = "italic"))
# Show plot
total_cases


# World map --------------------------------------------------------------------

dcon <- covid19.data(case = "ts-confirmed") # Showing confirmed cases 
Livemap <- live.map(dcon) # Live map for confirmed cases 


# ------------------------------------------------------------------------------
# Loading and plotting Google Trends data 
# ------------------------------------------------------------------------------

# Function that loads and plots Google Trends with customized search word
google_trend <- function(search_word, country) {
  
  # Updating the date
  timespan <- paste("2020-01-01", Sys.Date())
  
  # Changing country name to country code 
  countrycode <- countrycode(country, 
                             origin = "country.name", 
                             destination = "iso2c")
  
  # Loading google trend data
  trends <- gtrends(keyword = search_word,
                    time = timespan,
                    geo = "NO") [[1]] %>%
    tibble() %>% 
    select(date, hits) %>%
    mutate(date = ymd(date)) %>%                       # Reformats date
    mutate_at("hits", ~ ifelse(. == "<1", 0, .)) %>%   # Changes hits <1 to 0
    mutate_at("hits", ~ as.numeric(.)) %>%             # Changes hits to numeric
    rename(searches = hits)
  
  # Creating plot
  ggplot(trends, aes(x = date, y = searches)) + 
    geom_line(color = "chartreuse4") + 
    theme_minimal() +
    labs(x = NULL, 
         y = "Relative number of searches",
         title = paste("Google searches for '", search_word,"' in", country),
         subtitle = "Numbers are relative, with 100 being max") + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "gray40",
                                       size = 10,
                                       face = "italic"))
  
}

# Using the function on different search words
gtrend_corona <-google_trend("corona", "Norway")
gtrend_corona

gtrend_munnbind<-google_trend("munnbind", "Norway")
gtrend_munnbind

gtrend_vinmonopolet<-google_trend("vinmonopolet", "Norway")
gtrend_vinmonopolet

gtrend_netflix<-google_trend("netflix", "Norway")
gtrend_netflix


# ------------------------------------------------------------------------------
# Comparing Google Trends data and COVID-19 data
# ------------------------------------------------------------------------------

# Function that loads gtrends data and compares them to covid data in a plot
google_cases<- function(search_word, country, MA = 3) {
  
  # Updating the date
  timespan <- paste("2020-01-01", Sys.Date())
  
  # Changing country name to country code (covid and gtrends use different ones)
  countrycode <- countrycode(country, 
                             origin = "country.name", 
                             destination = "iso2c")
  
  # Loading google trend data
  g_trends <- gtrends(keyword = search_word,
                      time = timespan,
                      geo = countrycode) [[1]] %>%
    tibble() %>% 
    select(date, hits) %>%
    mutate(date = ymd(date)) %>%                       # Reformats date
    mutate_at("hits", ~ ifelse(. == "<1", 0, .)) %>%   # Changes hits <1 to 0
    mutate_at("hits", ~ as.numeric(.)) %>%             # Changes hits to numeric
    rename(searches = hits)
  
  # Merge with covid data, make covid data relative (like gtrend data is)
  cases_gtrend <- inner_join(g_trends, cases_covid, by = "date") %>%
    tibble() %>% 
    mutate(infected_relative = round(new_cases/max(new_cases)*100)) %>% 
    select(date, searches, infected_relative)
  
  # Create plot
  ggplot(cases_gtrend, aes(x = date)) + 
    geom_ma(aes(y = searches, color = "searches"), 
            ma_fun = SMA, n = MA, linetype = 1, size = 0.5) + 
    geom_ma(aes(y = infected_relative, color = "infected"), 
            ma_fun = SMA, n = MA, linetype = 1, size = 0.5) + 
    theme_minimal() +
    scale_color_manual(values = c("indianred4","chartreuse4")) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    labs(x = NULL, y = "Relative number of searches & infected",
         title = paste("Google searches for '", search_word,
                       "' & new corona cases in", country),
         subtitle = "Numbers are relative, with 100 being max") + 
    theme(legend.title = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "gray40",
                                       size = 10,
                                       face = "italic"))
  
}

# Use function on different search words
corona_cases <-google_cases("corona", "Norway")
corona_cases

munnbind_cases <-google_cases("munnbind", "Norway")
munnbind_cases

vinmonopolet_cases <-google_cases("vinmonopolet", "Norway")
vinmonopolet_cases

netflix_cases <-google_cases("netflix", "Norway")
netflix_cases


# ------------------------------------------------------------------------------
# Loading and plotting data on bankruptcies from SSB 
# ------------------------------------------------------------------------------

# This part only focuses on creating histograms for SSB-data for bankruptcies 
# per industry. All other code and plots related to other macroeconomic 
# variables from SSB can be found in the Shiny App and the code behind it. 

# Function that loads and wrangles data from SSB 
bankrupt <- function(industry) {
  
  ApiData(urlToData = 08551,
          Region = "Heile landet", 
          NACE2007 = industry, 
          ContentsCode = "Konkurser",
          Tid = T)[[1]] %>% 
    
    # Separate SSB date format into month and year
    separate(col = "måned",
             into = c("year", "month"),
             sep = "M") %>%
    
    # Filter values by year 2020
    filter(year == "2020") %>% 
    
    # Reformat and keep only the values needed
    mutate(value = as.numeric(value),
           date = as.Date(as.yearmon(paste(year, month), "%Y %m"))) %>% 
    select(industry = "næring (SN2007)",
           amount = "value",
           "date")
}

# Bankruptcies per industry each month, with relative values as well
bankruptcies <- bankrupt(T) %>% 
  mutate(normalized = amount / max(amount) * 100)


# Plotting the data on bankruptcies

# One at a time in a loop
industry_list <- unique(bankruptcies$industry)

for (i in seq_along(industry_list)) {
  plot <- 
    ggplot(subset(bankruptcies, bankruptcies$industry==industry_list[i]),
           aes(fill=date, y=amount, x=date)) +
    theme_minimal() +
    geom_bar(stat="identity") +
    labs(x = NULL,
         y = NULL,
         title = industry_list[i]) +
    scale_x_date(date_labels = "%B",
                 date_breaks = "1 month") +
    theme(legend.title = element_blank(), legend.position = "none")
  
  assign(paste0("plot", i), plot)
}

#-------------------------------------------------------------------------------

# SSB data: https://data.ssb.no/api/v0/dataset/
# Colors in r: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf




