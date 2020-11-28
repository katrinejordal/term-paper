###
## BAN400 - Financial & Economic data
###
# Libraries --------------------------------------------------------------------
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
library (PxWebApiData)


# COVID-19 data ----------------------------------------------------------------

cases_covid <- covid19.data(case = "ts-confirmed") %>%
  filter(Country.Region == "Norway") %>%
  gather(date, confirmed_cases) %>%
  slice(5:n()) %>%
  as_tibble() %>%
  mutate(date = ymd(date),
         confirmed_cases = as.numeric(confirmed_cases),
         new_cases = confirmed_cases - lag(
           confirmed_cases,
           n = 1,
           default = first(confirmed_cases)
    )
  )
 
#covid_total <- tibble(cases_covid[, c("confirmed_cases", "date")])
#covid_daily <- tibble(cases_covid[, c("new_cases", "date")])


# Google trends data -----------------------------------------------------------
gtrend_search <- function(search_word, country, MA = 1) {
 
  # Updating the date
  timespan <- paste("2020-01-01", Sys.Date())
  
  # Changing country name to country code
  countrycode <- countrycode(country, 
                             origin = "country.name", 
                             destination = "iso2c")
  
  # Loading google trend data
  google_trends <- gtrends(keyword = search_word,
                           time = timespan,
                           geo = "NO") [[1]] %>%
    select(date, hits) %>%
    mutate(date = ymd(date)) %>%
    mutate_at("hits", ~ ifelse(. == "<1", 0, .)) %>%
    mutate_at("hits", ~ as.numeric(.)) %>%
    rename(searches = hits)
  
}

corona <-gtrend_search("corona", "Norway")
# korona <- gtrend_search("korona", "Norway")
  
# google <- xts(gtrend$searches, gtrend$date)


# Currency ---------------------------------------------------------------------

currency_pair <- "NOK=X"

getSymbols(currency_pair, auto.assign = TRUE)

USD_NOK <- Cl(`NOK=X`)
USD_NOK <- USD_NOK["2020"]
colnames(USD_NOK) <- "USD_NOK_actual"
USD_NOK <- as.xts(transform(USD_NOK, USD_NOK_relative = 
                              USD_NOK_actual / max(USD_NOK_actual) * 100))

# Oil-price --------------------------------------------------------------------

getSymbols("DCOILBRENTEU", src = "FRED", auto.assign = TRUE) ["2020"]

# Extract the close column, only from 2020-01-01, changing column name
brent_oil <- DCOILBRENTEU["2020"] %>% 
  na.omit() %>% 
  setNames(., "brent_oil_actual")

# Creating column for relative values, for comparison with other values
brent_oil <- as.xts(transform(brent_oil, brent_oil_relative = 
                brent_oil_actual / max(brent_oil_actual) * 100))


# ------------------------------------------------------------------------------
# Loading economic data from SSB using SSBs own package "PxWebApiData"
# ------------------------------------------------------------------------------

# Different macroeconomic values are presented in different data sets at SSB,
# with variable names differing somewhat in each data set. We therefore created
# one function for each data set we wanted to load data from.

# Gross Domestic Product, import and export ------------------------------------

ssb_data <- function(macro_size) {

  ApiData(urlToData = 11721,
          Makrost = macro_size,
          ContentsCode = "Løpende priser, sesongjustert (mill. kr)",
          Tid = T) [[1]]
}

gdp <- ssb_data("bnpb.nr23_9")            # GDP including oil
gdp_ex_oil <- ssb_data("bnpb.nr23_9fn")   # GDP excluding oil
import <- ssb_data("imp.nrtot")           
export <- ssb_data("eks.nrtot")


# Consumer Price Index ---------------------------------------------------------

ssb_cpi <- function(macro_size) {
  ApiData(urlToData = 05327,
          Konsumgrp = macro_size,
          ContentsCode = "KPIJustIndMnd",
          Tid = T) [[1]] 
}

cpi_ja <- ssb_cpi("JA_TOTAL")    # Adjusted for tax changes
cpi_jae <- ssb_cpi("JAE_TOTAL")  # Adjusted for tax changes, ex. energy products


# Unemployment, both LFS (AKU) and NAV -----------------------------------------

ssb_wf <- function(macro_size) {
  
  ApiData(urlToData = 08931,
          Kjonn = "0",
          Alder = "15-74",
          ContentsCode = macro_size,
          Tid = T) [[1]] 
}

lfs <- ssb_wf("Arbeidslause2")
nav <- ssb_wf("Arbeidslause6")


# Money supply -----------------------------------------------------------------

ssb_ms <- function(macro_size) {
  
  ApiData(urlToData = 10945,
          ContentsCode = macro_size,
          Tid = T) [[1]] 
}

m1 <- ssb_ms("PengmengdBehM1")
m2 <- ssb_ms("PengmengdBehM2")
m3 <- ssb_ms("PengmengdBehM3")


# Data manipulation ------------------------------------------------------------

cleanup <- function(data_frame) {
  
  # Separate SSB date format into month and year
  separate(data = data_frame,
           col = "måned",
           into = c("year", "month"),
           sep = "M") %>%
    
    # Filter values by year 2020 
    filter(year == "2020") %>%
    
    # Reformat, normalize and keep only the values needed
    na.omit() %>% 
    transmute(amount = as.numeric(value),
              date = as.Date(as.yearmon(paste(year, month), "%Y %m")),
              normalized = amount / max(amount) * 100)
}

gdp <- cleanup(gdp)
gdp_ex_oil <- cleanup(gdp_ex_oil)
import <- cleanup(import)
export <- cleanup(export)
cpi_ja <- cleanup(cpi_ja)
cpi_jae <- cleanup(cpi_jae)
lfs <- cleanup(lfs)
nav <- cleanup(nav)
m1 <- cleanup(m1)
m2 <- cleanup(m2)
m3 <- cleanup(m3)

# Prøver å lage en loop eller funksjon som tar cleanup på alle data frames i 
# stedet for å gjøre det for alle df hver for seg. Kodene under gjør det, men
# navnene på data frames forsvinner...

#values <- list(gdp, gdp_ex_oil, import, export, cpi_ja, cpi_jae, lfs, nav, m1,
# m2, m3)
#res <- lapply(values, head, cleanup)
#for (i in 1:length(res)) {
#  assign(paste0("res", i), as.data.frame(res[[i]]))
#}


# Bankruptcies -----------------------------------------------------------------

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
    
    # Reformat, normalize and keep only the values needed
    mutate(value = as.numeric(value),
           date = as.Date(as.yearmon(paste(year, month), "%Y %m"))) %>% 
    select(industry = "næring (SN2007)",
           amount = "value",
           "date")
}
  
# Bankruptcies per industry each month
bankruptcies <- bankrupt(T) %>% 
  filter(industry != "Alle næringar") %>% 
  mutate(normalized = amount / max(amount) * 100)
  
# Bankruptcies in total each month
bankruptcies_total <- bankrupt("Alle næringar") %>% 
  mutate(normalized = amount / max(amount) * 100)


# Merge all SSB data into one data frame ---------------------------------------
# OBS: oljepris og valuta er xts. Kan ikke merges med SSB med mindre disse også 
# gjøres om til xts. Må se mer på merging av SSB, virker ikke enda. 

start_date <- as.Date("2020-01-01")
end_date <- Sys.Date()
dates <- seq(from = start_date, to = end_date, by = "days")

eco_xts <- merge(cases_covid, corona, gdp, gdp_ex_oil, import,
                 export, m1, m2, m3, cpi_ja, cpi_jae, lfs, nav, bankruptcies, 
                 bankruptcies_total)


merged_xts <- merge(eco_xts, dates)
merged_xts <- na.approx(merged_xts) # to get a line between the two points in time
merged_xts <- merge(merged_xts, dates) # carry forward

merged_df <- as.data.frame(merged_xts)


# Plots ------------------------------------------------------------------------
colnames(merged_df)

ggplot(merged_df, aes(x = dates)) + 
  theme_classic() + 
  labs(colour = "", x = "", y = "", title = "") + 
  scale_x_date(name = "", date_labels = "%B", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 10),
                     sec.axis = sec_axis(~ ., breaks = seq(0, 120, by = 10))) + 
  scale_color_manual(values = c("palegoldenrod",
                                "palegreen3",
                                "palegreen4",
                                "paleturquoise3", 
                                "paleturquoise4",
                                "palevioletred3",
                                "palevioletred4",
                                "peachpuff3",
                                "peachpuff4",
                                "skyblue3",
                                "skyblue4")) + 
  geom_line(aes(y = osebx, color = "osebx"), size = 2) + 
  geom_line(aes(y = oilservice, color = "oilservice"), size = 1) + 
  geom_line(aes(y = seafood, color = "seafood"), size = 1) +
  geom_line(aes(y = shipping, color = "shipping"), size = 1) +
  geom_line(aes(y = equity_certificates, color = "equity_certificates"), size = 1) +
  geom_line(aes(y = mid_cap, color = "mid_cap"), size = 1) +
  geom_line(aes(y = small_cap, color = "small_cap"), size = 1)

ggplot(merged_df, aes(x = dates)) + 
  theme_classic() + 
  labs(colour = "", x = "", y = "", title = "") + 
  scale_x_date(name = "", date_labels = "%B", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 10),
                     sec.axis = sec_axis(~ ., breaks = seq(0, 150, by = 10))) + 
  scale_color_manual(values = c("palegoldenrod",
                                "palegreen3",
                                "palegreen4",
                                "paleturquoise3", 
                                "paleturquoise4",
                                "palevioletred3",
                                "palevioletred4",
                                "peachpuff3",
                                "peachpuff4",
                                "skyblue3",
                                "skyblue4")) + 
  geom_line(aes(y = osebx, color = "osebx"), size = 2) + 
  geom_line(aes(y = energy, color = "energy"), size = 1) + 
  geom_line(aes(y = materials, color = "materials"), size = 1) +
  geom_line(aes(y = industry, color = "industry"), size = 1) +
  geom_line(aes(y = consumer_discretionary, color = "consumer_discretionary"), size = 1) +
  geom_line(aes(y = consumer_staples, color = "consumer_staples"), size = 1) +
  geom_line(aes(y = finance, color = "finance"), size = 1) +
  geom_line(aes(y = it, color = "it"), size = 1) +
  geom_line(aes(y = communication, color = "communication"), size = 1) +
  geom_line(aes(y = real_estate, color = "real_estate"), size = 1) +
  geom_line(aes(y = health_care, color = "health_care"), size = 1)




ggplot(merged_df, aes(x = dates)) + 
  theme_classic() + 
  labs(colour = "", x = "", y = "", title = "") + 
  scale_x_date(name = "", date_labels = "%B", date_breaks = "1 month") +
  scale_y_continuous(sec.axis = sec_axis(~ .)) + 
  geom_line(aes(y = covid_daily, color = "covid_daily")) 




## Planen var å få til noe sånt, illustre finansiell/økonomisk data og covid:
ggplot(merged_df, aes(x = dates)) + 
  geom_col(aes(y = covid_daily/5), size = 1, color = "paleturquoise4", fill = "paleturquoise4") +
  geom_line(aes(y = osebx), size = 1.5, color = "peachpuff3") +
  geom_line(aes(y = oilservice), size = 1.5, color = "peachpuff4") +
  scale_y_continuous(name = "OSEBX and Oilservice normalized to 100", 
                     sec.axis = sec_axis(~.*5, name = "Corona cases daily"))

# Få til label på slutten, slik at det er tydelig at det er OSEBX.
# Få det inn i en funksjon, sånn at man enkelt kan lage plots

#-------------------------------------------------------------------------------
#            OBS DENNE DELEN MÅ AVVENTES MED PGA FLYTTING OSLO BØRS
#            WORST CASE EFFEKTIVISER KODEN (!) OG BRUK EXCEL FILER
#-------------------------------------------------------------------------------

# Stocks and indices -----------------------------------------------------------

# Yahoo does not provide historical data for indicies anymore. 
# I could not find any other source. 
# Webscraping via investing.com could be a possibility. But seems like python
# is only option..
# For now, we download data from oslobors.no to xls files. 

# OSEBX
#osebx_xl <- read_excel("OSEBX.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(osebx = Siste / last(Siste) * 100)
#osebx <- xts(osebx_xl$osebx, osebx_xl$date)

# OBX
#obx_xl <- read_excel("OBX.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(obx = Siste / last(Siste) * 100)
#obx <- xts(obx_xl$obx, obx_xl$date)

# OBOSX - Oil service index
#obosx <- read_excel("OBOSX.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(obosx = Siste / last(Siste) * 100)
#oilservice <- xts(obosx$obosx, obosx$date)

# OBSFX - Seafood index
#obsfx <- read_excel("OBSFX.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(obsfx = Siste / last(Siste) * 100)
#seafood <- xts(obsfx$obsfx, obsfx$date)

# OBSHX - Shipping index
#obshx <- read_excel("OBSHX.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(obshx = Siste / last(Siste) * 100)
#shipping <- xts(obshx$obshx, obshx$date)

# OSEEX - Equity Certificate Index (egenkapitalbevis, sparebanker)
#oseex <- read_excel("OSEEX.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(oseex = Siste / last(Siste) * 100)
#equity_certificates <- xts(oseex$oseex, oseex$date)

# OSEMX - Mid cap index
#osemx <- read_excel("OSEMX.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(osemx = Siste / last(Siste) * 100)
#mid_cap <- xts(osemx$osemx, osemx$date)

# OSESX - Small cap index
#osesx <- read_excel("OSESX.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(osesx = Siste / last(Siste) * 100)
#small_cap <- xts(osesx$osesx, osesx$date)

# OSE10GI - Energy
#ose10gi <- read_excel("OSE10GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose10gi = Siste / last(Siste) * 100)
#energy <- xts(ose10gi$ose10gi, ose10gi$date)

# OSE15GI - Materials
#ose15gi <- read_excel("OSE15GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose15gi = Siste / last(Siste) * 100)
#materials <- xts(ose15gi$ose15gi, ose15gi$date)

# OSE20GI - Industry
#ose20gi <- read_excel("OSE20GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose20gi = Siste / last(Siste) * 100)
#industry <- xts(ose20gi$ose20gi, ose20gi$date)

# OSE25GI - Consumergoods Discretionary 
#ose25gi <- read_excel("OSE25GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose25gi = Siste / last(Siste) * 100)
#consumer_discretionary <- xts(ose25gi$ose25gi, ose25gi$date)

# OSE30GI - Consumergoods Staples 
#ose30gi <- read_excel("OSE30GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose30gi = Siste / last(Siste) * 100)
#consumer_staples <- xts(ose30gi$ose30gi, ose30gi$date)

# OSE35GI - Health care
#ose35gi <- read_excel("OSE35GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose35gi = Siste / last(Siste) * 100)
#health_care <- xts(ose35gi$ose35gi, ose35gi$date)

# OSE40GI - Finance 
#ose40gi <- read_excel("OSE40GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose40gi = Siste / last(Siste) * 100)
#finance <- xts(ose40gi$ose40gi, ose40gi$date)

# OSE45GI - IT
#ose45gi <- read_excel("OSE45GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose45gi = Siste / last(Siste) * 100)
#it <- xts(ose45gi$ose45gi, ose45gi$date)

# OSE50GI - Communication 
#ose50gi <- read_excel("OSE50GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose50gi = Siste / last(Siste) * 100)
#communication <- xts(ose50gi$ose50gi, ose50gi$date)

# OSE55GI - Utilities
#ose55gi <- read_excel("OSE55GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose55gi = Siste / last(Siste) * 100)
#utilities <- xts(ose55gi$ose55gi, ose55gi$date)

# OSE60GI - Real Estate 
#ose60gi <- read_excel("OSE60GI.xlsx") %>% 
#  select(1, 2) %>% 
#  rename(date = 1) %>% 
#  mutate(ose60gi = Siste / last(Siste) * 100)
#real_estate <- xts(ose60gi$ose60gi, ose60gi$date)



#list.files(pattern = "xlsx")


# Yahoo does provide historical data for individual stocks. 
#getSymbols("EQNR.OL", auto.assign = TRUE)
#EQNR <- Cl(EQNR.OL)
#EQNR <- EQNR["2020"]
#head(EQNR)

#-------------------------------------------------------------------------------

## Forbedringer script: --------------
## Laste inn data fra oslobørs på en bedre måte. 
## Enten webscrape investing.com, slik at man slipper å oppdatere excel-filene
## Eller gjøre som dag 3 i BAN420, laste inn alle filer i mappen raskt og enkelt
##
## Lage functioner til plotting (og laste inn data?), sånn at man enkelt kan velge
## hvilke økonomiske/finansielle størrelser man ønsker
## Mulig å tilgjengeliggjøre dette på en enkel html-side, eller noe github-greier?

## Linker til data:
# Indekser på oslobørs: https://www.oslobors.no/markedsaktivitet/#/list/shareindices/quotelist/intraday
# SSB data: https://data.ssb.no/api/v0/dataset/
# farger i r: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf




