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

data_covid <- covid19.data(case = "ts-confirmed")

cases <- data_covid %>% 
  filter(Country.Region == "Norway") %>% 
  gather(date, confirmed_cases) %>% 
  slice(5:n()) %>% 
  as_tibble() %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at("confirmed_cases", ~ as.numeric(.)) %>% 
  mutate(new_cases = 
           confirmed_cases - lag(confirmed_cases, 
                                 n = 1,
                                 default = first(confirmed_cases)))

covid_total <- xts(cases$confirmed_cases, cases$date)
covid_daily <- xts(cases$new_cases, cases$date)

# Google trends data -----------------------------------------------------------

search_word <- "corona"
country <- "Norway"
timespan <- paste("2020-01-01", Sys.Date())
MA <- 3

# Country & countrycode
countrycode <- countrycode(country, 
                           origin = "country.name", 
                           destination = "iso2c")

# Google Trends: Load data
data_gtrend <- gtrends(keyword = search_word, 
                       time = timespan,
                       geo = "NO")

# Manipulate google trends data
gtrend <- as_tibble(data_gtrend$interest_over_time) %>%
  select(date, hits) %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at("hits", ~ ifelse(. == "<1", 0, .)) %>%   
  mutate_at("hits", ~ as.numeric(.)) %>% 
  rename(searches = hits)

google <- xts(gtrend$searches, gtrend$date)

# Stocks and indicies -------------------

# Yahoo does not provide historical data for indicies anymore. 
# I could not find any other source. 
# Webscraping via investing.com could be a possibility. But seems like python
# is only option..
# For now, we download data from oslobors.no to xls files. 

# OSEBX
osebx_xl <- read_excel("OSEBX.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(osebx = Siste / last(Siste) * 100)
osebx <- xts(osebx_xl$osebx, osebx_xl$date)

# OBX
obx_xl <- read_excel("OBX.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(obx = Siste / last(Siste) * 100)
obx <- xts(obx_xl$obx, obx_xl$date)

# OBOSX - Oil service index
obosx <- read_excel("OBOSX.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(obosx = Siste / last(Siste) * 100)
oilservice <- xts(obosx$obosx, obosx$date)

# OBSFX - Seafood index
obsfx <- read_excel("OBSFX.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(obsfx = Siste / last(Siste) * 100)
seafood <- xts(obsfx$obsfx, obsfx$date)

# OBSHX - Shipping index
obshx <- read_excel("OBSHX.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(obshx = Siste / last(Siste) * 100)
shipping <- xts(obshx$obshx, obshx$date)

# OSEEX - Equity Certificate Index (egenkapitalbevis, sparebanker)
oseex <- read_excel("OSEEX.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(oseex = Siste / last(Siste) * 100)
equity_certificates <- xts(oseex$oseex, oseex$date)

# OSEMX - Mid cap index
osemx <- read_excel("OSEMX.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(osemx = Siste / last(Siste) * 100)
mid_cap <- xts(osemx$osemx, osemx$date)

# OSESX - Small cap index
osesx <- read_excel("OSESX.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(osesx = Siste / last(Siste) * 100)
small_cap <- xts(osesx$osesx, osesx$date)

# OSE10GI - Energy
ose10gi <- read_excel("OSE10GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose10gi = Siste / last(Siste) * 100)
energy <- xts(ose10gi$ose10gi, ose10gi$date)

# OSE15GI - Materials
ose15gi <- read_excel("OSE15GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose15gi = Siste / last(Siste) * 100)
materials <- xts(ose15gi$ose15gi, ose15gi$date)

# OSE20GI - Industry
ose20gi <- read_excel("OSE20GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose20gi = Siste / last(Siste) * 100)
industry <- xts(ose20gi$ose20gi, ose20gi$date)

# OSE25GI - Consumergoods Discretionary 
ose25gi <- read_excel("OSE25GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose25gi = Siste / last(Siste) * 100)
consumer_discretionary <- xts(ose25gi$ose25gi, ose25gi$date)

# OSE30GI - Consumergoods Staples 
ose30gi <- read_excel("OSE30GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose30gi = Siste / last(Siste) * 100)
consumer_staples <- xts(ose30gi$ose30gi, ose30gi$date)

# OSE35GI - Health care
ose35gi <- read_excel("OSE35GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose35gi = Siste / last(Siste) * 100)
health_care <- xts(ose35gi$ose35gi, ose35gi$date)

# OSE40GI - Finance 
ose40gi <- read_excel("OSE40GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose40gi = Siste / last(Siste) * 100)
finance <- xts(ose40gi$ose40gi, ose40gi$date)

# OSE45GI - IT
ose45gi <- read_excel("OSE45GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose45gi = Siste / last(Siste) * 100)
it <- xts(ose45gi$ose45gi, ose45gi$date)

# OSE50GI - Communication 
ose50gi <- read_excel("OSE50GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose50gi = Siste / last(Siste) * 100)
communication <- xts(ose50gi$ose50gi, ose50gi$date)

# OSE55GI - Utilities
ose55gi <- read_excel("OSE55GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose55gi = Siste / last(Siste) * 100)
utilities <- xts(ose55gi$ose55gi, ose55gi$date)

# OSE60GI - Real Estate 
ose60gi <- read_excel("OSE60GI.xlsx") %>% 
  select(1, 2) %>% 
  rename(date = 1) %>% 
  mutate(ose60gi = Siste / last(Siste) * 100)
real_estate <- xts(ose60gi$ose60gi, ose60gi$date)



list.files(pattern = "xlsx")


# Yahoo does provide historical data for individual stocks. 
getSymbols("EQNR.OL", auto.assign = TRUE)
EQNR <- Cl(EQNR.OL)
EQNR <- EQNR["2020"]
head(EQNR)





# Currency ---------------------------------------------------------------------

currency_pair <- "NOK=X"

getSymbols(currency_pair, auto.assign = TRUE)

USD_NOK <- Cl(`NOK=X`)
USD_NOK <- USD_NOK["2020"]
colnames(USD_NOK) <- "USD_NOK_actual"
USD_NOK <- as.xts(transform(USD_NOK, USD_NOK_scaled = 
                              USD_NOK_actual / first(USD_NOK_actual) * 100))

# Oil-price --------------------------------------------------------------------

getSymbols("DCOILBRENTEU", src = "FRED", auto.assign = TRUE)

# Extract the close column and only from 2020-01-01
brent_oil <- DCOILBRENTEU
brent_oil <- brent_oil["2020"]
brent_oil <- na.omit(brent_oil)
colnames(brent_oil) <- "brent_oil_actual"
brent_oil <- as.xts(
  transform(brent_oil, brent_oil_scaled = 
              brent_oil_actual / first(brent_oil_actual) * 100))


# ------------------------------------------------------------------------------
# Loading economic data from SSB using SSBs own package "PxWebApiData"
# ------------------------------------------------------------------------------

# Choosing "makrostørrelse", "statistikkvariabel", "tid", directly in function
# Separating SSBs date format into year and month, filter by 2020 values only
# Keeping variables "total gdp", "date" and "normalized gdp", in correct format

gdp_ssb <- ApiData(11721, 
               Makrost = "bnpb.nr23_9", 
               ContentsCode = "Løpende priser, sesongjustert (mill. kr)",
               Tid = T)[[1]] %>%
  separate(col = "måned", into = c("year", "month"), sep = "M") %>%
  filter(year == "2020") %>%
  transmute(amount = as.numeric(value),
            date = as.Date(as.yearmon(paste(year, month), "%Y %m")),
            normalized = amount / first(amount) * 100)

gdp_xts <- xts(gdp$normalized, gdp$date) 
colnames(gdp_xts) <- c("GDP")


# Economic data from SSB -------------------------------------------------------

## GDP Norway (Katrine får ikke denne til å virke uten: fileEncoding="Latin1")

gdp_csv <- read.csv2("https://data.ssb.no/api/v0/dataset/615167.csv?lang=no", fileEncoding = "Latin1") %>%
  rename(macro = makrostørrelse, 
         time = måned,
         var = statistikkvariabel,
         amount = X11721..Makroøkonomiske.hovedstørrelser..etter.makrostørrelse..måned.og.statistikkvariabel) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         macro == "bnpb.nr23_9 Bruttonasjonalprodukt, markedsverdi",
         var == "Løpende priser, sesongjustert (mill. kr)") %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

gdp_csv <- xts(gdp$normalize, gdp$date)

## GDP Norway excluding oil

gdp_ex_oil <- read.csv2("https://data.ssb.no/api/v0/dataset/615167.csv?lang=no") %>%
  rename(macro = makrostørrelse, 
         time = måned,
         var = statistikkvariabel,
         amount = X11721..Makroøkonomiske.hovedstørrelser..etter.makrostørrelse..måned.og.statistikkvariabel) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         macro == "bnpb.nr23_9fn Bruttonasjonalprodukt Fastlands-Norge, markedsverdi",
         var == "Løpende priser, sesongjustert (mill. kr)") %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

gdp_ex_oil <- xts(gdp_ex_oil$normalize, gdp_ex_oil$date)

## Import

import <- read.csv2("https://data.ssb.no/api/v0/dataset/615167.csv?lang=no") %>%
  rename(macro = makrostørrelse, 
         time = måned,
         var = statistikkvariabel,
         amount = X11721..Makroøkonomiske.hovedstørrelser..etter.makrostørrelse..måned.og.statistikkvariabel) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         macro == "imp.nrtot Import i alt",
         var == "Løpende priser, sesongjustert (mill. kr)") %>% 
  filter(macro == "imp.nrtot Import i alt") %>% 
  filter(var == "Løpende priser, sesongjustert (mill. kr)") %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

import <- xts(import$normalize, import$date)


## Export

export <- read.csv2("https://data.ssb.no/api/v0/dataset/615167.csv?lang=no") %>%
  rename(macro = makrostørrelse, 
         time = måned,
         var = statistikkvariabel,
         amount = X11721..Makroøkonomiske.hovedstørrelser..etter.makrostørrelse..måned.og.statistikkvariabel) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         macro == "eks.nrtot Eksport i alt",
         var == "Løpende priser, sesongjustert (mill. kr)") %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

export <- xts(export$normalize, export$date)

## Moneysupply M1

m1 <- read.csv2("https://data.ssb.no/api/v0/dataset/172769.csv?lang=no") %>%
  rename(var = statistikkvariabel, 
         time = måned,
         amount = X10945..Pengemengden.M1..M2.og.M3..etter.statistikkvariabel.og.måned) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         var == "Pengemengden M1. Beholdninger (mill. kr)") %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

m1 <- xts(m1$normalize, m1$date)

## Moneysupply M2

m2 <- read.csv2("https://data.ssb.no/api/v0/dataset/172769.csv?lang=no") %>%
  rename(var = statistikkvariabel, 
         time = måned,
         amount = X10945..Pengemengden.M1..M2.og.M3..etter.statistikkvariabel.og.måned) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         var == "Pengemengden M2. Beholdninger (mill. kr)") %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

m2 <- xts(m2$normalize, m2$date)

## Moneysupply M3

m3 <- read.csv2("https://data.ssb.no/api/v0/dataset/172769.csv?lang=no") %>%
  rename(var = statistikkvariabel, 
         time = måned,
         amount = X10945..Pengemengden.M1..M2.og.M3..etter.statistikkvariabel.og.måned) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         var == "Pengemengden M3. Beholdninger (mill. kr)") %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

m3 <- xts(m3$normalize, m3$date)

## Inflation: KPI-JA

kpi_ja <- read.csv2("https://data.ssb.no/api/v0/dataset/1118.csv?lang=no") %>%
  rename(macro = konsumgruppe, 
         time = måned,
         var = statistikkvariabel,
         amount = X05327..KPI.JA.og.KPI.JAE..etter.konsumgruppe..måned.og.statistikkvariabel) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         macro == "JA_TOTAL KPI-JA Totalindeks",
         var == "KPI-JA, KPI-JAE. KPI-JE og KPI-JEL (2015=100) Månedlig") %>% 
  # Since, SSB report ".." if NA and , as delimiter:
  mutate_all(~na_if(., "..")) %>% 
  mutate_at("amount", ~ as.numeric(sub(",", ".", amount, fixed = TRUE))) %>%
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100) # double check this

kpi_ja <- xts(kpi_ja$normalize, kpi_ja$date)

## Inflation: KPI-JA

kpi_jae <- read.csv2("https://data.ssb.no/api/v0/dataset/1118.csv?lang=no") %>%
  rename(macro = konsumgruppe, 
         time = måned,
         var = statistikkvariabel,
         amount = X05327..KPI.JA.og.KPI.JAE..etter.konsumgruppe..måned.og.statistikkvariabel) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         macro == "JAE_TOTAL KPI-JAE Totalindeks",
         var == "KPI-JA, KPI-JAE. KPI-JE og KPI-JEL (2015=100) Månedlig") %>% 
  # Since, SSB report ".." if NA and , as delimiter:
  mutate_all(~na_if(., "..")) %>% 
  mutate_at("amount", ~ as.numeric(sub(",", ".", amount, fixed = TRUE))) %>%
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100) # double check this

kpi_jae <- xts(kpi_jae$normalize, kpi_jae$date)


## Unemployment (AKU)

aku <- read.csv2("https://data.ssb.no/api/v0/dataset/1054.csv?lang=no") %>%
  rename(sex = kjønn,
         age = alder,
         time = måned,
         var = statistikkvariabel,
         amount = X08931..Sysselsetting.og.arbeidsløyse.for.personar.15.74.år..etter.kjønn..alder..statistikkvariabel.og.måned) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         sex == "0 Begge kjønn",
         age == "15-74 15-74 år",
         var == "Arbeidslause (AKU) (1 000 personar), sesongjustert") %>% 
  mutate_all(~na_if(., "..")) %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

aku <- xts(aku$normalize, aku$date)


## Unemployment (NAV)

nav <- read.csv2("https://data.ssb.no/api/v0/dataset/1054.csv?lang=no") %>%
  rename(sex = kjønn,
         age = alder,
         time = måned,
         var = statistikkvariabel,
         amount = X08931..Sysselsetting.og.arbeidsløyse.for.personar.15.74.år..etter.kjønn..alder..statistikkvariabel.og.måned) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020",
         sex == "0 Begge kjønn",
         age == "15-74 15-74 år",
         var == "Registrerte arbeidslause ved NAV (1 000 personar), sesongjustert") %>%
  mutate_all(~na_if(., "..")) %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

nav <- xts(nav$normalize, nav$date)

## Bankruptcies

bankruptcies <- read.csv2("https://data.ssb.no/api/v0/dataset/924816.csv?lang=no") %>%
  rename(time = måned,
         variable = statistikkvariabel,
         amount = X09695..Opna.konkursar..etter.måned.og.statistikkvariabel) %>% 
  separate(col = "time", into = c("year", "month"), sep = "M") %>% 
  filter(year == "2020") %>%
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(normalize = amount / first(amount) * 100)

bankruptcies <- xts(bankruptcies$normalize, bankruptcies$date)



# Merge all data ---------------------------------------------------------------

start_date <- as.Date("2020-01-01")
end_date <- Sys.Date()
dates <- seq(from = start_date, to = end_date, by = "days")

eco_xts <- merge(covid_total, covid_daily, google, gdp, gdp_ex_oil, import,
                 export, m1, m2, m3, kpi_ja, kpi_jae, aku, nav, bankruptcies,
                 USD_NOK, brent_oil, osebx, obx, oilservice, seafood, shipping, 
                 equity_certificates, mid_cap, small_cap, energy, materials,
                 industry, consumer_discretionary, consumer_staples,
                 health_care, finance, it, communication, utilities,
                 real_estate)


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




