# BAN400 term paper
###### Repository for BAN400 term paper 

## Thoughts behind the project
For the BAN420 term paper, we downloaded covid19 statistics and corona related google trend searches and compared the two. We then visualized the data and findings in different plots, and predicted a forecast for new cases for the remaing part of 2020. We found this topic to be quite interesting ang highly relevant, and therefore decided to keep on exploring it in the BAN400 term paper. We were curious about how the pandemic have affected different macroeconomic values and finance in Norway, and wanted to take a closer look into this. In the BAN420 term paper, the results were only available for the end user when running the script, for example using Rstudio. For the BAN400 term paper we wanted the results to be available for any end user, aslo those unfamiliar with R and Rstudio. We wanted to be able to present our results and findings throug a dashboard available to anyone. 

#### Problems and challenges
We  wanted to analyze how the pandemic have affected the economy by looking at changes in stock indices at Oslo Børs over the past months. We wanted to extract data for different indexes (eg. OSEBX - Main index, OBSFX - Seafood Index, OSE45GI - IT Sector Index, and so forth) and compare them to the development of the pandemic in Norway. This would normally have been quite a simple task using historical data from Yahoo Finance, loading it directly into R. 
Unfortunately, Yahoo only historical data from these indices up until 2018. We tried different financial packages in R, but none included the indicies provided by Oslo Børs. 
Therefore we went to oslobors.no, homepage of Oslo Børs, and thought we could download data from the indicies there. 
However, Oslo Børs is currently in the process of changing trading platforms and web sites containing stock data. Due to this we have not been able to find a way to load live, updated stock index data for Oslo Børs into R, as this is not available on Oslo Børs' current web pages or the new web pages (Euronext) as of today (28.11.20). The only option we could find for using stock data from Oslo Børs, is to download excel-files locally and load these into R. This is naturally not the best option as it is more interesting to look at updated data, but due to the situation it was the only option we could find. 

## SSB macroeconomic data
Using SSBs own R package "PxWebApiData" we have downloaded live, updated data from SSB into R. The different macroeconomic values are presented in different data sets from SSB, with the variable names differing somewhat. The number of variables also vary between the different data sets. For example, the variable for macroeconomic sizes related to GDP is called "Makrost" and the sizes it is measured in is called "ContentsCode". On the other hand, the data set on unemployment conatins several variables in addition to the variable "ContentsCode": "Kjonn" and "Alder". The code is presented below to illutrate better: 

#### GDP
```javascript
ssb_data <- function(macro_size) {

  ApiData(urlToData = 11721,
          Makrost = macro_size,
          ContentsCode = "Løpende priser, sesongjustert (mill. kr)",
          Tid = T) [[1]]
}

gdp <- ssb_data("bnpb.nr23_9")            # GDP including oil
gdp_ex_oil <- ssb_data("bnpb.nr23_9fn")   # GDP excluding oil
```
#### Unemployment
```javascript
ssb_wf <- function(macro_size) {
  
  ApiData(urlToData = 08931,
          Kjonn = "0",
          Alder = "15-74",
          ContentsCode = macro_size,
          Tid = T) [[1]] 
}

lfs <- ssb_wf("Arbeidslause2")
nav <- ssb_wf("Arbeidslause6")
```

This made it challenging to create a single function for loading data into R from the different data sets. After trying some different options for creating a function that loads the SSB data into R, we found that the best way was to create a function for each data set we wanted to download data from. 

## Merging the data
abcdefghijklmnop

## Visualizing the data
abcdefghijklmnop

## Creating dashboard
abcdefghijklmnop
