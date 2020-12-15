# BAN400 term paper
###### Repository for BAN400 term paper 

## Thoughts behind the project
For the BAN420 term paper, we downloaded covid19 statistics and corona related google trend searches, with the purpose of visualizing and comparing the two. We found this topic to be quite interesting ang highly relevant, and therefore decided to keep on exploring it in the BAN400 term paper. We were curious about how the pandemic have affected different macroeconomic values and finance in Norway, and wanted to take a closer look into this. In the BAN420 term paper, the results were only available for the end user when running the script, for example using Rstudio. For the BAN400 term paper we wanted the results to be available for any end user, aslo those unfamiliar with R and Rstudio. We wanted to be able to present our results and findings throug a dashboard and/or Shiny app available to anyone. 

#### Problems and challenges
We  wanted to analyze how the pandemic have affected the economy by looking at changes in stock indices at Oslo Børs over the past months. We wanted to extract data for different indexes (eg. OSEBX - Main index, OBSFX - Seafood Index, OSE45GI - IT Sector Index, and so forth) and compare them to the development of the pandemic in Norway. This would normally have been quite a simple task using historical data from e.g. Yahoo Finance, loading it directly into R. Unfortunately, Yahoo only have historical data from these indices up until 2018. We tried different financial packages in R, but none included the indicies provided by Oslo Børs. 

Therefore, we went to oslobors.no, homepage of Oslo Børs, to see if we could download data from the indicies there. However, Oslo Børs is currently in the process of changing trading platforms and web sites containing stock data. Due to this we have not been able to find a way to load live, updated stock index data for Oslo Børs into R. This is (as of December 2020) not available for download on Oslo Børs' current web pages or on the new platform (Euronext) without a membership. The only option we could find for using stock data from Oslo Børs, is to download excel-files locally and load these into R. This is naturally not the best option as it is more interesting to look at updated data, but due to the situation it was the only option we could find. We therefore decided not to pursue this topic any further.

Another challenge we had was related to the documentation of functions. Unfortunately, we were not able to use docstrings without getting error messages. This happened both on our own functions, and on examples and tutorials provided. Instead we tried to write good comments. 


## Term paper part one: data vizualisation
For the first part of the term paper, we decided to pick up where we left things with the BAN420 term paper. This part of the BAN400 term paper focuses on collecting, wrangling, visualizing and comparing data related to the COVID-19 pandemic and google trends. The code related to this part is called BAN400_Ben.R. 

#### COVID-19 data
To load the covid-19 related data, we have used a package called covid19.analytics. It uses data from CSSE at John Hopkins University as its source, which has shown to be relatively reliable and is commonly used as a source for corona-statistics. The script downloads and wrangles data for Norway, both new daily cases and the cummulative confirmed cases. It also plots these two different variables into one plot each. 

#### Google trends data
To load the Google Trends data we have used a package called gtrendsR. We created a function that sets the start date to 01.01.2020 and updates the end date dynamically to current day's date, changes the country name to country code (as covid-data and gtrend-data origiannly use different ones), loads the gtrends specified by selected search word, reformats the data as needed and present the results in a ggplot. The function can be used with any given search word. 

#### Comparing covid-data and Google trends data
We also thought it would be interesting to see how certain google search trends related to covid-statistics, so we created a function that loads and reformats Google Trends , merges it with the already downloaded covid-data and plot them in a ggplot for comparison. This function is very similar to thw one above, but we wantet a function for both Google Trend data only, and for the comparison of search trends with covid-statistics. 

#### SSB-data on bankruptcies per industry
The last part of this script looks into SSB-data on bankruptcies per industry. We created a function that loads and manipulate data from SSB on bankruptcies, and use this function to create a data frame with data on bankruptcies per industry. This is then plotted in two different ways: one with all industries gathered using facet wrap, and one with a for loop creating a plot for each industry.  

#### Presentation
As mentioned, we wanted our results to be available to anyone, also those who are unfamiliar with R. For this part we chose to present our results in a flexdashbord, as the structure and content of the data frames varied somewhat. The dashboard consist of three pages. The first site shows plots from Google searches and covid-19 cases. To improve this part we would want to get the function itself into the dashboard. By doing that we could write search words and get results direcly in the dashboard. The second site shows simple plots of bankruptcies per industry. The last page shows a live map of confirmed cases of covid-19 from all over the world. By using the Covid-19 package we can direcly get the live map and deliver it in the dashboard. This shows how easy and user friendly many of the packages are. Rmarkdown and flexdashboard have many oportunities we would like to look more into. For example using shiny directly in the Dashboard in orther to get everything in one place. The Dashboard is a very nice way to make a report and easely makes a HTML page without any HTML coding. We think it will be handy in the future to know about this part of R. It is versatile and can be used in many different projects and jobs. To render the HTML file use the following command: rmarkdown::render("Dashboard.Rmd"). A link will be added to the folder and you can view it in a web browser på pressing Dashboard.html on files. 

## Term paper part two: Shiny App
We were also curios of Shiny App and wanted to learn this type of presentation, and decided to present parts of the term paper using Shiny. This part focuses on macroeconomic data from SSB. The script is created so that this data will update as new data becomes available at SSB. At the same time, the end dates used in the script will be updated to current day's date. Link to shiny app: https://katrinejordal.shinyapps.io/BAN400-term-paper/
#### SSB macroeconomic data
Using SSBs own R package "PxWebApiData" we have downloaded live, updated data from SSB into R. The different macroeconomic values are presented in different data sets from SSB, with variable names and number of variables differing somewhat. For example, the variable for macroeconomic sizes related to GDP is called "Makrost" and the sizes it is measured in is called "ContentsCode". On the other hand, the data set on unemployment conatins several variables in addition to the variable "ContentsCode": "Kjonn" and "Alder". The code is presented below to illutrate better: 

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

This made it challenging to create a single function for loading data into R from the different data sets at SSB. After trying some different options, we found that the best way was to create a function for each data set we wanted to download data from. After this, we also created a function that cleans up the data frames, as this part was the same for all types of data. The only exception is data on bankruptcies, which needed slightly different manipulation. Therfore, we created an extra function for this part. 

Short explanation on the loading part of the function:
urlToData = table ID from SSB
Tid = T) [[1]] makes sure that ine gather data from all available time periods 
Any code between the two refers to different variables in the data sets

When passing the cleanup function to the data, we tried to use a list combined with a for loop, but unfortunately this caused som problems with the Shiny app. It worked fine isolated in Rstudio, but to not mess up the Shiny App we kept it a bit ineffective but "safe".

All SSB data (except bankruptcies per industry) is then merged into one single data frame, with a column to identify the different variables. 

#### User Interface
For the UI part we have chosen to include two sets of input-areas in the sidebar panel, and two plots corresponding with these. The plots we decided to display in the Shiny App is (1) an overview of macroeconomic variables' development during the pandemic, and (2) an overview of bankruptcies per industry during the pandemic. The user can select variables and date range, and industries and date range. We wanted to keep the UI simple, minimalistic and easy to understand and use. Therefore, we also added some text describing how to select input. 

#### Server logic
For both input we created some tests to make sure that the date input was correct, while displaying an error message if not. The code used for plotting is similar to other parts of the term paper, but adjusted so that it will be reactive with the user's input. 

## Some final reflections
The term paper has been an interesting and fun project to work with. As beginners in R just some months ago, we have had a steep learning curve and it has absolutely been demanding and challenging. However, we have been able to focus on learning things we see as valuable and useful, gaining a lot of relevant competance. It has been a good experience!
