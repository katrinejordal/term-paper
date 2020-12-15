#===============================================================================
#                   BAN400 - TERM PAPER PART TWO: SHINY APP                    #
#===============================================================================

# Loading libraries needed
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
library(shiny)

# ------------------------------------------------------------------------------
# Loading economic data from SSB using SSBs own package "PxWebApiData"
# ------------------------------------------------------------------------------

# Different macroeconomic values are presented in different data sets at SSB,
# with different variable names and number of variables in each data set. We 
# therefore created one function for each data set we wanted to load data from.

# For further explanation of the functions, see the readme.md file


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

lfs <- ssb_wf("Arbeidslause2")  # Labor Force Survey (AKU)
nav <- ssb_wf("Arbeidslause6")  # Registered unemployment at NAV


# Money supply -----------------------------------------------------------------

ssb_ms <- function(macro_size) {
    
    ApiData(urlToData = 10945,
            ContentsCode = macro_size,
            Tid = T) [[1]] 
}

m1 <- ssb_ms("PengmengdBehM1")  # Money Supply 1
m2 <- ssb_ms("PengmengdBehM2")  # Money Supply 2
m3 <- ssb_ms("PengmengdBehM3")  # Money Supply 3


# Data manipulation ------------------------------------------------------------

# Creating a function that can be used for wrangling all SSB data  
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


# Applying the function to the data frames
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


# Creating an ID-column called "variable" (needed for Shiny app) 
gdp$variable <- c("gdp")
gdp_ex_oil$variable <- c("gdp_ex_oil")
import$variable <- c("import")
export$variable <- c("export")
cpi_ja$variable <- c("cpi_ja")
cpi_jae$variable <- c("cpi_jae")
lfs$variable <- c("lfs")
nav$variable <- c("nav")
m1$variable <- c("m1")
m2$variable <- c("m2")
m3$variable <- c("m3")


# Bankruptcies -----------------------------------------------------------------

# Bankruptcies data has a slightly different format at SSB, thus it needs to be 
# manipulated in a slightly different way. Also want to extract bankruptcies per 
# industry, as well as total number of bankruptcies.

# Function that loads and wrangles bankruptcy data
bankrupt <- function(industry) {
    
    ApiData(urlToData = 08551,
            Region = "Heile landet", 
            NACE2007 = industry, 
            ContentsCode = "Konkurser",
            Tid = T)[[1]] %>% 
        
        separate(col = "måned",
                 into = c("year", "month"),
                 sep = "M") %>%
        
        filter(year == "2020") %>% 
        
        # Reformat and keep only the values needed
        mutate(value = as.numeric(value),
               date = as.Date(as.yearmon(paste(year, month), "%Y %m"))) %>% 
        select(industry = "næring (SN2007)",
               amount = "value",
               "date")
}

# Bankruptcies in total each month, including relative values
bankruptcies_total <- bankrupt("Alle næringar") %>% 
    mutate(normalized = amount / max(amount) * 100,
           variable = c("bankruptcies")) %>% 
    select(amount, date, normalized, variable)

# Bankruptcies per industry each month, including relative values
bankruptcies <- bankrupt(T) %>% 
    filter(industry != "Alle næringar") %>% 
    mutate(normalized = amount / max(amount) * 100)


# Merging all data frames into a single one, removing NA from LFS
economy <- rbind(gdp, gdp_ex_oil, export, import, cpi_ja, cpi_jae, lfs, nav, m1,
                 m2, m3, bankruptcies_total) %>%
    na.omit()


# Creating a dynamic value for today's date
today <- as.character(Sys.Date())


# Creating list of available macroeconomic values 
variable_list <- list(
    "GDP" = "gdp",
    "GDP excluding oil" = "gdp_ex_oil",
    "Import" = "import",
    "Export" = "export",
    "Consumer Price Index JA" = "cpi_ja",
    "Consumer Price Index JAE" = "cpi_jae",
    "LFS unemployment" = "lfs",
    "NAV unemployment" = "nav",
    "Money Supply 1" = "m1",
    "Money Supply 2" = "m2",
    "Money Supply 3" = "m3",
    "Total bankruptcies" = "bankruptcies total")


# Define User Interface for Shiny App ------------------------------------------
ui <- fluidPage(
    
    # Application title
    titlePanel("The effects of the corona pandemic on the Norwegian economy"),
    
    br(), br(),  # Adds space
    
    sidebarLayout(
        
        # First sidebar panel
        sidebarPanel(
            h3("Macroeconomic changes", align = "center"), 
            br(),
            p("Select the macroeconomic variable(s) and time span you are \n
              interested in from the list below. Selected values will appear \n
              in the plot to the left, displaying an overview of its \n
              development during the corona pandemic. To deselct a value, \n
              simply press the 'delete' tab on your keyboard. All values are \n 
              relative, so that the highest value of a macroeconomic variable \n
              equals 100."),
            br(),
            # Select macroeconomic variables input
            selectInput(inputId = "variable", 
                        label = strong("Select a macroeconomic variable"), 
                        choices = variable_list, 
                        selected = "gdp", 
                        multiple = TRUE),
            
            # Select date range to be plotted
            dateRangeInput(inputId = "date", 
                           label = strong("Select a date range"), 
                           start = "2020-01-01", end = today,
                           min = "2020-01-01", max = today),
        
        br(), br(),

            h3("Bankruptcies per industry", align = "center"), 
            br(),
            p("Select industries and time span you are interested in from the \n
              list below. Selected values will appear in the plot to the left,\n
              displaying an overview of its development during the corona \n
              pandemic. To deselct a value, simply press the 'delete' tab on \n
              your keyboard."),
            br(),
            # Select industries input
            selectInput(inputId = "industry",
                        label = strong("Select an industry"),
                        choices = unique(bankruptcies$industry),
                        selected = "Jordbruk, skogbruk og fiske",
                        multiple = TRUE),
            
            # Select date range to be plotted
            dateRangeInput(inputId = "date_ind", 
                           label = strong("Select a date range"), 
                           start = "2020-01-01", end = today,
                           min = "2020-01-01", max = today),
        ),
        
        # Show plot in main paneø
        mainPanel(
            plotOutput("economy_plot"),
            br(),br(),
            plotOutput("bank_plot")
        )
    )
)


# Define server logic for Shiny App --------------------------------------------
server <- function(input, output) {
    
    # Subset data
    selected_data <- reactive({
        req(input$date)
        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), 
                      "Error: Please provide both a start and an end date."))
        validate(need(input$date[1] < input$date[2], 
                      "Error: Start date should be earlier than end date."))
        filter(economy, economy$variable %in% input$variable) %>% 
            filter(date >= input$date[1] & date <= input$date[2])
    })
    
    # Create plot
    output$economy_plot <- renderPlot({
        ggplot(selected_data(), aes(x = date, y = normalized, color = variable)) +
            geom_line() +
            theme_minimal() +
            labs(x = NULL,
                 y = "Relative values",
                 title = paste("Overview of macroeconomic variables in Norway during the COVID-19 pandemic"),
                 subtitle = "Data retrieved from SSB, normalized with 100 being max") +
            scale_x_date(date_labels = "%B",
                         date_breaks = "1 month") + 
            scale_y_continuous(breaks = seq(20, 100, by = 10)) +
            theme(legend.title = element_blank(),
                  plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(color = "gray40",
                                               size = 10,
                                               face = "italic"))
    })
    
    # Subset data
    selected_industries <- reactive({
        req(input$date_ind)
        validate(need(!is.na(input$date_ind[1]) & !is.na(input$date_ind[2]), 
                      "Error: Please provide both a start and an end date."))
        validate(need(input$date_ind[1] < input$date_ind[2], 
                      "Error: Start date should be earlier than end date."))
        filter(bankruptcies, bankruptcies$industry %in% input$industry) %>% 
            filter(date >= input$date_ind[1] & date <= input$date_ind[2])
    })
    
    # Create plot
    output$bank_plot <- renderPlot({
        ggplot(selected_industries(), aes(x = date, y = amount, color = industry)) +
            geom_line() +
            theme_minimal() +
            labs(x = NULL,
                 y = "Bankruptcies",
                 title = paste("Overview of bankruptcies per industry in Norway during the COVID-19 pandemic"),
                 subtitle = "Data retrieved from SSB") +
            scale_x_date(date_labels = "%B",
                         date_breaks = "1 month") + 
            scale_y_continuous(breaks = seq(0, 130, by = 10)) +
            theme(legend.title = element_blank(),
                  plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(color = "gray40",
                                               size = 10,
                                               face = "italic"))
    }) 
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)