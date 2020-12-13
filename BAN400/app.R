#===============================================================================
#                              BAN400 - TERM PAPER                             #
#===============================================================================

# ------------------------------------------------------------------------------
# Loading libraries needed
# ------------------------------------------------------------------------------
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


# Loading data -----------------------------------------------------------------

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


# Cleaning data ----------------------------------------------------------------

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


# Merging data -----------------------------------------------------------------

merged_ssb <- 
    merge(export, gdp, by = "date") %>% 
    transmute(export_amount = amount.x,
           export_normalized = normalized.x,
           gdp_amount = amount.y,
           gdp_normalized = normalized.y,
           date = date
           )

# Creating list of available macroeconomic values ------------------------------
variable_list <- list(
    "GDP" = "GDP",
    "GDP excluding oil" = "GDP ex oil",
    "Import" = "Import",
    "Export" = "Export")
#    "CPI JA" = "CPI JA",
#    "CPI JAE" = "CPI JAE",
#    "LFS unemployment" = "lfs",
#    "NAV unemployment" = "nav",
#    "Money Supply 1" = "m1",
#    "Money Supply 2" = "m2",
#    "Money Supply 3" = "m3",
#    "Total bankruptcies" = "bankruptcies total")

today <- as.character(Sys.Date())


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The effects of the corona pandemic on the Norwegian economy"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "variable", 
                        label = strong("Select a macroeconomic value"), 
                        choices = variable_list, 
                        selected = "GDP", 
                        multiple = TRUE),
            
            # Select date range to be plotted
            dateRangeInput(inputId = "date", 
                           label = strong("Select a date range"), 
                           start = "2020-01-01", end = today,
                           min = "2020-01-01", max = today),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    selected_trends <- reactive({
        req(input$date)
        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), 
                      "Error: Please provide both a start and an end date."))
        validate(need(input$date[1] < input$date[2], 
                      "Error: Start date should be earlier than end date."))
        
    })
    
    output$plot1 <- renderPlot({
        
        ggplot(data_temp(), aes(x=Date, y=diff, color=Country)) +
            geom_line() +
            xlab("Date") +
            ylab("Cases per Day") +
            ggtitle("Confirmed Cases per Day") +
            theme(
                legend.position="right",
                axis.title = element_text(size=16),
                axis.text = element_text(size=16),
                plot.title = element_text(size=20),
                legend.title = element_text(size=16),
                legend.text = element_text(size=18)
            )
        
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
