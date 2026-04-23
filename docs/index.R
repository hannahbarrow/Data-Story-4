# UTILITIES APP

library(shiny)

#-------------------------------------------------------------------------------
# Data PREP---------------------------------------------------------------------

################################################################################
# DATASET for Data Story 4: Sewanee utilities & weather
# ******************************************************************************
# Ensure "sewanee_weather.rds" & "utilities.rds" are in your working directory
# ******************************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(stringr)
library(lubridate)

rm(list = ls()) # clear environment first
dir() # look at files in your working directory

# utilities  ===================================================================
load('utilities.rds') # loads two datasets

# dataset #1: Utilities data for every campus building (water, electricity, natural gas)
# caution: many rows have missing data
utilities %>% as.data.frame %>% head
utilities %>% as.data.frame %>% tail

## i wanna make a year_month column
year <- utilities$year
m <- utilities$month
month <- str_pad(m, width = 2, side = "left", pad = "0")
year_month <- paste0(year, "-", month) %>% ym

utilities_pretty <- utilities %>% 
  mutate(year_month = year_month) %>% 
  select(building, type, year_month, gallons, gal_per_day, water_cost)

utilities_pretty %>% as.data.frame %>% head
utilities_pretty$building %>% unique

#-------------------------------------------------------------------------------
# Define UI for application that draws a histogram------------------------------
ui <- fluidPage(
  titlePanel("Sewanee Water Utilities Exploration"),
  p("Data Story 4"),
  br(), 
  tabsetPanel(
    #WATER----------------------------------------------------------------------
    tabPanel(h4("Water"),
             br(), 
             fluidRow(column(4, sliderInput(inputId = "time",
                                            label = "Select time range",
                                            min = min(utilities_pretty$year_month),
                                            max = max(utilities_pretty$year_month),
                                            value = range(utilities_pretty$year_month))),
                      column(4, uiOutput("bldgs")),
                             #selectInput(inputId = "bldgs",
                                         #label = "Select buildings",
                                         #multiple = TRUE,
                                         #choices = unique(utilities_pretty$building),
                                         #selected = "Chaplain House")),
                      column(4, radioButtons(inputId = "vars",
                                             label = "Select variable",
                                             choices = names(utilities_pretty)[4:6],
                                             selected = "gallons",
                                             inline = TRUE))),
             br(), 
             br(), 
             fluidRow(column(1),
                      column(10, plotOutput("utilitiesplot")),
                      column(1))
    ),
    
    #datetable_-----------------------------------------------------------------
    tabPanel(h4("Data viewer"),
             fluidRow(column(12, DTOutput("dt1")))
    )
  )
)


#-------------------------------------------------------------------------------
# Define server logic required to draw a histogram------------------------------
server <- function(input, output) {
  
  rv <- reactiveValues()
  rv$utilities_pretty <- utilities_pretty
  
  #WATER------------------------------------------------------------------------
  observe({
    rv$utilities_pretty <- utilities_pretty %>% filter(building %in% input$bldgs)
  })
  
  output$utilitiesplot <- renderPlot({
    ggplot(rv$utilities_pretty,
           aes_string(x = 'year_month', 
               y = input$vars,
               group = 'building',
               color = 'building'))+
      geom_path()+
      xlim(input$time)
  })
  
  output$bldgs <- renderUI({
    
    bldgs <- utilities_pretty %>% pull(building) %>% unique
    
    selectInput(inputId = "bldgs",
                label = "Select buildings",
                multiple = TRUE,
                choices = bldgs,
                selected = "Chaplain House")
  })
  
  #datetable--------------------------------------------------------------------
  output$dt1 <- renderDT({ rv$utilities_pretty })
  
}

#-------------------------------------------------------------------------------
# Run the application-----------------------------------------------------------
shinyApp(ui = ui, server = server)
