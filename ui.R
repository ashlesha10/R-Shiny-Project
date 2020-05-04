# Load "necessary" libraries
library(shiny)
library(DT)
library(tidyverse)
library(forcats)
library(magrittr)

# Load data
house <- read_csv('House_Price_data.csv')
names(house) <- gsub(" ", "", names(house))

# Get lists of all neighbourhoods and which to exclude by default, for plotting
house$Neighborhood = as.factor(house$Neighborhood) # for levels to be pulled, it needs to be formatted as factor
all_neighbourhoods <- levels(pull(house, 'Neighborhood'))
exclude_neighbourhoods <- setdiff(levels(house$Neighborhood), c("Blmngtn", "StoneBr", "Mitchel", "OldTown", "NoRidge"))

# Define UI for application
fluidPage(
  # Application title
  titlePanel("Week 4 Assignment- R Shiny - House Price Data "),
  
  # Sidebar with various Exploratory Data Analysis choices
  sidebarLayout(
    sidebarPanel(
       # Select variable for x-axis
       selectInput(inputId = "x",
                   label = "Select parameter for X-axis:",
                   choices = c("GrLivArea", "TotalSF", "TotalBsmtSF", "HouseAge"),
                   selected = "GrLivArea"),
       
       # Select variable for colour
       selectInput(inputId = "z",
                   label = "Colour:",
                   choices = c("Neighborhood", "CentralAir", "MSSubClass", "SaleType", "SaleCondition", "Foundation"),
                   selected = "Neighborhood"),
       
       # Select the number of smoothing splines vs. SalePrice
       sliderInput(inputId = "smoother",
                   label = "Do you want the graph to be more Smooth?", 
                   min = 1, max = 19, step = 2,
                   value = 5),
       
       # Select which neighbourhoods to exclude from the plot, as it can get cluttered quickly
       selectInput(inputId = "neighbourhoods",
                   label = "Exclude neighbourhoods:",
                   choices = all_neighbourhoods,
                   selected = exclude_neighbourhoods,
                   multiple = TRUE,
                   selectize = TRUE),
       
       # Place the boxplot on the lower left side
       plotOutput(outputId = "boxPlot")
    ),
    
    # Show scatter plot and table of selected data on the right side (main area)
    mainPanel(
       plotOutput(outputId = "scatterPlot", brush = "plot_brush"),
       htmlOutput(outputId = "instructions"),
       dataTableOutput(outputId = "houseTable"),
       br()
    )
  )
)
