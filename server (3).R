# Load "necessary" packages
library(shiny)
library(DT)
library(readr)
library(dplyr)

# Load data
house <- read_csv('House_Price_data.csv')
names(house) <- gsub(" ", "", names(house))

# Find variables responsible for missing values (NAs)
nas <- house %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  select_if(. > 0)

# Exclude variables missing more than 1500 values (roughly >50% of dataset)
exclude_nas <- nas %>% select_if(. > 1500) %>% names(.)
house %<>% select(-one_of(exclude_nas)) # select_ threw error, replaced with select()
# raw %<>% select(-one_of(exclude_nas))


# Calculate house total square feet
house %<>% mutate(TotalSF = TotalBsmtSF + `1stFlrSF` + `2ndFlrSF`)



# Now that we have imputed missing values, the question is the level of sparsity in terms of % of 0's
sparse_num <- house %>% 
  summarise_all(funs(sum(. == 0) / n())) %>%
  select_if(. > 0)

# Dropping a few manually
house %<>% select( -Id)

# Whatever is character at this point is a factor
house %<>% mutate_if(.predicate = is.character, .funs = factor)


# Some feature engineering
house %<>% mutate(HouseAge = YrSold - YearBuilt, 
                  Remodeled = ifelse(YearBuilt != `YearRemod/Add`, 1, 0),
                  NewHouse = ifelse(YrSold == YearBuilt, 1, 0),
                  GarageAge = ifelse(GarageYrBlt == 0, 0, ifelse(YrSold == GarageYrBlt, 1, YrSold - GarageYrBlt)),
                  SoldInSummer = ifelse(MoSold %in% c("5", "6", "7"), 1, 0)
                  # Add total SF
)

# Dropping those variables
house %<>% select(-c(YrSold, YearBuilt, `YearRemod/Add`, GarageYrBlt, MoSold))

# Create vectors of variable names
vars_factor <- names(house %>% select_if(is.factor))
vars_numeric <- names(house %>% select_if(is.numeric) %>% select(-SalePrice))
vars_output <- "SalePrice"
if (length(vars_factor) + length(vars_numeric) + 1 != ncol(house)) {
  print("We have a problem!")
}

# Drop
house %<>% select(-LotConfig, -BldgType, -HouseStyle, -RoofStyle)

# Collapse factor levels based on Birk's keen insight
house %<>% mutate(
  MSSubClass = fct_collapse(MSSubClass, 
                            `1StNew` = "020", 
                            `1St` = c("030", "040", "045", "050"),
                            `2StNew` = "060",
                            `2St` = c("070", "075"),
                            Split = c("080", "085"),
                            Multi = c("090", "190"),
                            PUDNew = "120",
                            PUD = c("150", "160", "180")),
  MSZoning = fct_collapse(MSZoning,
                          Res = c("RH", "RL", "RM"),
                          NonRes = c("A (agr)", "C (all)", "I (all)")),
  Condition1 = fct_collapse(Condition1,
                            Pos = c("PosA", "PosN"),
                            Neg = c("RRAe", "RRAn", "RRNe", "RRNn", "Artery", "Feedr")),
  Exterior1st = fct_collapse(Exterior1st,
                             Good = c("CemntBd", "ImStucc", "PreCast", "Stone", "VinylSd"),
                             Bad = c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock", "HdBoard", "MetalSd", "Plywood", "Stucco", "Wd Sdng", "WdShing")),
  Exterior2nd = fct_collapse(Exterior2nd,
                             Good = c("Other", "PreCast", "VinylSd"),
                             Bad = c("CmentBd", "AsbShng", "AsphShn", "Brk Cmn", "BrkFace", "CBlock", "HdBoard", "ImStucc", "MetalSd", "Plywood", "Stone", "Stucco", "Wd Sdng", "Wd Shng")),
  Foundation = fct_collapse(Foundation,
                            PC = "PConc",
                            Other = c("BrkTil", "CBlock", "Slab", "Stone", "Wood")),
  GarageType = fct_collapse(GarageType,
                            Good = c("Attchd", "BuiltIn"),
                            Bad = c("2Types", "Basment", "CarPort", "Detchd")),
  SaleType = fct_collapse(SaleType,
                          Old = c("COD", "Con", "ConLD", "ConLI", "ConLw", "CWD", "Oth", "VWD", "WD")),
  SaleCondition = fct_collapse(SaleCondition,
                               New = "Partial",
                               Old = c("Abnorml", "AdjLand", "Alloca", "Family", "Normal"))
)

# Drop two of those that have very, very few observations in some group, as they are edge cases
house %<>% select(-MSZoning, -Condition1)

# Update variable name vectors
vars_factor <- names(house %>% select_if(is.factor))
vars_numeric <- names(house %>% select_if(is.numeric) %>% select(-SalePrice))



# Define server logic required to draw the plots
function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(data = house %>% filter(!(Neighborhood %in% input$neighbourhoods)),
      aes_string(x = input$x, y = "SalePrice")) +
    geom_point(aes_string(col = input$z)) +
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, input$splines), se = F) +
    labs(
      y = "Sale Price ($)"
    )
  })
  
  output$boxPlot <- renderPlot({
    ggplot(data = house %>% filter(!(Neighborhood %in% input$neighbourhoods))) +
      geom_boxplot(aes_string(x = input$z, y = "SalePrice", fill = input$z)) +
      theme(legend.position="none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      labs(
        x = ""
      )
  })
  
  output$instructions <- renderUI({
    HTML("Click and drag selection of observations in scatter plot to inspect closer.<br>")
  })
  
  # Create data table
  output$houseTable <- DT::renderDataTable({
    brushedPoints(house, brush = input$plot_brush) %>% 
      filter(!(Neighborhood %in% input$neighbourhoods)) %>%
      select(SalePrice, Neighborhood, TotalSF, HouseAge, SaleCondition, GarageCars, GarageAge)
  })
  
}

