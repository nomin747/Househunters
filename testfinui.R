library(shiny)
library(tidyverse)
library(dplyr)
library(shiny)
library(purrr)
library(ggplot2)
library(usmap)
library(sf)
library(plotly)
library(shiny)
library(shinydashboard)
library(maps)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(datadigest) 
library(rio)
library(DT)
library(stargazer)
library(markdown)

#Plot data
newcleandata <- read.csv("newcleandata.csv")
yearlist <- unique(newcleandata$Year)
bdrmlist <- unique(newcleandata$BdRm)

#historical data
housingdata <- read.csv("bycounty.csv")
housingdata <- housingdata %>% mutate(median = median / 1000)
housingdata <- housingdata[order(housingdata$State, housingdata$RegionName),]
statelist <- unique(housingdata$State)

#Boxplot
boxp <- read.csv("bycounty.csv")
boxp$BdRm <- as.factor(boxp$BdRm)
boxp$Year <- as.factor(boxp$Year)
boxp <- boxp %>% mutate(median = median / 1000)
q1 <- quantile(boxp$median, 0.25)
q3 <- quantile(boxp$median, 0.75)
iqr <- IQR(boxp$median)
new_box <- subset(boxp, boxp$median > (q1 - 1.5*iqr) & boxp$median < (q3 + 1.5*iqr))

#Data summary
housingdata1 <- read.csv("reformatted_housing_data (1).csv")
county <- na.omit(group_by(housingdata1, BdRm, RegionName, Year, State))
bycounty <- summarise(county, median = median(Price))
bycounty2 <- new_box
bycounty2$BdRm <- as.numeric(bycounty2$BdRm)
bycounty2$Year <- as.numeric(bycounty2$Year)
bycounty3 <- new_box[c(1,3,5,6,7)]

#Filter data
median2022 <- bycounty[bycounty$Year >= 2022,]
median2022 <- median2022[order(median2022$BdRm, median2022$Year, median2022$State, median2022$median),]
property_tax <- read.csv("property_tax_data.csv")
median2022$tax <- property_tax$tax.rate[match(median2022$State, property_tax$state)]


r = 0.07
n = 30*12
p = median2022$median - median2022$median * 0.2
mr = r / 12
tx = median2022$median * median2022$tax
txm = tx / 12

M = p*(mr*((1+mr)**n))/(((1+mr)**n)-1) + txm 
median2022$Montly_payment <- M


rng1 <- range(median2022$BdRm, na.rm = TRUE)
rng2 <- range(median2022$median, na.rm = TRUE)
rng3 <- range(median2022$tax, na.rm = TRUE)
rng4 <- range(median2022$Montly_payment, na.rm = TRUE)


ui <- navbarPage("House Hunters",
                 tabPanel("Filter", 
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("bdrm", "Bedroom", min = rng1[1], max = rng1[2], value = rng1),
                              sliderInput("price", "Median Price", min = rng2[1], max = 1000000, round = TRUE, value = rng2),
                              sliderInput("tax", "Tax", min = rng3[1], max = rng3[2], value = rng3),
                              sliderInput("pmt", "Monthly Payment", min = rng4[1], max = 20000, value = rng4),
                              make_ui(median2022$State, "State"),
                              make_ui(median2022$RegionName, "County")
                              
                            ),
                            mainPanel(
                              tableOutput("data")
                            )
                          )
                 ),
                 tabPanel("Plot", 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Year", "Year", levels(newcleandata$Year), choices = yearlist),
                              selectInput("BdRm", "Bedroom", levels(newcleandata$BdRm), choices = bdrmlist),
                              #selectInput("state", "State", levels(newcleandata$state),
                              #unique(newcleandata$state))
                              actionButton("update", "Update Plot")
                            ),
                            
                            mainPanel(
                              plotOutput("plot")
                            )
                          )),
                 
                 tabPanel("Historical Data",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("State1", "Select State",  choices = statelist),
                              selectInput( "County1", "Select County",  choices = NULL),
                              selectInput("BdRm1", "Select Bedroom size", choices = NULL),
                              #selectInput("state", "State", levels(newcleandata$state),
                              #unique(newcleandata$state))
                              #actionButton("update", "Update Plot")
                            ),
                            
                            mainPanel(
                              plotOutput("histplot")
                            )
                          )
                          ),
                 
                 tabPanel("Data Summary",
                          dashboardPage(
                            dashboardHeader(title = "House Hunters", dropdownMenuOutput("msgOutput")),
                            dashboardSidebar(
                              sliderInput(
                                "Slider1",
                                label = h3("Train/Test Split %"),
                                min = 0,
                                max = 100,
                                value = 75
                              ),
                              textOutput("cntTrain"),
                              textOutput("cntTest"),
                              br()
                            ),
                            
                            dashboardBody(
                              fluidPage(
                                box(
                                  selectInput(
                                    "SelectX",
                                    label = "Select variables:",
                                    choices = names(bycounty3),
                                    multiple = TRUE,
                                    selected = names(bycounty3)
                                  ),
                                  solidHeader = TRUE,
                                  width = "3",
                                  status = "primary",
                                  title = "X variable"
                                ),
                                box(
                                  selectInput("SelectY", label = "Select variable to predict:", choices = names(bycounty3)),
                                  solidHeader = TRUE,
                                  width = "3",
                                  status = "primary",
                                  title = "Y variable"
                                )
                                
                              ),
                              
                              fluidPage(  
                                
                                tabBox(
                                  id = "tabset1",
                                  height = "1000px",
                                  width = 12,
                                  
                                  tabPanel("Data",
                                           box(withSpinner(DTOutput(
                                             "Data"
                                           )), width = 12)),
                                  tabPanel(
                                    "Data Summary",
                                    box(withSpinner(verbatimTextOutput("Summ")), width = 6),
                                    box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
                                  ),
                                  
                                  #                                  tabPanel("Plots",
                                  #                                           box(withSpinner(plotOutput(
                                  #                                             "Corr"
                                  #                                           )), width = 12)),
                                  #box(withSpinner(verbatimTextOutput("CorrMatrix")), width = 12),
                                  tabPanel(
                                    "Model",
                                    box(
                                      withSpinner(verbatimTextOutput("Model")),
                                      width = 6,
                                      title = "Model Summary"
                                    ),
                                    
                                    box(
                                      withSpinner(verbatimTextOutput("ImpVar")),
                                      width = 5,
                                      title = "Variable Importance"
                                    )
                                  ),
                                  
                                  tabPanel(
                                    "Prediction",
                                    box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
                                    box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
                                  ),
                                  
                                  tabPanel(
                                    "Histogram",
                                    box(withSpinner(plotOutput("Histogram")), width = 9, title = "Histogram of median price")
                                  ),
                          
                                  tabPanel(
                                    "Boxplot",
                                    box(withSpinner(plotOutput("boxplotbdrm")), width = 6, title = "Boxplot of Bedrooms"),
                                    box(withSpinner(plotOutput("boxplotYear")), width = 6, title = "Boxplot of Years")
                                  )
                                )
                              )
                            )
                          ))
)