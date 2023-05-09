library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)
library(caret)
library(stargazer)

#Boxplot
boxp <- read.csv("bycounty.csv")
boxp$BdRm <- as.factor(boxp$BdRm)
boxp$Year <- as.factor(boxp$Year)
boxp <- boxp %>% mutate(median = median / 1000)
q1 <- quantile(boxp$median, 0.25)
q3 <- quantile(boxp$median, 0.75)
iqr <- IQR(boxp$median)
new_box <- subset(boxp, boxp$median > (q1 - 1.5*iqr) & boxp$median < (q3 + 1.5*iqr))


server <- function(input, output, session) {
  
  selected <- reactive({
    filter_var(median2022$BdRm, input$bdrm) &
      filter_var(median2022$median, input$price) &
      filter_var(median2022$State, input$State) &
      filter_var(median2022$tax, input$tax) &
      filter_var(median2022$Montly_payment, input$pmt) &
      filter_var(median2022$RegionName, input$County)
    
  })
  
  
  output$data <- renderTable(median2022[selected(),], 20)
  
  v <- reactiveValues(
    data = newcleandata,
    title = "All Data"
  )
  
  observeEvent(input$update, {
    v$data <- filter(newcleandata,
                     Year == input$Year,
                     BdRm == input$BdRm)
    v$title <- sprintf("Year: %s, Bedroom: %s", 
                       input$Year,
                       input$BdRm)
  })
  # assign values to `output` here
  
  output$plot <- renderPlot({
    data <- filter(newcleandata,
                   Year == input$Year,
                   BdRm == input$BdRm)
    
    plot_usmap(regions = "state", data = v$data, values = "Price", color = "blue", labels = TRUE) +
      scale_fill_continuous(name = "Price", low = "white", high = "blue", label = scales::comma) + 
      theme(legend.position = "right") + 
      ggtitle("Housing Price data")
    
  })
  
  output$title <- renderText({
    input$update
    
    sprintf("Year: %s, Bedroom: %s", 
            input$Year,
            input$BdRm)
    
  })
  
  #Historical data
  State2 <- reactive({
    filter(housingdata, State == input$State1)
  })
  
  observeEvent(State2(), {
    choices <- unique(State2()$RegionName)
    updateSelectInput(session, "County1", choices = choices)
  })
  
  county <- reactive({
    req(input$County1)
    filter(State2(), RegionName == input$County1)
  })
  
  observeEvent(county(), {
    choices <- unique(county()$BdRm)
    updateSelectInput(session, "BdRm1", choices = choices)
  })
  
  output$histplot <- renderPlot({
    req(input$BdRm1)
    data <- county() %>%
      filter(BdRm == input$BdRm1) %>%
      select(Year, median)
    ggplot(data, aes(x = Year, y = median, label=sprintf("%0.2f", round(median, digits = 2)))) + geom_col(fill = "#0099f9") +
      geom_text(size = 3.5, vjust = -0.2) +
      labs(
        title = sprintf("Historical data of %s from 2000 to 2023", input$County1),
        y = "Median price of the house in 1000"
      )
  })
  
  
  #Data summary
  InputDataset <- reactive({
    bycounty2
  })
  
  
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- bycounty2
    }
    else{
      dt <- bycounty2[, c(input$SelectX)]
    }
    
  })
  
  
  observe({
    lstname <- names(InputDataset())
    updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  output$Summ <-
    renderPrint(
      stargazer(
        InputDataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(InputDataset()))
  output$structure <- renderPrint(str(InputDataset()))
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })
  
  
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  output$Data <- renderDT(InputDataset())
  
  
  cormat <- reactive({
    round(cor(InputDataset()), 1)
  })
  #  output$Corr <-
  #    renderPlot(corrplot(
  #      cormat(),
  #      type = "lower",
  #      order = "hclust",
  #      method = "number"
  #    ))
  
  
  #Code section for Linear Regression-----------------------------------------------------------------------------
  
  f <- reactive({
    as.formula(paste(input$SelectY, "~."))
  })
  
  
  Linear_Model <- reactive({
    lm(f(), data = trainingData())
  })
  
  output$Model <- renderPrint(summary(Linear_Model()))
  output$Model_new <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = "text",
        title = "Model Results",
        digits = 1,
        out = "table1.txt"
      )
    )
  
  Importance <- reactive({
    varImp(Linear_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  
  price_predict <- reactive({
    predict(Linear_Model(), testData())
  })
  
  tmp <- reactive({
    tmp1 <- testData()
    tmp1[, c(input$SelectY)]
  })
  
  
  actuals_preds <-
    reactive({
      data.frame(cbind(actuals = tmp(), predicted = price_predict()))
    })
  
  Fit <-
    reactive({
      (
        plot(
          actuals_preds()$actuals,
          actuals_preds()$predicted,
          pch = 16,
          cex = 1.3,
          col = "blue",
          main = "Best Fit Line",
          xlab = "Actual",
          ylab = "Predicted"
        )
      )
    })
  
  output$Prediction <- renderPlot(Fit())
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
    plot(Linear_Model())
    par(mfrow = c(1, 1)) # Change back to 1 x 1
    
  })
  
  output$digest <- renderExplorer({
    
    explorer(data = bycounty2$data, demo = F)
    
  })

  output$Histogram <- renderPlot({
    ggplot(new_box, aes(median, fill = BdRm, colour = BdRm)) + 
      geom_histogram(alpha = 0.5, position = "identity") + 
      labs(x = 'Median price in 1000')
  })  

  
  output$boxplotbdrm <- renderPlot({
    ggplot(new_box, aes(x = BdRm, y = median, fill = BdRm)) + geom_boxplot(notch = TRUE) + 
      labs(
        y = "Median price of the house in 1000"
      )
  })
  
  output$boxplotYear <- renderPlot({
    ggplot(new_box, aes(x = Year, y = median, fill = Year)) + geom_boxplot(notch = TRUE) + 
      labs(
        y = "Median price of the house in 1000"
      )

  })
}