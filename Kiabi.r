require(shinydashboard)
install require(dplyr)
require(dbplyr)
require(purrr)
require(shiny)
require(highcharter)
require(DT)
require(htmltools)
require(plotly)
require(data.table)
require(shinyWidgets)
require(tidyr)
require(here)
require(dummies)
require(forecast)
require(data.table)
require(shinythemes)
require(tidyverse)

Sample2 <- fread('data/Kiabi_Tran_Sales_Data.csv',stringsAsFactors = F,header=T)
df <- read.csv("data/promo.csv")
df$X<-NULL

df$date = as.Date(df$date,format="%Y-%m-%d")
#df$posting_date<-NULL
#df <- df[0:,]

df_test <- read.csv("data/promo2019.csv")
df_test$date = as.Date(df_test$date,format="%Y-%m-%d")

xreg_temp <- read.csv(here("data", "Dubai_Calendar.csv"))
xreg_temp$date <- as.Date(xreg_temp$date,format="%Y-%m-%d")
xreg_temp<-xreg_temp[1591:1826,]

xreg_mega <- merge(df,xreg_temp,all=TRUE)
xreg_temp <- xreg_mega
ui <- dashboardPage(
  dashboardHeader(
    
    title = "KIABI"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Forecast", tabName = "forecast",icon = icon("send",lib='glyphicon')),
      menuItem("Sales", tabName = "sales",icon = icon("send",lib='glyphicon')),
      menuItem("Inventory", tabName = "inventory",icon = icon("send",lib='glyphicon'))
    )
  ),
  dashboardBody(
    
    mainPanel(
      width=12,
      tags$style(".small-box.bg-yellow { background-color: #ffffff !important; color: #000000 !important; }"),
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(valueBoxOutput("SalesQuantity",width = 3),
                         valueBoxOutput("GrossSalesValue" ,width = 3),
                         valueBoxOutput("GrossRetailValue" ,width = 3),
                         valueBoxOutput("GrossDiscount" ,width = 3)),
                fluidRow(
                  column(width = 2, htmlOutput("Retail_Location"))
                  # column(width = 7, htmlOutput("YearMonth")),
                  # column(width = 3, htmlOutput("Season_Category"))
                ),
                fluidRow(
                  column(width = 6, highchartOutput("hcontainer", height = "280px")),
                  column(width = 6, highchartOutput("hcontainer1", height = "280px"))
                ),
                fluidRow(
                  column(width = 6, plotlyOutput("hcontainer2", height = "280px")),
                  column(width = 6, highchartOutput("hcontainer3", height = "280px"))
                )
        ),
        
        tabItem(tabName = "forecast",
                fluidRow(
                  column(4,dateInput('date',
                                              label = 'First Available Date',
                                              value = '2019-01-01'
                  ) ),
                  column(4, dateInput('date2',
                                              label = 'Last available Date',
                                              value = '2019-01-05'
                  )),
                
                column(4, selectInput("promo", "Promo",c("BABY.BUNDL" = "BABY.BUNDL","BUY.2.GET" = "BUY.2.GET",
                                                         "BUY2GET1"="BUY2GET1","KIABI.40TH"="KIABI.40TH","LOYALTY"="LOYALTY",
                                                         "SUPERSALE"="SUPERSALE","VIP.WEEKEN"="VIP.WEEKEN"))
        )),
               fluidRow(
                  plotlyOutput("plot"),br(),br(),plotlyOutput("plotzoom")
                  
                )
                
        )
        
        
      )
      
      # Output: Tabset w/ plot, summary, and table ----
      
    )
    
  )
  
)
server <- function(input, output,session) {
  
  output$Retail_Location = renderUI({
    newSample<-Sample2
    radioButtons(inputId = "location",
                 label = "Location:",
                 inline = TRUE,
                 choices = as.character(unique(newSample$`Retail location Key`)))
  })
  observe({
    
    # Can also set the label and select items
    updateRadioButtons(session, "Retail_Location",
                       label = "Location:",
                       inline = TRUE,
                       choices = as.character(unique(Sample2$`Retail location Key`)))
  })
  
  # 
  # output$YearMonth = renderUI({ 
  #   radioButtons(
  #     inputId = "yearmonth",
  #     label = "Year Month:",
  #     inline = TRUE,
  #     choices = as.character(unique(Sample2$YearMonth))
  #   )
  # })
  # 
  # 
  #   output$Season_Category = renderUI({ 
  #   radioButtons(inputId = "season",
  #                label = "Season:",
  #                inline = TRUE,
  #                choices = unique(Sample2$Season))
  # })
  
  canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
  
  output$hcontainer <- renderHighchart({      
    
    Sample3 <- Sample2[Sample2$`Retail location Key` == input$location,]
    # Sample2 <- Sample2[Sample2$YearMonth == input$yearmonth,]
    # Sample2 <- Sample2[Sample2$Season == input$season,]
    
    Sample3 <- Sample3 %>% group_by(Division) %>% summarise(Sales_Quantity = sum(`Sales Quantity`))   
    print(Sample3)
    highchart() %>% 
      hc_title(text="Sales Quantity by Division") %>%
      hc_xAxis(categories = Sample3$Division) %>% 
      hc_add_series(name = "Sales Quantity", data = Sample3$Sales_Quantity) %>%
      hc_plotOptions(series = list(stacking = FALSE, events = list(click = canvasClickFunction))) %>%
      hc_chart(type = "column")
    
  })
  
  canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
  
  output$hcontainer1 <- renderHighchart({
    
    abc <- input$canvasClicked[2]
    
    if(is.null(abc)) {
      Sample3 <- Sample2
    }
    else
    {
      Sample3 <- subset(Sample2,Sample2$Division == abc)
    }
    Sample3 <- Sample3[Sample3$`Retail location Key` == input$location,]
    # Sample2 <- Sample2[Sample2$YearMonth == input$yearmonth,]
    # Sample2 <- Sample2[Sample2$Season == input$season,]
    
    Sample3 <- Sample3 %>% group_by(Brand) %>% summarise(Sales_Quantity = sum(`Sales Quantity`))   
    highchart() %>%
      hc_title(text="Sales Quantity by Brand") %>%
      hc_xAxis(categories = Sample3$Brand) %>%
      hc_add_series(name = "Sales Quantity", data = Sample3$Sales_Quantity) %>%
      hc_plotOptions(series = list(stacking = FALSE)) %>%
      hc_chart(type = "bar") 
    
  })
  
  output$hcontainer2 <- renderPlotly({
    abc <- input$canvasClicked[2]
    
    if(is.null(abc)) {
      Sample3 <- Sample2
    }
    else
    {
      Sample3 <- subset(Sample2,Sample2$Division == abc)
    }
    Sample3 <- Sample3[Sample3$`Retail location Key` == input$location,]
    # Sample2 <- Sample2[Sample2$YearMonth == input$yearmonth,]
    # Sample2 <- Sample2[Sample2$Season == input$season,]
    
    
    #Sample3_mean <- Sample3 %>% group_by(Category) %>% summarise(Sales_Quantity = mean(`AUR`,na.rm=TRUE))
    Sample4_mean = aggregate(cbind(AUR)~Category,data = Sample3,mean,na.rm=TRUE)
    
    Sample4_sum = aggregate(cbind(Retail_Value,`Sales Quantity`)~Category,data = Sample3,sum)
    
    
    Sample5= merge(Sample4_mean,Sample4_sum,by="Category",all=TRUE)
print("___________________________________________________________________________")
print(Sample5)
   
    # highchart() %>%
    #   hc_title(text="AUR by Category") %>%
    #   hc_xAxis(categories = Sample3$Category) %>%
    #   hc_add_series(name = "AUR by Category", data = Sample3$Sales_Quantity) %>%
    #   hc_plotOptions(series = list(stacking = FALSE)) %>%
    #   hc_chart(type = "bar")
    # 
    plot_ly(Sample5, x = ~AUR, y = ~`Retail_Value`, text = ~Category, type = 'scatter', mode = 'markers',
                 marker = list(size = ~'Sales Quantity', opacity = 0.5)) %>%
      layout(title = 'Category wise AUR Comparsion',
             xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE),
             plot_bgcolor='rgba(255, 255, 255,0)',
             paper_bgcolor='rgba(255, 255, 255,0)')

    
    
    
  })
  
  output$hcontainer3 <- renderHighchart({
    
    abc <- input$canvasClicked[2]
    
    if(is.null(abc)) {
      Sample3 <- Sample2
    }
    else
    {
      Sample3 <- subset(Sample2,Sample2$Division == abc)
    }
    Sample3 <- Sample3[Sample3$`Retail location Key` == input$location,]
    # Sample2 <- Sample2[Sample2$YearMonth == input$yearmonth,]
    # Sample2 <- Sample2[Sample2$Season == input$season,]
    
    # Sample5 <- Sample2 %>% group_by(`Discount Type`) %>% summarise(Sales_Quantity = sum(`Normal Sales Value`))  
    #Sample5<-subset(Sample5,Sample5$`Discount Type`!="Not assigned")
    Sample3 = as.data.frame(table(Sample3$Promotion))
    print(Sample3)
    colnames(Sample3)=c("Promotion","Count")
    print(Sample3)
    # highchart() %>%
    #   hc_xAxis(categories = Sample2$Country) %>%
    #   hc_add_series(name = "Retail Amount", data = Sample2$Gross.Margin) %>%
    #   hc_plotOptions(series = list(stacking = FALSE)) %>%
    #   hc_chart(type = "pie")
    # 
    highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_add_series_labels_values(labels = Sample3$Promotion, values = Sample3$Count)%>% 
      hc_tooltip(pointFormat = paste('{point.y}Frequency <br/><b>{point.percentage:.1f}%</b>')) %>%
      hc_title(text = "Promotion Frequency")
    
    
  })
  
  output$SalesQuantity <- renderValueBox({
    abc <- input$canvasClicked[2]
    if(is.null(abc)) {
      Sample3 <- Sample2
    }
    else
    {
      Sample3 <- subset(Sample2,Sample2$Division == abc)
    }
    Sample3 <- Sample3[Sample3$`Retail location Key` == input$location,]
    # Sample2 <- Sample2[Sample2$YearMonth == input$yearmonth,]
    # Sample2 <- Sample2[Sample2$Season == input$season,]
    
    total = as.data.frame(table(Sample3$`Transaction Number`))
    total_count = length(unique(total$Var1))
    print(total_count)

    valueBox(
      formatC(total_count, format="d", big.mark=',')
      ,'No of Transactions'
      ,icon = icon("shopping-cart",lib='glyphicon'),
      color="navy")  
  })
  
  output$GrossSalesValue <- renderValueBox({ 
    abc <- input$canvasClicked[2]
    if(is.null(abc)) {
      Sample3 <- Sample2
    }
    else
    {
      Sample3 <- subset(Sample2,Sample2$Division == abc)
    }
    Sample3 <- Sample3[Sample3$`Retail location Key` == input$location,]
    # Sample2 <- Sample2[Sample2$YearMonth == input$yearmonth,]
    # Sample2 <- Sample2[Sample2$Season == input$season,]
    
    
    total <- sum(Sample3$`Sales Quantity`)
    
    valueBox(
      formatC(total, format="d", big.mark=',')
      ,'Sales Quantity'
      ,icon = icon("usd",lib='glyphicon'),
      color="navy")
  })
  
  
  
  output$GrossRetailValue <- renderValueBox({ 
    abc <- input$canvasClicked[2]
    if(is.null(abc)) {
      Sample3 <- Sample2
    }
    else
    {
      Sample3 <- subset(Sample2,Sample2$Division == abc)
    }
    Sample3 <- Sample3[Sample3$`Retail location Key` == input$location,]
    # Sample2 <- Sample2[Sample2$YearMonth == input$yearmonth,]
    # Sample2 <- Sample2[Sample2$Season == input$season,]
    
    
    total <- sum(Sample3$Retail_Value)
    
    valueBox(
      formatC(total, format="d", big.mark=',')
      ,'Gross Retail Value'
      ,icon = icon("gbp",lib='glyphicon'),
      color="navy")
  })
  
  output$GrossDiscount <- renderValueBox({
    abc <- input$canvasClicked[2]
    if(is.null(abc)) {
      Sample3 <- Sample2
    }
    else
    {
      Sample3 <- subset(Sample2,Sample2$Division == abc)
    }
    Sample3 <- Sample3[Sample3$`Retail location Key` == input$location,]
    # Sample2 <- Sample2[Sample2$YearMonth == input$yearmonth,]
    # Sample2 <- Sample2[Sample2$Season == input$season,]
    
    
    total <- sum(Sample3$`Discount Value Gross`)
   print(head(Sample3)) 
   print(str(Sample3))
   print(sum(Sample3$`Retail Value`))
    AUR = sum(Sample3$`Retail Value`)/sum(Sample3$`Sales Quantity`)
    print(AUR)
    valueBox(
      formatC(total, format="d", big.mark=',')
      ,'Gross Discount'
      ,icon = icon("gbp",lib='glyphicon'),
      color="navy"
    )
  })
  
  output$merged <- shiny::renderDataTable({
    xreg_subset <- df_test %>%
      filter(as.Date(Date) >= input$date, as.Date(Date) <= input$date2)
  })
  
  fit <- readRDS(here("models", 'arima.rds'))
  
  
  
  output$plot <- renderPlotly({
    if(input$promo == "BABY.BUNDL"){
      df_test$BABY.BUNDL[df_test$BABY.BUNDL %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "BUY.2.GET"){
      df_test$BUY.2.GET[df_test$BUY.2.GET %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "BUY2GET1"){
      df_test$BUY2GET1[df_test$BUY2GET1 %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "KIABI.40TH"){
      df_test$KIABI.40TH[df_test$KIABI.40TH %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "LOYALTY"){
      df_test$LOYALTY[df_test$LOYALTY %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "SUPERSALE"){
      df_test$SUPERSALE[df_test$SUPERSALE %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "VIP.WEEKEN"){
      df_test$VIP.WEEKEN[df_test$VIP.WEEKEN %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    xreg_subset <- df_test %>%
      filter(as.Date(date) >= input$date, as.Date(date) <= input$date2)
    
    #fit <- auto.arima(xreg_temp$Quantity,xreg=as.matrix(xreg_temp[,c(2:8)],seasonal=TRUE,stepwise=TRUE, approximation=TRUE))
    #saveRDS(fit, file = here("models", 'arima.rds'))
    fcast<-forecast(fit,xreg=as.matrix(xreg_subset[,c(2:8)]))
    #autoplot(obv_fcast)
    trace1obv <- list(
      x = df$date,
      y = df$Quantity,
      mode = "lines",
      name = "Train Data",
      type = "scatter",
      fill = 'tozeroy',
      uid = "6c00f3b4-cfdb-11e8-9236-a8a79588395f"
      
    )
    
    trace2obv <- list(
      x = xreg_subset$date,
      y = fcast$mean,
      mode = "lines",
      name = "Forecast Data",
      type = "scatter",
      fill = 'tozeroy',
      uid = "6c00f3b5-cfdb-11e8-9236-a8a79588395f"
    )
    data <- list(trace1obv, trace2obv)
    layout <- list(
      title = "Quantities Sold Forecast using ARIMA",
      xaxis = list(title = "Date",linecolor="black"),
      yaxis = list(title = "Quantities",linecolor="black")
      
    )
    p <- plot_ly()
    p <- add_trace(line = list(color='rgb(44,62,80)'),p, x=trace1obv$x, y=trace1obv$y, mode=trace1obv$mode, name=trace1obv$name, type=trace1obv$type, uid=trace1obv$uid)
    p <- add_trace(line = list(color='rgb(61,176,244)'),p, x=trace2obv$x, y=trace2obv$y, mode=trace2obv$mode, name=trace2obv$name, type=trace2obv$type, uid=trace2obv$uid)
    p <- layout(paper_bgcolor = 'rgba(255,255,255,0)',
                plot_bgcolor = 'transparent',p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, 
                axis.line.y = element_line(colour="white"),axis.line.y = element_line(colour="white"),linewidth = 5)
    
    
  }
  )
  
  
  output$plotzoom <- renderPlotly({
    if(input$promo == "BABY.BUNDL"){
      df_test$BABY.BUNDL[df_test$BABY.BUNDL %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "BUY.2.GET"){
      df_test$BUY.2.GET[df_test$BUY.2.GET %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "BUY2GET1"){
      df_test$BUY2GET1[df_test$BUY2GET1 %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "KIABI.40TH"){
      df_test$KIABI.40TH[df_test$KIABI.40TH %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "LOYALTY"){
      df_test$LOYALTY[df_test$LOYALTY %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "SUPERSALE"){
      df_test$SUPERSALE[df_test$SUPERSALE %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    else if(input$promo == "VIP.WEEKEN"){
      df_test$VIP.WEEKEN[df_test$VIP.WEEKEN %in% 0] <- 1
      # xreg_temp$`Promo.Code_15%_Off` <- xreg_temp[xreg_temp$`Promo.Code_15%_Off`==1]
    }
    
    xreg_subset <- df_test %>%
      filter(as.Date(date) >= input$date, as.Date(date) <= input$date2)
    fcast<-forecast(fit,xreg=as.matrix(xreg_subset[,c(2:8)]))
    
    trace1obv <- list(
      x = xreg_subset$date,
      y = fcast$mean,
      mode = "lines",
      name = "Train Data",
      type = "bar",
      uid = "6c00f3b5-cfdb-11e8-9236-a8a79588395f"
    )
    dataobv <- list(trace1obv)
    layout <- list(
      title = "Quantities Sold Forecast using ARIMA",
      xaxis = list(title = "Date",linecolor="black"),
      yaxis = list(title = "Quantities",linecolor="black")
    )
    p <- plot_ly()
    p <- add_trace(marker = list(color = '#3db0f4'),p, x=trace1obv$x, y=trace1obv$y, mode=trace1obv$mode, name=trace1obv$name, type=trace1obv$type, uid=trace1obv$uid)
    p <- layout(paper_bgcolor = 'rgba(255,255,255,0)',
                plot_bgcolor = 'transparent',p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, linecolor = "black",linewidth = 5)
    p
  })
  
  
  
}
shinyApp(ui, server)
