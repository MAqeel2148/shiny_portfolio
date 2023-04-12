library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(patchwork)




library(shiny)

### ui dashboardpage Inside components #we cannot write the ui above the components 
##header
header <- dashboardHeader(title = "My Dashboard")
##sidebar
sidebar <- dashboardSidebar(
  selectInput("n",
              "Customer Name",
              "Names"
              
  ),
  sliderInput("bins",
              "Select Bins:",
              min = 1,
              max = 50,
              value = 30
  )
)
###body
body <- dashboardBody(
  fluidRow(
    valueBoxOutput(
      "value1",
      width = 2
    ),
    valueBoxOutput(
      "value2",
      width = 2
    ),
    valueBoxOutput(
      "value3",
      width = 2
    ),
    valueBoxOutput(
      "value4",
      width = 2
    ),
    valueBoxOutput(
      "value5",
      width = 2
    ),
    valueBoxOutput(
      "value6",
      width = 2
    ),
    box(
      plotOutput("category")
    ),
    box(
      plotOutput("total")
    ),
    box(
      plotOutput("sales")
    ),
    box(
      plotOutput("property")
    )
  )
)
##Ui dashboardpage
ui <- dashboardPage(header, sidebar, body)
####render side
server <- function(input, output, session) {
###dynamic data pipe
##reactive
  data <- reactive({
    req(input$n)
    df <- storeData %>% filter(`Customer Name` %in% input$n) %>% group_by(total_year_withUs) %>% summarise(sales = sum(m_total_spent),
                                                                                                         transaction = sum(n_transactions),
                                                                                                         lag = sum(m_lag_of_days),
                                                                                                         shipping = sum(m_shipping_days),
                                                                                                         rfm = sum(RFM_value),
                                                                                                         years = sum(total_year_withUs))
  })
  sales.cat <- df_newData %>% group_by(Category) %>% filter(duplication_exist != "TRUE") %>% summarise(value = sum(Sales)) 
  total.revenue <- sum(storeData$m_total_spent)
  sales.property <- storeData %>% group_by(`Customer Name`) %>% summarise(total_spent = sum(m_total_spent))
##oberve
  observe(
    updateSelectInput(session, inputId = "n", choices = storeData$`Customer Name`)
  )
  
  
##rendering work here----------------------------------------> 
  output$value1 <- renderValueBox({
    valueBox(
      formatC(data()[2], format="d", big.mark=',')
      ,paste('Total Spent')
      ,icon = icon("dollar",lib='font-awesome')
      ,color = "purple"
      )  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(data()[3], format="d", big.mark=',')
      ,paste('Total Transactions')
      ,icon = icon("shop",lib='font-awesome')
      ,color = "aqua") 
  })
  output$value3 <- renderValueBox({
   gap_x <- ifelse(is.na(data()[4]), "One Time", data()[4])
    valueBox(
      formatC(gap_x, format="d", big.mark=',')
      ,paste('Visit_Gaps')
      ,icon = icon("T",lib='font-awesome')
      ,color = "aqua") 
  })
  output$value4 <- renderValueBox({ 
    shipping_x <- ifelse(data()[5] == 0, "Same Day", data()[5])
    valueBox(
      formatC(shipping_x, format="d", big.mark=',')
      ,paste('Medain Ship days')
      ,icon = icon("T",lib='font-awesome')
      ,color = "aqua") 
  })
  output$value5 <- renderValueBox({ 
    valueBox(
      formatC(data()[6], format="d", big.mark=',')
      ,paste('RFM_value')
      ,icon = icon("value",lib='font-awesome')
      ,color = "aqua") 
  })
  output$value6 <- renderValueBox({ 
    top_x <- ifelse(data()[6] >= 15, "Top",
                    ifelse(data()[6] <=5, "Danger", "Normal"))
    color_x <- ifelse(top_x == "Top", "green", 
                      ifelse(top_x == "Normal","yellow", "red"))
    valueBox(
      formatC(top_x, format="d", big.mark=',')
      ,paste('customer category')
      ,icon = icon("T",lib='glyphicon')
      ,color = color_x) 
  })

#### graphs start here ---------------------------------> 
  output$category <- renderPlot({
    percent <- sales.cat$value/total.revenue
    ggplot(data = sales.cat, 
            aes(x=percent, y=Category, fill= Category), main = "Overall") + 
      geom_bar(width = 3, stat = "identity") + coord_polar(theta = "y", start = 0) + 
      ggtitle("Overall Results of Product Category")
  })
   output$total <- renderPlot({
     ggplot(data = storeData, aes(x = m_total_spent,
                                  y = total_year_withUs)) +
       geom_point()+
       geom_point(data = data(), aes(x = sales,
                                     y = years), color = "red", size = 10) +
       ggtitle("Overall Results with Individual customer Dot indication") +
       scale_x_continuous(breaks = seq(1,3000, by = 200))
  })
  output$sales <- renderPlot({
    Median_Total_spent = storeData$m_total_spent
    bins = seq(min(Median_Total_spent), max(Median_Total_spent), length.out = input$bins + 1)
    hist(Median_Total_spent, breaks = bins, col = "lightblue",
         main = "Overall Results of Customers Spent")
  })
  output$property <- renderPlot({
    ggplot(data = sales.property, aes( y = total_spent ))+
      geom_boxplot() +
      geom_point(data = data(), aes( x = 0,
                                     y = sales),
                 color = "red", size = 10) + ggtitle("Overall Results with Dot indication") +
      scale_y_continuous(breaks = seq(1,3000, by = 200)) 
      
     
  })
  
}

shinyApp(ui, server)