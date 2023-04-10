library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(mapproj)
library(maps)
library(shinyTree)




###
choiceNames <- setNames(as.character(storeData$total_spent), storeData$`Customer Name`)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "My Dashboard")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("General Overview", tabName = "dashboard", icon = icon("dashboard")),
    selectInput(inputId = "select_customer",
                label = "Select Customer:",
                choices = (choiceNames)  
                ),
    actionButton("action", "Reset")
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value2")
  ,valueBoxOutput("value1")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Revenue per Product Category"
    ,status = "primary"
    ,solidHeader = FALSE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyPrd", height = "300px")
  )
  ,box(
    title = "Revenue per Customer"
    ,status = "primary"
    ,solidHeader = FALSE 
    ,collapsible = TRUE
    ,plotOutput("revenuebyCustomer", height = "300px")
  ),
  box(
    title = "Customer count based on RFM Value"
    ,status = "primary"
    ,solidHeader = FALSE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyrfm", height = "300px")
  ),
  box(
    title = "Medain Revenue"
    ,status = "primary"
    ,solidHeader = FALSE 
    ,collapsible = TRUE 
    ,plotOutput("property", height = "300px")
  ),
  box(
    title = "Revenue Year-wise"
    ,status = "primary"
    ,solidHeader = FALSE 
    ,collapsible = TRUE 
    ,plotOutput("period", height = "300px")
    
  )
)

# combine the two fluid rows to make the body
body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            frow1, frow2
    )
  )
)
#completing the ui part with dashboardPage
ui <- dashboardPage(title = "My Dashboard", header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output) { 
  #some data manipulation to derive the values of KPI boxes
  sales.top <- storeData %>% group_by(`Customer Name`) %>% summarise(value = sum(total_spent))
  sales.cat <- df_newData %>% filter(duplication_exist != "TRUE") %>% group_by(Category) %>% summarise(value = sum(Sales)) %>% arrange(desc(Category))
  total.revenue <- sum(storeData$total_spent)
  sales.account <- storeData %>% group_by(`Customer Name`) %>% summarise(value = sum(total_spent)) %>% filter(value==max(value))  
  prof.prod <- df_newData %>% filter(duplication_exist != "TRUE") %>%  group_by(Category) %>% summarise(value = sum(Sales)) %>% filter(value==max(value)) 
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=',')
      ,paste('Top Customer:',sales.account$`Customer Name`)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Expected Revenue'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Top Product:',prof.prod$Category)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })
  #creating the plotOutput content
  output$revenuebyPrd <- renderPlot({
    percent <- sales.cat$value/total.revenue
    ggplot(data = sales.cat, 
           mapping = aes(x=Category, y=percent, fill= Category)) + 
      geom_bar(width = 4, stat = "identity") + coord_polar(theta = "y", start = 0)
      
  })
  output$revenuebyCustomer <- renderPlot({
    ggplot(data = storeData , aes(total_spent)) +
      geom_histogram(binwidth = 700)
  })
  output$revenuebyrfm <- renderPlot({
    ggplot(data = storeData,
           aes(x = RFM_segment, fill = factor(RFM_segment))) +
      geom_histogram(binwidth = 1, col = "#007bc2") +
      scale_y_continuous(breaks = seq(1,100, by = 10)) +
      scale_x_continuous(breaks = seq(1,15, by = 1))
  })
  output$property <- renderPlot({
   ggplot(data = storeData,
          aes( y = total_spent)) +
      geom_boxplot()
  })
  output$period <- renderPlot({
  ggplot(data = storeData, aes(x = total_years_withUs, y = total_spent)) +
      geom_point() 
  })
}
#run/call the shiny app
shinyApp(ui, server)
