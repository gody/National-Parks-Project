library(shiny)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(maps)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets) 
library(raster)
library(usmap)
library(treemap)
library(RColorBrewer)

if("d3treeR" %in% rownames(installed.packages()) == FALSE) {devtools::install_github("timelyportfolio/d3treeR")}
library(d3treeR)

tidy_visits <- readRDS('data/NP Visits.rds')
np <- readRDS('data/NP General Information.rds')
species <- readRDS('data/NP Species.rds')


full_data <- left_join(tidy_visits, np, by=c('Park' = 'Name'))

ui_3 = fluidPage(
  fluidRow(
    column(12,
           dataTableOutput('table')
    )
  )
)

ui_2 <- fluidPage(
  fluidRow(
    column(
      10,
      box(plotOutput("chorophlets"), width = 12)
    ),
    column(
      2,
      radioButtons("radio", h3("Choroplet Options"),
                   choices = list("Amount of Parks" = "parks", 
                                  "Total Area" = "area",
                                  "Visits" = "visits"), selected = "parks")
    )
  )
)
  
ui_1 <- fluidPage(
  theme = shinytheme("cerulean"),
  includeCSS("styles.css"),
  fluidRow(
    column(
      3,
      selectInput(
        "park", "",
        np$Name
      ),
      textOutput("description")
    ),
    column(
      9,
      fluidRow(),
      valueBox(
        uiOutput("parkDate"), "Date Established", icon = icon("fas fa-calendar-alt"), width = 4
      ),
      valueBox(
        uiOutput("parkArea"), "Total Area (Acres)", icon = icon("fas fa-globe-americas"), width = 4
      ),
      valueBox(
        uiOutput("parkStates"), "State(s)", icon = icon("fas fa-map-marked-alt"), width = 4
      ),
      valueBox(
        uiOutput("animals"), "Different Animals Species", icon = icon("fas fa-crow"), width = 4
      ),
      valueBox(
        uiOutput("plants"), "Different Plants Species", icon = icon("fas fa-leaf"), width = 4
      ),
      valueBox(
        uiOutput("endangerSpecies"), "Protected Endanger Species", icon = icon("fas fa-exclamation-triangle"), width = 4
      ),
    )
  ),
  fluidRow(),
  fluidRow(
    box(title = "Park Location", status = "primary", color = "blue", solidHeader = TRUE,
        collapsible = TRUE, leafletOutput("parkMap"), width = 4),
    box(title = "Average Monthly Visits", status = "primary", color = "blue", solidHeader = TRUE,
        collapsible = TRUE, plotOutput("monthPlot"), width = 4),
    box(title = "Total Annual Visits", status = "primary", color = "blue", solidHeader = TRUE,
        collapsible = TRUE, plotOutput("yearPlot"), width = 4)
  ),
  fluidRow(
    box(title = "Interactive Biodiversity", status = "primary", color = "blue", solidHeader = TRUE,
        collapsible = TRUE, d3tree2Output("speciesTreeMap"), width = 6),
    box(title = "Protected Species", status = "primary", color = "blue", solidHeader = TRUE,
        collapsible = TRUE, plotOutput("endanger"), width = 6)
  )
)

# Define UI for National Parks ----
ui_0 <- fluidPage(
  # use this in non shinydashboard app
  setBackgroundColor(color = "ghostwhite"),
  useShinydashboard(),
  # -----------------
  
  #  plotOutput("usParkMap"),# Copy the line below to make a slider range 
  fluidRow(
    valueBox(
      uiOutput("num_parks"), "Total Number of Parks", icon = icon("fas fa-map-marked"), width = 3
    ),
    valueBox(
      uiOutput("total_visits"), "Total Visits Since 1979 (in Millions)", icon = icon("fas fa-users"), width = 3
    ),
    valueBox(
      uiOutput("total_areas"), "Total Area", icon = icon("fas fa-tree"), width = 3
    ),
    valueBox(
      uiOutput("newest"), "Newest Parks", icon = icon("fas fa-star"), width = 3
    )
  ),
  # fluidRow(sliderInput("slider2", label = h3("Year Range"), min = 1979, 
  #               max = 2017, value = c(1979, 2017))
  # ),
  fluidRow(
    box(title = "Interactive Map", status = "primary", color = "blue", solidHeader = TRUE,
        collapsible = TRUE, leafletOutput("mymap")),
    box(title = "Visit Trend", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, plotOutput("visitsTrend"))
  ),
  fluidRow(
    box(title = "Top 5 States With More Parks", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, plotOutput("rankingStates")),
    box(title = "Top 5 Parks With More Average Anual Visits", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, plotOutput("rankingVisits"))
  )
)

## Final UI, link all different components
ui <- navbarPage(
  "National Parks Analytics",
  tabPanel("Global Analytics", ui_0),
  tabPanel("By Park", ui_1),
  tabPanel("By State", ui_2),
  navbarMenu(
    "More",
    tabPanel("Explore the Bio Dataset", ui_3)
  )
)

# Define server logic  ----
server <- function(input, output) {
  
  ## -------------------------------------------- Server Global Section -------------------------##
  
  output$num_parks <- renderText({ 
    61
  })
  output$total_visits <- renderText({ 
    format(sum(tidy_visits$Visits, na.rm = TRUE)/1000000, nsmall=1, digits = 1, big.mark=",")
  })
  output$total_areas <- renderText({
    x <- np %>%
      group_by(Name) %>%
      summarise(Area = mean(Area)) %>%
      .$Area %>%
      sum()
    format(x/1000000,nsmall=1, digits = 1, big.mark=",")
  })
  output$newest <- renderText({ 
    "Acadia"
  })
  
  output$mymap <- renderLeaflet({
    
    map_data <- tidy_visits %>%
      group_by(Park) %>%
      summarise(total.visits = sum(Visits)) %>%
      left_join(np, ., by = c('Name' = 'Park'))
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircles(lng = ~Long, 
                 lat = ~Lat, 
                 popup = ~Name,
                 weight = 1,
                 radius = ~sqrt(total.visits)*15) %>%
      setView(-95.6650, 37.600, 3.4)
  })
  
  output$visitsTrend <- renderPlot({
    tidy_visits %>%
      group_by(Year) %>%
      summarise(total.visits = sum(Visits, na.rm = TRUE)) %>%
      filter(Year < 2017) %>%
      ggplot(aes(x = Year, y = total.visits)) + 
      geom_point(size = 2) +
      geom_line(size = 0.5, color="blue") +
      labs(y="Visitors",
           x="Year",
           title='Park Visits Trends') +
      theme_classic() +
      scale_fill_brewer(palette="Blues")
  })
  
  output$rankingStates <- renderPlot({
    np %>%
      group_by(State) %>%
      summarise(park.count = n()) %>%
      arrange(desc(park.count)) %>%
      top_n(5) %>%
      ggplot(aes(x=fct_reorder(State, park.count),y=park.count, fill = State)) + 
      geom_bar(stat='identity') + 
      coord_flip() +
      labs(y="# of Parks",
           x="State",
           title='Top States by Amount of Parks') +
      theme_classic() +
      scale_fill_brewer(palette="Blues")
  })
  
  output$rankingVisits <- renderPlot({
    tidy_visits %>%
      group_by(Park) %>%
      summarise(total.visits = mean(Visits, na.rm = TRUE)) %>%
      arrange(desc(total.visits)) %>%
      top_n(5) %>%
      ggplot(aes(x=fct_reorder(Park, total.visits),y=total.visits, fill = Park)) + 
      geom_bar(stat='identity') + 
      coord_flip() +
      labs(y="# of Visitors",
           x="Park",
           title='Top Parks by Visitors') +
      theme_classic() +
      scale_fill_brewer(palette="Blues")
  })
  ## --------------------------------------------------------------------------------------------##
  
  ## -------------------------------------------- Server By Park --------------------------------##
  
  
  output$description <- renderText({ 
    x <- np %>%
      filter(Name == input$park) %>%
      .$Description
    toString(x)
  })
  
  output$header <- renderUI(
    h1(input$park)
  )
  
  output$parkDate <- renderText({ 
    x1 <- np %>%
      filter(Name == input$park) %>%
      .$Date
    toString(x1)
  })
  output$parkArea <- renderText({ 
    x2 <- np %>%
      filter(Name == input$park) %>%
      .$Area
    format(x2/1000, nsmall=1, digits = 1, big.mark=",")
  })
  output$parkStates <- renderText({ 
    x3 <- np %>%
      filter(Name == input$park) %>%
      .$State
    toString(x3)
  })
  output$animals <- renderText({ 
    x1 <- species %>%
      filter(`Park Name` == input$park, Category %in% c("Mammal", "Bird", "Reptile", "Amphibian", "Fish",
                                                      "Spider/Scorpion", "Insect", "Invertebrate", 
                                                      "Crab/Lobster/Shrimp", "Slug/Snail" )) %>%
      group_by(Category) %>%
      summarise(size = n()) 
    toString(sum(x1$size))
  })
  output$plants <- renderText({ 
    x2 <- species %>%
      filter(`Park Name` == input$park, Category %in% c("Vascular Plant", "Fungi", "Nanovascular Plant", "Algae")) %>%
      group_by(Category) %>%
      summarise(size = n()) 
    toString(sum(x2$size))
  })
  output$endangerSpecies <- renderText({ 
    x3 <- species %>%
      filter(`Park Name` == input$park, `Conservation Status` == "Endangered") %>%
      count()
    toString(x3)
  })
  
  output$monthPlot <- renderPlot({
    tidy_visits %>%
      group_by(Park, Month) %>%
      summarise(total.visits = mean(Visits)) %>%
      filter(Park == input$park) %>%
      ggplot(aes(x = Month, y = total.visits, group = 1)) +
      geom_point(size = 2) +
      geom_line(size = 0.5, color="blue") +
      scale_x_discrete(limits = toupper(month.abb)) +
      theme_classic() +
      scale_fill_brewer(palette="Blues")
  })
  
  output$yearPlot <- renderPlot({
    tidy_visits %>%
      group_by(Park, Year) %>%
      summarise(total.visits = sum(Visits)) %>%
      filter(Park == input$park) %>%
      ggplot(aes(x = Year, y = total.visits, group = 1)) +
      geom_point(size = 2) +
      geom_line(size = 0.5, color="blue") +
      theme_classic() +
      scale_fill_brewer(palette="Blues")
  })
  
  output$parkMap <- renderLeaflet({
    map_data <- np %>%
      filter(Name == input$park)
      
    leaflet(map_data) %>%
      addTiles() %>%
      addMarkers(lng = ~Long, 
                 lat = ~Lat, 
                 popup = ~Name) %>%
      setView(map_data$Long, map_data$Lat,10)
      
  })
  
  output$speciesTreeMap <- renderD3tree2({
    test <- species %>%
      group_by(`Park Name`,Category, Order) %>%
      summarise(size = n()) %>%
      filter(`Park Name` == input$park)
    
    t <- treemap(test,c("Category", "Order"), "size", palette="Blues")
    d3tree2( t ,  rootname = "Species Distribution" )
  })
  
  output$endanger <- renderPlot({
    species %>%
      group_by(`Park Name`,Category, `Conservation Status`) %>%
      summarise(size = n()) %>%
      drop_na() %>%
      filter(`Park Name` == input$park, `Conservation Status` != "Under Review") %>%
      ggplot(aes(x = Category, y = size, fill = `Conservation Status`)) +
      geom_bar(position="stack", stat="identity") + 
      scale_fill_brewer(palette="YlOrRd") +
      theme_minimal()
  })
  
  ## --------------------------------------------------------------------------------------------##

  output$chorophlets <- renderPlot({
    
    visit <- tidy_visits %>%
      group_by(Park) %>%
      summarise(avg.visits = mean(Visits))
    
    test <- np %>%
      left_join(visit, by = c("Name" = "Park")) %>%
      drop_na() %>%
      group_by(state) %>%
      summarize(parks = n(), area = log(sum(Area)), visits = sum(avg.visits))
    
    
    plot_usmap("states",
               data = test, values = input$radio, color = "white") + 
      labs(title = "Amount of National Parks per State") +
      theme(legend.position = "right")
  })
  
  ## --------------------------------------------------------------------------------------------##
  
  output$table <- renderDataTable({
    species %>%
      filter(`Park Name` == input$park) %>%
      subset(select = -c(`Species ID`,X14))
      },
    options = list(pageLength = 50)
  )
  
}

shinyApp(ui, server)