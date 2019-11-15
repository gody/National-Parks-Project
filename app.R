library(shiny)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(maps)
library(gganimate)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)

tidy_visits <- readRDS('data/NP Visits.rds')
np <- readRDS('data/NP General Information.rds')


full_data <- left_join(tidy_visits, np, by=c('Park' = 'Name'))
  
ui_1 <- fluidPage(
  theme = shinytheme("darkly"),
  fluidRow(
    selectInput(
      "park", "Select a park:",
      np$Name
    )
  ),
  fluidRow(
    column(
      5,
      box(leafletOutput("parkMap"), width = 12),
    ),
    column(
      4,
      h1("{Park Name}"),
      textOutput("description")
    ),
    column(
      3,
      valueBox(
        uiOutput("parkDate"), "Date Stablish", icon = icon("fas fa-calendar-alt"), width = 12
      ),
      valueBox(
        uiOutput("parkArea"), "Total Area", icon = icon("fas fa-globe-americas"), width = 12
      ),
      valueBox(
        uiOutput("parkStates"), "State(s)", icon = icon("fas fa-map-marked-alt"), width = 12
      ),
    )
  ),
  fluidRow(
    box(plotOutput("monthPlot"), width = 4),
    box(plotOutput("yearPlot"), width = 4)
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
    box(leafletOutput("mymap")),
    box(plotOutput("visitsTrend"))
  ),
  fluidRow(
    box(plotOutput("rankingStates")),
    box(plotOutput("rankingVisits"))
  )
)

## Final UI, link all different components
ui <- navbarPage(
  "National Parks Analytics",
  tabPanel("Global Analytics", ui_0),
  tabPanel("By Park", ui_1),
  navbarMenu(
    "More",
    tabPanel("Sub-Component A"),
    tabPanel("Sub-Component B")
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
    format(sum(np$`Area (acres)`, na.rm = TRUE), nsmall=1, digits = 1, big.mark=",")
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
                 radius = ~sqrt(total.visits)*15)
  })
  
  output$visitsTrend <- renderPlot({
    tidy_visits %>%
      group_by(Year) %>%
      summarise(total.visits = sum(Visits, na.rm = TRUE)) %>%
      filter(Year < 2017) %>%
      ggplot(aes(x = Year, y = total.visits)) + 
      geom_point(size = 2) +
      geom_line(size = 0.5) +
      labs(y="Visitors",
           x="Year",
           title='Park Visits Trends') +
      theme_classic()
  })
  
  output$rankingStates <- renderPlot({
    np %>%
      group_by(State) %>%
      summarise(park.count = n()) %>%
      arrange(desc(park.count)) %>%
      top_n(5) %>%
      ggplot(aes(x=fct_reorder(State, park.count),y=park.count)) + 
      geom_bar(stat='identity') + 
      coord_flip() +
      labs(y="# of Parks",
           x="State",
           title='Top States by Amount of Parks') +
      theme_classic()
  })
  
  output$rankingVisits <- renderPlot({
    tidy_visits %>%
      group_by(Park) %>%
      summarise(total.visits = sum(Visits, na.rm = TRUE)) %>%
      arrange(desc(total.visits)) %>%
      top_n(5) %>%
      ggplot(aes(x=fct_reorder(Park, total.visits),y=total.visits)) + 
      geom_bar(stat='identity') + 
      coord_flip() +
      labs(y="# of Visitors",
           x="Park",
           title='Top Parks by Visitors') +
      theme_classic()
  })
  ## --------------------------------------------------------------------------------------------##
  
  ## -------------------------------------------- Server By Park --------------------------------##
  
  
  output$description <- renderText({ 
    x <- np %>%
      filter(Name == input$park) %>%
      .$Description
    toString(x)
  })
  
  output$parkDate <- renderText({ 
    x1 <- np %>%
      filter(Name == input$park) %>%
      .$Date
    toString(x1)
  })
  output$parkArea <- renderText({ 
    x2 <- np %>%
      filter(Name == input$park) %>%
      .$`Area (acres)`
    x2 <- 'x,xxx.xx' 
    # format(x2/1000, nsmall=1, digits = 1, big.mark=",")
  })
  output$parkStates <- renderText({ 
    x3 <- np %>%
      filter(Name == input$park) %>%
      .$State
    toString(x3)
  })
  
  output$monthPlot <- renderPlot({
    tidy_visits %>%
      group_by(Park, Month) %>%
      summarise(total.visits = mean(Visits)) %>%
      filter(Park == input$park) %>%
      ggplot(aes(x = Month, y = total.visits, group = 1)) +
      geom_point(size = 2) +
      geom_line(size = 0.5) +
      scale_x_discrete(limits = toupper(month.abb))
  })
  
  output$yearPlot <- renderPlot({
    tidy_visits %>%
      group_by(Park, Year) %>%
      summarise(total.visits = sum(Visits)) %>%
      filter(Park == input$park) %>%
      ggplot(aes(x = Year, y = total.visits, group = 1)) +
      geom_point(size = 2) +
      geom_line(size = 0.5)
  })
  
  output$parkMap <- renderLeaflet({
    map_data <- np %>%
      filter(Name == input$park)
      
    leaflet(map_data) %>%
      addTiles() %>%
      addMarkers(lng = ~Long, 
                 lat = ~Lat, 
                 popup = ~Name)
  })
  
  ## --------------------------------------------------------------------------------------------##
  
  

  ## ------- MAP EXAMPLE LEAFLET---------- ##
  
  # output$year_range <- renderPrint({ input$slider2 })
  
  
  
  
  
  # output$animation <- renderImage({
  #   # A temp file to save the output.
  #   # This file will be removed later by renderImage
  #   outfile <- tempfile(fileext='.gif')
  #   
  #   # now make the animation
  #   p <- tidy_visits %>%
  #     group_by(Park, Year) %>%
  #     summarise(total.visits = sum(Visits)) %>%
  #     ggplot(aes(x = fct_reorder(Park, total.visits), total.visits, fill = Park)) +
  #     geom_bar(stat='identity',show.legend = FALSE) +
  #     coord_flip() +
  #     # Here comes the gganimate specific bits
  #     labs(title = 'Year: {closest_state}', x = 'Park', y = 'Year Visits') +
  #     transition_states(Year, transition_length = 1, state_length = 1) +
  #     ease_aes('linear')
  #   animate(p, fps=10)
  #   
  #   anim_save("outfile.gif", animate(p)) # New
  # })
}

shinyApp(ui, server)