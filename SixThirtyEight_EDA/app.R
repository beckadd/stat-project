#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tmap)
library(leaflet)

load("../data/african_gdps.RData")
load("../data/africa.sf")
data(World)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploratory Data Analysis: African Economic Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            selectInput(inputId = "measured",
                        label = "Data:",
                        choices = 
                            c(
                                "% change CPI" = "percentchange_cpi",
                                "% change GDP" = "percentchange_gdp",
                                "GDP" = "gdp"),
                        selected = "% change CPI"
                        ),
            
            checkboxGroupInput(inputId ="country",
                          label = "Countries:",
                          choices = unique(african_sf$country)
                          ),
        
            helpText(
                "Move this slider to see
                how the selected measured 
                data changed over time.")
            ),
        
        mainPanel = mainPanel(
            tabsetPanel(
                tabPanel("boxplot", plotOutput("boxplot")),
                tabPanel("map", leafletOutput("map")),
                tabPanel("table", tableOutput("table"))
                )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$boxplot <- renderPlot({
        african_sf %>%
            filter(country %in% input$country) %>%
            ggplot(mapping = aes(
                x = year,
                y = african_sf$(input$measured),
                color = region
            )) +
                geom_point()
    })
    
    output$map <- renderLeaflet({
        map <- tm_shape(filter(World, continent == "Africa")) +
            tm_borders("gray", lwd = 0.1) +
            tm_shape(african_sf) +
            tm_polygons(col = "input$measured", title = "", style = "log10_pretty") +
            tm_text(text = "percent change", size = 0.4) +
            tm_layout("Annual %change CPI",
                      legend.outside = T)
        
        tmap_leaflet(map)
    })
    
    output$table <- renderDataTable(
        african_sf %>%
            filter(!is.na(input$measured)) %>%
            select(country, year, input$measured)
            )
}

# Run the application 
shinyApp(ui = ui, server = server)
