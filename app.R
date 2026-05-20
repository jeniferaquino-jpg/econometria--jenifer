# =========================================================
# GAPMINDER INSIGHTS DASHBOARD - SHINY COMPLETO
# LISTO PARA SUBIR A SHINYAPPS.IO
# =========================================================

# =========================================================
# INSTALAR PAQUETES (SOLO UNA VEZ)
# =========================================================

install.packages(c(
  "shiny",
  "shinydashboard",
  "plotly",
  "ggplot2",
  "dplyr",
  "gapminder",
  "DT",
  "leaflet",
  "scales",
  "htmltools",
  "rsconnect"
))

# =========================================================
# LIBRERIAS
# =========================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(gapminder)
library(DT)
library(leaflet)
library(scales)
library(htmltools)

# =========================================================
# DATA
# =========================================================

data(gapminder)

gapminder$gdpPercap <- round(gapminder$gdpPercap, 2)

# =========================================================
# UI
# =========================================================

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Gapminder Insights"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      selectInput(
        "continent",
        "Seleccionar Continente:",
        choices = c("All", unique(gapminder$continent)),
        selected = "All"
      ),
      
      sliderInput(
        "year",
        "Seleccionar Año:",
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = 2007,
        step = 5,
        sep = ""
      )
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #0b1326;
        }
        
        .small-box {
          border-radius: 15px;
        }
        
        .box {
          border-radius: 15px;
        }
      "))
    ),
    
    tabItems(
      
      # =====================================================
      # TAB DASHBOARD
      # =====================================================
      
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          
          valueBoxOutput("lifeExpBox", width = 4),
          valueBoxOutput("gdpBox", width = 4),
          valueBoxOutput("populationBox", width = 4)
        ),
        
        fluidRow(
          
          box(
            title = "Income vs Life Expectancy",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("bubblePlot", height = 400)
          ),
          
          box(
            title = "Continents Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("pieChart", height = 400)
          )
        ),
        
        fluidRow(
          
          box(
            title = "GDP Trend",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("trendPlot", height = 350)
          ),
          
          box(
            title = "World Map",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            leafletOutput("mapPlot", height = 350)
          )
        ),
        
        fluidRow(
          
          box(
            title = "Gapminder Data Table",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("dataTable")
          )
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  # =======================================================
  # FILTRO
  # =======================================================
  
  filtered_data <- reactive({
    
    data <- gapminder %>%
      filter(year == input$year)
    
    if(input$continent != "All"){
      data <- data %>%
        filter(continent == input$continent)
    }
    
    data
  })
  
  # =======================================================
  # VALUE BOXES
  # =======================================================
  
  output$lifeExpBox <- renderValueBox({
    
    avg_life <- round(mean(filtered_data()$lifeExp), 1)
    
    valueBox(
      value = avg_life,
      subtitle = "Life Expectancy",
      icon = icon("heartbeat"),
      color = "green"
    )
  })
  
  output$gdpBox <- renderValueBox({
    
    avg_gdp <- dollar(mean(filtered_data()$gdpPercap))
    
    valueBox(
      value = avg_gdp,
      subtitle = "GDP per Capita",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$populationBox <- renderValueBox({
    
    total_pop <- comma(sum(filtered_data()$pop))
    
    valueBox(
      value = total_pop,
      subtitle = "Population",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  # =======================================================
  # BUBBLE PLOT
  # =======================================================
  
  output$bubblePlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = gdpPercap,
        y = lifeExp,
        size = pop,
        color = continent,
        text = country
      )
    ) +
      geom_point(alpha = 0.7) +
      scale_x_log10() +
      theme_minimal() +
      labs(
        x = "GDP per Capita",
        y = "Life Expectancy"
      )
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # =======================================================
  # PIE CHART
  # =======================================================
  
  output$pieChart <- renderPlotly({
    
    data_pie <- filtered_data() %>%
      group_by(continent) %>%
      summarise(pop = sum(pop))
    
    plot_ly(
      data_pie,
      labels = ~continent,
      values = ~pop,
      type = "pie"
    )
  })
  
  # =======================================================
  # TREND PLOT
  # =======================================================
  
  output$trendPlot <- renderPlotly({
    
    trend_data <- gapminder
    
    if(input$continent != "All"){
      trend_data <- trend_data %>%
        filter(continent == input$continent)
    }
    
    trend <- trend_data %>%
      group_by(year) %>%
      summarise(
        gdp = mean(gdpPercap)
      )
    
    p <- ggplot(trend, aes(x = year, y = gdp)) +
      geom_line(color = "blue", linewidth = 1.5) +
      geom_point(color = "red", size = 2) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Average GDP"
      )
    
    ggplotly(p)
  })
  
  # =======================================================
  # MAPA
  # =======================================================
  
  output$mapPlot <- renderLeaflet({
    
    data_map <- filtered_data()
    
    leaflet(data_map) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~runif(nrow(data_map), -180, 180),
        lat = ~runif(nrow(data_map), -60, 80),
        radius = ~sqrt(pop)/1000,
        popup = ~paste(
          "<b>Country:</b>", country,
          "<br><b>Life Exp:</b>", lifeExp,
          "<br><b>GDP:</b>", gdpPercap
        ),
        color = "cyan",
        fillOpacity = 0.6
      )
  })
  
  # =======================================================
  # DATA TABLE
  # =======================================================
  
  output$dataTable <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)

# =========================================================
# SUBIR A SHINYAPPS.IO
# =========================================================

# 1. Crear cuenta en:
# https://www.shinyapps.io/

# 2. Ejecutar esto en RStudio:

library(rsconnect)

rsconnect::setAccountInfo(
  name='jeniferaquino',
  token='A6A0BCC0712317ED2EF95A41DE232ED4',
  secret='np9PsM2M7vebGhgpm57Ei7ENEVO9iXrCdpikfeEl'
)

# 3. Luego subir:

rsconnect::deployApp()

# =========================================================
# IMPORTANTE
# =========================================================

# Guarda este archivo como:
# app.R

# Luego abre app.R en RStudio y ejecuta:
# shinyApp(ui, server)

# O simplemente:
# Run App