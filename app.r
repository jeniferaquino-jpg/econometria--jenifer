# =========================================================
# DASHBOARD GAPMINDER FINAL - VERSION COMPLETA Y FUNCIONAL
# =========================================================

# =========================================================
# INSTALAR PAQUETES (EJECUTAR SOLO UNA VEZ)
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
  "forecast",
  "viridis"
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
library(forecast)
library(viridis)

# =========================================================
# BASE DE DATOS
# =========================================================

data <- gapminder

# =========================================================
# UI
# =========================================================

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Gapminder Dashboard Pro"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Mapa",
        tabName = "mapa",
        icon = icon("globe")
      ),
      
      menuItem(
        "Proyecciones",
        tabName = "forecast",
        icon = icon("chart-area")
      ),
      
      menuItem(
        "Regresión",
        tabName = "regresion",
        icon = icon("project-diagram")
      ),
      
      menuItem(
        "Datos",
        tabName = "datos",
        icon = icon("table")
      ),
      
      br(),
      
      selectInput(
        "continent",
        "Seleccionar Continente",
        choices = c("All", unique(data$continent)),
        selected = "All"
      ),
      
      sliderInput(
        "year",
        "Seleccionar Año",
        min = min(data$year),
        max = max(data$year),
        value = 2007,
        step = 5,
        sep = ""
      ),
      
      selectInput(
        "country",
        "Seleccionar País",
        choices = unique(data$country),
        selected = "Peru"
      )
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # =========================================================
      # DASHBOARD
      # =========================================================
      
      tabItem(
        
        tabName = "dashboard",
        
        fluidRow(
          
          valueBoxOutput("countriesBox"),
          valueBoxOutput("lifeBox"),
          valueBoxOutput("gdpBox")
        ),
        
        fluidRow(
          
          box(
            
            width = 8,
            
            title = "PIB vs Esperanza de Vida",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            plotlyOutput("scatterPlot", height = 500)
          ),
          
          box(
            
            width = 4,
            
            title = "Población por Continente",
            
            status = "success",
            
            solidHeader = TRUE,
            
            plotlyOutput("piePlot", height = 500)
          )
        ),
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Evolución Histórica",
            
            status = "warning",
            
            solidHeader = TRUE,
            
            plotlyOutput("linePlot", height = 500)
          )
        )
      ),
      
      # =========================================================
      # MAPA
      # =========================================================
      
      tabItem(
        
        tabName = "mapa",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Mapa Interactivo",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            leafletOutput("map", height = 700)
          )
        )
      ),
      
      # =========================================================
      # FORECAST
      # =========================================================
      
      tabItem(
        
        tabName = "forecast",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Proyección de Esperanza de Vida",
            
            status = "danger",
            
            solidHeader = TRUE,
            
            plotlyOutput("forecastPlot", height = 600)
          )
        )
      ),
      
      # =========================================================
      # REGRESION
      # =========================================================
      
      tabItem(
        
        tabName = "regresion",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Regresión Lineal",
            
            status = "success",
            
            solidHeader = TRUE,
            
            plotlyOutput("regressionPlot", height = 600)
          )
        )
      ),
      
      # =========================================================
      # TABLA
      # =========================================================
      
      tabItem(
        
        tabName = "datos",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Base de Datos",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            DTOutput("table")
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
  
  # =========================================================
  # FILTROS
  # =========================================================
  
  filtered_data <- reactive({
    
    df <- data %>%
      filter(year == input$year)
    
    if(input$continent != "All"){
      
      df <- df %>%
        filter(continent == input$continent)
    }
    
    df
  })
  
  # =========================================================
  # VALUE BOXES
  # =========================================================
  
  output$countriesBox <- renderValueBox({
    
    valueBox(
      
      value = nrow(filtered_data()),
      
      subtitle = "Número de Países",
      
      icon = icon("flag"),
      
      color = "blue"
    )
  })
  
  output$lifeBox <- renderValueBox({
    
    valueBox(
      
      value = round(mean(filtered_data()$lifeExp),1),
      
      subtitle = "Esperanza de Vida",
      
      icon = icon("heartbeat"),
      
      color = "green"
    )
  })
  
  output$gdpBox <- renderValueBox({
    
    valueBox(
      
      value = round(mean(filtered_data()$gdpPercap),0),
      
      subtitle = "PIB per cápita",
      
      icon = icon("dollar-sign"),
      
      color = "yellow"
    )
  })
  
  # =========================================================
  # GRAFICO SCATTER
  # =========================================================
  
  output$scatterPlot <- renderPlotly({
    
    p <- ggplot(
      
      filtered_data(),
      
      aes(
        
        x = gdpPercap,
        y = lifeExp,
        color = continent,
        size = pop,
        text = country
      )
      
    ) +
      
      geom_point(alpha = 0.8) +
      
      scale_x_log10() +
      
      theme_minimal() +
      
      labs(
        
        x = "PIB per cápita",
        y = "Esperanza de Vida"
      )
    
    ggplotly(p, tooltip = c("text","x","y"))
  })
  
  # =========================================================
  # PIE CHART
  # =========================================================
  
  output$piePlot <- renderPlotly({
    
    pie_data <- filtered_data() %>%
      
      group_by(continent) %>%
      
      summarise(pop = sum(pop))
    
    plot_ly(
      
      pie_data,
      
      labels = ~continent,
      
      values = ~pop,
      
      type = "pie"
    )
  })
  
  # =========================================================
  # GRAFICO DE TENDENCIA
  # =========================================================
  
  output$linePlot <- renderPlotly({
    
    line_data <- data %>%
      
      group_by(year, continent) %>%
      
      summarise(
        
        lifeExp = mean(lifeExp),
        
        .groups = "drop"
      )
    
    p <- ggplot(
      
      line_data,
      
      aes(
        
        x = year,
        y = lifeExp,
        color = continent
      )
      
    ) +
      
      geom_line(linewidth = 1.5) +
      
      geom_point(size = 2) +
      
      theme_minimal() +
      
      labs(
        
        title = "Tendencia de Esperanza de Vida",
        
        x = "Año",
        
        y = "Esperanza de Vida"
      )
    
    ggplotly(p)
  })
  
  # =========================================================
  # MAPA INTERACTIVO
  # =========================================================
  
  output$map <- renderLeaflet({
    
    leaflet(data = filtered_data()) %>%
      
      addTiles() %>%
      
      addCircleMarkers(
        
        lng = ~gdpPercap/300,
        
        lat = ~lifeExp,
        
        radius = ~sqrt(pop)/1000,
        
        color = "blue",
        
        fillOpacity = 0.7,
        
        popup = ~paste(
          
          "<b>País:</b>", country,
          
          "<br>",
          
          "<b>Continente:</b>", continent,
          
          "<br>",
          
          "<b>Esperanza de Vida:</b>", round(lifeExp,1),
          
          "<br>",
          
          "<b>PIB:</b>", round(gdpPercap,0)
        )
      )
  })
  
  # =========================================================
  # PROYECCION
  # =========================================================
  
  output$forecastPlot <- renderPlotly({
    
    country_data <- data %>%
      
      filter(country == input$country)
    
    ts_data <- ts(country_data$lifeExp)
    
    fit <- auto.arima(ts_data)
    
    pred <- forecast(fit, h = 5)
    
    future_years <- seq(
      
      max(country_data$year) + 5,
      
      by = 5,
      
      length.out = 5
    )
    
    forecast_df <- data.frame(
      
      year = future_years,
      
      forecast = as.numeric(pred$mean)
    )
    
    p <- ggplot() +
      
      geom_line(
        
        data = country_data,
        
        aes(x = year, y = lifeExp),
        
        linewidth = 1.5,
        
        color = "blue"
      ) +
      
      geom_line(
        
        data = forecast_df,
        
        aes(x = year, y = forecast),
        
        linewidth = 1.5,
        
        color = "red"
      ) +
      
      geom_point(
        
        data = forecast_df,
        
        aes(x = year, y = forecast),
        
        size = 3,
        
        color = "red"
      ) +
      
      theme_minimal() +
      
      labs(
        
        title = paste("Forecast:", input$country),
        
        x = "Año",
        
        y = "Esperanza de Vida"
      )
    
    ggplotly(p)
  })
  
  # =========================================================
  # REGRESION
  # =========================================================
  
  output$regressionPlot <- renderPlotly({
    
    model <- lm(
      
      lifeExp ~ gdpPercap,
      
      data = filtered_data()
    )
    
    p <- ggplot(
      
      filtered_data(),
      
      aes(
        
        x = gdpPercap,
        
        y = lifeExp
      )
      
    ) +
      
      geom_point(
        
        color = "blue",
        
        alpha = 0.7
      ) +
      
      geom_smooth(
        
        method = "lm",
        
        color = "red",
        
        se = TRUE
      ) +
      
      scale_x_log10() +
      
      theme_minimal() +
      
      labs(
        
        title = "Relación entre PIB y Esperanza de Vida",
        
        subtitle = paste(
          
          "R² =",
          
          round(summary(model)$r.squared,2)
        ),
        
        x = "PIB per cápita",
        
        y = "Esperanza de Vida"
      )
    
    ggplotly(p)
  })
  
  # =========================================================
  # TABLA
  # =========================================================
  
  output$table <- renderDT({
    
    datatable(
      
      filtered_data(),
      
      options = list(pageLength = 10)
    )
  })
}

# =========================================================
# EJECUTAR APP
# =========================================================

shinyApp(ui = ui, server = server)
