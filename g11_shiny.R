library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(bslib)
library(shiny)
library(plotly)    
library(viridis)   
library(tidyverse)

car_data <- read.csv('https://uwmadison.box.com/shared/static/e73vopf1jcyi3fwyu6pkb8jp2xnqu3zg.csv')

# Data cleaning for Odometer vs Selling Price by Make graph

car_data_odometer <- car_data %>%
  mutate(
    make = str_to_title(make),
    make = case_when(
      make %in% c("Chev Truck", "Chevrolet") ~ "Chevrolet",
      make %in% c("Ford", "Fort Tk", "Ford Truck") ~ "Ford",
      make %in% c("Dodge", "Dodge Tk") ~ "Dodge",
      make %in% c("Gmc", "Gmc Truck") ~ "GMC",
      make %in% c("Hyundai", "Hyundai Tk") ~ "Hyundai",
      make %in% c("Land Rover", "Landrover") ~ "Land Rover",
      make %in% c("Mazda", "Mazda Tk") ~ "Mazda",
      make %in% c("Mercedes", "Mercedes-B", "Mercedes-Benz") ~ "Mercedes-Benz",
      make %in% c("Volkswagen", "Vw") ~ "Volkswagen",
      TRUE ~ make)) %>%
  group_by(make) %>%
  filter(n() >= 120) %>%
  ungroup() %>%
  filter(
    !is.na(odometer), !is.na(sellingprice),
    is.finite(odometer), is.finite(sellingprice) & make != "") %>%
  filter(between(sellingprice, quantile(sellingprice, 0.05), quantile(sellingprice, 0.95)))

lm_params_odometer <- car_data_odometer %>%
  group_by(make) %>%
  summarise(
    slope = coef(lm(sellingprice ~ odometer))[2],
    intercept = coef(lm(sellingprice ~ odometer))[1]
  )

car_data_odometer_sampled <- car_data_odometer %>%
  group_by(make) %>%
  slice_sample(n = 20) %>%
  ungroup()

car_data_odometer_sampled <- car_data_odometer_sampled %>%
  left_join(lm_params_odometer, by = "make") %>%
  mutate(predicted_price = intercept + slope * odometer)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "lux"
  ),
  
  titlePanel("Car Price Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Car Profitability Analysis'",
        sliderInput("profit_threshold",
                    "Minimum Profitability:",
                    min = 0, max = 5000, value = 1000, step = 100),
        selectInput("make_filter",
                    "Select Car Make:",
                    choices = c("All", unique(tolower(car_data$make))),
                    selected = "All"),
        textInput("model_filter",
                  "Filter by Model (Optional):",
                  value = ""),
        checkboxInput("remove_outliers", "Exclude Outliers (Avg Profit > $30,000)", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.tabs == 'Selling Price VS. Odometer'",
        h4("Hover Information"),
        verbatimTextOutput("hover_info_box") # Display hover info here
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs", 
        
        # Introduction Panel
        tabPanel("Introduction",
                 h4("Welcome to the Car Price Analysis Dashboard"),
                 p("This application allows you to explore car price trends, profitability, and sales data."),
                 p("Use the tabs to navigate between different sections:"),
                 tags$ul(
                   tags$li("Car Profitability Analysis: Analyze which car makes and models are the most profitable."),
                   tags$li("Sales and Prices Map: View an interactive map showing car sales and average prices."),
                   tags$li("Color VS. Price: Explore trends in car prices over time based on car colors."),
                   tags$li("Selling Price VS. Odometer: Examine the depreciation rate of different car brands. The red line depicts how the mileage affects resale value, a steeper line means a greater rate of depreciation.")
                 ),
                 p("Use the filters in the sidebar to customize your analysis.")
        ),
        
        # First Tab: Car Profitability Analysis
        tabPanel("Car Profitability Analysis",
                 h4("Car Profitability Analysis"),
                 plotlyOutput("profitPlot", height = "500px"),
                 tableOutput("profitTable")
        ),
        
        # Second Tab: Sales and Prices Map
        tabPanel("Sales and Prices Map",
                 h4("Interactive Map of Car Sales and Average Prices"),
                 leafletOutput("map", height = "600px")
        ),
        
        # Third Tab: Color VS. Price
        tabPanel("Color VS. Price",
                 h4("Average Price Over Time by Car Color"),
                 plotOutput("colorPricePlot", height = "500px")
        ),
        
        # Fourth Tab: Selling Price VS. Odometer
        tabPanel("Selling Price VS. Odometer",
                 h4("Selling Price VS. Odometer"),
                 plotOutput("priceOdometerPlot", height = "700px", 
                            hover = hoverOpts(id = "plot_hover", delay = 100))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  ## Car Profitability Analysis
  processed_data <- reactive({
    car_data %>%
      filter(!is.na(mmr) & !is.na(sellingprice) & !is.na(make) & !is.na(model)) %>%
      mutate(
        make = tolower(make),
        model = tolower(model),
        mmr = ceiling(mmr),
        sellingprice = ceiling(sellingprice),
        profit = round(sellingprice - mmr, 2)
      )
  })
  
  car_profit_data <- reactive({
    data <- processed_data() %>%
      filter(
        (input$make_filter == "All" | make == input$make_filter),
        grepl(input$model_filter, model, ignore.case = TRUE)
      ) %>%
      group_by(make, model) %>%
      summarize(
        avg_profit = round(mean(profit, na.rm = TRUE), 2),
        total_sales = n()
      )
    
    if (input$remove_outliers) {
      data <- data %>% filter(avg_profit <= 30000)
    }
    
    data %>% filter(avg_profit >= input$profit_threshold)
  })
  
  output$profitPlot <- renderPlotly({
    data <- car_profit_data()
    p <- ggplot(data, aes(
      x = reorder(paste(make, model, sep = " "), avg_profit),
      y = avg_profit,
      text = paste("Make: ", make, "<br>Model: ", model, "<br>Avg Profit: $", avg_profit)
    )) +
      geom_col(fill = "lightgreen") +
      coord_flip() +
      labs(title = "Average Vehicle Profitability",
           x = "Car Make and Model",
           y = "Average Profit (USD)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$profitTable <- renderTable({
    car_profit_data() %>%
      arrange(desc(avg_profit)) %>%
      rename(Make = make, Model = model, `Average Profit` = avg_profit, `Total Sales` = total_sales)
  })
  
   ##Sales and Prices Map
  ###Placeholder
  
  ## Color VS. Price Plot
output$colorPricePlot <- renderPlot({
 ms2_data <- car_data %>%
    filter(color != "—" & !grepl("^[0-9]+$", color) & !is.na(color) & color != "") %>%
    mutate(color = ifelse(color == "lime", NA, color))  # Replace "lime" with NA

ms2_data_avg <- ms2_data %>%
  group_by(year, color) %>%
  summarize(avg_price = mean(sellingprice, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(color)) 

recent_years = max(ms2_data$year) - 2
color_recent_avg = ms2_data %>%
    filter(year > recent_years) %>%
    group_by(color) %>%
    summarize(recent_avg_price = mean(sellingprice, na.rm = TRUE)) %>%
    arrange(desc(recent_avg_price)) %>%
    pull(color)

ms2_data_avg$color <- factor(ms2_data_avg$color, levels = color_recent_avg)

ggplot(ms2_data_avg, aes(x = year, y = avg_price)) +
    geom_line(color = "skyblue") +
    labs(title = "Average Price Over Time by Car Color", x = "Year", y = "Price (USD)") +
    theme_minimal() +
    facet_wrap(~ color) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})


  ## Selling Price VS. Odometer
  output$priceOdometerPlot <- renderPlot({
    ggplot(car_data_odometer_sampled, aes(x = odometer, y = sellingprice)) +
      geom_point(alpha = 0.6, color = "skyblue") +
      geom_abline(aes(slope = slope, intercept = intercept), color = "#F08080", linewidth = 1) +
      facet_wrap(~ make) +
      coord_cartesian(ylim = c(0, NA)) + # Stretch y-axis
      labs(
        title = "Selling Price vs Odometer by Make",
        x = "Odometer (Miles)",
        y = "Selling Price (USD)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
  
  output$hover_info_box <- renderText({
    hover <- input$plot_hover
    
    if (is.null(hover)) {
      return("Hover over a point to see details.")
    }
    
    hover_point <- nearPoints(car_data_odometer_sampled, hover, threshold = 10, maxpoints = 1)
    
    if (nrow(hover_point) == 0) {
      return("No point nearby.")
    }
    
    make <- hover_point$make
    slope <- round(hover_point$slope, 4)
    selling_price <- round(hover_point$sellingprice, 2)
    odometer <- hover_point$odometer
    
    paste0("Make: ", make, "\n",
           "Slope: ", slope, "\n",
           "Selling Price: $", selling_price, "\n",
           "Odometer: ", odometer, " miles")
  })
}


shinyApp(ui = ui, server = server)