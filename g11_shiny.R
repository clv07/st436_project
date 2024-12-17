library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(bslib)
library(shiny)
library(plotly)    
library(viridis)   
library(tidyverse)
library(maps)
library(sf)
library(DT)

############################################################
# SECTION 1: DATA PREPARATION
############################################################

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

state_map <- c(
  "al" = "Alabama", "ak" = "Alaska", "az" = "Arizona", "ar" = "Arkansas", "ca" = "California",
  "co" = "Colorado", "ct" = "Connecticut", "de" = "Delaware", "fl" = "Florida", "ga" = "Georgia",
  "hi" = "Hawaii", "id" = "Idaho", "il" = "Illinois", "in" = "Indiana", "ia" = "Iowa",
  "ks" = "Kansas", "ky" = "Kentucky", "la" = "Louisiana", "me" = "Maine", "md" = "Maryland",
  "ma" = "Massachusetts", "mi" = "Michigan", "mn" = "Minnesota", "ms" = "Mississippi", "mo" = "Missouri",
  "mt" = "Montana", "ne" = "Nebraska", "nv" = "Nevada", "nh" = "New Hampshire", "nj" = "New Jersey",
  "nm" = "New Mexico", "ny" = "New York", "nc" = "North Carolina", "nd" = "North Dakota", "oh" = "Ohio",
  "ok" = "Oklahoma", "or" = "Oregon", "pa" = "Pennsylvania", "ri" = "Rhode Island", "sc" = "South Carolina",
  "sd" = "South Dakota", "tn" = "Tennessee", "tx" = "Texas", "ut" = "Utah", "vt" = "Vermont",
  "va" = "Virginia", "wa" = "Washington", "wv" = "West Virginia", "wi" = "Wisconsin", "wy" = "Wyoming",
  "dc" = "District of Columbia"
)

### Helper functions
# Function to load and prepare car data
prepare_car_data <- function(data) {
  # Brand standardization mapping
  brand_map <- c(
    "kia" = "Kia", "bmw" = "BMW", "volvo" = "Volvo", "nissan" = "Nissan", "chevrolet" = "Chevrolet",
    "audi" = "Audi", "ford" = "Ford", "hyundai" = "Hyundai", "buick" = "Buick", "cadillac" = "Cadillac",
    "acura" = "Acura", "lexus" = "Lexus", "infiniti" = "Infiniti", "jeep" = "Jeep", "mercedes-benz" = "Mercedes-Benz",
    "mercedes" = "Mercedes-Benz", "mercedes-b" = "Mercedes-Benz",
    "mitsubishi" = "Mitsubishi", "mazda" = "Mazda", "mini" = "MINI", "land rover" = "Land Rover", "lincoln" = "Lincoln",
    "jaguar" = "Jaguar", "volkswagen" = "Volkswagen", "vw" = "Volkswagen", "toyota" = "Toyota", "subaru" = "Subaru",
    "scion" = "Scion", "porsche" = "Porsche", "dodge" = "Dodge", "fiat" = "FIAT", "chrysler" = "Chrysler",
    "ferrari" = "Ferrari", "honda" = "Honda", "gmc" = "GMC", "ram" = "Ram", "smart" = "Smart",
    "bentley" = "Bentley", "pontiac" = "Pontiac", "saturn" = "Saturn", "maserati" = "Maserati", "mercury" = "Mercury",
    "hummer" = "HUMMER", "landrover" = "Land Rover", "gmc truck" = "GMC", "rolls-royce" = "Rolls-Royce",
    "oldsmobile" = "Oldsmobile", "isuzu" = "Isuzu", "geo" = "Geo", "daewoo" = "Daewoo", "plymouth" = "Plymouth",
    "tesla" = "Tesla", "airstream" = "Airstream", "dot" = "DOT", "aston martin" = "Aston Martin", "fisker" = "Fisker",
    "lamborghini" = "Lamborghini", "lotus" = "Lotus", "dodge tk" = "Dodge", "chev truck" = "Chevrolet", "ford tk" = "Ford",
    "ford truck" = "Ford", "saab" = "Saab", "suzuki" = "Suzuki"
  )

  # Load data
  car_data <- data
 
  # Standardize brand names and handle NA values
  car_data$make <- tolower(car_data$make) # Ensure case-insensitivity
  car_data$make <- ifelse(is.na(brand_map[car_data$make]), car_data$make, brand_map[car_data$make]) # Apply map, keep original if NA

  # Optionally remove rows with NA in 'make' if they still exist
  car_data <- na.omit(car_data)

  return(car_data)
}

# Load and prepare US state map data
prepare_state_map <- function() {
  states_sf <- map_data("state") %>%
    rename(state = region) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(state) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("MULTILINESTRING")
  
  # Add state abbreviations
  state_abbr <- setNames(state.abb, tolower(state.name))
  states_sf <- states_sf %>%
    mutate(state = tolower(state_abbr[state]))
  return(states_sf)
}

# Convert state abbreviations to full state names
convert_to_DT <- function(data) {
  data$state <- sapply(data$state, function(abbr) {
    if (tolower(abbr) %in% names(state_map)) {
      state_map[[tolower(abbr)]]
    } else {
      NA # Return NA if no matching abbreviation is found
    }
  })

  # Select the necessary columns after updating the state names
  data <- data %>%
    select(make, model, state, odometer, color, sellingprice)

  return(data)
}

summarize_data <- function(data) {
  data %>%
    group_by(state) %>%
    summarise(
      total_sales = n(),
      avg_price = mean(sellingprice, na.rm = TRUE),
      top_make = if_else(length(names(which.max(table(make)))) > 0, names(which.max(table(make)))[1], NA_character_),
      top_model = if_else(length(names(which.max(table(model)))) > 0, names(which.max(table(model)))[1], NA_character_)
    )
}

state_map_car_data <- prepare_car_data(car_data)
states_sf <- prepare_state_map()

############################################################
# SECTION 2: UI for Shiny App
############################################################

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "lux"
  ),
  
  titlePanel("Car Price Analysis Dashboard"),
  
  # Conditional filter based on tab
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Car Profitability Analysis'",
        sliderInput("profit_threshold",
                    "Minimum Profitability:",
                    min = 0, max = 5000, value = 1000, step = 100),
        selectInput("make_filter",
                    "Select Car Make:",
                    choices = c("All", sort(unique(tolower(car_data$make)))),
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
      ),
       conditionalPanel(
        condition = "input.tabs == 'Sales and Prices Map'",
        helpText("Use the filters below to explore car sales and average prices across the United States."),
        sliderInput("yearRange", "Select Year Range:",
                    min = min(state_map_car_data$year, na.rm = TRUE),
                    max = max(state_map_car_data$year, na.rm = TRUE),
                    value = c(min(state_map_car_data$year, na.rm = TRUE), max(car_data$year, na.rm = TRUE))),
        helpText("Adjust the year range to focus on cars sold within specific years."),
        sliderInput("odometerRange", "Select Odometer Range:",
                    min = min(state_map_car_data$odometer, na.rm = TRUE),
                    max = max(state_map_car_data$odometer, na.rm = TRUE),
                    value = c(min(state_map_car_data$odometer, na.rm = TRUE), max(car_data$odometer, na.rm = TRUE))),
        helpText("Use the odometer range to filter cars based on mileage. Set the range from low to high miles."),
        selectInput("make", "Select Vehicle Make:", 
                    choices = sort(unique(state_map_car_data$make)), 
                    selected = "Ford", multiple = TRUE),
        helpText("Select one or more car makes to view their average prices and sales data.")
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
                 plotlyOutput("profitPlot", height = "750px"),
                 tableOutput("profitTable")
        ),
        
        # Second Tab: Sales and Prices Map
        tabPanel("Sales and Prices Map",
                 h4("Interactive Map of Car Sales and Average Prices"),
                 helpText("Begin adjusting the slider to view the map data."),
                 helpText("Click to select a state to view detailed information."),
                 leafletOutput("map", height = "600px"),
                 DTOutput("dataTable")
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

############################################################
# SECTION 3: Server for Shiny App
############################################################

server <- function(input, output, session) {
  
  ## Car Profitability Analysis
  ### Reactive Elements
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
  
  ### Bar plot
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
  
  ### Table
  output$profitTable <- renderTable({
    car_profit_data() %>%
      arrange(desc(avg_profit)) %>%
      rename(Make = make, Model = model, `Average Profit` = avg_profit, `Total Sales` = total_sales)
  })
  
  ## Sales and Prices Map
  ### Reactive elements
  sales_price_map <- reactive({
    state_map_car_data %>%
      filter(
        year >= input$yearRange[1] & year <= input$yearRange[2],
        odometer >= input$odometerRange[1] & odometer <= input$odometerRange[2],
        make %in% input$make
      )
  })

  # Update the map with filtered data
  observe({
    # Join filtered data with spatial data and remove states with no sales data
    state_data <- summarize_data(sales_price_map()) %>%
      inner_join(states_sf, by = "state") %>%
      st_as_sf()

    # Update polygons and legend with filtered data
    leafletProxy("map", data = state_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~ colorBin("YlGnBu", state_data$avg_price, bins = 5)(avg_price),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5, color = "#666", fillOpacity = 0.7, bringToFront = TRUE
        ),
        popup = ~ paste(
          "<b>State:</b>", state_map[state],
          "<br><b>Total Sales:</b>", total_sales,
          "<br><b>Avg Price:</b> $", formatC(avg_price, format = "f", digits = 2),
          "<br><b>Top Make:</b>", top_make,
          "<br><b>Top Model:</b>", top_model
        )
      ) %>%
      addLegend(
        pal = colorBin("YlGnBu", state_data$avg_price, bins = 5),
        values = ~avg_price,
        opacity = 0.7,
        title = "Avg Price (USD $)",
        position = "bottomright"
      )
  })
  
  # Map centered on the USA
  output$map <- renderLeaflet({
    leaflet(data = states_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4)  # Center on the USA
  })

  
  # Table
  output$dataTable <- renderDT({
    datatable(
      convert_to_DT(sales_price_map()),
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  ## Color VS. Price Plot
  output$colorPricePlot <- renderPlot({
    ms2_data <- car_data %>%
      filter(color != "—" & !grepl("^[0-9]+$", color) & !is.na(color) & color != "") %>%
      mutate(color = ifelse(color == "lime", NA, color)) # Replace "lime" with NA

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
      facet_wrap(~color) +
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