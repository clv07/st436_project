### FUNCTIONING FINAL?

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(maps)
library(DT)

############################################################
# SECTION 1: DATA PREPARATION
############################################################

# Function to load and prepare car data
prepare_car_data <- function() {
  # Brand standardization mapping
  brand_map <- c(
    "kia" = "Kia", "bmw" = "BMW", "volvo" = "Volvo", "nissan" = "Nissan", "chevrolet" = "Chevrolet",
    "audi" = "Audi", "ford" = "Ford", "hyundai" = "Hyundai", "buick" = "Buick", "cadillac" = "Cadillac",
    "acura" = "Acura", "lexus" = "Lexus", "infiniti" = "Infiniti", "jeep" = "Jeep", "mercedes-benz" = "Mercedes-Benz",
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
  
  # Load the car data from a CSV file
  car_data <- read_csv("https://uwmadison.box.com/shared/static/m1fc9k18oy62to1bjuslsjli0kub5xrl.csv", show_col_types = FALSE)
  
  # Standardize brand names and handle NA values
  car_data$make <- tolower(car_data$make) # Ensure case-insensitivity
  car_data$make <- ifelse(is.na(brand_map[car_data$make]), car_data$make, brand_map[car_data$make]) # Apply map, keep original if NA
  
  # Optionally remove rows with NA in 'make' if they still exist
  car_data <- na.omit(car_data)
  
  return(car_data)
}

# Function to load and prepare US state map data
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

# Load Data
car_data <- prepare_car_data()
states_sf <- prepare_state_map()

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

############################################################
# Section: Shiny App with Additional Filters
############################################################

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Total Number of Sales and Average Vehicle Price by U.S. State"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Use the filters below to explore car sales and average prices across the United States."),
      sliderInput("yearRange", "Select Year Range:",
                  min = min(car_data$year, na.rm = TRUE),
                  max = max(car_data$year, na.rm = TRUE),
                  value = c(min(car_data$year, na.rm = TRUE), max(car_data$year, na.rm = TRUE))),
      helpText("Adjust the year range to focus on cars sold within specific years."),
      sliderInput("odometerRange", "Select Odometer Range:",
                  min = min(car_data$odometer, na.rm = TRUE),
                  max = max(car_data$odometer, na.rm = TRUE),
                  value = c(min(car_data$odometer, na.rm = TRUE), max(car_data$odometer, na.rm = TRUE))),
      helpText("Use the odometer range to filter cars based on mileage. Set the range from low to high miles."),
      selectInput("make", "Select Vehicle Make:", 
                  choices = unique(car_data$make), 
                  selected = "Ford", multiple = TRUE),
      helpText("Select one or more car makes to view their average prices and sales data.")
    ),
    mainPanel(
      h4("Interactive Map of Car Sales and Average Prices"),
      helpText("Click on a state to view details on total sales, average selling price, top make and top model.
               Use the filters to adjust the data displayed on the map."),
      leafletOutput("map", height = "600px"),
      DTOutput("dataTable")
    )
  )
)

# Define the Shiny server
server <- function(input, output, session) {
  # Function to convert state abbreviations to full state names
  convert_to_DT <- function(data) {
    data$state <- sapply(data$state, function(abbr) {
      if (tolower(abbr) %in% names(state_map)) {
        state_map[[tolower(abbr)]]
      } else {
        NA  # Return NA if no matching abbreviation is found
      }
    })
    
    # Select the necessary columns after updating the state names
    data <- data %>%
      select(make, model, state, odometer, color, sellingprice)
    
    return(data)
  }
  
  # Reactive data filtered by year, odometer, and make
  filtered_data <- reactive({
    car_data %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2],
             odometer >= input$odometerRange[1] & odometer <= input$odometerRange[2],
             make %in% input$make)
  })
  
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
  
  # Render the map, centered on the USA
  output$map <- renderLeaflet({
    leaflet(data = states_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4)  # Center on the USA
  })
  
  # Update the map with filtered data
  observe({
    # Join filtered data with spatial data and remove states with no sales data
    state_data <- summarize_data(filtered_data()) %>%
      inner_join(states_sf, by = "state") %>%
      st_as_sf()
    
    # Update polygons and legend with filtered data
    leafletProxy("map", data = state_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~colorBin("YlGnBu", state_data$avg_price, bins = 5)(avg_price),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5, color = "#666", fillOpacity = 0.7, bringToFront = TRUE
        ),
        popup = ~paste(
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
  
  # Render the data table
  output$dataTable <- renderDT({
    datatable(
      convert_to_DT(filtered_data()),
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)