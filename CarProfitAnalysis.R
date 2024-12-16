library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

car_data <- read.csv('https://uwmadison.box.com/shared/static/e73vopf1jcyi3fwyu6pkb8jp2xnqu3zg.csv')

ui <- fluidPage(
  titlePanel("Car Profitability Analysis"),
  fluidRow(
    column(12, 
           p("This app analyzes car profitability based on data from multiple car makes and models.
              It uses average market price of vehicles to determine which makes & models tend to 
              have higher selling prices than their market value, indicating higher chances of profitability
              for sellers.
              Use the controls in the sidebar to filter the data based on profitability thresholds, 
              car make, and model. You can also exclude outliers with extremely high profitability."),
           p("Use the controls in the sidebar to filter the data based on profitability thresholds, 
              car make, and model. You can also exclude outliers with extremely high profitability.
              The bar chart visualizes average profitability for each make and model, while the table 
              provides detailed numerical data.")
    )
  ),
  sidebarLayout(
    sidebarPanel(
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
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotlyOutput("profitPlot", height = "500px")),
        tabPanel("Table", tableOutput("profitTable"))
      )
    )
  )
)

server <- function(input, output) {
  
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
        avg_profit = round(mean(profit, na.rm = TRUE),2),
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
      y = round(avg_profit,2),
      text = paste("Make: ", make, "<br>Model: ", model, "<br>Avg Profit: $", avg_profit)
    )) +
      geom_col(fill = "green") +
      coord_flip() +
      labs(title = "Average Vehicle Profitability",
           x = "Car Make and Model",
           y = "Average Profit ($)") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 7, hjust = 1),
        plot.title = element_text(size = 12, face = "bold", hjust = 0, margin = margin(b = 10)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$profitTable <- renderTable({
    car_profit_data() %>%
      arrange(desc(avg_profit)) %>%
      rename(Make = make, Model = model, `Average Profit` = avg_profit, `Total Sales` = total_sales)
  })
}

shinyApp(ui = ui, server = server)





