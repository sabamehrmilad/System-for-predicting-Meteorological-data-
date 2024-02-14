library(shiny)
library(forecast)
library(prophet)
library(plotly)

ui <- fluidPage(
  titlePanel("Time Series Prediction and Plot Saving"),
  fileInput("precipitation_csv_file", "Upload Precipitation CSV File"),
  fileInput("temperature_csv_file", "Upload Temperature CSV File"),
  actionButton("predict_button", "Predict and Save Plots"),
  plotlyOutput("precipitation_plot"),
  plotlyOutput("temperature_plot")
)

server <- function(input, output, session) {
  precipitation_data <- NULL
  temperature_data <- NULL
  
  observeEvent(input$predict_button, {
    # Check if a CSV file for precipitation is uploaded
    if (is.null(input$precipitation_csv_file)) {
      return(NULL)
    }
    
    # Check if a CSV file for temperature is uploaded
    if (is.null(input$temperature_csv_file)) {
      return(NULL)
    }
    
    # Read the uploaded CSV files
    precipitation_data <- read.csv(input$precipitation_csv_file$datapath)
    temperature_data <- read.csv(input$temperature_csv_file$datapath)
    
    # Assuming your DATE column is named "date" and corresponding columns for y
    colnames(precipitation_data) <- c('ds', 'y')
    colnames(temperature_data) <- c('ds', 'y')
    
    # Convert date columns to Date objects
    precipitation_data$ds <- as.Date(precipitation_data$ds)
    temperature_data$ds <- as.Date(temperature_data$ds)
    
    # Convert temperature data to Celsius
    temperature_data$y <- (temperature_data$y - 32) / 1.8
    
    # Create Prophet objects for both datasets
    m_precip <- prophet()
    m_temp <- prophet()
    
    # Fit the models on the data
    m_precip <- fit.prophet(m_precip, precipitation_data)
    m_temp <- fit.prophet(m_temp, temperature_data)
    
    # Create data frames with future dates for prediction
    future_precip <- make_future_dataframe(m_precip, periods = 730)
    future_temp <- make_future_dataframe(m_temp, periods = 730)
    
    # Make predictions using the trained models
    forecast_precip <- predict(m_precip, future_precip)
    forecast_temp <- predict(m_temp, future_temp)
    
    # Extract forecasted values for the next 730 days
    forecasted_values_precip <- forecast_precip$yhat[1:730]
    forecasted_values_temp <- forecast_temp$yhat[1:730]
    
    # Create a data frame with the forecasted values and corresponding dates
    forecasted_data <- data.frame(
      ds = seq(as.Date(tail(precipitation_data$ds, 1)), by = "day", length.out = 730),
      precipitation = forecasted_values_precip,
      temperature = forecasted_values_temp
    )
    
    
    
    # Get the name of the uploaded CSV files
    precip_file_name <- tools::file_path_sans_ext(input$precipitation_csv_file$name)
    temp_file_name <- tools::file_path_sans_ext(input$temperature_csv_file$name)
    
    # Generate the HTML and image file names based on the uploaded file names
    precip_html_file_name <- paste0(precip_file_name, "_precipitation_prediction_plot.html")
    precip_image_file_name <- paste0(precip_file_name, "_precipitation_prediction_plot.png")
    
    temp_html_file_name <- paste0(temp_file_name, "_temperature_prediction_plot.html")
    temp_image_file_name <- paste0(temp_file_name, "_temperature_prediction_plot.png")
    
    # Return the plotly objects for precipitation and temperature
    output$precipitation_plot <- renderPlotly({
      forecast_plotly <- plot_ly(data = forecast_precip, x = ~ds)
      forecast_plotly <- forecast_plotly %>% 
        add_trace(y = ~yhat, name = 'Precipitation Forecast', type = 'scatter', mode = 'lines')
      
      # Add the yhat_lower and yhat_upper bands
      forecast_plotly <- forecast_plotly %>% 
        add_trace(y = ~yhat_lower, name = 'kuraklik', fill = 'tozeroy', type = 'scatter', mode = 'none')
      forecast_plotly <- forecast_plotly %>% 
        add_trace(y = ~yhat_upper, name = 'nemlilik', fill = 'tonexty', type = 'scatter', mode = 'none')
      
      # Specify the directory path where you want to save the HTML file
      precip_html_file_path <- "F:/Xampp/htdocs/DroughtManagementSystem/public/images/"
      precip_html_file_name <- paste0(precip_file_name, "_precipitation_prediction_plot.html")
      
      
      # Save the plotly object as an HTML file in the specified directory
      htmlwidgets::saveWidget(forecast_plotly, file.path(precip_html_file_path, precip_html_file_name))
      
      
      forecast_plotly
    })
    
    output$temperature_plot <- renderPlotly({
      forecast_plotly <- plot_ly(data = forecast_temp, x = ~ds)
      forecast_plotly <- forecast_plotly %>% 
        add_trace(y = ~yhat, name = 'Temperature Forecast', type = 'scatter', mode = 'lines')
      
      # Add the yhat_lower and yhat_upper bands
      forecast_plotly <- forecast_plotly %>% 
        add_trace(y = ~yhat_lower, name = 'Lower Bound', fill = 'tozeroy', type = 'scatter', mode = 'none')
      forecast_plotly <- forecast_plotly %>% 
        add_trace(y = ~yhat_upper, name = 'Upper Bound', fill = 'tonexty', type = 'scatter', mode = 'none')
      
      # Specify the directory path where you want to save the HTML file
      temp_html_file_path <- "F:/Xampp/htdocs/DroughtManagementSystem/public/images/"
      temp_html_file_name <- paste0(temp_file_name, "_temperature_prediction_plot.html")
      
      
      # Save the plotly object as an HTML file in the specified directory
      htmlwidgets::saveWidget(forecast_plotly, file.path(temp_html_file_path, temp_html_file_name))
      
      
      
      forecast_plotly
    })
  })
}

shinyApp(ui, server)