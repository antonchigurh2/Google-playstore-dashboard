# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinycssloaders)
library(dplyr)

# Load the Google Play Store dataset (assuming it's stored as "googleplaystore.csv")
google_play_data <- read.csv("googleplaystore.csv")

# Preprocess the 'androidVersion' column
google_play_data$Android_Version <- as.numeric(gsub("[^0-9]", "", google_play_data$`androidVersion_updated`))

# Define the UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Google Play Store App Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary"),
      menuItem("Visuals", tabName = "visuals"),
      menuItem("Data", tabName = "data"),
      valueBox(round(mean(google_play_data$Rating, na.rm = TRUE), 1), "Average Rating", color = 'light-blue', icon = icon("star"), width = 12)
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(
                valueBox('Google Play Store', "Full Insights Dashboard", color = 'light-blue', icon = icon("google"), width = 4),
                valueBox(nrow(google_play_data), "Total Apps", color = 'yellow', icon = icon("mobile"), width = 2),
                valueBox(sum(google_play_data$Type == "Free"), "Free Apps", color = 'green', icon = icon("dollar"), width = 3),
                valueBox(sum(google_play_data$Type == "Paid"), "Paid Apps", color = 'red', icon = icon("dollar-sign"), width = 3),
                
              ),
              box(
                title = "Dashboard Overview",
                solidHeader = TRUE,
                background = "blue",
                width = 6,
                textOutput("dashboard_info")
              ),
              
              box(
                title = "How to use:",
                solidHeader = TRUE,
                background = "blue",
                width = 6,
                textOutput("usage")
              ),
              
              fluidRow(
                box(
                  title = "Summary of Ratings",
                  solidHeader = TRUE,
                  
                  # Change background color
                  border = "2px solid #ccc",     # Add border
                  padding = "15px",              # Add padding
                  boxShadow = "0px 2px 4px #888",# Add box shadow
                  shinycssloaders::withSpinner(plotlyOutput("rating_summary"))
                ),
                
                box(title = "Select Android Version", width = 3, solidHeader = TRUE,
                      selectInput("android_version", "Android Version",
                                  choices = c("All", sort(unique(google_play_data$Android_Version)))), hr()),
                box(title = "Number of Compatible Apps", width = 3, solidHeader = TRUE, verbatimTextOutput("app_count")),
              
                fluidRow(
                infoBox(
                  "Games",
                  verbatimTextOutput("most_installed_game"),
                  icon = icon("gamepad"),
                  width = 6,
                  color = "purple"  # Customize color
                ),
                infoBox(
                  "Social",
                  verbatimTextOutput("most_installed_social"),
                  icon = icon("users"),
                  width = 6,
                  color = "blue"  # Customize color
                ),
                infoBox(
                  "Tools",
                  verbatimTextOutput("most_installed_tools"),
                  icon = icon("wrench"),
                  width = 6,
                  color = "green"  # Customize color
                )
              )
              )
      ),
      tabItem(tabName = "visuals",
              fluidRow(
                box(title = "Rating Distribution", width = 6, solidHeader = TRUE, shinycssloaders::withSpinner(plotlyOutput("rating_distribution"))),
                box(title = "Content Rating Distribution", width = 6, solidHeader = TRUE, shinycssloaders::withSpinner(plotlyOutput("content_rating_distribution"))),
                box(title = "Price Distribution for Paid Apps", width = 6, solidHeader = TRUE,
                    shinycssloaders::withSpinner(plotlyOutput("price_distribution_paid_apps")),
                    sliderInput("price_bin_size", "Bin Size", min = 1, max = 50, value = 10)
                ),
              
                box(title = "App Size Distribution (Histogram)", width = 6, solidHeader = TRUE,
                    plotOutput("size_distribution"),
                    sliderInput("size_bin_size", "Bin Size (MB)", min = 1, max = 10, value = 1)
                )
              )
      ),
      
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Filter Data", width = 4, solidHeader = TRUE,
                    sliderInput("rating", "Select Rating", min = 0, max = 5, value = c(0, 5), step = 0.1),
                    selectInput("category", "Select Category", choices = c("All", sort(unique(google_play_data$Category)))),
                    selectInput("type", "Select Type", choices = c("All", "Free", "Paid")),
                    hr()
                ),
                
                box(title = "Google Play Store Apps", width = 8, solidHeader = TRUE,
                    shinycssloaders::withSpinner(dataTableOutput("play_store_data"))),
                box(title = "Category-wise Number of Apps", width = 12, solidHeader = TRUE, shinycssloaders::withSpinner(plotlyOutput("category_app_count")))
              )
      )
    )
  )
)
# Define the server
server <- function(input, output) {
  
  # Define the general information output
  output$dashboard_info <- renderText({
    "
      Welcome to the Google Play Store App Analysis Dashboard! Explore insights into 
      millions of apps across categories. Understand trends, distributions, 
      and user preferences. Perfect for developers, marketers, and enthusiasts.

    "
  })
  

  
  output$usage <- renderText({
    "
    How to Use:
    Summary Tab: Get key metrics and insights.
    Visuals Tab: Explore interactive visualizations.
    Data Tab: Filter and explore by rating, category, and type.
    Start exploring Google Play Store apps today!"
  })
  
  # Summary of ratings
  output$rating_summary <- renderPlotly({
    ggplotly(
      ggplot(google_play_data, aes(x = Rating)) +
        geom_histogram(fill = "steelblue", alpha = 0.8, bins = 20) +
        labs(title = "Summary of Ratings", x = "Rating", y = "Frequency") +
        xlim(0, 5)
    )
  })
  
  # Rating distribution
  output$rating_distribution <- renderPlotly({
    ggplotly(
      ggplot(google_play_data, aes(x = Rating)) +
        geom_density(fill = "steelblue", alpha = 0.8) +
        labs(title = "Rating Distribution", x = "Rating", y = "Density") +
        xlim(0, 5)
    )
  })
  
  # Content rating distribution
  output$content_rating_distribution <- renderPlotly({
    # Filter out rows with missing values in "Content Rating" column
    google_play_data_filtered <- google_play_data[!is.na(google_play_data$`Content.Rating`), ]
    
    # Count the occurrences of each content rating category
    content_rating_counts <- google_play_data_filtered %>%
      count(`Content.Rating`, sort = TRUE)
    
    # Create a data frame with content rating labels and their counts
    content_rating_labels <- c("Everyone", "Teen", "Everyone 10+", "Mature 17+", "Adults only 18+", "Unrated")
    content_rating_data <- data.frame(Content_Rating = content_rating_labels,
                                      Count = ifelse(content_rating_labels %in% content_rating_counts$`Content.Rating`,
                                                     content_rating_counts$n, 0))
    
    # Generate the pie chart
    plot_ly(content_rating_data, labels = ~Content_Rating, values = ~Count, type = "pie") %>%
      layout(title = "Content Rating Distribution")
  })
  
  # Most installed game
  output$most_installed_game <- renderText({
    # Filter the dataset for games
    game_data <- google_play_data[google_play_data$Category == "GAME", ]
    
    # Find the game with the most installs
    most_installed_game <- game_data[which.max(game_data$No..of.installs), "App"]
    
    # Create the output string
    output_string <- paste("Most Installed Game: ", most_installed_game)
    
    # Return the output string
    return(output_string)
  })
  
  # Price distribution for paid apps
  output$price_distribution_paid_apps <- renderPlotly({
    paid_apps <- google_play_data[google_play_data$Type == "Paid", ]
    
    # Convert Price column to numeric
    paid_apps$Price <- as.numeric(gsub("[$]", "", paid_apps$Price))
    
    # Define bin sizes for the histogram
    price_min <- min(paid_apps$Price, na.rm = TRUE)
    price_max <- max(paid_apps$Price, na.rm = TRUE)
    bin_width <- as.numeric(input$price_bin_size)
    num_bins <- ceiling((price_max - price_min) / bin_width)
    
    # Create a histogram using plotly
    plot_ly(paid_apps, x = ~Price, type = "histogram",
            marker = list(color = "#F6B959", line = list(width = 1)), # Adjust line width
            autobinx = FALSE,
            xbins = list(start = price_min, end = price_max, size = bin_width,
                         startend = TRUE)) %>%
      layout(title = "Price Distribution for Paid Apps",
             xaxis = list(title = "Price", tickmode = "array", tickvals = seq(price_min, price_max, by = bin_width), ticktext = seq(price_min, price_max, by = bin_width)),
             yaxis = list(title = "Density"))
  })
  
  # Most installed social
  output$most_installed_social <- renderText({
    # Filter the dataset for games
    social_data <- google_play_data[google_play_data$Category == "SOCIAL", ]
    
    # Find the game with the most installs
    most_installed_social <- social_data[which.max(social_data$No..of.installs), "App"]
    
    # Create the output string
    output_string <- paste("Most Installed Social Media Platform: ", most_installed_social)
    
    # Return the output string
    return(output_string)
  })
  
  # Most installed tool
  output$most_installed_tools <- renderText({
    # Filter the dataset for games
    tools_data <- google_play_data[google_play_data$Category == "TOOLS", ]
    
    # Find the game with the most installs
    most_installed_tools <- tools_data[which.max(tools_data$No..of.installs), "App"]
    
    # Create the output string
    output_string <- paste("Most Installed Tool: ", most_installed_tools)
    
    # Return the output string
    return(output_string)
    
    
  })
  
  
  # Installs vs. Reviews
  output$installs_reviews <- renderPlotly({
    plot_ly(google_play_data, x = ~Installs, y = ~Reviews, color = ~Type) %>%
      add_markers() %>%
      layout(title = "Installs vs. Reviews", xaxis = list(title = "Installs"), yaxis = list(title = "Number of Reviews"))
  })
  
  # Category-wise number of apps
  output$category_app_count <- renderPlotly({
    category_counts <- google_play_data %>%
      count(Category, sort = TRUE)
    
    plot_ly(category_counts, x = ~Category, y = ~n, type = "bar",
            marker = list(color = "yellowgreen")) %>%
      layout(title = "Category-wise Number of Apps", xaxis = list(title = "Category"), yaxis = list(title = "Number of Apps"))
  })
  
  # Preprocess the Size column to convert all sizes into megabytes
  google_play_data$Size_MBs <- as.numeric(gsub("[A-Za-z]", "", google_play_data$Size))
  google_play_data$Size_MBs[grepl("k", google_play_data$Size)] <- google_play_data$Size_MBs[grepl("k", google_play_data$Size)] / 1024

  # Define the histogram for Size
  output$size_distribution <- renderPlot({
    # Filter out the rows where Size_MBs is NA
    filtered_data <- google_play_data[!is.na(google_play_data$Size_MBs), ]
    
    # Create the histogram using the selected bin size
    ggplot(filtered_data, aes(x = Size_MBs)) +
      geom_histogram(binwidth = input$size_bin_size, fill = "#00A65A", color = "white") +
      labs(title = "App Size Distribution", x = "Size (MB)", y = "Frequency")
  })
  
  # Filtered data
  filtered_data <- reactive({
    temp_data <- google_play_data
    temp_data <- temp_data[temp_data$Rating >= input$rating[1] & temp_data$Rating <= input$rating[2], ]
    if (input$category != "All") {
      temp_data <- temp_data[temp_data$Category == input$category, ]
    }
    if (input$type != "All") {
      temp_data <- temp_data[temp_data$Type == input$type, ]
    }
    temp_data <- temp_data %>% arrange(desc(Installs))
    temp_data <- head(temp_data, 50)
    return(temp_data)
  })
  
  # Render filtered data in a DataTable
  output$play_store_data <- renderDataTable({
    filtered_data()
  }, options = list(pageLength = 5))
  
  # Filtered data
  filtered_data <- reactive({
    if (input$android_version == "All") {
      return(google_play_data)
    } else {
      google_play_data %>%
        filter(Android_Version <= as.numeric(input$android_version))
    }
  })
  
  # Number of apps
  output$app_count <- renderText({
    paste("Number of Apps:", nrow(filtered_data()))
  })
}

# Run the Shiny app
shinyApp(ui, server)