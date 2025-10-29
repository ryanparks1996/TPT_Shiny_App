# Load libraries ----
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(quantmod)
library(readxl)
library(bslib)
library(readxl)

# Source helper functions----
source("helpers.R")


# ------------------------------------------------------------------------------
# Define UI for looking at weekly TPT reports 
# User can group by store, district, region or company wide----
ui <- page_fluid(
  titlePanel("Weekly TPT Report"),
  card(
    helpText("Please selecet 'Average TPT Report' file from Monday Dashboard"),
    fileInput("upload", NULL, accept = ".xlsx")
  ),
  
  #layout_columns(
  #col_widths = 4,
  # Select region----
  #selectInput(
  #"region",
  #label = "Select region",
  # choices = c("All", sort(unique(TPT_Report$Region)))
  #),
  
  layout_columns(
    col_widths = 4,
    # Select region (dynamic ui)----
    selectInput(
      "region",
      label = "Select region",
      choices = NULL
    ),
    
    # Select district (dynamic ui)----
    selectInput(
      "district",
      label = "Select district",
      choices = NULL
    ),
    
    # Select site (dynamic ui)----
    selectInput(
      "site",
      label = "Select site",
      choices = NULL
    ),
    
    # Select time frame----
    #sliderInput(
    #"timeRange",
    # label = "Select range of weeks",
    #  min = 1,
    # Each week the max number of weeks will increase
    #max = max(TPT_Report$Week),
    # value = c(1, max(TPT_Report$Week))
    #),
    # Select time frame (dynamic ui)----
    sliderInput(
      "timeRange",
      label = "Select range of weeks",
      min = 1,
      # Each week the max number of weeks will increase
      max = 52,
      value = c(1, 52)
    ),
    
    # Toggle to select weekly averages across all selected stores----
    checkboxInput(
      "allSites",
      label = "Plot median of all stores",
      value = FALSE
    ),
    
    # Toggle to select weekly averages across all selected stores----
    # Currently disabled 
    checkboxInput(
      "monthly",
      label = "Monthly Averages (CURRENTLY DEACTIVATED)",
      value = FALSE
    )
  ),
  
  tableOutput("head"),
  plotOutput("plot"),
  verbatimTextOutput("summary")
)

# ------------------------------------------------------------------------------
# Define server logic----
server <- function(input, output) {
  
  # File upload wizard
  data <- reactive({
    req(input$upload)
    
    filepath <- input$upload$datapath
    if (!input$monthly) {
      file_convert_to_tidy(filepath, "Week to Week")
    } else { # Disabled function by always choosing 'Week to Week'
      #file_convert_to_tidy(filepath, "Period to Period")
      file_convert_to_tidy(filepath, "Week to Week")
    }
  })
  
  
  # Interactive drill down from region -> district -> site---------------------- 
  
  ## REGION ##
  # Get list of Regions and update choices in program
  observeEvent(data(), {
    choices <- unique(data()$Region)
    updateSelectInput(inputId = "region", choices = c("All", sort(choices)))
  })
  
  region <- reactive({
    req(input$upload)
    if (input$region != "All") {
      filter(data(), Region == input$region)
    } else return(data())
  }) 
  
  ## DISTRICT ##
  # Get list of DMs in a selected Region and update choices in program
  observeEvent(region(), {
    choices <- unique(region()$DM)
    updateSelectInput(inputId = "district", choices = c("All", sort(choices)))
  })
  
  district <- reactive({
    req(input$district)
    # Filter data by selected DM 
    if (input$district != "All") {
      filter(region(), DM == input$district) 
    } else return(region()) 
  })
  
  ## SITE ##
  # Get list of Sites in a selected Region/District and update choices in program
  observeEvent(district(), {
    choices <- unique(district()$`Site name`)  
    updateSelectInput(inputId = "site", choices = c("All", sort(choices)))
  })
  
  site <- reactive({
    req(input$site)
    #Filter data by selected Site
    if (input$site != "All") {
      filter(district(), `Site name` == input$site) 
    } else return(district())
  })
  # End of drill down-----------------------------------------------------------
  
  
  averageAllStores <- reactive({
    weeklyData <- site()
    if (input$allSites) {
      weeklyData <- weeklyData %>%
        group_by(Week) %>%
        mutate(TPT = median(TPT)) # Median used due to large outliers
    } 
    return(weeklyData)
  })
  
  
  # Add column to data to indicate if TPT is out of range
  #(Used for color coding the plot)
  flaggedTPT <- reactive({
    averageAllStores() %>%
      mutate(color_condition = ifelse(!(TPT >= 3.11 | TPT <= 2.89),"red", "blue"))
  })
  
  # Adjust time range max value
  observeEvent(data(), {
    maxWeek <- max(data()$Week)
    updateSliderInput(inputId = "timeRange", max = maxWeek)
  })
  
  output$plot <- renderPlot({
    req(input$upload)
    #filtering data by time range selected
    selectedData <- flaggedTPT() %>%
      filter(Week >= input$timeRange[1] &
               Week <= input$timeRange[2])
    
    # Create data frame that contains horizontal line of 3 (goal TPT)
    # (Used in plot(s) as reference line)
    goal <- data.frame(yintercept=3, Goal = "3.0 TPT")
    
    # Create ggplot object----
    TPT_Plot <- ggplot(data = selectedData, 
                       aes(x = factor(Week), 
                           y = TPT,
                           color = color_condition)) +
      scale_color_discrete(labels = c("red" = "Outside range", 
                                      "blue" = "Inside range"))
    
    # Plot line plot for averages and simple scatter plot otherwise
    if (input$allSites) {
      TPT_Plot <- ggplot(data=selectedData, 
                         aes(x = Week, 
                             y = TPT))
      
      TPT_Plot <- TPT_Plot + geom_line() +
        geom_point(aes(color=color_condition), size = 3) +
        scale_color_discrete(labels = c("red" = "Outside range", 
                                        "blue" = "Inside range"))
    } else {
      TPT_Plot <- TPT_Plot + geom_point()
    }
    
    TPT_Plot <- TPT_Plot +
      geom_hline(aes(yintercept=yintercept, linetype="3.0"), goal) + 
      scale_linetype_manual(name = "Goal", values = "dashed") +
      labs(title = "Weekly TPT",
           x = "Week",
           color = "TPT",
           subtitle = paste("Region:", input$region, 
                            "\t \t District:", input$district, 
                            "\t \t Site:", input$site,
                            "\t \t Median TPT:", input$allSites)
      ) 
    
    TPT_Plot
  })
  
  # Create table of summary statistics----
  output$summary <- renderPrint({
    req(input$upload)
    
    cat("TPT Summary Statistics \rRegion:", input$region, 
        "\rDistrict:", input$district, 
        "\rSite:", input$site, 
        "\rFrom: Week", input$timeRange[1], "to Week", input$timeRange[2], "\n")
    dataset <- site() %>%
      filter(Week >= input$timeRange[1] &
               Week <= input$timeRange[2]) %>%
      select(TPT)
    
    summary(dataset)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
