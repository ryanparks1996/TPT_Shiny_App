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
  
  layout_columns(
    col_widths = 4,
    # Select region----
    selectInput(
      "region",
      label = "Select region",
      choices = c("All", sort(unique(TPT_Report$Region)))
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
    sliderInput(
      "timeRange",
      label = "Select range of weeks",
      min = 1,
      # Each week the max number of weeks will increase
      max = max(TPT_Report$Week),
      value = c(1, max(TPT_Report$Week))
    ),
    
    # Toggle to select weekly averages across all selected stores----
    checkboxInput(
      "allSites",
      "Plot median of all stores",
      value = FALSE
    ),
    # Toggle to select weekly averages across all selected stores----
    checkboxInput(
      "monthly",
      "Monthly Averages (CURRENTLY DEACTIVATED)",
      value = FALSE
    )
  ),
  
  
  plotOutput("plot"),
  verbatimTextOutput("summary")
)

# ------------------------------------------------------------------------------
# Define server logic----
server <- function(input, output) {
  
  data <- reactive({
    req(input$upload)
    if (input$monthly) {
      file_convert_to_tidy(input$upload$name, "Week to Week")
    } else {
      file_convert_to_tidy(input$upload$name, "Period to Period")
    }
  })
  
  # Interactive drill down from region -> district -> site---- 
  region <- reactive({
    if (input$region != "All") {
      filter(TPT_Report, Region == input$region)
    } else return(TPT_Report)
  })
  
  observeEvent(region(), {
    choices <- unique(region()$DM)
    updateSelectInput(inputId = "district", choices = c("All", sort(choices)))
  })
  
  district <- reactive({
    req(input$district)
    if (input$district != "All") {
      filter(region(), DM == input$district)
    } else return(region()) # Returns rows from all DMs in a selected region
  })
  
  observeEvent(district(), {
    choices <- unique(district()$Site.name)
    updateSelectInput(inputId = "site", choices = c("All", sort(choices)))
  })
  
  # Final step of drill down.
  site <- reactive({
    req(input$site)
    if (input$site != "All") {
      filter(district(), Site.name == input$site)
    } else return(district())
  })
  
  averageAllStores <- reactive({
    weeklyData <- site()
    if (input$allSites) {
      weeklyData <- weeklyData %>%
        group_by(Week) %>%
        mutate(TPT = median(TPT))
    } 
    return(weeklyData)
  })
  
  
  output$plot <- renderPlot({
    req(input$upload)
    
    selectedData <- averageAllStores() %>%
      filter(Week >= input$timeRange[1] &
               Week <= input$timeRange[2])
    
    
    # Create data frame that contains horizontal line of 3 (goal TPT)
    goal <- data.frame(yintercept=3, Goal = "3.0 TPT")
    # Create ggplot object----
    TPT_Plot <- ggplot(data=selectedData, aes(x=factor(Week), y=TPT))
    
    # Plot line plot for averages and simple scatter plot otherwise
    if (input$allSites) {
      TPT_Plot <- ggplot(data=selectedData, aes(x=Week, y=TPT))
      TPT_Plot <- TPT_Plot + geom_line() +
        geom_point(aes(colour = TPT))
    } else {
      TPT_Plot <- TPT_Plot + geom_point(aes(color=TPT))
    }
    
    TPT_Plot <- TPT_Plot +
      scale_color_gradient2(low="black", mid="purple", high="red", midpoint=3) +
      geom_hline(aes(yintercept=yintercept, linetype="3.0"), goal) + 
      scale_linetype_manual(name = "Goal",
                            values = 2,) +
      labs(title = "Weekly TPT", 
           subtitle = paste("Region:", input$region, 
                            "\t \t District:", input$district, 
                            "\t \t Site:", input$site,
                            "\t \t Median TPT:", input$allSites),
           x="Week",
           color = "TPT")
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