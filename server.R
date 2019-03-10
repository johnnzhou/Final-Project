library(shiny)
library(dplyr)
library(plotly)

major_enrollment <- read.csv("data/major_enrollment.csv", stringsAsFactors = F)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # John's plot
    output$dist_plot <- renderPlot({
    
        # generate bins based on input$bins from ui.R
        x <- faithful[, 2] 
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = "darkgray", border = "white")
    
    })
    
    ############################ Colson ############################
    trend_line <- reactive({
        input$trend
    })
    
    perc_range <- reactive({
        range <- input$perc_select
        major_enrollment <- filter(major_enrollment,
               perc_to_double(perc_female) > range[1],
               perc_to_double(perc_female) < range[2])
    })
    
    perc_to_double <- function(x) {
        as.numeric(gsub("%", "", x)) / 100
    }
    
    output$female_perc_vs_major_pay <- renderPlotly({
        p <- plot_ly(
            x = perc_to_double(perc_range()$perc_female),
            y = perc_range()$median_pay,
            type = "scatter",
            mode = "markers",
            color = perc_range()$median_pay,
            size = perc_range()$median_pay,
            showlegend = F,
            text = paste("Major:", perc_range()$major,
                         "<br>Percentage of Female:",
                         perc_range()$perc_female,
                         "<br>Median Salary After 5 Years:",
                         perc_range()$median_pay),
            hoverinfo = "text"
            
        ) %>%
            layout(title = "Major Female Percentage vs Major Median Pay",
                   xaxis =
            list(title = "Percentage of Female in Majors"),
                   yaxis =
            list(title = "Median Salary for Majors After 5 Years"))
        
        if (trend_line()) {
             p <- add_lines(p, y = ~fitted(loess(
                 perc_range()$median_pay ~
                  perc_to_double(perc_range()$perc_female))))
        }
        p
    })
    
}
