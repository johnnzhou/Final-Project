library(shiny)
library(dplyr)
library(plotly)
library(shiny)
source("analysis.R")

major_enrollment <- read.csv("../data/major_enrollment.csv", stringsAsFactors = F)


server <- function(input, output) {
    
    output$dist_plot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x <- faithful[, 2] 
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = "darkgray", border = "white")
        
    })
    
    diff_data <- reactive({
        difference <- major_enrollment %>% 
                        arrange(-`Percentage of Male`) %>% 
                        filter(major %in% input$major_list_top)
        difference <- melt(difference,
                            id.vars = "major",
                             variable.name = "type",
                             value.name = "percentage")
        return(difference)
    })
    output$diff_plot <- renderPlot({
        diff_plot <- ggplot(diff_data()) +
            geom_bar(stat = "identity",
                     mapping = aes(
                         x = major,
                         y = percentage,
                         fill = type,
                         width=0.8
                     )
            )+
            geom_text(aes(x = major,
                          y = percentage,
                          label = paste0(round(percentage),"%")), 
                          vjust=1.6, 
                          color="black",
                          size = 3
                          )+
            scale_fill_brewer(palette = "Set3")+
            labs(
                title = "Male Dominant Majors",
                x = "",
                y = "Number of people in the major by gender",
                fill = ""
            )+
            theme(
                plot.title = element_text(
                    hjust = 0.5,
                    vjust = 0.5,
                    face = "bold"
                )
            )+
            theme(legend.position="bottom", legend.box = "horizontal")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        return(diff_plot)
    })
    
    # second plot
    diff_data_least <- reactive({
        difference_least <- major_enrollment %>% 
            arrange(`Percentage of Female`) %>% 
            filter(major %in% input$major_list_least)
        difference_least <- melt(difference_least,
                           id.vars = "major",
                           variable.name = "type",
                           value.name = "percentage")
        return(difference_least)
    })
    output$diff_plot_least <- renderPlot({
        diff_plot_least <- ggplot(diff_data_least()) +
            geom_bar(stat = "identity",
                     mapping = aes(
                         x = major,
                         y = percentage,
                         fill = type,
                         width=0.8
                     )
            )+
            geom_text(aes(x = major,
                          y = percentage,
                          label = paste0(round(percentage),"%")), 
                      vjust=1.6, 
                      color="black",
                      size = 3
            )+
            scale_fill_brewer(palette = "Set2")+
            labs(
                title = "Female Dominant Majors",
                x = "",
                y = "Number of people in the major by gender",
                fill = ""
            )+
            theme(
                plot.title = element_text(
                    hjust = 0.5,
                    vjust = 0.5,
                    face = "bold"
                )
            )+
            theme(legend.position="bottom", legend.box = "horizontal")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        return(diff_plot_least)
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