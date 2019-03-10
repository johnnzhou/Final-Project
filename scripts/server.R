

library(shiny)
source("analysis.R")

shinyServer(function(input, output) {
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
    
    
  
})
