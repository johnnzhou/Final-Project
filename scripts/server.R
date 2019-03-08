
library(shiny)
source("analysis.R")

shinyServer(function(input, output) {
    diff_data <- reactive({
        difference <- major_enrollment %>% 
                        arrange(-diff) %>% 
                        filter(Major %in% input$major_list) %>% 
                        select(Major, Males, Females) %>% 
                        melt(id.vars = "Major",
                             variable.name = "Gender",
                             value.name = "Number")
                        
                        
        return(difference)
    })
    # output$text <- renderText({
    #     major_list
    # })
    output$diff_plot <- renderPlot({
        diff_plot <- ggplot(diff_data()) +
            geom_bar(stat = "identity",
                     mapping = aes(
                         x = Major,
                         y = Number,
                         fill = Gender    
                     )
            )+
            geom_text(aes(x = Major,
                          y = Number,
                          label = Number), 
                          vjust=1.6, 
                          color="black",
                          size = 3
                          )+
            scale_fill_brewer(palette = "Set3")+
            labs(
                x = "Majors",
                y = "Difference of number of people in the major by gender"
            )+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        return(diff_plot)
    })
  
})
