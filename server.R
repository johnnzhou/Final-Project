library(shiny)
library(dplyr)

major_enrollment <- read.csv("data/major_enrollment.csv", stringsAsFactors = F)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
    
  })
  
}
