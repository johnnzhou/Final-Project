library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Gender and Salary",
    
    # tab 1
    tabPanel(
        "",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                sliderInput()
            ),
            mainPanel(
                plotOutput("")
            )
        )
    ),
    
    # tab 2
    tabPanel(
        "",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                sliderInput()
            ),
            mainPanel(
                plotOutput("")
            )
        )
    ),
    
    # tab 3
    tabPanel(
        "",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                sliderInput()
            ),
            mainPanel(
                plotOutput("")
            )
        )
    ),   
    
    # tab4
    tabPanel(
        "",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                sliderInput()
            ),
            mainPanel(
                plotOutput("")
            )
        )
    )
)
