#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
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
))
