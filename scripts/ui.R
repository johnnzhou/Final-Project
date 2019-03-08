# install.packages("shiny")

library(shiny)
source("analysis.R")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    "Gender and Salary",
    # tab 1
    tabPanel(
        "Gender Difference in Majors",
        titlePanel("Gender Difference in Majors"),
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput("major_list", 
                                   "Majors",
                                   selected = major_list, 
                                   choices = major_list)
                
            ),
            mainPanel(
                plotOutput("diff_plot", height = "900px")
                # textOutput("text")
            )
        )
    ),
    
    # tab 2
    tabPanel(
        "",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                
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
                
            ),
            mainPanel(
                plotOutput("")
            )
        )
    )
))
