# install.packages("shiny")
library("tableHTML")
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
                tags$style(make_css(list(".well", 'width', "310px"))),
                tags$style(make_css(list(".col-sm-4", 'width', "350px"))),
                tags$style(make_css(list("#major_list_top", "height", "600px"))),
                tags$style(make_css(list("#diff_plot_least", "margin-top", "30px"))),
                tags$style(make_css(list(".col-sm-8", "width", "70%"))),
                checkboxGroupInput("major_list_top", 
                                   "Majors",
                                   selected = major_list_top, 
                                   choices = major_list_top),
                checkboxGroupInput("major_list_least", 
                                   "Majors",
                                   selected = major_list_least, 
                                   choices = major_list_least)
            ),
            mainPanel(
                plotOutput("diff_plot", height = "600px"),
                plotOutput("diff_plot_least", height = "600px")
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
