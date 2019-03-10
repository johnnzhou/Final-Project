# install.packages("shiny")
library("tableHTML")
library(shiny)
source("analysis.R")
# Define UI for application that draws a histogram
ui <- navbarPage(
    "Gender and Salary",
    
    # Overview
    tabPanel(
        "Overview",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                
            ),
            mainPanel(
                plotOutput("")
            )
        )
    ),
    
    # tab 2
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
    
    # tab 3
    tabPanel(
        "The Main Contributor to Wage Gap",
        titlePanel("Female Percentage vs. Major Median Salary"),
        sidebarLayout(
            sidebarPanel(
                checkboxInput("trend", label = "Show Trend Line", value = F),
                sliderInput("perc_select", label = "Select Percentage Range",
                            min = 0, max = 1, value = c(0, 1))
            ),
            mainPanel(
                plotlyOutput("female_perc_vs_major_pay")
            )
        )
    ),
    
    # tab4
    tabPanel(
        "Tab 4",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                # sliderInput()
            ),
            mainPanel(
                
            )
        )
    ),

    # tab 5
    tabPanel(
        "About Us",
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
                
            )
        )
    )
)