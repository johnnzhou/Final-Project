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
        titlePanel("Overview: Gender, Major and Salary Gap"),
        img(src = 
"https://recruitingtimes.org/wp-content/uploads/2016/11/Gender-Pay-Gap.png"),
        h6("Copyright Recruiting Times"),
        h3("Author:"),
        tags$li("Jason Li"), 
        tags$li("Zhennan Zhou"), 
        tags$li("Colson Xu"), 
        tags$li("Matthew Cho"),
        h3("Project Description"),
        p("Our group will use the statistics of enrollment of each major in universities. 
            Institute of Education Sciences gathered these statistics through surveys across
            United States. The website of National Center for Education Statistics allows us to gain access
             for the data. ", "The", a(strong("datasets"), href = "https://nces.ed.gov/programs/digest/"),
          "we are using includes education information starting from 2012 to current year. 
          More specifically, we will be looking at statistics about genders, degree types, study areas, and ethnicities."),
        p("This data set can be beneficial for many people to view. 
          However, our target audience are females because they may feel discriminated due to their gender. 
          They might believe that there are gender salary gaps. 
          With the data that our team analyzes, we can compare different years of salaries for each college major. 
          Then we will analyze major choices that males and females tend to make. 
          Using this dataset, we can help our target audience discover the real reason behind gender salary gap."),
        p("The audience of this project will,", em("hopefully"), "learn:"),
        tags$li("gender and salary gaps armong different majors,"), 
        tags$li("the cause of gender gaps,"), 
        tags$li("gender difference in different majors,"), 
        tags$li("salary gap between majors and genders")


        
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
    )
)
