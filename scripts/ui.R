# install.packages("shiny")
library("tableHTML")
library(shiny)
source("analysis.R")
# Define UI for application that draws a histogram
shinyUI(navbarPage(
    "Gender and Salary",
    
    # Overview
    tabPanel(
        "Overview",
        tags$style(HTML("
             @import url('https://fonts.googleapis.com/css?family=Oswald:500');
            body {
                background-color:#f9f9f9
            }          
            h2 {
                        font-family: 'Oswald', sans-serif;
                        font-weight: 500;
                        line-height: 1.1;
                        color: #393c42;
            }
            h3 {
                        font-family: 'Oswald', sans-serif;
                        font-weight: 500;
                        line-height: 1.1;
                        color: #393c42;
            }
            h6 {
						font-family: 'Oswald', sans-serif;
                        font-weight: 100;
                        line-height: 1.1;
                        color: #545859;
                        text-align: left;
            }
                        
        ")),
        titlePanel("Exploring Salary Gap in Genders and Majors"),
        img(src = 
"https://recruitingtimes.org/wp-content/uploads/2016/11/Gender-Pay-Gap.png"),
        h6("Copyright Recruiting Times"),
        h3("Authors:"),
        tags$li("Jason Li"), 
        tags$li("Zhennan Zhou"), 
        tags$li("Colson Xu"), 
        tags$li("Matthew Cho"),
        h3("Project Description"),
        p("Our group utilizes the data of enrollment of each major in universities from
            Institute of Education Sciences gathered through surveys across
            United States.", a(strong("National Center for Education Statistics"), href = "https://nces.ed.gov/programs/digest/") ,"allows us to gain access
             for the data. ",
          "We are using information about education across the United States starting from 2012 to current year. 
          Specifically, we will be looking at data regarding genders, degree types, study areas."),
        h3("Our goal:"),
        tags$li("gender and salary gaps armong different majors,"), 
        tags$li("gender difference in different majors,"), 
        tags$li("salary gap between majors and genders"),
        h3("Data"),
        p("Understanding data is a key part in analyzing and organizing data.
          As we're aware of the difficulty of sourcing data via API, we chose to use the", em(".csv"), "data downloaded from the",
          a(strong("National Center for Education Statistics"),href="https://nces.ed.gov/"), "in this project."),
        h3("Audience"),
        p("While anybody can get some benefits from this report, 
          our targeted audience for this project are those who care about the salary gap in different majors and genders;
          college students who want to explore what majors they want to get in based on the salary; 
          people who are concerned about gender inequality in the areas they are majoring in.
        
          With gender and salary gap becoming an increasingly important and highly-debated topic,
        we expect there to be a lot of people looking for this kind of information. 
          We hope their concerns are answered through various visualizations.
          ")
        
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
    
    # This tab shows the top 10 jobs and the bottom 10 jobs and the 
    # gender difference in those jobs
    tabPanel(
        "Gender Difference in Jobs",
        titlePanel("Gender Difference in Top 10 jobs vs Bottom 10 jobs"),
        sidebarLayout(
            sidebarPanel(
                selectInput("work", label = h3("Top 10 vs Bottom 10"),
                            choices = list("Top" = 1, "Bottom" = 2))
            ),
            mainPanel(
                plotOutput("job_plot")
            )
        )
    ),

    # tab 5
    tabPanel(
        "About Us",
        titlePanel("")
        
    )
))
