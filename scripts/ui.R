library(tableHTML)
library(shiny)
source("analysis.R")

shinyUI(navbarPage(
  "Gender and Salary",
  tabPanel(
    "Overview",
    tags$style(HTML("
             @import url('https://fonts.googleapis.com/css?family=Oswald:500');
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
    titlePanel("Overview: Gender, Major and Salary Gap"),
    h6("Copyright Recruiting Times"),
    h3("Author:"),
    tags$li("Jason Li"),
    tags$li("Zhennan Zhou"),
    tags$li("Colson Xu"),
    tags$li("Matthew Cho"),
    h3("Project Description"),
    p(
      "Our group will use the statistics of enrollment
        of each major in universities.
        Institute of Education Sciences gathered these statistics
        through surveys across United States.
        The website of National Center for Education Statistics
        allows us to gain access for the data. ",
      "The", a(strong("datasets"),
        href = "https://nces.ed.gov/programs/digest/"
      ),
      "we are using includes education information
        starting from 2012 to current year.
        More specifically, we will be looking at statistics about genders,
        degree types, study areas, and ethnicities."
    ),
    p("This data set can be beneficial for many people to view.
          However, our target audience are females because they may feel
            discriminated due to their gender.
          They might believe that there are gender salary gaps.
          With the data that our team analyzes, we can compare
            different years of salaries for each college major.
          Then we will analyze major choices
            that males and females tend to make.
          Using this dataset, we can help our target audience discover
            the real reason behind gender salary gap."),
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
    p(
      "The bar graph below uses the data from
            National Center for Education Statistics,
            which demonstrate the percentage difference
            of males and females in different majors.
          Specifically, the two  bar plots compare the number
        of male students and female students top-20 male
        dominant and female dominant majors.
          The datasets can be chosen with", strong("different checkboxes"),
      "that categorize different majors."
    ),
    p(
      "The", strong("Show trend"),
      "checkbox is used to show the percentage of male and female
    in the different salary levels,
    which illustrates the general trend between the percentage
    of male/female and the median salary."
    ),
    p("We have concluded from this chart and analysis that across the country,
          females tend to have less salary than males on average.
          The percentage of female working in the industry
            decreases as the median salary increases.
          Even in the Female Dominant Majors, less females work in the industry
        than males do as the average salary increases.
        In highly-paid industries,
      such as Computer Science and Electrical Engineering,
      the percentage of males outnumber the percentage of female."),
    hr(),
    sidebarLayout(
      sidebarPanel(
        tags$style(make_css(list(".well", "width", "310px"))),
        tags$style(make_css(list(".col-sm-4", "width", "350px"))),
        tags$style(make_css(list("#major_list_top", "height", "1000px"))),
        tags$style(make_css(list("#diff_plot_least", "margin-top", "30px"))),
        tags$style(make_css(list(".col-sm-8", "width", "70%"))),
        checkboxInput("male_trend",
          "Show Trend",
          value = TRUE
        ),
        checkboxGroupInput("major_list_top",
          "Majors",
          selected = major_list_top,
          choices = major_list_top
        ),
        checkboxInput("female_trend",
          "Show Trend",
          value = TRUE
        ),
        checkboxGroupInput("major_list_least",
          "Majors",
          selected = major_list_least,
          choices = major_list_least
        )
      ),
      mainPanel(
        plotOutput("diff_plot", height = "600px"),
        plotOutput("trend_plot_male", height = "450px"),
        hr(),
        plotOutput("diff_plot_least", height = "600px"),
        plotOutput("trend_plot_female", height = "450px")
      )
    )
  ),

  # tab 3
  tabPanel(
    "The Main Contributor to Wage Gap",
    titlePanel("Female Percentage vs. Major Median Salary"),
    h4(
        "In this study, we compare how male and female choose
        their major, and how those major pays in five years.
        We believe this study can best reflect the real reason behind
        gender salary gap. The reason is that through comparing
        major choice and how well ", strong("that major "), "pays,
        we illiminate individual factors that create wage gap like
        how one perform in job and how he/she is willing to show up
        anytime his/her boss calls without prior notice.
        Thus, we can isolate the one variable we want to study: ",
        strong("INDIVIDUAL MAJOR CHOICE"), br(), br(),
        "Furthermore, the salary data we use is the median base
        salary after working in a position 5 years. ", strong(
            "It illiminates confunding variables like pregnancy leave
            and individual performance."
    )
        ),
    sidebarLayout(
      sidebarPanel(
        checkboxInput("trend", label = "Show Trend Line", value = F),
        sliderInput("perc_select",
          label = "Select Percentage Range",
          min = 0, max = 1, value = c(0, 1)
        )
      ),
      mainPanel(
        plotlyOutput("female_perc_vs_major_pay"),
        hr(),
        plotlyOutput("male_perc_vs_major_pay")
      )
    )
  ),

  # This tab shows the top 10 jobs and the bottom 10 jobs and the
  # gender difference in those jobs
  tabPanel(
    "Gender Difference in Jobs",
    titlePanel("Top 10 vs Bottom 10 jobs"),
    sidebarLayout(
      sidebarPanel(
        selectInput("work",
          label = h3("Top vs Bottom"),
          choices = list("Top" = 1, "Bottom" = 2)
        )
      ),
      mainPanel(
        plotOutput("job_plot", width = "900px", height = "600px")
      )
    )
  )
))
