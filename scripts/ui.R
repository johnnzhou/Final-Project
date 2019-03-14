library(tableHTML)
library(shiny)
library(lintr)
library(shinythemes)
source("analysis.R")

ui <- navbarPage(
    theme = shinytheme("yeti"),
    "Gender Gap",
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
        titlePanel("Gender Gap Statistics in the U.S"),
        h6("Copyright Recruiting Times"),
        h3("Purpose of the project"),
        h4("Ever heard of the term Gender Gap? It is commonly known as the
            gender pay gap, which is the difference in the average salary
            earned between men and women. Even though today's society is
            moving towards gender equality, people still believe that there
            is a massive difference in salary among men and women. However,
            in reality there really isn't that big of a gender pay gap
            in our society. This project will support the statement that
            there isn't a salary discrimation among women compared to men."),
        h4("To support our statment, our group will use the statistics of
            enrollment of each major in universities gathered by the
            Institute of Education Sciences through surveys across
            United States. One can find more information
            about the dataset", a(strong("here,"),
            href = "https://nces.ed.gov/programs/digest/"),
            "which includes education information
            from 2012 to present year."),
        h4("There are a lot of factors when considering the gender salary gap.
            It is nearly impossible to consider all the factors that
            contributes to the gender salary gap, however our team chose
            the most valuable factors when considering gender gap salary from
            our datas. Our project will foucs on audiences who believe there
            is still a huge difference in gender salary gap and with this
            project, we hope to have changed their mind by the end."),
        h4("The three", em("charts"), " of this project will represent:"),
        tags$li("Gender Difference among Majors"),
        tags$li("Main contributor to Wage Gap"),
        tags$li("Gender difference in Occupations"),
        h3("Authors:"),
        tags$li("Matthew Cho"),
        tags$li("Jason Li"),
        tags$li("Colson Xu"),
        tags$li("Zhennan Zhou")
      ),

    # John
    tabPanel(
        "Gender Difference in Majors",
        titlePanel("Gender Difference in Majors"),
        h4("The bar graph below uses the data from National Center for
            Education Statistics, which demonstrate the percentage difference
            of males and females in different majors. Specifically, the two
            bar plots compare the number of male students and female
            students top-20 male dominant and female dominant majors.
            The datasets can be chosen with", strong("different checkboxes"),
            "that categorize different majors."),
        h4("The", strong("Show trend"),
            "checkbox is used to show the percentage of male and female
            in the different salary levels, which illustrates the general
            trend between the percentage of male/female and the
            median salary."),
        h4("We have concluded from this chart and analysis that across
            the country, females tend to have less salary than males on average.
            The percentage of female working in the industry decreases as the
            median salary increases. Even in the Female Dominant Majors,
            less females work in the industry than males do as the average
            salary increases. In highly-paid industries, such as Computer
            Science and Electrical Engineering, the percentage of males
            outnumber the percentage of female."),
        hr(),
        sidebarLayout(
            sidebarPanel(
                tags$style(make_css(list(".well", "width", "310px"))),
                tags$style(make_css(list(".col-sm-4", "width", "350px"))),
                tags$style(make_css(list(
                    "#major_list_top", "height", "1000px"))),
                tags$style(make_css(list(
                    "#diff_plot_least", "margin-top", "30px"))),
                tags$style(make_css(list(".col-sm-8", "width", "70%"))),
                checkboxInput("male_trend", "Show Trend", value = TRUE),
                checkboxGroupInput("major_list_top", "Majors",
                    selected = major_list_top,
                    choices = major_list_top),
                checkboxInput("female_trend", "Show Trend", value = TRUE),
                checkboxGroupInput("major_list_least", "Majors",
                    selected = major_list_least,
                    choices = major_list_least)
            ),
            mainPanel(
                plotlyOutput("diff_plot_top", height = "600px"),
                plotOutput("trend_plot_male", height = "450px"),
                hr(),
                plotlyOutput("diff_plot_least", height = "600px"),
                plotOutput("trend_plot_female", height = "450px")
            )
        )
    ),

    # tab 3
    tabPanel(
        "The Main Contributor to Wage Gap",
        titlePanel("Female Percentage vs. Major Median Salary"),
        h4("In this study, we compare how male and female choose
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
            and individual performance.")),
        sidebarLayout(
            sidebarPanel(
                checkboxInput("trend", label = "Show Trend Line", value = F),
                sliderInput("perc_select", "Select Percentage Range",
                    min = 0, max = 1, value = c(0, 1))
            ),
            mainPanel(
                plotlyOutput("female_perc_vs_major_pay"),
                hr(),
                plotlyOutput("male_perc_vs_major_pay"),
                br()
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
                selectInput("work", h3("Top vs Bottom"),
                list("Top" = 1, "Bottom" = 2)),
                radioButtons("lower_perc", "Lower Salary Occupations",
                             choices = lower_perc$Occupation),
                radioButtons("higher_perc",
                             "Higher Salary Occupations",
                             choices = higher_perc$Occupation)
            ),
            mainPanel(
                fluidRow(
                    width = 7, plotOutput("job_plot", height = "400px"),
                    column(width = 6, plotlyOutput("lowest_plot")),
                    column(width = 6, plotlyOutput("highest_plot"))
                )
            )
        )
    )
)