## prepare necessary packages
# install.packages("shinythemes")
# install.packages("tableHTML")
# install.packages("shiny")

library(shinythemes)
library(tableHTML)
library(shiny)
library(lintr)
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
    img(
      src =
    "https://recruitingtimes.org/wp-content/uploads/2016/11/Gender-Pay-Gap.png"
    ),
    h6("Copyright Recruiting Times"),
    h3("Purpose of the project"),
    p(
      "Ever heard of the term Gender Gap? It is commonly known as the
gender pay gap, which is the difference in the average salary earned between
men and women. Even though today's society is moving towards gender equality,
people still believe that there is a massive difference in salary among men
and women. However, in reality there really isn't that big of a gender pay gap
in our society. This project will support the statement that there isn't
that big of a salary discrimation between men and women as people belive
      there is."
    ),

    p(
      "To support our statment, our group will use the statistics of enrollment
        of each major in universities gathered by the Institute of Education
Sciences through surveys across United States. One can find more information
 about the dataset", a(strong("here,"),
        href = "https://nces.ed.gov/programs/digest/"
      ),
      "which includes education information
    from 2012 to present year.",
      a("The other dataset coming from Department of Labor",
        href = "https://www.dol.gov/wb/occupations_interactive_txt.htm"),
    "gives us information regarding gender percentage
      and salary in different occupations."
    ),
    p("There are a lot of factors when considering the gender salary gap.
It is nearly impossible to consider all the factors that contributes to the
gender salary gap, however our team chose the most valuable factors when
considering gender gap salary from our datas. Our project will foucs on
audiences who believe there is still a huge difference in gender salary
gap and with this project, we hope to have changed their mind by the end."),
    h3("The plots"),
    p("The three", em("charts"), " of this project will represent:"),
    tags$li("Gender Difference among Majors"),
    tags$li("Main contributor to Wage Gap"),
    tags$li("Gender difference in Occupations"),
    h3("Authors:"),
    tags$li("Matthew Cho"),
    tags$li("Jason Li"),
    tags$li("Colson Xu"),
    tags$li("Zhennan Zhou")
  ),

  # tab 2
  tabPanel(
    "Gender Difference in Majors",
    titlePanel("Gender Difference in Majors"),
    p(
      "The bar graph below uses the data from
            National Center for Education Statistics,
            which demonstrates the percentage
            of males and females in different majors.
          Specifically, the two bar plots compare the number
        of male students and female students in the top-20 male
        dominant and female dominant majors.
          The datasets can be chosen with", strong("different checkboxes"),
      "that categorize different majors."
    ),
    p(
      "The", strong("Show trend"),
      "checkbox is used to show the percentage of male and female
      in different salary levels,
      which illustrates the general trend between the percentage
      of male/female and their median salary."
    ),
    p("We have concluded from these charts and analysis that across the country,
        females tend not to have highly-paid majors, such as engineering and
        information science, as female tends to have
        less salary than males on average. The percentage of female and
        male in the industries diverges as the median salary increases. Even
      though this is the case(male-dominant majors),
        the salary difference is not as big as we
      imagined in the first place. There are some industries,
      such as Health Science and Communication, where females are shining out.
     Females are making more contributions and earning more
      in those industries."),
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
        hr(),
        plotOutput("trend_plot_male", height = "450px"),
        hr(),
        plotOutput("diff_plot_least", height = "600px"),
        hr(),
        plotOutput("trend_plot_female", height = "450px")
      )
    )
  ),

  # tab 3
  tabPanel(
    "The Main Contributor to Wage Gap",
    titlePanel("Female Percentage vs. Major Median Salary"),
    p(
      "In this study, we compare how males and females choose
        their major, and the major salaries in five years.
        We believe this study can best reflect the reason behind
        gender salary gap. The reason is that through comparing
        major choice and how well ", strong("that major "), "pays,
        we eliminate individual factors that create wage gap like
        how one performs in a job and how they are willing to show up
        anytime their boss calls without prior notice.
        Thus, we can isolate the one variable we want to study: ",
      strong("INDIVIDUAL MAJOR CHOICE"), br(), br(),
      "Furthermore, the salary data we use is the median base
        salary after working in a position for 5 years. ", strong(
        "It eliminates confounding variables like pregnancy leave
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
    p(
      "In this tab, users can select an input to get the ",
      strong("top 10 occupations based on salary"), "or",
      strong("bottom 10 occupations based on salary."),
    "These two datas are shown
    through bar graphs with the x-axis displaying the different occupations and
    y-axis displaying the salaries.", "Underneath the salary graphs, users can
    select one occupations from any", strong("top 10 most paid"), "or",
      strong("bottom 10 least paid"),
    "occupations to view the gender differences in
    the pie chart."
    ),
    p("From the bar graph and the pie chart that is shown below. The bar graph
 shows the top 10 and the bottom 10 jobs. The pie chart shows the
gender percentage in each of those jobs. We can conclude that the most of the
top 10 jobs have a higher percentage of male then female, while the bottom 10
jobs shows higher percentage of female then male. This proves that when
calculating the median salary for male and female, it shows that male will
have a higher salary from this dataset which would be biased and not calculated
accordingly with other factors."),
    sidebarLayout(
      sidebarPanel(
        selectInput("work",
          label = h3("Top vs Bottom"),
          choices = list("Top" = 1, "Bottom" = 2)
        ),
        radioButtons("lower_perc",
          "Lower Salary Occupations",
          choices = lower_perc$Occupation
        ),
        radioButtons("higher_perc",
          "Higher Salary Occupations",
          choices = higher_perc$Occupation
        )
      ),
      mainPanel(
        plotOutput("job_plot", width = "900px", height = "600px"), fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"),
            plotOutput("lowest_plot", width = "450px", height = "600px"),
            plotOutput("highest_plot", width = "450px", height = "600px")
          )
        )
      )
    )
  )
)
