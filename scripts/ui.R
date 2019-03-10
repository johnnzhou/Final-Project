library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "One Reason for Gender Wage Gap",

    # tab 1
    tabPanel(
        "Overview",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                
            ),
            mainPanel(
                
            )
        )
    ),

    # tab 2
    tabPanel(
        "Gender Distribution in Majors",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                
            ),
            mainPanel(
                
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
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                
            ),
            mainPanel(
                
            )
        )
    )
)
