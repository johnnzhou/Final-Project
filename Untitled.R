library(dplyr)
library(ggplot2)
library(shiny)
library(lintr)


jobs <- read.csv("data/job_salary_and_gender_percentage.csv", stringsAsFactors = FALSE)
server <- function(input, output) {

least <- reactive({
least <- tbl_df(jobs) %>%
    select(Occupation, Median.earnings.total) %>%
    arrange(Median.earnings.total) %>%
    head(10) %>%
    as.data.frame()
return(least)
})

most <- reactive({
most <- tbl_df(jobs) %>%
    select(Occupation, Median.earnings.total) %>%
    arrange(-Median.earnings.total) %>%
    head(10) %>%
    as.data.frame()
return(most)
})

output$top_job <- renderPlot({
    ggplot(data = most()) %>%
    geom_bar(stat = "identity", mapping = aes(x = Occupation, y = Median.earnings.total,
                                              fill = Occupation)) +
    labs(x = "Job Title", y = "Salary", title = "Top 10 paid jobs in U.S") +
    theme_bw() + theme(plot.title = element_text(size = 20, face = "bold",
                                                 hjust = 0.5))
})

output$low_job <- renderPlot({ 
    ggplot(data = least()) %>%
    geom_bar(stat = "identity", mapping = aes(x = Occupation, y = Median.earnings.total,
                                                  fill = Occupation)) +
    labs(x = "Job Title", y = "Salary", title = "Least 10 paid jobs in U.S") +
    theme_bw() + theme(plot.title = element_text(size = 20, face = "bold",
                                                     hjust = 0.5))
    })





}

