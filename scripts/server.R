library(shiny)
library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
library(ggrepel)


source("analysis.R")

major_data <- read.csv("data/major_enrollment.csv", stringsAsFactors = F)
major_enrollment1 <- read.csv("data/major_enrollment.csv",
  stringsAsFactors = FALSE
)

jobs <- read.csv("data/job_salary_and_gender_percentage.csv",
  stringsAsFactors = FALSE
)


server <- function(input, output) {
  diff_data <- reactive({
    difference <- major_enrollment %>%
      arrange(-`Percentage of Male`)
    difference <- left_join(difference, best_25, by = "major")
    difference <- melt(difference,
      id.vars = c("major", "median_pay"),
      variable.name = "type",
      value.name = "percentage"
    )
    difference <- drop_na(difference)

    difference <- filter(difference, major %in% input$major_list_top)
    return(difference)
  })
  
  output$diff_plot <- renderPlot({
    diff_plot <- ggplot(diff_data()) +
      geom_bar(
        stat = "identity",
        mapping = aes(
          x = major,
          y = percentage,
          fill = type
        )
      ) +
      geom_text(aes(
        x = major,
        y = percentage,
        label = paste0(round(percentage), "%")
      ),
      vjust = 1.6,
      color = "black",
      size = 3
      ) +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = "Male Dominant Majors",
        x = "Major",
        y = "Number of people in the major by gender",
        fill = ""
      ) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          vjust = 0.5,
          face = "bold"
        )
      ) +
      theme(legend.position = "bottom", legend.box = "horizontal") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(diff_plot)
  })

  output$trend_plot_male <- renderPlot({
    if (input$male_trend) {
      trend <- ggplot(diff_data()) +
        geom_smooth(
          method = "loess",
          mapping = aes(
            x = median_pay,
            y = percentage,
            fill = type,
            color = type
          ),
          span = 1,
          alpha = .2
        ) +
        scale_color_brewer(palette = "Set3") +
        scale_fill_brewer(palette = "Set3") +
        labs(
          x = "Median Salary",
          y = "Percantage"
        ) +
        geom_text_repel(aes(x = median_pay, y = percentage, label = major),
          size = 3, alpha = 0.75
        ) +
        theme(legend.position = "bottom", legend.box = "horizontal")
    } else {
      trend <- 0
    }
    return(trend)
  })

  # second plot
  diff_data_least <- reactive({
    difference_female <- major_enrollment %>%
      arrange(-`Percentage of Female`)
    difference_female <- left_join(difference_female,
      worst_25,
      by = "major"
    )
    difference_female <- filter(
      difference_female,
      major %in% input$major_list_least
    )


    difference_female <- melt(difference_female,
      id.vars = c("major", "median_pay"),
      variable.name = "type",
      value.name = "percentage"
    )
    difference_female <- drop_na(difference_female)

    return(difference_female)
  })

  output$diff_plot_least <- renderPlot({
    diff_plot_least <- ggplot(diff_data_least()) +
      geom_bar(
        stat = "identity",
        mapping = aes(
          x = major,
          y = percentage,
          fill = factor(type,
            levels = c(
              "Percentage of Female",
              "Percentage of Male"
            )
          )
        ),
        position = "dodge"
      ) +
      geom_label_repel(aes(
        x = major,
        y = percentage,
        label = paste0(round(percentage), "%")
      ),
      vjust = 3,
      color = "black",
      size = 3,
      nudge_x = -0.2,
      nudge_y = 1,
      force = 10
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Female Dominant Majors",
        x = "Major",
        y = "Number of people in the major by gender",
        fill = ""
      ) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          vjust = 0.5,
          face = "bold"
        )
      ) +
      theme(legend.position = "bottom", legend.box = "horizontal") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(diff_plot_least)
  })

  output$trend_plot_female <- renderPlot({
    if (input$female_trend) {
      trend_female <- ggplot(diff_data_least()) +
        geom_smooth(
          method = "loess",
          mapping = aes(
            x = median_pay,
            y = percentage,
            fill = type,
            color = type
          ),
          span = 1,
          alpha = .2
        ) +
        scale_color_brewer(palette = "Set2") +
        scale_fill_brewer(palette = "Set2") +
        labs(
          x = "Median Salary",
          y = "Percantage"
        ) +
        geom_label_repel(aes(
          x = median_pay,
          y = percentage,
          label = major
        ), size = 3, alpha = 0.75) +
        theme(legend.position = "bottom", legend.box = "horizontal")
    } else {
      trend_female <- 0
    }
    return(trend_female)
  })

  ############################ Colson ############################
  trend_line <- reactive({
    input$trend
  })

  perc_range <- reactive({
    range <- input$perc_select
    major_enrollment <- filter(
      major_enrollment1,
      perc_to_double(perc_female) > range[1],
      perc_to_double(perc_female) < range[2]
    )
  })

  perc_to_double <- function(x) {
    as.numeric(gsub("%", "", x)) / 100
  }

  output$female_perc_vs_major_pay <- renderPlotly({
    p <- plot_ly(
      x = perc_to_double(perc_range()$perc_female),
      y = perc_range()$median_pay,
      type = "scatter",
      mode = "markers",
      color = perc_range()$median_pay,
      size = perc_range()$median_pay,
      showlegend = F,
      text = paste(
        "Major:", perc_range()$major,
        "<br>Percentage of Female:",
        perc_range()$perc_female,
        "<br>Median Salary After 5 Years:",
        perc_range()$median_pay
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Major Female Percentage vs Major Median Pay",
        xaxis =
          list(title = "Percentage of Female in Majors"),
        yaxis =
          list(title = "Median Salary for Majors After 5 Years")
      )

    if (trend_line()) {
      p <- add_lines(p, y = ~ fitted(loess(
        perc_range()$median_pay ~
        perc_to_double(perc_range()$perc_female)
      )))
    }
    p
  })

  output$male_perc_vs_major_pay <- renderPlotly({
    p <- plot_ly(
      x = perc_to_double(perc_range()$perc_male),
      y = perc_range()$median_pay,
      type = "scatter",
      mode = "markers",
      color = perc_range()$median_pay,
      size = perc_range()$median_pay,
      showlegend = F,
      text = paste(
        "Major:", perc_range()$major,
        "<br>Percentage of Male:",
        perc_range()$perc_male,
        "<br>Median Salary After 5 Years:",
        perc_range()$median_pay
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Major Male Percentage vs Major Median Pay",
        xaxis =
          list(title = "Percentage of Male in Majors"),
        yaxis =
          list(title = "Median Salary for Majors After 5 Years")
      )

    if (trend_line()) {
      p <- add_lines(p, y = ~ fitted(loess(
        perc_range()$median_pay ~
        perc_to_double(perc_range()$perc_male)
      )))
    }
    p
  })
  ############################ Matthew ############################

  # this data frame shows the bottom 10 paid jobs and the gender percentage
  # in those jobs
  least <- reactive({
    low <- jobs %>%
      select(
        Occupation, Median.earnings.total,
        Percentage.of.women.in.occupational.group
      ) %>%
      rename(
        Occupation = Occupation, salary = Median.earnings.total,
        women = Percentage.of.women.in.occupational.group
      ) %>%
      mutate(men = 100 - women) %>%
      arrange(salary) %>%
      head(10) %>%
      as.data.frame()
    low$Occupation[1] <- "Attendants"
    low$Occupation[3] <- "Food Workers"
    low$Occupation[4] <- "Service Workers"
    low$Occupation[6] <- "Agricultural Workers"
    low$Occupation[7] <- "Cafeteria Attendants"
    low$Occupation[8] <- "Clothing Workers"
    low$Occupation[10] <- "Housekeeping Cleaners"


    return(low)
  })

  # this data frameshows the top 10 paid jobs and the gender percentage
  # in those jobs
  most <- reactive({
    high <- jobs %>%
      select(
        Occupation, Median.earnings.total,
        Percentage.of.women.in.occupational.group
      ) %>%
      rename(
        Occupation = Occupation, salary = Median.earnings.total,
        women = Percentage.of.women.in.occupational.group
      ) %>%
      mutate(men = 100 - women) %>%
      arrange(-salary) %>%
      head(10) %>%
      as.data.frame()
    high$Occupation[1] <- "Surgeons"
    high$Occupation[4] <- "Engineering Managers"

    return(high)
  })

  # creates a graph of the top 10 jobs and the bottom 10 jobs
  output$job_plot <- renderPlot({
    if (input$work == 1) {
      data_f <- most()
      title <- "Top 10 paid jobs in U.S"
    } else {
      data_f <- least()
      title <- "Least 10 paid jobs in U.S"
    }
    ggplot(data = data_f) +
      geom_bar(
        stat = "identity",
        mapping = aes(x = Occupation, y = salary)
      ) +
      labs(x = "Job Title", y = "Salary", title = title) +
      theme_bw() +
      theme(plot.title = element_text(
        size = 30, face = "bold",
        hjust = 0.5
      )) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(text = element_text(size = 15))
  })
}
