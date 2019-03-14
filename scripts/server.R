## prepare necessary packages
# install.packages("tableHTML")
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggrepel")
# install.packages("plotly")

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggrepel)
library(lintr)
source("analysis.R")

major_data <- read.csv("data/major_enrollment.csv", stringsAsFactors = F)
jobs <- read.csv("data/job_salary_and_gender_percentage.csv",
                 stringsAsFactors = FALSE)

server <- function(input, output) {

    ############################ John ############################
    diff_data_top <- reactive({
        difference <- major_enrollment %>%
            arrange(-`Percentage of Male`) %>%
            left_join(best_25, by = "major") %>%
            melt(id.vars = c("major", "median_pay"),
                 variable.name = "type",
                 value.name = "percentage") %>%
            drop_na() %>%
            filter(major %in% input$major_list_top)
        return(difference)
    })

    output$diff_plot_top <- renderPlotly({
        plot_ly(diff_data_top(),
                text = paste0("Major: ",
                              diff_data_top()$major,
                              "<br>Percentage: ",
                              double_to_perc(diff_data_top()$percentage)),
                x = ~major,
                y = ~percentage,
                type = "bar",
                hoverinfo = "text",
                color = ~type) %>%
            layout(title = "Best Paid Majors and Their Gender Percentage",
                   xaxis = list(title = "Majors"),
                   yaxis = list(title = "Gender Distribution"),
                   barmode = "stack")
    })

    output$trend_plot_male <- renderPlot({
        if (input$male_trend) {
            trend <- ggplot(diff_data_top()) +
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
                geom_label_repel(aes(x = median_pay, y = percentage,
                                     label = major), size = 3, alpha = 0.75) +
                theme(legend.position = "bottom", legend.box = "horizontal")
        } else {
            trend <- 0
        }
        return(trend)
    })

    # Second Plot
    diff_data_least <- reactive({
        difference_female <- major_enrollment %>%
            arrange(-`Percentage of Female`) %>%
            left_join(worst_25, by = "major") %>%
            filter(major %in% input$major_list_least) %>%
            melt(id.vars = c("major", "median_pay"),
                 variable.name = "type",
                 value.name = "percentage") %>%
            drop_na()
        return(difference_female)
    })

    output$diff_plot_least <- renderPlotly({
        plot_ly(diff_data_least(),
                text = paste0("Major: ",
                              diff_data_least()$major,
                              "<br>Percentage: ",
                              double_to_perc(diff_data_least()$percentage)),
                x = ~major,
                y = ~percentage,
                type = "bar",
                hoverinfo = "text",
                color = ~type) %>%
            layout(title = "Best Paid Majors and Their Gender Percentage",
                   xaxis = list(title = "Majors"),
                   yaxis = list(title = "Gender Distribution"),
                   barmode = "stack")
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
                geom_label_repel(aes(x = median_pay, y = percentage,
                                     label = major), size = 3, alpha = 0.75) +
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

    perc_range_female <- reactive({
        range <- input$perc_select
        filter(major_data,
               perc_to_double(perc_female) > range[1],
               perc_to_double(perc_female) < range[2]
        )
    })

    perc_range_male <- reactive({
        range <- input$perc_select
        filter(major_data,
               perc_to_double(perc_male) > range[1],
               perc_to_double(perc_male) < range[2]
        )
    })

    output$female_perc_vs_major_pay <- renderPlotly({
        p <- plot_ly(
            x = perc_to_double(perc_range_female()$perc_female),
            y = perc_range_female()$median_pay,
            type = "scatter",
            mode = "markers",
            color = perc_range_female()$median_pay,
            size = perc_range_female()$median_pay,
            showlegend = F,
            text = paste("Major:", perc_range_female()$major,
                         "<br>Percentage of Female:",
                         perc_range_female()$perc_female,
                         "<br>Median Salary After 5 Years:",
                         perc_range_female()$median_pay),
            hoverinfo = "text"
            ) %>%
            layout(
                title = "Major Female Percentage vs Major Median Pay",
                xaxis = list(title = "Percentage of Female in Majors"),
                yaxis = list(title = "Median Salary for Majors After 5 Years")
            )

        if (trend_line()) {
            p <- add_lines(p, y = ~ fitted(loess(
                perc_range_female()$median_pay ~
                perc_to_double(perc_range_female()$perc_female))))
        }
        p
    })

    output$male_perc_vs_major_pay <- renderPlotly({
        p <- plot_ly(
            x = perc_to_double(perc_range_male()$perc_male),
            y = perc_range_male()$median_pay,
            type = "scatter",
            mode = "markers",
            color = perc_range_male()$median_pay,
            size = perc_range_male()$median_pay,
            showlegend = F,
            text = paste("Major:", perc_range_male()$major,
                         "<br>Percentage of Male:",
                         perc_range_male()$perc_male,
                         "<br>Median Salary After 5 Years:",
                         perc_range_male()$median_pay),
            hoverinfo = "text"
            ) %>%
            layout(
                title = "Major Male Percentage vs Major Median Pay",
                xaxis = list(title = "Percentage of Male in Majors"),
                yaxis = list(title = "Median Salary for Majors After 5 Years")
            )

        if (trend_line()) {
            p <- add_lines(p, y = ~ fitted(loess(
                perc_range_male()$median_pay ~
                perc_to_double(perc_range_male()$perc_male))))
        }
        p
    })

    ############################ Matthew ############################
    # This data frame shows the bottom 10 paid jobs and the gender percentage
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
            mutate(men = 100 - women) %>% arrange(salary) %>% head(10) %>%
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

    # This data frameshows the top 10 paid jobs and the gender percentage
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
            ) %>% mutate(men = 100 - women) %>% arrange(-salary) %>%
            head(10) %>% as.data.frame()
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
            title <- "Bottom 10 paid jobs in U.S"
        }

        ggplot(data = data_f) +
            geom_bar(
            stat = "identity",
            mapping = aes(x = Occupation, y = salary, fill = salary)
        ) +
        labs(x = "Job Title", y = "Salary", title = title) +
        theme_bw() +
        theme(plot.title = element_text(
            size = 30, face = "bold",
            hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(text = element_text(size = 15, face = "bold"),
            legend.position = "none")
    })

    ############################ Jason #################################
    blank_theme <- theme_minimal() +
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 14, face = "bold")
        )

    lowest_paid <- reactive({
        difference_1 <- lower_perc %>%
            filter(Occupation %in% input$lower_perc) %>%
            melt(id.vars = c("Occupation", "salary"),
                 variable.name = "gender",
                 value.name = "percentage")
        return(difference_1)
    })

    output$lowest_plot <- renderPlotly({
        plot_ly(
            lowest_paid(),
            labels = ~gender,
            values = ~percentage,
            hoverinfo = "none",
            marker = list(colors = c("#E76F51", "#0b93ae")),
            showlegend = F,
            type = "pie"
        ) %>%
            layout(title = paste(lowest_paid()$Occupation,
                                 "Gender Distribution"))
    })

    # Higher Pay Occupation
    highest_paid <- reactive({
        difference_2 <- higher_perc %>%
            filter(Occupation %in% input$higher_perc) %>%
            melt(id.vars = c("Occupation", "salary"),
                 variable.name = "gender",
                 value.name = "percentage")
        return(difference_2)
    })

    output$highest_plot <- renderPlotly({
        plot_ly(
            highest_paid(),
            labels = ~gender,
            values = ~percentage,
            hoverinfo = "none",
            marker = list(colors = c("#E76F51", "#0b93ae")),
            type = "pie"
        ) %>%
            layout(title = paste(highest_paid()$Occupation,
                                 "Gender Distribution"))
    })
}
