library(shiny)
library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
source("analysis.R")

major_enrollment1 <- read.csv("../data/major_enrollment.csv", 
                              stringsAsFactors = FALSE)
jobs <- read.csv("../data/job_salary_and_gender_percentage.csv",
                 stringsAsFactors = FALSE)


server <- function(input, output) {
  
  diff_data <- reactive({
    difference <- major_enrollment %>% 
      arrange(-`Percentage of Male`) %>% 
      filter(major %in% input$major_list_top)
    difference <- melt(difference,
                       id.vars = "major",
                       variable.name = "type",
                       value.name = "percentage")
    return(difference)
  })
  
  output$diff_plot <- renderPlot({
    diff_plot <- ggplot(diff_data()) +
      geom_bar(stat = "identity",
               mapping = aes(
                 x = major,
                 y = percentage,
                 fill = type,
                 width = 0.8
               )
      )+
      geom_text(aes(x = major,
                    y = percentage,
                    label = paste0(round(percentage), "%")), 
                vjust = 1.6, 
                color = "black",
                size = 3
      )+
      scale_fill_brewer(palette = "Set3")+
      labs(
        title = "Male Dominant Majors",
        x = "",
        y = "Number of people in the major by gender",
        fill = ""
      )+
      theme(
        plot.title = element_text(
          hjust = 0.5,
          vjust = 0.5,
          face = "bold"
        )
      )+
      theme(legend.position="bottom", legend.box = "horizontal")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(diff_plot)
  })
  
  # second plot
  diff_data_least <- reactive({
    difference_least <- major_enrollment %>% 
      arrange(`Percentage of Female`) %>% 
      filter(major %in% input$major_list_least)
    difference_least <- melt(difference_least,
                             id.vars = "major",
                             variable.name = "type",
                             value.name = "percentage")
    return(difference_least)
  })
  output$diff_plot_least <- renderPlot({
    diff_plot_least <- ggplot(diff_data_least()) +
      geom_bar(stat = "identity",
               mapping = aes(
                 x = major,
                 y = percentage,
                 fill = type,
                 width=0.8
               )
      )+
      geom_text(aes(x = major,
                    y = percentage,
                    label = paste0(round(percentage),"%")), 
                vjust=1.6, 
                color="black",
                size = 3
      )+
      scale_fill_brewer(palette = "Set2")+
      labs(
        title = "Female Dominant Majors",
        x = "",
        y = "Number of people in the major by gender",
        fill = ""
      )+
      theme(
        plot.title = element_text(
          hjust = 0.5,
          vjust = 0.5,
          face = "bold"
        )
      )+
      theme(legend.position = "bottom", legend.box = "horizontal")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(diff_plot_least)
  })
  
  ############################ Colson ############################
  trend_line <- reactive({
    input$trend
  })
  
  perc_range <- reactive({
    range <- input$perc_select
    major_enrollment <- filter(major_enrollment1,
                               perc_to_double(perc_female) > range[1],
                               perc_to_double(perc_female) < range[2])
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
      text = paste("Major:", perc_range()$major,
                   "<br>Percentage of Female:",
                   perc_range()$perc_female,
                   "<br>Median Salary After 5 Years:",
                   perc_range()$median_pay),
      hoverinfo = "text"
      
    ) %>%
      layout(title = "Major Female Percentage vs Major Median Pay",
             xaxis =
               list(title = "Percentage of Female in Majors"),
             yaxis =
               list(title = "Median Salary for Majors After 5 Years"))
    
    if (trend_line()) {
      p <- add_lines(p, y = ~fitted(loess(
        perc_range()$median_pay ~
          perc_to_double(perc_range()$perc_female))))
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
      text = paste("Major:", perc_range()$major,	
                   "<br>Percentage of Male:",	
                   perc_range()$perc_male,	
                   "<br>Median Salary After 5 Years:",	
                   perc_range()$median_pay),	
      hoverinfo = "text"	
      
    ) %>%	
      layout(title = "Major Male Percentage vs Major Median Pay",	
             xaxis =	
               list(title = "Percentage of Male in Majors"),	
             yaxis =	
               list(title = "Median Salary for Majors After 5 Years"))	
    
    if (trend_line()) {	
      p <- add_lines(p, y = ~fitted(loess(	
        perc_range()$median_pay ~	
          perc_to_double(perc_range()$perc_male))))	
    }
    p
  })
  ############################ Matthew ############################    
  
  #this data frame shows the bottom 10 paid jobs and the gender percentage
  #in those jobs
  least <- reactive({
    low <- jobs %>%
      select(Occupation, Median.earnings.total,
             Percentage.of.women.in.occupational.group) %>%
      rename(Occupation = Occupation, salary = Median.earnings.total,
             women = Percentage.of.women.in.occupational.group) %>%
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
  
  #this data frameshows the top 10 paid jobs and the gender percentage
  #in those jobs
  most <- reactive({
    high <- jobs %>%
      select(Occupation, Median.earnings.total,
             Percentage.of.women.in.occupational.group) %>%
      rename(Occupation = Occupation, salary = Median.earnings.total, 
             women = Percentage.of.women.in.occupational.group) %>%
      mutate(men = 100 - women) %>%
      arrange(-salary) %>%
      head(10) %>%
      as.data.frame()
    high$Occupation[1] <- "Surgeons"
    high$Occupation[4] <- "Engineering Managers"
    
    return(high)
  })
  
  #creates a graph of the top 10 jobs and the bottom 10 jobs
  output$job_plot <- renderPlot({
    if (input$work == 1) {
      data_f <- most()
      title <- "Top 10 paid jobs in U.S"
    } else {
      data_f <- least()
      title <- "Least 10 paid jobs in U.S"
    }
    ggplot(data = data_f) +
      geom_bar(stat = "identity", mapping = aes(x = Occupation, y = salary)) +
      labs(x = "Job Title", y = "Salary", title = title) +
      theme_bw() + theme(plot.title = element_text(size = 30, face = "bold",
                                                   hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(text = element_text(size = 15))
  })
  ############################ Jason #################################
  blank_theme <- theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold")
    )
  
  diff_1 <- reactive({
    difference_1 <- lower_perc %>%
      filter(Occupation %in% input$lower_perc)
    difference_1 <- melt(difference_1,
                         id.vars = c("Occupation", "salary"),
                         variable.name = "gender",
                         value.name = "percentage")
    return(difference_1)
  })
  
  output$lowest_plot <- renderPlot({
    lowest_plot <- ggplot(diff_1()) +
      geom_bar(stat = "identity",
               mapping = aes(
                 x = Occupation,
                 y = percentage,
                 fill = gender,
                 label = gender
               )
      ) + coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set2")+
      geom_text(aes(x = Occupation,
                    y = percentage,
                    label = paste0(percentage, "%")), 
                vjust = 1,
                hjust = -3,
                color = "black",
                size = 3
      )+
      labs(
        title = "Lowest Salary Occupation Gender ") + 
      blank_theme +
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text=element_text(size=15),
            axis.title=element_text(size=20,face="bold"),
            axis.text.x=element_blank())
 
    return(lowest_plot)
    
  })
  
  # Higher Pay Occupation
  diff_2 <- reactive({
    difference_2 <- higher_perc %>%
      filter(Occupation %in% input$higher_perc)
    difference_2 <- melt(difference_2,
                         id.vars = c("Occupation", "salary"),
                         variable.name = "gender",
                         value.name = "percentage")
    return(difference_2)
  })
  
  output$highest_plot <- renderPlot({
    highest_plot <- ggplot(diff_2()) +
      geom_bar(stat = "identity",
               mapping = aes(
                 x = Occupation,
                 y = percentage,
                 fill = gender,
                 label = gender
               )
      ) + coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3")+
      geom_text(aes(x = Occupation,
                    y = percentage,
                    label = paste0(percentage, "%")), 
                vjust = 1,
                hjust = -3,
                color = "black",
                size = 3
      ) +
      labs(
        title = "Highest Salary Occupation Gender ") + 
      blank_theme + 
      theme(plot.title = element_text(hjust = 0.5), 
      axis.text=element_text(size=15),
      axis.title=element_text(size=20,face="bold"),
      axis.text.x=element_blank())
    return(highest_plot)
    
  })
  
}