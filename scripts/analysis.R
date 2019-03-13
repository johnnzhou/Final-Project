library("dplyr")
library("tidyr")
library("reshape2")
library("ggplot2")
library("plotly")

## John
major_enrollment <-
    read.csv("../data/major_enrollment.csv", stringsAsFactors = FALSE)

major_enrollment$diff <- major_enrollment$male - major_enrollment$female
major_enrollment <- major_enrollment %>% 
                    mutate(total = male + female,
                            perc_male = (male / total) * 100,
                            perc_female = (female / total) * 100) %>% 
                    select(major, perc_male, perc_female, diff)
major_list_top <- major_enrollment %>% 
    arrange(-diff) %>%
    select(major) %>%
    head(20)

major_list_least <- major_enrollment %>% 
    arrange(diff) %>%
    select(major) %>% 
    head(20)

major_enrollment <- major_enrollment[-4]
colnames(major_enrollment) <- c("major", 
                          "Percentage of Male", 
                          "Percentage of Female"
                          )

major_list_top <- unlist(major_list_top, use.names = FALSE)
major_list_least <- unlist(major_list_least, use.names = FALSE)


overview <- my_data <- read.delim("../README.md")
                
