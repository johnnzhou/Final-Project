# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("plotly")

library("dplyr")
library("tidyr")
library("reshape2")
library("ggplot2")
library("plotly")

## John
major_enrollment <- read.csv("../data/major_enrollment.csv", stringsAsFactors = FALSE)

major_enrollment$diff <- major_enrollment$Males - major_enrollment$Females
major_list <- major_enrollment %>% 
                arrange(-diff) %>% 
                head(10) %>% 
                select(Major)
major_list <- unlist(major_list, use.names = FALSE)
                
difference <- major_enrollment %>% 
    arrange(-diff) %>% 
    select(Major, Males, Females) %>% 
    melt(id.vars = "Major",
         variable.name = "Gender",
         value.name = "Number") %>% 
    head(10)
                
diff_plot <- ggplot(difference) +
    geom_bar(stat = "identity",
             mapping = aes(
                 x = Major,
                 y = Number,
                 fill = Gender    
             )
    )+
    geom_text(mapping = aes(
        x = Major,
        y = Number,
        label = Number
    ))+
    scale_fill_brewer(palette = "Set3")+
    labs(
        x = "Majors",
        y = "Difference of number of people in the major by gender"
    )+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))