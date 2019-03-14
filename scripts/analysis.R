library("dplyr")
library("tidyr")
library("reshape2")
library("ggplot2")
library("plotly")

## John
major_enrollment <-
  read.csv("data/major_enrollment.csv", stringsAsFactors = FALSE)
best_25 <- read.csv("data/best_25.csv", stringsAsFactors = FALSE)
colnames(best_25) <- c("major", "median_pay")

worst_25 <- read.csv("data/worst_25.csv", stringsAsFactors = FALSE)
colnames(worst_25) <- c("major", "median_pay")

major_enrollment$diff <- major_enrollment$male - major_enrollment$female
major_enrollment <- major_enrollment %>%
  mutate(
    total = male + female,
    perc_male = (male / total),
    perc_female = (female / total)
  ) %>%
  select(major, perc_male, perc_female, diff)

major_enrollment <- major_enrollment[-4]
colnames(major_enrollment) <- c(
  "major",
  "Percentage of Male",
  "Percentage of Female"
)


major_list_top <- major_enrollment %>%
  arrange(-`Percentage of Male`)
major_list_top <- left_join(major_list_top, best_25, by = "major")
major_list_top <- melt(major_list_top,
  id.vars = c("major", "median_pay"),
  variable.name = "type",
  value.name = "percentage"
)
major_list_top <- drop_na(major_list_top) %>%
  select(major) %>%
  head(20)
major_list_top <- unlist(major_list_top, use.names = FALSE)


major_list_least <- major_enrollment %>%
  arrange(-`Percentage of Female`)
major_list_least <- left_join(major_list_least, worst_25, by = "major")
major_list_least <- melt(major_list_least,
  id.vars = c("major", "median_pay"),
  variable.name = "type",
  value.name = "percentage"
)
major_list_least <- drop_na(major_list_least) %>%
  select(major) %>%
  head(20)
major_list_least <- unlist(major_list_least, use.names = FALSE)


##################Jason#####################
jobs <- read.csv("data/job_salary_and_gender_percentage.csv",
                 stringsAsFactors = FALSE)

jobs_filter <- jobs %>%
  select(Occupation, Median.earnings.total,
         Percentage.of.women.in.occupational.group) %>%
  rename(Occupation = Occupation, salary = Median.earnings.total,
         women = Percentage.of.women.in.occupational.group) %>%
  mutate(men = 100 - women)

lower_perc <- jobs_filter %>%
  arrange(salary) %>%
  head(10)
lower_perc$Occupation[1] <- "Attendants"
lower_perc$Occupation[3] <- "Food Workers"
lower_perc$Occupation[4] <- "Service Workers"
lower_perc$Occupation[6] <- "Agricultural Workers"
lower_perc$Occupation[7] <- "Cafeteria Attendants"
lower_perc$Occupation[8] <- "Clothing Workers"
lower_perc$Occupation[10] <- "Housekeeping Cleaners"


higher_perc <- jobs_filter %>%
  arrange(-salary) %>%
  head(10)
higher_perc$Occupation[1] <- "Surgeons"
higher_perc$Occupation[4] <- "Engineering Managers"

