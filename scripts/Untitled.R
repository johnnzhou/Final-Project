jobs <- read.csv("data/job_salary_and_gender_percentage.csv",
                 stringsAsFactors = FALSE)


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