library("dplyr")

best_25 <- read.csv("../data/best_25.csv", stringsAsFactors = FALSE)
worst_25 <- read.csv("../data/worst_25.csv", stringsAsFactors = FALSE)


colnames(best_25) <- c("Occupation", "Salary")
colnames(worst_25) <- c("Occupation", "Salary")


