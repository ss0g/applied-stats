setwd("C:/Users/tntje/work/school/applied-stats/s2/assignment10")

library(ggplot2)
library(GGally)
library(dplyr)

obama <- read.csv("Election08.csv")

printf <- function(...) {
    print(sprintf(...))
}

# q1

# a
regres01 <- glm(ObamaWin ~ Income, data = obama, family = binomial("logit"))
summary(regres01)

# b
regres02 <- glm(ObamaWin ~ HS, data = obama, family = binomial("logit"))
summary(regres02)

# c
regres03 <- glm(ObamaWin ~ BA, data = obama, family = binomial("logit"))
summary(regres03)

# d
regres04 <- glm(ObamaWin ~ Dem.Rep, data = obama, family = binomial("logit"))
summary(regres04)

# q2
p_for_income_50000 <- predict.glm(regres01, newdata = data.frame("Income" = c(50000)), type = "response")
printf("if income=50000, prob. of obama win is %.3f", p_for_income_50000)

income_for_p_50_percent <- -(-12.43) / (0.0003494)
printf("if prob. = 50%%, income = %.3f", income_for_p_50_percent)

income_for_odds_0_25 <- (log(0.25) - (-12.43)) / (0.0003494)
printf("if odds = 0.25, income = %.3f", income_for_odds_0_25)

# q3
odds_ratio <- exp(0.0003494)
printf("odds ratio: %.3f", odds_ratio)

# q4
zcr <- 1.959964
sb <- 0.0001050
low <- exp(0.0003494 - (zcr * sb))
high <- exp(0.0003494 + (zcr * sb))
printf("95%% confidence interval for odds ratio: (%.8f, %.8f)", low, high)

# q5
obama$IncomeTh <- 0.001 * obama$Income
regres05 <- glm(ObamaWin ~ IncomeTh, data = obama, family = binomial("logit"))
summary(regres05)

# q6
odds_ratio2 <- exp(0.3494)
printf("odds ratio (with IncomeTh): %.3f", odds_ratio2)

sb2 <- 1.050e-7
low2 <- exp(0.3494 - (zcr * sb2))
high2 <- exp(0.3494 + (zcr * sb2))
printf("95%% confidence interval for odds ratio (with IncomeTh): (%.8f, %.8f)", low2, high2)
