setwd("C:/Users/tntje/work/school/applied-stats/assignment13")

library(ggplot2)

# 14.20
bb <- read.csv("blackbears.csv")
regres1 <- lm(bb$hr~bb$age+bb$weight)
summary(regres1)
anova(regres1)
qqp1 <- ggplot(data = bb, aes(sample = resid(regres1))) + geom_qq()
qqp1 <- qqp1 + labs(
  x = "Theoretical",
  y = "Sample",
  title = "Normal probability plot of resid(regres1)"
)
qqp1
rp1 <- ggplot(data = bb, aes(x = predict(regres1), y = resid(regres1))) + geom_point()
rp1 <- rp1 + labs(
  x = "predict(regres1)",
  y = "resid(regres1)",
  title = "Residual plot of regres1"
)
rp1

# 14.27
prey <- read.csv("prey and predation.csv")
regres2 <- lm(prey$time~prey$length+prey$speed)
f_regres2 <- function(length, speed) {
  return(regres2$coefficients[1] + regres2$coefficients[2] * length + regres2$coefficients[3] * speed)
}
summary(regres2)
anova(regres2)
qqp2 <- ggplot(data = prey, aes(sample = resid(regres2))) + geom_qq()
qqp2 <- qqp2 + labs(
  x = "Theoretical",
  y = "Sample",
  title = "Normal probability plot of resid(regres2)"
)
qqp2
rp2 <- ggplot(data = prey, aes(x = predict(regres2), y = resid(regres2))) + geom_point()
rp2 <- rp2 + labs(
  x = "predict(regres2)",
  y = "resid(regres2)",
  title = "Residual plot of regres2"
)
rp2

prey$ls <- with(prey, length / speed)
regres3 <- lm(prey$time~prey$ls)
summary(regres3)
anova(regres3)

