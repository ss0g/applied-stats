setwd("C:/Users/tntje/work/school/applied-stats/assignment08")

library(ggplot2)

data("mtcars")
head(mtcars)

dispml = mtcars$disp * 16.3871

lm1 <- lm(mtcars$hp~mtcars$cyl)
a1 <- lm1$coefficients[1]
b1 <- lm1$coefficients[2]

summary(lm1)
anova(lm1)

sp1 <- ggplot(data = mtcars, aes(x = cyl, y = hp, color = dispml)) + geom_point()
sp1 <- sp1 + geom_abline(aes(slope = b1, intercept = a1))
sp1 <- sp1 + labs(
  x = "Cylinders",
  y = "Horsepower",
  color = "Displacement (ml)",
  title = "Horsepower as a function of # of cylinders"
)
sp1

lm2 <- lm(mtcars$hp~dispml)
a2 <- lm2$coefficients[1]
b2 <- lm2$coefficients[2]

summary(lm2)
anova(lm2)

sp2 <- ggplot(data = mtcars, aes(x = dispml, y = hp, color = cyl)) + geom_point()
sp2 <- sp2 + geom_abline(aes(slope = b2, intercept = a2))
sp2 <- sp2 + labs(
  x = "Displacement (ml)",
  y = "Horsepower",
  color = "Cylinders",
  title = "Horsepower as a function of displacement"
)
sp2
