setwd("C:/Users/tntje/work/school/applied-stats/assignment14")

library(ggplot2)

data("mtcars")

bp1 <- ggplot(data = mtcars, aes(x = am, y = qsec)) +
  geom_boxplot(aes(group = am))
bp1

bc1 <- ggplot(data = mtcars, aes(x = factor(carb))) +
  geom_bar()
bc1

bc2 <- ggplot(data = mtcars, aes(x = factor(gear), fill = factor(cyl))) +
  geom_bar(position = "stack")
bc2

sp1 <- ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point()
sp1

lm_sp2 <- lm(mtcars$qsec~mtcars$hp/mtcars$wt)

# this histogram is useful to see the distribution of power to weight ratio
sp2 <- ggplot(data = mtcars, aes(x = hp / wt)) +
  geom_histogram(binwidth = 10)
sp2
