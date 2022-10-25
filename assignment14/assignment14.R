setwd("C:/Users/tntje/work/school/applied-stats/assignment14")

library(ggplot2)

data("mtcars")

bp1 <- ggplot(data = mtcars, aes(x = factor(am), y = qsec, fill = factor(am))) +
  geom_boxplot(aes(group = am)) +
  scale_x_discrete(labels = c("automatic", "manual")) +
  coord_flip() +
  labs(
    x = "Transmission type",
    y = "Quarter mile time (s)",
    title = "Comparison of distributions of quarter mile times for different transmission types"
  )
bp1

bc1 <- ggplot(data = mtcars, aes(x = factor(carb), fill = factor(carb))) +
  geom_bar() +
  scale_fill_discrete(name = "Carburetor type") +
  labs(
    x = "Carburetor type",
    y = "Count",
    title = "Counts of different carburetor types"
  )
bc1

bc2 <- ggplot(data = mtcars, aes(x = factor(gear), fill = factor(cyl))) +
  geom_bar(position = "stack") +
  labs(
    x = "Number of forward gears",
    y = "Count",
    title = "Counts of different numbers of forward gears"
  )
bc2

sp1 <- ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  scale_fill_discrete(name = "Number of cylinders") +
  labs(
    x = "Weight (1000lbs)",
    y = "MPG",
    title = "MPG vs weight"
  )
sp1

# this histogram is useful to see the distribution of power to weight ratio
hg1 <- ggplot(data = mtcars, aes(x = hp / wt)) +
  geom_histogram(binwidth = 10) +
  labs(
    x = "Power to weight ratio (hp/1000lb)",
    y = "Count",
    title = "Distribution of power to weight ratio"
  )
hg1
