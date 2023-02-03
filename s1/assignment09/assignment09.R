setwd("C:/Users/tntje/work/school/applied-stats/assignment09")

library(ggplot2)
library(dplyr)

brushing_method <- read.csv("./Brushing Method.csv")
View(brushing_method)

head(brushing_method)

bdata <- filter(brushing_method, method == "b")
cdata <- filter(brushing_method, method == "c")

summb <- summary(bdata)
summc <- summary(cdata)
summb
summc

print(paste("sd of percent (b): ", sd(bdata$percent)))
print(paste("sd of percent (c): ", sd(cdata$percent)))

lm1 <- lm(brushing_method$cw~brushing_method$dw)
lm1

f_lm1 <- function(x) {
  return (lm1$coefficients[1] + lm1$coefficients[2] * x)
}

sresids <- c()
for (i in lm1$residuals) {
  sresids <- append(sresids, i^2)
}
ssresid1 <- sum(sresids)
rm(sresids)

xbar1 <- mean(brushing_method$dw)
sxdevs <- c()
for (i in brushing_method$dw) {
  sxdevs <- append(sxdevs, (i - xbar1)^2)
}
sxx1 <- sum(sxdevs)
rm(sxdevs)
  
lm1b <- lm(bdata$dw~bdata$cw)
lm1c <- lm(cdata$dw~cdata$cw)

bp1 <- ggplot(data = brushing_method, aes(x = method, y = percent, fill = method)) + geom_boxplot()
bp1 <- bp1 + labs(
  x = "Cleaning method",
  y = "Weight reduction (%)",
  title = "Comparison of weight reduction after cleaning with brush vs Calgon"
)
bp1 <- bp1 + scale_x_discrete(labels = c("Brush", "Calgon"))
bp1 <- bp1 + scale_fill_discrete(labels = c("Brush", "Calgon"))
bp1 <- bp1 + coord_flip()
bp1 <- bp1 + geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.25)
bp1

sp1 <- ggplot(data = brushing_method, aes(x = dw, y = cw, color = percent)) + geom_point()
sp1 <- sp1 + labs(
  x = "Dirty weight (g)",
  y = "Clean weight (g)",
  title = "Clean weight as a function of dirty weight"
)
sp1$labels$colour <- "% difference"
sp1 <- sp1 + geom_abline(aes(slope = lm1$coefficients[2], intercept = lm1$coefficients[1]))
sp1

qqp1 <- ggplot(data = brushing_method, aes(sample = dw)) + geom_qq()
qqp1 <- qqp1 + labs(
  x = "Theoretical",
  y = "Sample",
  title = "Normal probability plot of dirty weight"
)
qqp1

rp1 <- ggplot(data = brushing_method, aes(x = dw, y = resid(lm1))) + geom_point()
rp1 <- rp1 + labs(
  x = "Dirty weight (g)",
  y = "Residual",
  title = "Plot of dirty weight vs residual"
)
rp1 <- rp1 + geom_hline(aes(yintercept = 0))
rp1

qqp1b <- ggplot(data = bdata, aes(sample = percent)) + geom_qq()
qqp1b <- qqp1b + labs(
  x = "Theoretical",
  y = "Sample",
  title = "Normal probability plot of % difference with brush"
)
qqp1b

qqp1c <- ggplot(data = cdata, aes(sample = percent)) + geom_qq()
qqp1c <- qqp1c + labs(
  x = "Theoretical",
  y = "Sample",
  title = "Normal probability plot of % difference with Calgon"
)
qqp1c

t.test(bdata$percent, cdata$percent, alternative = "two.sided", var.equal = FALSE)
