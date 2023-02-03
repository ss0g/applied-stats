# ALWAYS SET WORKING DIRECTORY FIRST!!!!!!

setwd("C:/Users/tntje/work/school/applied-stats/assignment08")

library(ggplot2)

data("USArrests")
head(USArrests)

# 1
plot(USArrests$Assault, USArrests$Murder, main = "Murder rate as a function of assault rate", xlab = "Assault Rate (per 100,000)", ylab = "Murder Rate (per 100,000)", pch = 23, bg = "white", col = "red", col.lab = "red4", col.main = "red4")

# 2
regres1 <- lm(USArrests$Murder~USArrests$Assault)
a <- regres1$coefficients[1]
b <- regres1$coefficients[2]

# 3
summary(regres1)

# 4
plot(USArrests$Assault, resid(regres1), main = "Residual plot: Assault rate per 100,000 vs residuals", xlab = "Assault Rate (per 100,000)", ylab = "Residual", pch = 23, bg = "white", col = "red", col.lab = "red4", col.main = "red4")
abline(0, 0)

# to get ssresid for #7
anova(regres1)

# 1 with ggplot2
sp1 <- ggplot(data=USArrests, aes(x=Assault, y=Murder)) + geom_point()
sp1 <- sp1 + geom_abline(aes(slope=b, intercept=a))
sp1 <- sp1 + labs(
  x = "Assault rate per 100,000",
  y = "Murder rate per 100,000",
  title = "Murder rate as a function of assault rate"
)
sp1

# 4 with ggplot2
sp2 <- ggplot(data=USArrests, aes(x=Assault, y=resid(regres1))) + geom_point()
sp2 <- sp2 + geom_hline(aes(yintercept=0))
sp2 <- sp2 + labs(
  x = "Assault Rate per 100,000",
  y = "Residual",
  title = "Residual plot: Assault rate per 100,000 vs residuals"
)
sp2

