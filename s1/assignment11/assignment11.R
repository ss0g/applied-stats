setwd("C:/Users/tntje/work/school/applied-stats/assignment11")

library(ggplot2)

nfl <- read.csv("./nfl.csv")
# View(data)

regres1 <- lm(nfl$WinPct~nfl$PointsAg)
summary(regres1)
anova(regres1)
rp1 <- ggplot(data = nfl, aes(x = PointsAg, y = resid(regres1))) + geom_point()
rp1 <- rp1 + labs(
  x = "PointsAg",
  y = "resid(regres1)"
)
rp1 <- rp1 + geom_hline(aes(yintercept = 0))
rp1

regres2 <- lm(nfl$WinPct~nfl$PointsFor)
summary(regres2)
anova(regres2)
rp2 <- ggplot(data = nfl, aes(x = PointsFor, y = resid(regres2))) + geom_point()
rp2 <- rp2 + labs(
  x = "PointsFor",
  y = "resid(regres2)"
)
rp2 <- rp2 + geom_hline(aes(yintercept = 0))
rp2

regres3 <- lm(nfl$WinPct~nfl$PointsAg+nfl$PointsFor)
summary(regres3)
anova(regres3)
rp3 <- ggplot(data = nfl, aes(x = predict(regres3), y = resid(regres3))) + geom_point()
rp3 <- rp3 + labs(
  x = "predict(regres3)",
  y = "resid(regres3)"
)
rp3 <- rp3 + geom_hline(aes(yintercept = 0))
rp3

regres4 <- lm(nfl$WinPct~nfl$PointsAg*nfl$PointsFor)
summary(regres4)
anova(regres4)
rp4 <- ggplot(data = nfl, aes(x = predict(regres4), y = resid(regres4))) + geom_point()
rp4 <- rp4 + labs(
  x = "predict(regres4)",
  y = "resid(regres4)"
)
rp4 <- rp4 + geom_hline(aes(yintercept = 0))
rp4
