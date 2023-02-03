setwd("C:/Users/tntje/work/school/applied-stats/assignment15")

library(ggplot2)
library(dplyr)
library(GGally)

# PART B
pdata <- read.csv("./Perception Data.csv");

pdata$materialglass <- ifelse(pdata$material == "glass", 1, 0)
pdata$materialplastic <- ifelse(pdata$material == "plastic", 1, 0)
pdata$materialtin <- ifelse(pdata$material == "tin", 1, 0)

# no interaction
lm01p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation,
     data = pdata
     )
summary(lm01p)

# x6 = x3 : x4
lm02p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       max.width : min.width,
     data = pdata
     )
summary(lm02p)

lm03p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       height : max.width,
     data = pdata
     )
summary(lm03p)

lm04p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       height : max.width +
       height : elongation,
     data = pdata
  )
summary(lm04p)

lm05p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       height : max.width +
       max.width : elongation,
     data = pdata
  )
summary(lm05p)

lm06p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       height : elongation,
     data = pdata
  )
summary(lm06p)

lm07p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       material : height,
     data = pdata
     )
summary(lm07p)

lm08p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       material : height +
       height : max.width,
     data = pdata
  )
summary(lm08p)

lm09p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       material : height +
       height : max.width +
       material : max.width,
     data = pdata
  )
summary(lm09p)

lm10p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       material : height +
       height : max.width +
       material : elongation,
     data = pdata
  )
summary(lm10p)

lm11p <-
  lm(volume ~
       material +
       height +
       max.width +
       min.width +
       elongation +
       material : height +
       material : elongation,
     data = pdata
  )
summary(lm11p)

lm12p <-
  lm(volume ~
       material +
       height +
       max.width +
       material : height +
       height : max.width,
     data = pdata
  )
summary(lm12p)

lm13p <-
  lm(volume ~
       material +
       height +
       max.width +
       material : height +
       height : max.width +
       min.width : max.width,
     data = pdata
  )
summary(lm13p)

lm_final_p <- lm13p
summary(lm_final_p)
anova(lm_final_p)

qqp1_p <- ggplot(pdata, aes(sample = resid(lm_final_p))) +
  geom_qq() +
  labs(
    x = "Theoretical",
    y = "Sample",
    title = "Normal Q-Q plot of residuals"
  )
qqp1_p

rp1_p <- ggplot(pdata, aes(x = predict(lm_final_p), y = resid(lm_final_p))) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  labs(
    x = "Predicted values",
    y = "Residuals",
    title = "Residuals vs predicted values"
  )
rp1_p

ggsave(filename = "qqp1_p.png", plot = qqp1_p, device = "png", width = 6.5, height = 2, units = "in")
ggsave(filename = "rp1_p.png", plot = rp1_p, device = "png", width = 6.5, height = 2, units = "in")

# PART C
tdata = read.csv("./tigerdata.csv")

tdata$Region1 <- ifelse(tdata$Region == 1, 1, 0)
tdata$Region2 <- ifelse(tdata$Region == 2, 1, 0)
tdata$Region3 <- ifelse(tdata$Region == 3, 1, 0)
tdata$Region4 <- ifelse(tdata$Region == 4, 1, 0)
tdata$Region5 <- ifelse(tdata$Region == 5, 1, 0)

tdata$Sexmale <- ifelse(tdata$Sex == "male", 1, 0)

pairs(tdata[,3:7], pch=19)

for (i in 3:7) {
  print(paste(paste(colnames(tdata)[i], "mean:"), mean(tdata[,i])))
  print(paste(paste(colnames(tdata)[i], "sd:"), sd(tdata[,i])))
}
rm(i)

msp1_t <- ggmatrix(list(
  ggplot(tdata, aes(x = NoseBlack, y = Age)),
  ggplot(tdata, aes(x = PawCircumference, y = Age)),
  ggplot(tdata, aes(x = Weight, y = Age)),
  ggplot(tdata, aes(x = Size, y = Age))
), 1, 4, xAxisLabels = colnames(tdata)[4:7], ylab = "Age") + geom_point()
msp1_t
ggsave(filename = "msp1_t.png", plot = msp1_t, device = "png", width = 6.5, height = 3, units = "in")

slm01t <- lm(Age ~ NoseBlack, data = tdata)
summary(slm01t)
anova(slm01t)

sp1_t <- ggplot(tdata, aes(x = NoseBlack, y = Age)) +
  geom_point() +
  labs(
    x = "NoseBlack",
    y = "Age",
    title = "Age vs NoseBlack"
  )
sp1_t

qqp1_t <- ggplot(tdata, aes(sample = NoseBlack)) +
  geom_qq() +
  labs(
    x = "Theoretical",
    y = "Sample",
    title = "Normal Q-Q plot of NoseBlack"
  )
qqp1_t

rp1_t <- ggplot(tdata, aes(x = NoseBlack, y = resid(slm01t))) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  labs(
    x = "NoseBlack",
    y = "Residuals",
    title = "Residuals vs NoseBlack"
  )
rp1_t

ggsave(filename = "sp1_t.png", plot = sp1_t, device = "png", width = 6.5, height = 2, units = "in")
ggsave(filename = "qqp1_t.png", plot = qqp1_t, device = "png", width = 6.5, height = 2, units = "in")
ggsave(filename = "rp1_t.png", plot = rp1_t, device = "png", width = 6.5, height = 2, units = "in")

sxx1_t <- sum(sapply(as.vector(tdata$NoseBlack), function(x) (x - mean(tdata$NoseBlack))^2))

predict_q7_t <- predict(slm01t, data.frame(NoseBlack = c(0.1, 0.5, 0.9)))
predict_q7_t

lm01t <-
  lm(Age ~
       factor(Region) +
       NoseBlack +
       PawCircumference +
       Weight +
       Size +
       Sex,
     data = tdata
     )
summary(lm01t)

lm02t <-
  lm(Age ~
       factor(Region) +
       NoseBlack +
       PawCircumference +
       Sex,
     data = tdata
     )
summary(lm02t)

lm03t <-
  lm(Age ~
       Region1 +
       Region2 +
       Region3 +
       Region5 +
       NoseBlack +
       PawCircumference +
       Sex,
     data = tdata
     )
summary(lm03t)

lm04t <-
  lm(Age ~
       Region1 +
       Region2 +
       Region3 +
       Region5 +
       NoseBlack +
       PawCircumference +
       Weight +
       Sex,
     data = tdata
  )
summary(lm04t)

lm05t <-
  lm(Age ~
       Region3 +
       Region5 +
       NoseBlack +
       PawCircumference +
       Sex,
     data = tdata
  )
summary(lm05t)

lm06t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sex,
     data = tdata
  )
summary(lm06t)

lm07t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sexmale +
       0,
     data = tdata
  )
summary(lm07t)

lm08t <-
  lm(Age ~
       Region3 +
       Region5 +
       NoseBlack +
       PawCircumference +
       Sexmale +
       0,
     data = tdata
  )
summary(lm08t)

lm09t <-
  lm(Age ~
       Region1 +
       Region2 +
       Region3 +
       Region4 +
       Region5 +
       NoseBlack +
       PawCircumference +
       Weight +
       Size +
       Sexmale +
       0,
     data = tdata
  )
summary(lm09t)

lm10t <-
  lm(Age ~
       NoseBlack +
       PawCircumference +
       Sexmale +
       0,
     data = tdata
  )
summary(lm10t)

lm11t <-
  lm(Age ~
       NoseBlack +
       PawCircumference +
       Sex,
     data = tdata
  )
summary(lm11t)

lm12t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sexmale +
       NoseBlack : PawCircumference +
       0,
     data = tdata
  )
summary(lm12t)

lm13t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sexmale +
       NoseBlack : Sexmale +
       0,
     data = tdata
  )
summary(lm13t)

lm14t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sexmale +
       NoseBlack : Region3 +
       0,
     data = tdata
  )
summary(lm14t)

lm15t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sexmale +
       Sexmale : PawCircumference +
       0,
     data = tdata
  )
summary(lm15t)

lm16t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sexmale : PawCircumference +
       0,
     data = tdata
  )
summary(lm16t)

lm17t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       Sexmale +
       Sexmale : PawCircumference +
       0,
     data = tdata
  )
summary(lm17t)

lm18t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sexmale : PawCircumference +
       Sexmale : NoseBlack +
       0,
     data = tdata
  )
summary(lm18t)

lm19t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       Sexmale : PawCircumference +
       Region3 : NoseBlack +
       0,
     data = tdata
  )
summary(lm19t)

lm20t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       Sexmale : PawCircumference +
       0,
     data = tdata
  )
summary(lm20t)

lm21t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       Sexmale +
       0,
     data = tdata
  )
summary(lm21t)

lm22t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference +
       0,
     data = tdata
  )
summary(lm22t)

lm23t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       log(PawCircumference) : log(Weight) : log(Size) +
       0,
     data = tdata
     )
summary(lm23t)

lm24t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       log(PawCircumference) : log(Weight) +
       0,
     data = tdata
  )
summary(lm24t)

lm25t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       log(PawCircumference) : log(Size) +
       0,
     data = tdata
  )
summary(lm25t)

lm25t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       log(PawCircumference) : atan(Weight / Size) +
       0,
     data = tdata
  )
summary(lm25t)

lm26t <-
  lm(Age ~
       Region3 +
       NoseBlack +
       PawCircumference : atan(Weight / Size) +
       0,
     data = tdata
  )
summary(lm26t)

lm_final_t <- lm22t
summary(lm_final_t)
anova(lm_final_t)

qqp2_t <- ggplot(tdata, aes(sample = resid(lm_final_t))) +
  geom_qq() +
  labs(
    x = "Theoretical",
    y = "Sample",
    title = "Normal Q-Q plot of residuals"
  )
qqp2_t

rp2_t <- ggplot(tdata, aes(x = predict(lm_final_t), y = resid(lm_final_t))) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  labs(
    x = "Predicted values",
    y = "Residuals",
    title = "Residuals vs. predicted values"
  )
rp2_t

ggsave(filename = "qqp2_t.png", plot = qqp2_t, device = "png", width = 6.5, height = 2, units = "in")
ggsave(filename = "rp2_t.png", plot = rp2_t, device = "png", width = 6.5, height = 2, units = "in")
