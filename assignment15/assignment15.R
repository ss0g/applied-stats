setwd("C:/Users/tntje/work/school/applied-stats/assignment15")

library(ggplot2)
library(dplyr)

# PART B
pdata <- read.csv("./Perception Data.csv");

pdata_mat_recoding <- c(0, 1, 2, 3)
names(pdata_mat_recoding) <- c("glass", "plastic", "tin", "cardboard")
pdata_mat_num <- recode(pdata$material, !!!pdata_mat_recoding)

# no interaction
lm01p <- 
  lm(volume ~
       pdata_mat_num +
       height +
       max.width +
       min.width +
       elongation,
     data = pdata
     )

# x6 = x3 : x4
lm02p <- 
  lm(volume ~ 
       pdata_mat_num +
       height +
       max.width +
       min.width +
       elongation +
       max.width * min.width,
     data = pdata
     )

# x6 = x3 : x4, x7 = x2 : x5
lm03p <-
  lm(volume ~
       pdata_mat_num +
       height +
       max.width +
       min.width +
       elongation +
       max.width : min.width +
       height : elongation,
     data = pdata
     )

# x6 = x2 : x3
lm04p <-
  lm(volume ~
       pdata_mat_num +
       height +
       max.width +
       min.width +
       elongation +
       height : max.width,
     data = pdata
     )

# x6 = I(height / max.width)
lm05p <-
  lm(volume ~
       pdata_mat_num +
       height +
       max.width +
       min.width +
       elongation +
       I(height / max.width),
     data = pdata
     )

# x6 = height : pdata_mat_num
lm06p <-
  lm(volume ~
       pdata_mat_num +
       height +
       max.width +
       min.width +
       elongation +
       height : pdata_mat_num,
     data = pdata
  )

lm_final_p <- lm03p
summary(lm_final_p)
anova(lm_final_p)

qqp1_p <- ggplot(data = pdata, aes(sample = resid(lm_final_p))) +
  geom_qq() +
  labs(
    x = "Theoretical",
    y = "Sample",
    title = "Normal Q-Q plot of residuals"
  )
qqp1_p

rp1_p <- ggplot(data = pdata, aes(x = predict(lm_final_p), y = resid(lm_final_p))) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  labs(
    x = "Predicted values",
    y = "Residuals",
    title = "Scatter plot of residuals vs predicted values"
  )
rp1_p

ggsave(filename = "qqp1_p.png", plot = qqp1_p, device = "png", width = 6.5, height = 2, units = "in")
ggsave(filename = "rp1_p.png", plot = rp1_p, device = "png", width = 6.5, height = 2, units = "in")

# PART C
tdata = read.csv("./tigerdata.csv")
