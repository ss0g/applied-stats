setwd("C:/Users/tntje/work/school/applied-stats/assignment18")

library(ggplot2)
library(GGally)
library(dplyr)

fishdata_raw <- read.csv("./fishdata.csv")

fishdata <- data.frame(fishdata_raw)
colnames(fishdata) <- fishdata[1,]
colnames(fishdata)[1] <- "Trial.Num"
fishdata <- fishdata[-1,]
rownames(fishdata) <- 1:nrow(fishdata)
fishdata <- apply(fishdata, MARGIN = c(1, 2), as.numeric)
fishdata <- data.frame(fishdata)

fishdata_chosen <- fishdata[,c(1:4)]
colnames(fishdata_chosen)[2:4] <- c("BodLen", "Duration", "Amplitude")
fishdata_chosen$Chosen <- rep(factor(1), each = nrow(fishdata_chosen))

fishdata_rejected <- fishdata[,c(1, 5:7)]
colnames(fishdata_rejected)[2:4] <- c("BodLen", "Duration", "Amplitude")
fishdata_rejected$Chosen <- rep(factor(0), each = nrow(fishdata_rejected))

fishdata_split <- rbind(fishdata_chosen, fishdata_rejected)
rownames(fishdata_split) <- 1:nrow(fishdata_split)

BodLen_comparison_density_plot <- function() {
  return(
    ggplot(fishdata, aes()) +
      geom_density(aes(x = R.BodLen, fill = "Rejected")) +
      geom_density(aes(x = C.BodLen, fill = "Chosen")) +
      scale_fill_manual(values = c("Chosen" = "#8888ff88", "Rejected" = "#ff888888")) +
      labs(
        x = "Body Length",
        y = "Density",
        fill = "Legend"
      ) +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
      )
  )
}

Duration_comparison_density_plot <- function() {
  return(
    ggplot(fishdata, aes()) +
      geom_density(aes(x = R.Duration, fill = "Rejected")) +
      geom_density(aes(x = C.Duration, fill = "Chosen")) +
      scale_fill_manual(values = c("Chosen" = "#8888ff88", "Rejected" = "#ff888888")) +
      labs(
        x = "Body Length",
        y = "Density",
        fill = "Legend"
      ) +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
      )
  )
}

Amplitude_comparison_density_plot <- function() {
  return(
    ggplot(fishdata, aes()) +
      geom_density(aes(x = R.Amplitude, fill = "Rejected")) +
      geom_density(aes(x = C.Amplitude, fill = "Chosen")) +
      scale_fill_manual(values = c("Chosen" = "#8888ff88", "Rejected" = "#ff888888")) +
      labs(
        x = "Body Length",
        y = "Density",
        fill = "Legend"
      ) +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
      )
  )
}
