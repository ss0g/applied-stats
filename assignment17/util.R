library(ggplot2)
library(GGally)

quick_residplot <- function(model, sample_size) {
  s <- c()
  if (missing(sample_size)) {
    s <- 1:nrow(model$model)
  } else {
    s <- sample(nrow(model$model), sample_size)
  }
  plot <- ggplot(model$model[s,], aes(predict(model)[s], resid(model)[s])) +
    geom_hline(yintercept = 0) +
    geom_point()
  return(plot)
}

quick_residdensityplot <- function(model) {
  plot <- ggplot(model$model, aes(resid(model))) +
    geom_density(fill = "#88888888") +
    geom_function(color = "#ff0000ff", fun = dnorm, args = list(mean = mean(resid(model)), sd = sd(resid(model))))
  return(plot)
}

quick_residqqplot <- function(model, sample_size) {
  s <- c()
  if (missing(sample_size)) {
    s <- 1:nrow(model$model)
  } else {
    s <- sample(nrow(model$model), sample_size)
  }
  plot <- ggplot(model$model[s,], aes(sample = resid(model)[s])) +
    geom_qq() +
    geom_qq_line(color = "#ff0000ff")
  return(plot)
}
