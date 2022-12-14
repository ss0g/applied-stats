setwd("C:/Users/tntje/work/school/applied-stats/assignment17")

start_time <- Sys.time()

set.seed(1)

library(dplyr)
library(ggplot2)
library(GGally)
library(fastDummies)
library(extrafont)
library(svglite)

source("util.R")

source("theme-foundation.R", encoding = "UTF-8")

font_import(prompt = FALSE, pattern = "Gotham")
loadfonts()

start_import_time <- Sys.time()
data_raw <- read.csv("./dataset.csv")
end_import_time <- Sys.time()
sprintf("Time to import: %.3f seconds", end_import_time - start_import_time)

data <- data_raw[6:21]
data$sqrtpop <- with(data, sqrt(popularity))
data$key <- factor(data$key)
data$time_signature <- factor(data$time_signature)

data <- dummy_cols(data)

data$genre_category_A <- ifelse(
  data$track_genre %in% c(
    "alt-rock",
    "hard-rock",
    "j-rock",
    "psych-rock",
    "punk-rock",
    "rock-n-roll",
    "rock"
  ), 1, 0
)

data$genre_category_B <- ifelse(
  data$track_genre %in% c(
    "cantopop",
    "indie-pop",
    "indie",
    "j-pop",
    "k-pop",
    "mandopop",
    "pop-film",
    "pop",
    "power-pop",
    "synth-pop"
  ), 1, 0
)

data$genre_category_C <- ifelse(
  data$track_genre %in% c(
    "black-metal",
    "death-metal",
    "heavy-metal",
    "metal",
    "metalcore"
  ), 1, 0
)

data$genre_category_D <- ifelse(
  data$track_genre %in% c(
    "chicago-house",
    "club",
    "dance",
    "dancehall",
    "deep-house",
    "house",
    "j-dance"
  ), 1, 0
)

data$genre_category_E <- ifelse(
  data$track_genre %in% c(
    "hip-hop",
    "trip-hop"
  ), 1, 0
)

data$genre_category_F <- ifelse(
  data$track_genre %in% c(
    "children",
    "disney",
    "kids"
  ), 1, 0
)

data$genre_category_G <- ifelse(
  data$track_genre %in% c(
    "detroit-techno",
    "dubstep",
    "edm",
    "electro",
    "electronic",
    "minimal_techno",
    "techno",
    "trance"
  ), 1, 0
)

data$genre_category_H <- ifelse(
  data$track_genre %in% c(
    "afrobeat",
    "brazil",
    "french",
    "german",
    "indian",
    "iranian",
    "latin",
    "latino",
    "spanish",
    "swedish",
    "turkish",
    "world-music"
  ), 1, 0
)

data_rel_10 <- data.frame(data)[data$popularity > 10,]
data_rel_20 <- data.frame(data)[data$popularity > 20,]
data_rel_30 <- data.frame(data)[data$popularity > 30,]
data_rel_40 <- data.frame(data)[data$popularity > 40,]
data_rel_50 <- data.frame(data)[data$popularity > 50,]
data_rel_60 <- data.frame(data)[data$popularity > 60,]

data_rel_10_sample_100 <- data_rel_10[sample(nrow(data_rel_10), 100),]

mean_pops <- aggregate(data$popularity, list(data$track_genre), FUN = mean)
mean_pops <- setNames(mean_pops, c("track_genre", "popularity"))

mean_pops_expl <- aggregate(data$popularity, list(data$explicit), FUN = mean)
mean_pops_expl <- setNames(mean_pops_expl, c("explicit", "popularity"))

mean_pops_key <- aggregate(data$popularity, list(factor(data$key)), FUN = mean)
mean_pops_key <- setNames(mean_pops_key, c("key", "popularity"))

ggpairs(data[sample(nrow(data), 100),], columns = c(1, 4, 5, 7, 9:14))
ggpairs(data_rel_10[sample(nrow(data_rel_10), 100),], columns = c(1, 4, 5, 7, 9:14))

# head(data)

source("models.R")

lmfinal <- get_final_model()
quick_residdensityplot(lmfinal)
quick_residqqplot(lmfinal, 1000)
quick_residplot(lmfinal, 1000)

summ <- summary(lmfinal)
fstatistic <- unname(summ$fstatistic[1])
dfnum <- unname(summ$fstatistic[2])
dfdenom <- unname(summ$fstatistic[3])
rsq <- summ$r.squared
rsqadj <- summ$adj.r.squared

sprintf("F-statistic: %.6f on %d and %d DF, p-value: %.30f", fstatistic, dfnum, dfdenom, pf(fstatistic, dfnum, dfdenom, lower.tail =  FALSE))
sprintf("R-squared: %.6f, R-squared(adjusted): %.6f", rsq, rsqadj)

source("plots.R")

test_plot()
pairs()
bar_plot_genre()
bar_plot_genre_sorted()
bar_plot_key()
`?_plot_key`()
scatter_plot_matrix_popularity()

ggsave(filename = "bar_plot_genre_sorted.svg", plot = bar_plot_genre_sorted(), device = "svg", width = 8, height = 4, units = "in")
ggsave(filename = "scatter_plot_matrix_popularity.svg", plot = scatter_plot_matrix_popularity(), device = "svg", width = 8, height = 2, units = "in")

chisq_gof_res <- chisq.test(mean_pops$popularity)
print(chisq_gof_res)

chisq_ind_res <- chisq.test(mean_pops_expl$popularity)
print(chisq_ind_res)

genre_aov <- aov(popularity ~ track_genre, data = data)
explicit_ttest <- t.test(popularity ~ explicit, data = data)
mode_ttest <- t.test(popularity ~ mode, data = data)
key_aov <- aov(popularity ~ key, data = data)
tsig_aov <- aov(popularity ~ time_signature, data = data)

end_time <- Sys.time()
sprintf("Total time to run: %.3f seconds", difftime(end_time, start_time, units = "secs"))
