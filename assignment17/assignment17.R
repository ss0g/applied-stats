setwd("C:/Users/tntje/work/school/applied-stats/assignment17")

start_time <- Sys.time()

set.seed(1)

source("util.R")

library(dplyr)
library(ggplot2)
library(GGally)
library(fastDummies)

start_import_time <- Sys.time()
data_raw <- read.csv("./dataset.csv")
end_import_time <- Sys.time()
sprintf("Time to import: %.3f seconds", end_import_time - start_import_time)

data <- data_raw[6:21]
data$sqrtpop <- with(data, sqrt(popularity))
data$key <- factor(data$key)
data$time_signature <- factor(data$time_signature)

data <- dummy_cols(data)

data_rel_10 <- data.frame(data)[data$popularity > 10,]
data_rel_20 <- data.frame(data)[data$popularity > 20,]
data_rel_30 <- data.frame(data)[data$popularity > 30,]
data_rel_40 <- data.frame(data)[data$popularity > 40,]
data_rel_50 <- data.frame(data)[data$popularity > 50,]
data_rel_60 <- data.frame(data)[data$popularity > 60,]

ggpairs(data[sample(nrow(data), 100),], columns = c(1, 4, 5, 7, 9:14))
ggpairs(data_rel_10[sample(nrow(data_rel_10), 100),], columns = c(1, 4, 5, 7, 9:14))

# head(data)

lm01 <- lm(
  popularity ~
    duration_ms +
    explicit +
    danceability +
    energy +
    key +
    loudness +
    mode +
    speechiness +
    acousticness +
    instrumentalness +
    liveness +
    valence +
    tempo +
    time_signature +
    track_genre,
  data = data
)

summary(lm01)

lm02 <- lm(
  popularity ~
    explicit +
    danceability +
    energy +
    mode,
  data = data
)

summary(lm02)

lm03 <- lm(
  popularity ~
    danceability,
  data = data
)

summary(lm03)

lm04 <- lm(
  popularity ~
    duration_ms +
    explicit +
    danceability +
    energy +
    key +
    loudness +
    mode +
    speechiness +
    acousticness +
    instrumentalness +
    liveness +
    valence +
    tempo +
    time_signature +
    track_genre +
    0,
  data = data
)

summary(lm04)

lm05 <- lm(
  popularity ~
    explicit +
    danceability +
    energy +
    mode +
    valence +
    0,
  data = data
)

summary(lm05)

lm06 <- lm(
  popularity ~
    explicit +
    danceability +
    energy +
    key +
    speechiness +
    acousticness +
    valence +
    0,
  data = data
)

summary(lm06)

lm07 <- lm(
  popularity ~
    explicit +
    danceability +
    energy +
    speechiness +
    acousticness +
    valence +
    0,
  data = data
)

summary(lm07)

lm08 <- lm(
  popularity ~
    explicit +
    danceability +
    energy +
    speechiness +
    acousticness +
    valence +
    key * mode +
    0,
  data = data
)

summary(lm08)

lm09 <- lm(
  popularity ~
    explicit +
    danceability +
    energy +
    speechiness +
    acousticness +
    valence +
    energy : track_genre +
    0,
  data = data
)

summary(lm09)

lm10 <- lm(
  popularity ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * track_genre +
    0,
  data = data
)

summary(lm10)

lm11 <- lm(
  popularity ~
    explicit +
    danceability +
    energy +
    speechiness +
    acousticness +
    valence +
    track_genre : energy +
    0,
  data = data
)

summary(lm11)

lm12 <- lm(
  popularity ~
    danceability +
    energy +
    speechiness +
    valence +
    energy : track_genre_afrobeat +
    energy : `track_genre_alt-rock` +
    energy : track_genre_alternative +
    energy : `track_genre_black-metal` +
    energy : track_genre_bluegrass +
    energy : track_genre_breakbeat +
    energy : `track_genre_chicago-house` +
    energy : track_genre_chill +
    energy : track_genre_classical +
    energy : track_genre_club +
    energy : track_genre_comedy +
    energy : track_genre_country +
    energy : track_genre_dance +
    energy : track_genre_dancehall +
    energy : `track_genre_death-metal` +
    energy : `track_genre_detroit-techno` +
    energy : track_genre_disco +
    energy : track_genre_disney +
    energy : `track_genre_drum-and-bass` +
    energy : track_genre_edm +
    energy : track_genre_electro +
    energy : track_genre_funk +
    energy : track_genre_german +
    energy : track_genre_goth +
    energy : track_genre_grindcore +
    energy : track_genre_guitar +
    energy : track_genre_happy +
    energy : track_genre_hardstyle +
    energy : `track_genre_heavy-metal` +
    energy : `track_genre_honky-tonk` +
    energy : track_genre_house +
    energy : track_genre_idm +
    energy : track_genre_industrial +
    energy : track_genre_iranian +
    energy : `track_genre_j-dance` +
    energy : `track_genre_j-idol` +
    energy : track_genre_jazz +
    energy : `track_genre_k-pop` +
    energy : track_genre_kids +
    energy : track_genre_latin +
    energy : track_genre_latino +
    energy : track_genre_malay +
    energy : `track_genre_minimal-techno` +
    energy : track_genre_opera +
    energy : track_genre_party +
    energy : `track_genre_pop-film` +
    energy : `track_genre_power-pop` +
    energy : track_genre_reggae +
    energy : track_genre_reggaeton +
    energy : track_genre_rock +
    energy : track_genre_rockabilly +
    energy : track_genre_romance +
    energy : track_genre_sad +
    energy : track_genre_salsa +
    energy : track_genre_sleep +
    energy : track_genre_soul +
    energy : track_genre_study +
    energy : track_genre_tango +
    energy : `track_genre_trip-hop` +
    0,
  data = data
)

summary(lm12)

lm13 <- lm(
  popularity ~
    danceability +
    energy +
    speechiness +
    valence +
    valence : track_genre +
    0,
  data = data
)

summary(lm13)

lm14 <- lm(
  popularity ~
    danceability +
    energy +
    speechiness +
    valence +
    valence : mode +
    valence : mode : track_genre +
    0,
  data = data
)

summary(lm14)

lm15 <- lm(
  popularity ~
    explicit +
    poly(danceability, 2) +
    valence +
    0,
  data = data
)

summary(lm15)

lm16 <- lm(
  popularity ~
    explicit +
    poly(danceability, 2) * valence +
    0,
  data = data
)

summary(lm16)

lm17 <- lm(
  popularity ~
    explicit +
    poly(danceability, 3) +
    danceability : valence : energy : mode +
    track_genre +
    0,
  data = data
)

summary(lm17)

lm18 <- lm(
  popularity ~
    explicit +
    poly(danceability, 3) +
    danceability * valence * energy * mode +
    track_genre +
    0,
  data = data
)

summary(lm18)

lm19 <- lm(
  popularity ~
    explicit +
    poly(danceability, 2) +
    valence +
    valence : energy +
    danceability : energy : mode +
    track_genre +
    0,
  data = data
)

summary(lm19)

lm20 <- lm(
  popularity ~
    explicit +
    poly(danceability, 2) +
    valence +
    valence : energy +
    danceability : energy : mode +
    danceability : track_genre +
    0,
  data = data
)

summary(lm20)

lm21 <- lm(
  popularity ~
    danceability,
  data = data
)

summary(lm21)

lm22 <- lm(
  popularity ~
    danceability * valence * energy * acousticness * instrumentalness +
    0,
  data = data
)

summary(lm22)

lm23 <- lm(
  popularity ~
    danceability * valence * energy * acousticness * instrumentalness -
    acousticness -
    danceability : acousticness -
    energy : acousticness -
    valence : instrumentalness -
    acousticness : instrumentalness -
    danceability : energy : acousticness -
    danceability : valence : instrumentalness -
    valence : energy : instrumentalness -
    danceability : acousticness : instrumentalness -
    valence : acousticness : instrumentalness -
    energy : acousticness : instrumentalness -
    danceability : valence : energy : instrumentalness -
    danceability : valence : acousticness : instrumentalness -
    danceability : energy : acousticness : instrumentalness -
    valence : energy : acousticness : instrumentalness -
    danceability : valence : energy : acousticness : instrumentalness +
    0,
  data = data
)

summary(lm23)

lm24 <- lm(
  popularity ~
    danceability * track_genre +
    0,
  data = data
)

summary(lm24)

lm25 <- lm(
  popularity ~
    danceability +
    track_genre +
    0,
  data = data
)

summary(lm25)

lm26 <- lm(
  popularity ~
    danceability +
    track_genre,
  data = data
)

summary(lm26)

lm27 <- lm(
  popularity ~
    atan(danceability - 0.5) * track_genre,
  data = data
)

summary(lm27)

lm28 <- lm(
  popularity ~
    atan(danceability - 0.5) * track_genre +
    0,
  data = data
)

summary(lm28)

lm29 <- lm(
  popularity ~
    atan(danceability - 0.5) * danceability * track_genre +
    0,
  data = data
)

summary(lm29)

lm30 <- lm(
  popularity ~
    danceability +
    ifelse(danceability > 0.75, 1, 0),
  data = data
)

summary(lm30)

lm31 <- lm(
  popularity ~
    danceability +
    ifelse(danceability > 0.75, 1, 0) +
    0,
  data = data
)

summary(lm31)

lm32 <- lm(
  popularity ~
    danceability +
    ifelse(danceability > 0.75, 1, 0) +
    track_genre +
    0,
  data = data
)

summary(lm32)

lm32 <- lm(
  popularity ~
    danceability +
    ifelse(danceability > 0.75, 1, 0) +
    track_genre +
    0,
  data = data
)

summary(lm32)

lm33 <- lm(
  popularity ~
    danceability +
    ifelse(danceability > 0.75, 1, 0) +
    valence +
    track_genre +
    0,
  data = data
)

summary(lm33)

lm34 <- lm(
  popularity ~
    danceability +
    ifelse(danceability > 0.50, 1, 0) +
    valence +
    track_genre +
    0,
  data = data
)

summary(lm34)

lm35 <- lm(
  popularity ~
    danceability * ifelse(danceability > 0.50, 1, 0) +
    valence +
    track_genre +
    0,
  data = data
)

summary(lm35)

lm36 <- lm(
  popularity ~
    danceability : ifelse(danceability > 0.50, 1, 0) +
    valence : ifelse(valence > 0.50, 1, 0) +
    track_genre +
    0,
  data = data
)

summary(lm36)

lm37 <- lm(
  popularity ~
    danceability +
    track_genre +
    atan2(loudness, energy) +
    0,
  data = data
)

summary(lm37)

lm38 <- lm(
  popularity ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * track_genre +
    energy * loudness +
    0,
  data = data
)

summary(lm38)

lm39 <- lm(
  popularity ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * track_genre +
    energy * loudness +
    0,
  data = data_rel_10
)

summary(lm39)

lm40 <- lm(
  sqrtpop ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * track_genre +
    energy * loudness +
    0,
  data = data_rel_10
)

summary(lm40)

lm41 <- lm(
  sqrtpop ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * track_genre +
    energy * loudness,
  data = data_rel_10
)

summary(lm41)

lm42 <- lm(
  sqrtpop ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_10
)

summary(lm42)

lm43 <- lm(
  sqrtpop ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_20
)

summary(lm43)

lm44 <- lm(
  sqrtpop ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_30
)

summary(lm44)

lm45 <- lm(
  sqrtpop ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_40
)

summary(lm45)

lm46 <- lm(
  sqrtpop ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_50
)

summary(lm46)

lm47 <- lm(
  sqrtpop ~
    explicit +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_60
)

summary(lm47)

lm48 <- lm(
  sqrtpop ~
    explicit_True +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_10
)

summary(lm48)

lm49 <- lm(
  asinh(popularity) ~
    explicit_True +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_10
)

summary(lm49)

lm50 <- lm(
  sqrt(1 + max(sqrtpop) - sqrtpop) ~
    explicit_True +
    danceability +
    speechiness +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_10
)

summary(lm50)

lm51 <- lm(
  sqrt(1 + max(sqrtpop) - sqrtpop) ~
    explicit_True +
    danceability +
    acousticness +
    valence +
    energy * loudness +
    0,
  data = data_rel_10
)

summary(lm51)

lm52 <- lm(
  sqrt(1 + max(sqrtpop) - sqrtpop) ~
    explicit_True +
    danceability +
    acousticness +
    valence +
    energy * loudness +
    track_genre +
    0,
  data = data_rel_10
)

summary(lm52)

lm53 <- lm(
  sqrtpop ~
    explicit_True +
    danceability +
    acousticness +
    valence +
    energy * loudness +
    track_genre +
    0,
  data = data_rel_10
)

summary(lm53)

lm54 <- lm(
  asinh(sqrtpop) ~
    explicit_True +
    danceability +
    acousticness +
    valence +
    energy * loudness +
    track_genre +
    0,
  data = data_rel_10
)

summary(lm54)

lm55 <- lm(
  asinh(popularity) ~
    explicit_True +
    danceability +
    acousticness +
    valence +
    energy * loudness +
    track_genre +
    0,
  data = data_rel_10
)

summary(lm55)

lm56 <- lm(
  asinh(sqrtpop) ~
    explicit_True +
    danceability +
    acousticness +
    valence +
    energy * loudness +
    track_genre,
  data = data_rel_10
)

summary(lm56)

lmfinal <- lm48
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

end_time <- Sys.time()
sprintf("Total time to run: %.3f seconds", difftime(end_time, start_time, units = "secs"))
