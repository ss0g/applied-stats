lm01 <- function() {
  return(
    lm(
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
  )
}

lm02 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        danceability +
        energy +
        mode,
      data = data
    )
  )
}

lm03 <- function() {
  return(
    lm(
      popularity ~
        danceability,
      data = data
    )
  )
}

lm04 <- function() {
  return(
    lm(
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
  )
}

lm05 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        danceability +
        energy +
        mode +
        valence +
        0,
      data = data
    )
  )
}

lm06 <- function() {
  return(
    lm(
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
  )
}

lm07 <- function() {
  return(
    lm(
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
  )
}

lm08 <- function() {
  return(
    lm(
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
  )
}

lm09 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        danceability +
        energy +
        speechiness +
        acousticness +
        valence +
        energy:track_genre +
        0,
      data = data
    )
  )
}

lm10 <- function() {
  return(
    lm(
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
  )
}

lm11 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        danceability +
        energy +
        speechiness +
        acousticness +
        valence +
        track_genre:energy +
        0,
      data = data
    )
  )
}

lm12 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        energy +
        speechiness +
        valence +
        energy:track_genre_afrobeat +
        energy:`track_genre_alt-rock` +
        energy:track_genre_alternative +
        energy:`track_genre_black-metal` +
        energy:track_genre_bluegrass +
        energy:track_genre_breakbeat +
        energy:`track_genre_chicago-house` +
        energy:track_genre_chill +
        energy:track_genre_classical +
        energy:track_genre_club +
        energy:track_genre_comedy +
        energy:track_genre_country +
        energy:track_genre_dance +
        energy:track_genre_dancehall +
        energy:`track_genre_death-metal` +
        energy:`track_genre_detroit-techno` +
        energy:track_genre_disco +
        energy:track_genre_disney +
        energy:`track_genre_drum-and-bass` +
        energy:track_genre_edm +
        energy:track_genre_electro +
        energy:track_genre_funk +
        energy:track_genre_german +
        energy:track_genre_goth +
        energy:track_genre_grindcore +
        energy:track_genre_guitar +
        energy:track_genre_happy +
        energy:track_genre_hardstyle +
        energy:`track_genre_heavy-metal` +
        energy:`track_genre_honky-tonk` +
        energy:track_genre_house +
        energy:track_genre_idm +
        energy:track_genre_industrial +
        energy:track_genre_iranian +
        energy:`track_genre_j-dance` +
        energy:`track_genre_j-idol` +
        energy:track_genre_jazz +
        energy:`track_genre_k-pop` +
        energy:track_genre_kids +
        energy:track_genre_latin +
        energy:track_genre_latino +
        energy:track_genre_malay +
        energy:`track_genre_minimal-techno` +
        energy:track_genre_opera +
        energy:track_genre_party +
        energy:`track_genre_pop-film` +
        energy:`track_genre_power-pop` +
        energy:track_genre_reggae +
        energy:track_genre_reggaeton +
        energy:track_genre_rock +
        energy:track_genre_rockabilly +
        energy:track_genre_romance +
        energy:track_genre_sad +
        energy:track_genre_salsa +
        energy:track_genre_sleep +
        energy:track_genre_soul +
        energy:track_genre_study +
        energy:track_genre_tango +
        energy:`track_genre_trip-hop` +
        0,
      data = data
    )
  )
}

lm13 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        energy +
        speechiness +
        valence +
        valence:track_genre +
        0,
      data = data
    )
  )
}

lm14 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        energy +
        speechiness +
        valence +
        valence:mode +
        valence:mode:track_genre +
        0,
      data = data
    )
  )
}

lm15 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        poly(danceability, 2) +
        valence +
        0,
      data = data
    )
  )
}

lm16 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        poly(danceability, 2) * valence +
        0,
      data = data
    )
  )
}

lm17 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        poly(danceability, 3) +
        danceability:valence:energy:mode +
        track_genre +
        0,
      data = data
    )
  )
}

lm18 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        poly(danceability, 3) +
        danceability * valence * energy * mode +
        track_genre +
        0,
      data = data
    )
  )
}

lm19 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        poly(danceability, 2) +
        valence +
        valence:energy +
        danceability:energy:mode +
        track_genre +
        0,
      data = data
    )
  )
}

lm20 <- function() {
  return(
    lm(
      popularity ~
        explicit +
        poly(danceability, 2) +
        valence +
        valence:energy +
        danceability:energy:mode +
        danceability:track_genre +
        0,
      data = data
    )
  )
}

lm21 <- function() {
  return(
    lm(
      popularity ~
        danceability,
      data = data
    )
  )
}

lm22 <- function() {
  return(
    lm(
      popularity ~
        danceability * valence * energy * acousticness * instrumentalness +
        0,
      data = data
    )
  )
}

lm23 <- function() {
  return(
    lm(
      popularity ~
        danceability * valence * energy * acousticness * instrumentalness -
        acousticness -
        danceability:acousticness -
        energy:acousticness -
        valence:instrumentalness -
        acousticness:instrumentalness -
        danceability:energy:acousticness -
        danceability:valence:instrumentalness -
        valence:energy:instrumentalness -
        danceability:acousticness:instrumentalness -
        valence:acousticness:instrumentalness -
        energy:acousticness:instrumentalness -
        danceability:valence:energy:instrumentalness -
        danceability:valence:acousticness:instrumentalness -
        danceability:energy:acousticness:instrumentalness -
        valence:energy:acousticness:instrumentalness -
        danceability:valence:energy:acousticness:instrumentalness +
        0,
      data = data
    )
  )
}

lm24 <- function() {
  return(
    lm(
      popularity ~
        danceability * track_genre +
        0,
      data = data
    )
  )
}

lm25 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        track_genre +
        0,
      data = data
    )
  )
}

lm26 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        track_genre,
      data = data
    )
  )
}

lm27 <- function() {
  return(
    lm(
      popularity ~
        atan(danceability - 0.5) * track_genre,
      data = data
    )
  )
}

lm28 <- function() {
  return(
    lm(
      popularity ~
        atan(danceability - 0.5) * track_genre +
        0,
      data = data
    )
  )
}

lm29 <- function() {
  return(
    lm(
      popularity ~
        atan(danceability - 0.5) * danceability * track_genre +
        0,
      data = data
    )
  )
}

lm30 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        ifelse(danceability > 0.75, 1, 0),
      data = data
    )
  )
}

lm31 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        ifelse(danceability > 0.75, 1, 0) +
        0,
      data = data
    )
  )
}

lm32 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        ifelse(danceability > 0.75, 1, 0) +
        track_genre +
        0,
      data = data
    )
  )
}

lm32 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        ifelse(danceability > 0.75, 1, 0) +
        track_genre +
        0,
      data = data
    )
  )
}

lm33 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        ifelse(danceability > 0.75, 1, 0) +
        valence +
        track_genre +
        0,
      data = data
    )
  )
}

lm34 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        ifelse(danceability > 0.50, 1, 0) +
        valence +
        track_genre +
        0,
      data = data
    )
  )
}

lm35 <- function() {
  return(
    lm(
      popularity ~
        danceability * ifelse(danceability > 0.50, 1, 0) +
        valence +
        track_genre +
        0,
      data = data
    )
  )
}

lm36 <- function() {
  return(
    lm(
      popularity ~
        danceability:ifelse(danceability > 0.50, 1, 0) +
        valence:ifelse(valence > 0.50, 1, 0) +
        track_genre +
        0,
      data = data
    )
  )
}

lm37 <- function() {
  return(
    lm(
      popularity ~
        danceability +
        track_genre +
        atan2(loudness, energy) +
        0,
      data = data
    )
  )
}

lm38 <- function() {
  return(
    lm(
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
  )
}

lm39 <- function() {
  return(
    lm(
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
  )
}

lm40 <- function() {
  return(
    lm(
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
  )
}

lm41 <- function() {
  return(
    lm(
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
  )
}

lm42 <- function() {
  return(
    lm(
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
  )
}

lm43 <- function() {
  return(
    lm(
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
  )
}

lm44 <- function() {
  return(
    lm(
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
  )
}

lm45 <- function() {
  return(
    lm(
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
  )
}

lm46 <- function() {
  return(
    lm(
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
  )
}

lm47 <- function() {
  return(
    lm(
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
  )
}

lm48 <- function() {
  return(
    lm(
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
  )
}

lm49 <- function() {
  return(
    lm(
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
  )
}

lm50 <- function() {
  return(
    lm(
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
  )
}

lm51 <- function() {
  return(
    lm(
      sqrt(1 + max(sqrtpop) - sqrtpop) ~
        explicit_True +
        danceability +
        acousticness +
        valence +
        energy * loudness +
        0,
      data = data_rel_10
    )
  )
}

lm52 <- function() {
  return(
    lm(
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
  )
}

lm53 <- function() {
  return(
    lm(
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
  )
}

lm54 <- function() {
  return(
    lm(
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
  )
}

lm55 <- function() {
  return(
    lm(
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
  )
}

lm56 <- function() {
  return(
    lm(
      asinh(sqrtpop) ~
        explicit_True +
        danceability +
        acousticness +
        valence +
        energy * loudness +
        track_genre,
      data = data_rel_10
    )
  )
}

lm57 <- function() {
  return(
    lm(
      popularity ~
        explicit_True +
        danceability +
        speechiness +
        acousticness +
        valence +
        energy +
        energy:genre_category_A +
        energy:genre_category_B +
        energy:genre_category_C +
        energy:genre_category_D +
        energy:genre_category_E +
        energy:genre_category_F +
        energy:genre_category_G +
        energy:genre_category_H,
      data = data_rel_10
    )
  )
}

lm58 <- function() {
  return(
    lm(
      popularity ~
        explicit_True +
        danceability +
        speechiness +
        acousticness +
        valence +
        energy +
        genre_category_A +
        genre_category_B +
        genre_category_C +
        genre_category_D +
        genre_category_E +
        genre_category_F +
        genre_category_G +
        genre_category_H,
      data = data_rel_10
    )
  )
}

lm59 <- function() {
  return(
    lm(
      sqrtpop ~
        explicit_True +
        poly(danceability, 2) +
        poly(speechiness, 2) +
        poly(acousticness, 2) +
        poly(valence, 2) +
        poly(energy, 2) +
        genre_category_A +
        genre_category_B +
        genre_category_C +
        genre_category_D +
        genre_category_E +
        genre_category_F +
        genre_category_G +
        genre_category_H,
      data = data_rel_10
    )
  )
}

lm60 <- function() {
  return(
    lm(
      sqrtpop ~
        explicit_True +
        poly(danceability, 3) +
        poly(speechiness, 3) +
        poly(acousticness, 3) +
        poly(valence, 3) +
        poly(energy, 3) +
        genre_category_A +
        genre_category_B +
        genre_category_C +
        genre_category_D +
        genre_category_E +
        genre_category_F +
        genre_category_G +
        genre_category_H,
      data = data_rel_10
    )
  )
}

lm61 <- function() {
  return(
    lm(
      popularity ~
        explicit_True +
        danceability +
        speechiness +
        acousticness +
        valence * energy +
        genre_category_A +
        genre_category_B +
        genre_category_C +
        genre_category_D +
        genre_category_E +
        genre_category_F +
        genre_category_G +
        genre_category_H,
      data = data_rel_10
    )
  )
}

lm62 <- function() {
  return(
    lm(
      popularity ~
        explicit_True +
        danceability +
        speechiness +
        acousticness +
        valence * energy +
        genre_category_A +
        genre_category_B +
        genre_category_E +
        genre_category_F +
        genre_category_G +
        genre_category_H,
      data = data_rel_10
    )
  )
}

lm63 <- function() {
  return(
    lm(
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
      data = data_rel_10
    )
  )
}

lm64 <- function() {
  return(
    lm(
      popularity ~
        explicit_True +
        danceability +
        speechiness +
        acousticness +
        valence +
        energy +
        track_genre_afrobeat +
        `track_genre_alt.rock` +
        track_genre_alternative +
        `track_genre_black.metal` +
        track_genre_bluegrass +
        track_genre_breakbeat +
        `track_genre_chicago.house` +
        track_genre_chill +
        track_genre_club +
        track_genre_dance +
        `track_genre_detroit.techno` +
        track_genre_disney +
        `track_genre_drum.and.bass` +
        track_genre_electro +
        track_genre_goth +
        track_genre_grindcore +
        track_genre_guitar +
        track_genre_happy +
        track_genre_hardstyle +
        `track_genre_heavy.metal` +
        `track_genre_hip.hop` +
        `track_genre_honky.tonk` +
        track_genre_house +
        track_genre_idm +
        track_genre_indie +
        track_genre_indie.pop +
        `track_genre_j.dance` +
        `track_genre_j.idol` +
        `track_genre_k.pop` +
        track_genre_kids +
        track_genre_latino +
        track_genre_malay +
        track_genre_metal +
        track_genre_party +
        track_genre_pop +
        `track_genre_pop.film` +
        `track_genre_power.pop` +
        track_genre_reggaeton +
        track_genre_rock +
        track_genre_salsa +
        track_genre_soul +
        track_genre_study +
        track_genre_tango,
      data = data_rel_10
    )
  )
}

lm65 <- function() {
  return(
    lm(
      popularity ~
        explicit_True +
        danceability +
        speechiness +
        acousticness +
        track_genre_afrobeat +
        track_genre_black.metal +
        track_genre_bluegrass +
        track_genre_breakbeat +
        track_genre_chicago.house +
        track_genre_dance +
        track_genre_drum.and.bass +
        track_genre_electro +
        track_genre_grindcore +
        track_genre_happy +
        track_genre_heavy.metal +
        track_genre_honky.tonk +
        track_genre_idm +
        track_genre_j.idol +
        track_genre_kids +
        track_genre_pop +
        track_genre_pop.film +
        track_genre_tango,
      data = data_rel_10
    )
  )
}

lm66 <- function() {
  return(
    lm(
      popularity ~
        explicit_True +
        danceability +
        speechiness +
        acousticness +
        track_genre_afrobeat +
        track_genre_black.metal +
        track_genre_breakbeat +
        track_genre_grindcore +
        track_genre_happy +
        track_genre_honky.tonk +
        track_genre_idm +
        track_genre_j.idol +
        track_genre_kids +
        track_genre_pop +
        track_genre_tango,
      data = data_rel_10
    )
  )
}

lm67 <- function() {
  return(
    lm(
      popularity ~
        explicit_True +
        danceability +
        speechiness +
        track_genre_breakbeat +
        track_genre_grindcore +
        track_genre_honky.tonk +
        track_genre_kids +
        track_genre_pop +
        track_genre_tango,
      data = data_rel_10
    )
  )
}

lm68 <- function() {
  return(
    lm(
      sqrtpop ~
        explicit_True +
        danceability +
        speechiness +
        track_genre_breakbeat +
        track_genre_grindcore +
        track_genre_honky.tonk +
        track_genre_kids +
        track_genre_pop +
        track_genre_tango,
      data = data_rel_10
    )
  )
}

lm69 <- function() {
  return(
    lm(
      sqrtpop ~
        explicit_True +
        danceability +
        speechiness +
        acousticness +
        track_genre_afrobeat +
        track_genre_black.metal +
        track_genre_breakbeat +
        track_genre_grindcore +
        track_genre_happy +
        track_genre_honky.tonk +
        track_genre_idm +
        track_genre_j.idol +
        track_genre_kids +
        track_genre_pop +
        track_genre_tango,
      data = data_rel_10
    )
  )
}

get_final_model <- function() {
  return(lm69())
}
