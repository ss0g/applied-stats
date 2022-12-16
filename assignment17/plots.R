col_bg1 <- "#121212ff"
col_bg2 <- "#181818ff"
col_bg3 <- "#313131ff"
col_fg1 <- "#ffffffff" # "white"
col_fg2 <- "#b3b3b3ff" # "white if selected"
col_fg3 <- "#5e5e5eff" # "grayed out"
col_border <- "#282828ff"
col_spgreen <- "#1db954ff"

plot_theme <- function() {
  return(
    theme_foundation(base_family = "Gotham") +
    theme(
      plot.background = element_rect(color = col_bg2, fill = col_bg2),
      panel.background = element_rect(color = col_bg2, fill = col_bg2),
      panel.grid.major = element_line(color = col_fg3),
      panel.grid.minor = element_line(color = col_bg3),
      panel.border = element_blank(), # element_rect(color = col_border, linetype = 1),
      
      plot.title = element_text(color = col_fg1, face = "bold"),
      
      axis.title = element_text(color = col_fg2),
      axis.text = element_text(color = col_fg2),
      axis.ticks = element_line(color = col_fg3),
      
      strip.background = element_rect(color = col_bg3, fill = col_bg3),
      strip.text = element_text(color = col_fg2)
    )
  )
}

test_plot <- function() {
  return(
    ggplot(data_rel_10[sample(nrow(data_rel_10), 100),], aes(danceability, popularity)) +
      geom_point(color = col_spgreen) +
      plot_theme()
  )
}

bar_plot_genre <- function() {
  return(
    ggplot(mean_pops, aes(track_genre, popularity)) +
      geom_bar(fill = col_spgreen, stat = "identity") +
      scale_x_discrete() +
      plot_theme() +
      theme(
        axis.text.x = element_text(angle = 90)
      )
  )
}

bar_plot_genre_sorted <- function() { # TODO: fix labels
  return(
    ggplot(mean_pops, aes(reorder(track_genre, popularity), popularity)) +
      geom_bar(fill = col_spgreen, stat = "identity") +
      scale_x_discrete() +
      labs(
        x = "Genre",
        y = "Mean popularity",
        title = "Mean Popularity by Genre"
      ) +
      plot_theme() +
      theme(
        panel.grid.major.x = element_blank(),
        
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 4)
      )
  )
}

bar_plot_key <- function() {
  return(
    ggplot(mean_pops_key, aes(key, popularity)) +
      geom_bar(fill = col_spgreen, stat = "identity") +
      scale_x_discrete() +
      plot_theme() +
      theme(
        panel.grid.major.x = element_blank(),
        
        axis.text.x = element_text(angle = 90)
      )
  )
}

`?_plot_key` <- function() {
  return(
    ggplot(mean_pops_key, aes(key, popularity)) +
      geom_point(color = col_spgreen) +
      scale_x_discrete() +
      plot_theme() +
      theme(
        panel.grid.major.x = element_blank()
      )
  )
}

violin_plot_key <- function() {
  return(
    ggplot(data, aes(key, popularity)) +
      geom_violin(fill = col_spgreen) +
      plot_theme() +
      theme(
        panel.grid.major.x = element_blank()
      )
  )
}

violin_box_plot_key <- function() {
  return(
    violin_plot_key() +
      geom_boxplot(fill = col_spgreen, width = 0.2)
  )
}

scatter_plot_matrix_popularity <- function() {
  return(
    ggmatrix(
      list(
        ggplot(data_rel_10_sample_100, aes(danceability, popularity)),
        ggplot(data_rel_10_sample_100, aes(energy, popularity)),
        ggplot(data_rel_10_sample_100, aes(loudness, popularity)),
        ggplot(data_rel_10_sample_100, aes(speechiness, popularity)),
        ggplot(data_rel_10_sample_100, aes(acousticness, popularity)),
        ggplot(data_rel_10_sample_100, aes(instrumentalness, popularity)),
        ggplot(data_rel_10_sample_100, aes(liveness, popularity)),
        ggplot(data_rel_10_sample_100, aes(valence, popularity))
      ),
      1,
      10,
      xAxisLabels = colnames(data_rel_10)[c(4, 5, 7, 9:13)] |> lapply(tools::toTitleCase) |> unlist(),
      ylab = "popularity"
    ) +
      geom_point(color = col_spgreen) +
      labs(
        y = "Popularity",
        title = "Popularity vs. Other Variables"
      ) +
      plot_theme() +
      theme(
        strip.text = element_text(size = 6),
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1)
      )
  )
}

density_plot_popularity <- function() {
  return(
    ggplot(data, aes(popularity)) +
      geom_density(color = col_spgreen, fill = col_spgreen) +
      labs(
        x = "Popularity",
        y = "Density",
        title = "Distribution of Popularity"
      ) +
      plot_theme()
  )
}

