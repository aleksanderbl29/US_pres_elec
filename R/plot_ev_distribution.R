plot_ev_distribution <- function(data, p_dem, p_rep, p_draw) {
  data %>%
    filter(t == max(t)) %>%
    group_by(draw) %>%
    summarise(dem_ev = sum(ev * (p_harris > 0.5))) |>
    mutate(winner = case_when(
      dem_ev >= 270 ~ "Democratic",
      dem_ev < 269 ~ "Republican",
      dem_ev == 269 ~ "No winner")) |>
    ggplot(aes(x = dem_ev, fill = winner)) +
    geom_vline(xintercept = 269.5) +
    geom_histogram(binwidth = 1) +
    theme_classic() +
    # cowplot::theme_cowplot() +
    scale_fill_manual(
      values = c('Democratic' = '#3A4EB1',
                 'Republican' = '#E40A04',
                 "No winner" = "black")) +
    labs(x = "Electoral college votes",
         y = NULL,
         fill = NULL) +
    coord_cartesian(expand = FALSE) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(),
      plot.caption = element_text(hjust = 0.5, face = "italic", size = 8)
    )
}

table_ev_distribution <- function(p_dem, p_rep, p_draw) {
  tibble(
    "Democratic" = p_dem,
    "No winner" = p_draw,
    "Republican" = p_rep
  ) %>%
    # gt() %>%
    # tab_style(
    #   style = list(
    #     cell_fill(color = "#3A4EB1"),
    #     cell_text(color = "white")  # For better contrast on blue
    #   ),
    #   locations = cells_column_labels(columns = "Democratic")
    # ) %>%
    # tab_style(
    #   style = list(
    #     cell_fill(color = "black"),
    #     cell_text(color = "white")  # For contrast on black
    #   ),
    #   locations = cells_column_labels(columns = "No winner")
    # ) %>%
    # tab_style(
    #   style = list(
    #     cell_fill(color = "#E40A04"),
    #     cell_text(color = "white")  # For contrast on red
    #   ),
    #   locations = cells_column_labels(columns = "Republican")
    # )
  tinytable::tt() %>%
    tinytable::style_tt(i = 0, j = 1:3, color = "white",
                        background = c("#3A4EB1", "#000000", "#E40A04"))
}


get_win_prob <- function(data, type) {

  if (type == "dem") {
    data$prob_dem[data$state == "--" & data$t == max(data$t)]
  } else if (type == "rep") {
    data$prob_rep[data$state == "--" & data$t == max(data$t)]
  } else if (type == "draw") {
    data$prob_draw[data$state == "--" & data$t == max(data$t)]
  }

}
