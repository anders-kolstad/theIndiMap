##| tar_interactive: FALSE

create_bar <- function(data, x) {
  data |>
    separate_longer_delim(x, " | ") |>
    ggplot() +
    geom_bar(aes_string(x = x),
             fill="#FFDB6D",
             colour="grey30") +
    theme_minimal(18)+
    coord_flip()+
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank())
}
