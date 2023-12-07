##| tar_interactive: FALSE
compile_profiles <- function(files) {
  files |>
    set_names(basename) |>
    map(read.csv) %>%
  list_rbind(names_to = "fileName") |>
  tidyr::pivot_wider(names_from = parameter, values_from = value)}
