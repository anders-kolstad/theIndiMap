##| tar_interactive: FALSE
scramble <- function(data) {
  #These are the multiple choice (mc) columns
  mc <- c("iContinent", "iCountry", "iET", "dOrigin", "iBiome")
  # resample to get 150 rows
  data[sample(1:nrow(data), 150, replace = T),] |>
    # replace the mc columns
    mutate(across(any_of(mc),scramble_ms))
}

