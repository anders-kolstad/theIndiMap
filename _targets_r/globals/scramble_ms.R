scramble_ms <- function(x) {
  vector <- x |> 
    as.data.frame() |>
    separate_longer_delim(cols = x, delim=" | ") |>
    distinct()
  long <- sample(vector[,1], 150, replace = T)
  # add some double values
  secondvalue <- sample(long, 20) # in 20 places
  secondvalue <- secondvalue[secondvalue!= ""] # rm blanks
  rows <- sample(1:150, 20, replace = F) # pick random row(vector) positions
  long[rows] <-  paste(long[rows], secondvalue, sep= " | ") # paste second value 
  long <- str_replace(long, "^.[|].", "") # rm leading " | "
  return(long)
  }
