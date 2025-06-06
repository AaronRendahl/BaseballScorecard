get_score <- function(game) {
  score <- game$plays |> summarize(R=sum(ToBase==4), .by=c(Inning, Side)) |>
    pivot_wider(names_from=Inning, values_from=R) |>
    arrange(Side) |> select(-Side) |>
    as.matrix()
  score <- cbind(score, R=rowSums(score, na.rm=TRUE))
  rownames(score) <- game$teams
  score
}

## helper functions
MDY_format <- function(x) {
  out <- paste0(format(x, "%B %e, %Y, ") |> str_replace("  ", " "),
                format(x, "%I:%M%P") |> str_remove("^0"))
  if_else(is.na(x), as.character(x), out)
}

add_ToBase <- function(x, pattern.out="^X") {
  ## this needs to be a vectorized function,
  ## so here's a function that can handle one at bat
  f1 <- function(B1, B2, B3, B4) {
    if(is.na(B1)) return(0)
    B <- c("out", B1, B2, B3, B4)
    to1 <- max(which(!is.na(B))-1)
    toX <- replace_na(key$Base[match(B1, key$Outcome)], 0)
    max(toX, to1) + stringr::str_detect(B[to1+1], pattern.out)*(-0.5)
  }
  ## and then we'll map across it to vectorize it
  x |> mutate(ToBase=purrr::pmap_dbl(list(B1, B2, B3, B4), f1))
}

add_Pitches <- function(x, NoPitch=BaseballScorecard::codes |> filter(Pitch=="NoPitch") |> pull("Code")) {
  x |> mutate(Pitches=Balls + Strikes + Fouls + !(Play %in% NoPitch | B1 %in% NoPitch))
}

cleanplays <- function(d) {
  d |>
    mutate(across(c("Balls", "Strikes", "Fouls"), \(x) replace_na(x, 0L))) |>
    ## .0$ is in case Google to xlsx adds a .0 to numbers
    mutate(across(c("B2", "B3", "B4"), \(x) { x |> stringr::str_remove(pattern="\\.0$") }))
}
