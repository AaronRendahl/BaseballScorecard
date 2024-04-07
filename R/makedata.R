## helper functions
get_ToBase <- function(B1, B2, B3, B4, pattern.out="^X") {
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
  purrr::pmap_dbl(list(B1, B2, B3, B4), f1)
}

get_LastPitch <- function(Play, NoPitch=key$Outcome[key$Pitch=="No Pitch"]) {
  !(Play %in% NoPitch)
}


