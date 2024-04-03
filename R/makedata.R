## helper functions

get_ToBase <- function(B1, B2, B3, B4, pattern.out="^X") {
  ## this needs to be a vectorized function, so
  ## here's a function that can handle one at bat
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
get_OutDuring <- function(B2, B3, B4) {
  f1 <- function(B2, B3, B4) {
    out <- NA
    B <- c(B2, B3, B4)
    ## need to remove any X at the beginning and any pitches at the end
    ## for the following code to find the at-bat correctly
    X <- stringr::str_subset(B, "^X") |> str_remove("-[0-9]+$")
    if(length(X) > 0) {
      out <- stringr::str_replace(X, ".*[^0-9]([0-9]*)$", "\\1") |>
        as.integer() |> replace_na(0L)
    }
    out
  }
  purrr::pmap_int(list(B2, B3, B4), f1)
}
get_Outcome <- function(Play, B1) {
  out <- if_else(!is.na(B1), B1, stringr::str_sub(Play, 1, 1)) |> stringr::str_replace("^E.*", "E")
  ## error check
  if(!all(out %in% key$Outcome)) {
    tmp <- out[! out %in% key$Outcome]
    warning(paste(paste0("[",tmp,"]"), collapse=" "))
  }
  out
}
get_RunnersOut <- function(Lineup, Inning, OutDuring) {
  out <- tibble(Lineup=Lineup, Inning=Inning, OutDuring=OutDuring)
  out <- out |> mutate(RunnersOut=0, idx=1:n())
  tmp <- out |> filter(!is.na(OutDuring))
  for(i in seq_len(nrow(tmp))) {
    if(tmp$OutDuring[i]==0) {
      out$RunnersOut[tmp$idx[i]] <- out$RunnersOut[tmp$idx[i]] + 1
    } else {
      foo <- filter(out, Lineup==tmp$OutDuring[i] & idx >= tmp$idx[i] & Inning==tmp$Inning[i])
      stopifnot(nrow(foo)>0)
      out$RunnersOut[foo$idx[1]] <- out$RunnersOut[foo$idx[1]] + 1
    }
  }
  pull(out, RunnersOut)
}
get_PitchesAtBat <- function(Count, Outcome) {
  Count + (key$Pitch[match(Outcome, key$Outcome)]!="No Pitch")*1L
}
get_Contact <- function(Play, B1) {
  ## do have some 1Bs with unknown Play for other teams...
  tibble(Play=Play, B1=B1) |>
    mutate(Play=if_else(B1 %in% c("BB", "HB"), B1, Play)) |>
    mutate(Play=na_if(Play, "_")) |>
    mutate(Play2 = str_replace(Play, "([A-Z]+)([1-9]).*", "\\1-\\2")) |>
    separate(Play2, c("HitType", "HitLocation"), remove = FALSE, convert=TRUE, fill="right") |>
    mutate(HitFar=if_else(HitLocation>=7, "Out", "In"),
           HitHard=if_else(HitFar=="Out" | HitType=="L", "Hard", "Soft"),
           HitType=str_replace(HitType, "K.*", "K"),
           HitType=str_replace(HitType, "[HB]B", "BBHB"),
           Outcome=if_else(is.na(HitHard), HitType, HitHard)) |>
    mutate(Outcome=if_else(is.na(Outcome) & B1=="1B", "Soft",
                           if_else(is.na(Outcome) & B1 %in% c("2B", "3B", "HR"), "Hard", Outcome))) |>
    pull(Outcome)
}

makedata <- function(d) {
  ## now process as needed, adding variables
  ## Outcome: to match Outcome column in key
  ## ToBase: which base they got to (use 0.5 to specify out between; eg, 2.5 if out between 2 and 3)
  ## OutDuring: if batter gets out later, during what at-bat did it happen?
  ## RunnersOut: how many runners got out during this at bat?
  ## PitchesAtBat: total pitches during at-bat
  d |>
    mutate(across(c("Balls", "Strikes", "Fouls"), \(x) replace_na(x, 0L))) |>
    mutate(across(c("B2", "B3", "B4"), \(x) {
      ## .0$ is in case Google to xlsx adds a .0 to numbers
      x |> stringr::str_remove(pattern="^X") |> stringr::str_remove(pattern="\\.0$")
    })) |>
    mutate(ToBase=get_ToBase(B1, B2, B3, B4)) |>
    mutate(Outcome=get_Outcome(Play, B1),
           OutDuring=get_OutDuring(B2, B3, B4),
           RunnersOut=get_RunnersOut(Lineup, Inning, OutDuring),
           Contact=get_Contact(Play, B1)) |>
    mutate(PitchesAtBat=get_PitchesAtBat(Balls + Strikes + Fouls, Outcome)) |>
    mutate(Row = 1:n()) |>
    select(Inning, Row, Lineup, everything())
}
