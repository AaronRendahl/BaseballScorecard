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
