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
