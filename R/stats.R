get_Contact <- function(p) {
  ## do have some 1Bs with unknown Play for other teams...
  p |> select(B1, Play, Fielders) |>
    mutate(Play=if_else(B1 %in% c("BB", "HB"), B1, Play)) |>
    mutate(Play=na_if(Play, "_")) |>
    rename(HitType="Play") |>
    mutate(HitLocation=str_sub(Fielders, 1, 1) |> as.integer()) |>
    mutate(HitFar=if_else(HitLocation>=7, "Out", "In"),
           HitHard=if_else(HitFar=="Out" | HitType=="L", "Hard", "Soft"),
           HitType=str_replace(HitType, "K.*", "K"),
           HitType=str_replace(HitType, "[HB]B", "BBHB"),
           Outcome=if_else(is.na(HitHard), HitType, HitHard)) |>
    mutate(Outcome=if_else(is.na(Outcome) & B1=="1B", "Soft",
                           if_else(is.na(Outcome) & B1 %in% c("2B", "3B", "HR"), "Hard", Outcome))) |>
    select(Outcome) |>
    mutate(X=1, idx=1:n()) |>
    pivot_wider(names_from=Outcome, values_from=X) |>
    select(Soft, Hard) |>
    mutate(across(everything(), \(x) replace_na(x, 0L)))
}


counting_stats_all <- function(g) {
  p <- g |> select(code, plays) |> unnest(plays)

  cs <- counting_stats(p) |> bind_cols(get_Contact(p)) |>
    mutate(AB = PA - SH - SF - BB - HB - CI - O) |>
    mutate(Balls = Balls + Ball,
           Strikes = Strikes + Fouls + Strike,
           Pitches = Balls + Strikes + Fouls) |>
    mutate(HBP=HB, BF=PA) |>
    mutate(MP=MP*onPlay, MR=MR*onPlay) |>
    mutate("SB+"=(SB+WP+PB+DI)*(1-Outs),
           "CS+"=(SB+WP+PB+DI)*Outs,
           "TA"=(AX + Balk + CS + AE + AFC + APB + PO + SB + AWP + AE + SFC + DI)*(1-Outs))

  cn <- c("Pitches", "Balls", "Strikes", "Fouls", "Outs", "R", "LOB",
          setdiff(names(cs), names(p))) |> setdiff(".idx")

  list(stats=cs, names=cn)
}

## Pitches, Balls, Strikes, Fouls, Play, B1, Advance, Base, Outs, Fielders
counting_stats <- function(d, key=BaseballScorecard::codes) {

  #codes <- readxl::read_excel("inst/extdata/codes.xlsx", "codes")
  #usethis::use_data(codes, overwrite=TRUE)

  d <- d |> mutate(.idx=1:n())

  key3 <- key |> select(Type, Code, ends_with("_")) |>
    pivot_longer(ends_with("_"), values_drop_na = TRUE) |>
    mutate(Type=paste0(name, Type)) |>
    select(-name)
  key <- key |> select(-Description) |> select(-ends_with("_"))
  key.num <- key |> select(Type, Code, where(is.numeric))
  key.chr <- key |> select(Type, Code, !where(is.numeric)) |>
    pivot_longer(c(-Type, -Code), values_drop_na = TRUE) |>
    select(-name) |>
    bind_rows(key3) |>
    mutate(X=1L) |>
    pivot_wider(names_from=value, values_from=X)
  key2 <- full_join(key.num, key.chr, by=c("Type", "Code")) |>
    mutate(across(c(-Code), \(x) {
      if(all(is.na(x))) x
      else replace_na(x, 0)
    }), .by=Type)

  ## can't reuse names from the original counting data
  stopifnot(!any(names(key2) %in% names(d)))
  nn <- setdiff(names(key2), c("Type", "Code"))

  ## check that all codes are found in the key
  mis <- setdiff(c(d$Play, d$B1, d$Advance, d$Advance_Play, d$Advance_B1), c(NA, key$Code))
  if(length(mis)>0) {
    stop(mis, " not found!")
  }

  dx <- d |> select(.idx, Play, B1, Advance,
                    Advance_Play, Advance_B1) |>
    pivot_longer(c(Play, B1, Advance, Advance_Play, Advance_B1),
                 values_drop_na = TRUE,
                 names_to="Type", values_to="Code") |>
    left_join(key2, by=c("Type", "Code")) |> select(-Type, -Code) |>
    summarize(across(everything(), \(x) {
      if(length(x)>1) {
        if(all(is.na(x))) {
          x <- NA
        } else {
          x <- x[!is.na(x)]
          if(length(x)>1) {
            x <- unique(x)
            if(length(x)>1) {
              stop("discrepancy!")
            }
          }
        }
      }
      x
    }), .by=c(.idx)) |>
    mutate(across(everything(), \(x) replace_na(x, 0)))
  d |> left_join(dx, by=".idx") |>
    mutate(Bases=case_when(Outs > 0 ~ 0,
                           is.na(Advance) ~ Base,
                           !is.na(Advance) ~ 1))
}
