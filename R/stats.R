get_score <- function(game) {
  score <- game$plays |> summarize(R=sum(ToBase==4), .by=c(Inning, Side)) |>
    pivot_wider(names_from=Inning, values_from=R) |>
    arrange(Side) |> select(-Side) |>
    as.matrix()
  score <- cbind(score, R=rowSums(score, na.rm=TRUE))
  rownames(score) <- game$teams
  score
}

## RBI, sort of; who was at bat when run scored, which isn't quite the same thing
# d1 |> select(Lineup, Outcome, B4) |> mutate(RBI_by=if_else(Outcome=="HR", Lineup, B4)) |>
#   filter(!is.na(RBI_by)) |> select(Lineup=RBI_by) |>
#   group_by(Lineup) |> summarize(RBI=n(), .groups="drop")

getBA <- function(H, AB) {
  if_else(!is.na(AB) & (AB > 1), H/AB, as.numeric(NA))
}
getIP <- function(x) {
  a <- x %/% 3
  b <- x %% 3
  a + b/10
}

## STATS CALCULATIONS
batter_calculations <- list("BA" = "getBA(H, AB)",
                            "K/PA" = "K / PA",
                            "OBPE" = "(H + BB + HBP + ROE) / PA",
                            "SLG" = "TB / AB",
                            "SLG + OBPE + notK/PA:\nBatting Sum" = "SLG + OBPE + (1 - `K/PA`)",
                            K.="K", BBHB="BB+HBP", BIP="PA - K - BBHB", H.="H",
                            "BIP/AB"="BIP/AB", "Hard/BIP"="Hard/BIP",
                            "Blank" = NA) |>
  lapply(function(x) parse(text=x))

batter_cols <- c("PA", "H", "AB", "BA", "R", Blank3="Blank", "K", "BB", "HBP", "ROE",
                 "1B", "2B", "3B", "HR")
batter_cols_ind <- c("Number", "Name", "Lineup", batter_cols)
batter_cols_team <- c(Blank1="Blank", "Team", Blank2="Blank", batter_cols,
                      "SLG", "OBPE", "K/PA")
batter_cols_total <- c("Number", "Name", "Games", "Blank1"="Blank",
                       batter_cols,
                       "SLG", "OBPE", "K/PA",
                       "SLG + OBPE + notK/PA:\nBatting Sum",
                       Blank4="Blank",
                       "K.", "BBHB", "BIP", "H.", "Soft", "Hard",
                       "BIP/AB", "Hard/BIP")

pitcher_calculations <- list("SR" = "Strikes/Pitches", "SR." = "SR",
                             "IP" = "getIP(Outs)",
                             "BBHB/BF" = "(BB + HB) / BF",
                             "Opp. OBP" = "(H + BB + HB) / BF",
                             "SR + notOB + notBBHB:\nPitching Sum" = "SR + (1-`Opp. OBP`) + (1-`BBHB/BF`)",
                             K.="K", BBHB="BB+HB", BIP="BF - K - BBHB", H.="H",
                             "Strike Rate"="SR",
                             "BIP/Strikes"="BIP/Strikes",
                             "Blank" = NA) |>
  lapply(function(x) parse(text=x))
pitcher_cols1 <- c("IP", "BF", "Strikes", "Pitches", "SR", "H", "AB", "K", "BB", "HB", "ROE",
                  "1B", "2B", "3B", "HR")
pitcher_cols2 <- c("SR.", "Opp. OBP", "BBHB/BF", "SR + notOB + notBBHB:\nPitching Sum")
pitcher_cols_ind <- c("Number", "Name", pitcher_cols1, pitcher_cols2)
pitcher_cols_team <- c("Team", pitcher_cols1, pitcher_cols2)
pitcher_cols_total <- c("Number", "Name", "Games",
                        pitcher_cols1, pitcher_cols2,
                        "Blank",
                        "K.", "BBHB", "BIP", "H.", "Soft", "Hard",
                        "Strike Rate", "BIP/Strikes")

runner_cols <- c("R", "SB+", "CS+")

calc_stats <- function(data, count_vars, calculations, by, total) {
  d <- data |> summarize(Games=length(unique(code)),
                         across(all_of(count_vars), \(x) sum(x, na.rm=TRUE)),
                         .by=all_of(by))
  if(!missing(total)) {
    d2 <- bind_cols(Games=length(unique(data$code)),
                    as_tibble(t(colSums(d[,count_vars]))),
                    as_tibble(t(total)))
    d <- bind_rows(d, d2)
  }
  for(n in names(calculations)) {
    d[[n]] <- with(d, eval(calculations[[n]]))
  }
  d
}

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
    #select(-any_of(c("NA", "idx"))) |>
    #rename_with(\(x) paste0(x, "_contact"))
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
           "CS+"=(SB+WP+PB+DI)*Outs)

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
