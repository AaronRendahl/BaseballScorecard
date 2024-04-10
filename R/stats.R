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
                            "SLG" = "(`1B` + 2*`2B` + 3*`3B` + 4*`HR`) / AB",
                            "SLG + OBPE + notK/PA:\nBatting Sum" = "SLG + OBPE + (1 - `K/PA`)",
                            K.="K", BBHB="BB+HBP", BIP="PA - K - BBHB", H.="H",
                            "Blank" = NA) |>
  lapply(function(x) parse(text=x))

batter_cols_ind <- c("PA", "H", "AB", "BA", "R", "Blank", "K", "BB", "HBP", "ROE",
                     "1B", "2B", "3B", "HR")
batter_cols_team <- c(batter_cols_ind, "SLG", "OBPE", "K/PA")
batter_cols_total <- c(batter_cols_team,
                       "SLG + OBPE + notK/PA:\nBatting Sum",
                       "Blank",
                       "K.", "BBHB", "BIP", "H.")
attr(batter_cols_total, "sortby") <- "SLG + OBPE + notK/PA:\nBatting Sum"

pitcher_calculations <- list("SR" = "S/P", "SR." = "S/P",
                             "IP" = "getIP(Outs)",
                             "BBHB/BF" = "(BB + HB) / BF",
                             "Opp. OBP" = "(H + BB + HB) / BF",
                             "SR + notOB + notBBHB:\nPitching Sum" = "SR + (1-`Opp. OBP`) + (1-`BBHB/BF`)",
                             K.="K", BBHB="BB+HB", BIP="BF - K - BBHB", H.="H",
                             "Blank" = NA) |>
  lapply(function(x) parse(text=x))
pitcher_cols_ind <- c("IP", "Outs", "BF", "S", "P", "SR", "H", "AB", "K", "BB", "HB", "ROE",
                      "1B", "2B", "3B", "HR")
pitcher_cols_team <- c(pitcher_cols_ind, "SR.", "Opp. OBP", "BBHB/BF", "SR + notOB + notBBHB:\nPitching Sum")
pitcher_cols_ind <- pitcher_cols_team
pitcher_cols_total <- c(pitcher_cols_team,
                        "Blank",
                        "K.", "BBHB", "BIP", "H.")
attr(pitcher_cols_total, "sortby") <- "SR + notOB + notBBHB:\nPitching Sum"

calc_stats <- function(data, calculations, columns) {
  ok <- intersect(names(calculations), columns)
  for(n in ok) {
    data[[n]] <- with(data, eval(calculations[[n]]))
  }
  sortby <- attr(columns, "sortby")
  if(!is.null(sortby)) {
    data <- data[order(-data[[sortby]]),]
  }
  keep <- c("about", "when", "vs", "Number", "Name", "G", "Lineup", "Order")
  data[c(intersect(keep, names(data)), columns)]
}

## Pitches, Balls, Strikes, Fouls, Play, B1, Advance, Base, isOut, Fielders
counting_stats <- function(d, key=readxl::read_excel("codes.xlsx", "key", skip=1)) {

  d <- d |> mutate(idx=1:n(), .before=1)

  key <- key |> select(-Description)
  key0 <- key |> select(Type, Code, Variable) |> filter(!is.na(Variable)) |>
    mutate(value=1L) |> pivot_wider(names_from=Variable, values_from=value)
  key1 <- key |> select(-Variable, -ends_with("_")) |>
    full_join(key0, by=c("Type", "Code"))
  key2 <- key |> select(Type, Code, ends_with("_")) |>
    rename_with(\(x) str_remove(x, "_$")) |>
    filter(Type %in% c("Play", "B1")) |>
    mutate(Type="B1Play")

  keys <- bind_rows(key1 |> nest(.by=Type),
                    key2 |> nest(.by=Type)) |>
    mutate(data=map(data, \(x) {x[colSums(!is.na(x))>0]})) |>
    mutate(data=map2(Type, data, \(n, x) {names(x)[1] <- n; x}))

  ## check that all codes are found in the key
  setdiff(c(d$Play, d$B1, d$Advance), c(NA, key$Code))

  ## check that B1/Play/B1Play don't have overlapping names
  keys |> filter(!Type %in% c("Advance")) |>
    mutate(A=map(data, \(x) tibble(var=names(x)[-1]))) |>
    select(Type, A) |> unnest(A) |>
    count(var) |> filter(n>1)

  keylist <- setNames(keys$data, keys$Type)

  da <- d |> filter(!is.na(Advance)) |>
    left_join(keylist$Advance, by="Advance")
  dp <- d |> filter(is.na(Advance)) |>
    mutate(B1Play=if_else(is.na(B1), Play, B1)) |>
    left_join(keylist$B1Play, by="B1Play") |>
    left_join(keylist$B1, by="B1") |>
    left_join(keylist$Play, by="Play") |>
    select(-B1Play)
  bind_rows(da, dp) |> arrange(idx)
}

old_counting_stats <- function(d) {

  d |> mutate(Outcome=get_Outcome(Play, B1),
              OutDuring=get_OutDuring(B2, B3, B4),
              RunnersOut=get_RunnersOut(Lineup, Inning, OutDuring),
              Contact=get_Contact(Play, B1)) |>
    select(-Out) |>
    left_join(key, by="Outcome") |>
    mutate( Strikes = Strikes + (Pitch == "Strike"),
            Balls   = Balls + (Pitch == "Ball"),
            Outs    = Out + RunnersOut,
            S  = Strikes + Fouls,
            P  = Balls + Strikes + Fouls,

            H  = Hit,
            AB = !is.na(Hit),
            PA  = Outcome != "_",
            K   = Outcome %in% c("K", "Kd"),

            BB  = Outcome == "BB",
            HB  = Outcome == "HB",
            ROE = Outcome %in% c("E", "Kd"),
           `1B` = Outcome == "1B",
           `2B` = Outcome == "2B",
           `3B` = Outcome == "3B",
            HR  = Outcome == "HR",
            R   = ToBase == 4)
}

## BATTER STATS
batter_counting_stats <- function(d) {
  d |> select(Lineup, Outcome, ToBase) |> left_join(key, by="Outcome") |>
    group_by(Lineup) |> summarize(
      PA=sum(Outcome!="_"), H=sum(Hit, na.rm=TRUE), AB=sum(!is.na(Hit)), #BA=NA,
      R=sum(ToBase==4),
      K=sum(Outcome %in% c("K", "Kd")), BB=sum(Outcome=="BB"), HBP=sum(Outcome=="HB"),
      ROE=sum(Outcome %in% c("E", "Kd")),
      `1B`=sum(Outcome=="1B"), `2B`=sum(Outcome=="2B"), `3B`=sum(Outcome=="3B"), HR=sum(Outcome=="HR"),
      .groups="drop")
}

## PITCHER STATS
## returns data set with Number, Order, and then all the counting stats
pitcher_counting_stats <- function(d) {
  d |> select(Pitcher, Balls, Strikes, Fouls, Outcome, RunnersOut) |> left_join(key, by="Outcome") |>
    mutate(Out = Out + RunnersOut) |> select(-RunnersOut) |>
    mutate(Strikes = Strikes + (Pitch == "Strike"), Balls = Balls + (Pitch == "Ball")) |>
    mutate(Order = as.integer(as_factor(paste(Pitcher)))) |>
    group_by(Order, Pitcher) |> summarize(
      Outs=sum(Out), BF=sum(Outcome!="_"),
      S=sum(Strikes+Fouls), P=sum(Balls+Strikes+Fouls),
      H=sum(Hit, na.rm=TRUE), AB=sum(!is.na(Hit)),
      K=sum(Outcome %in% c("K", "Kd")), BB=sum(Outcome=="BB"), HB=sum(Outcome=="HB"),
      ROE=sum(Outcome %in% c("E", "Kd")),
      `1B`=sum(Outcome=="1B"), `2B`=sum(Outcome=="2B"), `3B`=sum(Outcome=="3B"), HR=sum(Outcome=="HR"),
      .groups="drop") |> select(Pitcher, everything()) |> rename(Number="Pitcher")
}
