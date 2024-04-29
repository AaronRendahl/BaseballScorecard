season_stats <- function(gs, team) {
  stopifnot(is_tibble(gs))

  lx <- gs |> select(code, game) |> unnest(game) |> select(code, Side, Team, Lineup) |> unnest(Lineup)
  lx_Batter <- lx |> select(code, Side, Team, Number, Name)
  lx_Pitcher <- lx |> mutate(Side=3-Side) |> select(code, Side, Team, Number, Name)

  cs2 <- counting_stats_all(gs)
  cs <- cs2$stats
  cn <- cs2$names

  b1 <- cs |> rename(Number=Batter) |> left_join(lx_Batter, by=c('code', 'Side', 'Number')) |>
    filter(Team==team) |>
    calc_stats(cn, batter_calculations,
               by=c("Team", "Number", "Name"),
               total=c(Name="Team")) |>
    select(all_of(batter_cols_total)) |>
    arrange(`SLG + OBPE + notK/PA:\nBatting Sum`)

  p1 <- cs |> rename(Number=Pitcher) |> left_join(lx_Pitcher, by=c('code', 'Side', 'Number')) |>
    filter(Team==team) |>
    calc_stats(cn, pitcher_calculations,
               by=c("Team", "Number", "Name"),
               total=c(Name="Team")) |>
    select(all_of(pitcher_cols_total)) |>
    arrange(`SR + notOB + notBBHB:\nPitching Sum`)

  list(b1, p1) |> setNames(c("Batting", "Pitching"))
}
