season_player_stats <- function(gs, team) {
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
    arrange(desc(`SLG + OBPE + notK/PA:\nBatting Sum`))

  p1 <- cs |> rename(Number=Pitcher) |> left_join(lx_Pitcher, by=c('code', 'Side', 'Number')) |>
    filter(Team==team) |>
    calc_stats(cn, pitcher_calculations,
               by=c("Team", "Number", "Name"),
               total=c(Name="Team")) |>
    select(all_of(pitcher_cols_total)) |>
    arrange(desc(`SR + notOB + notBBHB:\nPitching Sum`))

  list(b1, p1) |> setNames(c("Batting", "Pitching"))
}

season_game_stats <- function(gs, team, number) {
  stopifnot(is_tibble(gs))

  lx <- gs |> select(code, game) |> unnest(game) |> select(code, Side, Team, Lineup) |> unnest(Lineup)
  lx_Batter <- lx |> select(code, Side, Team, Number, Name)
  lx_Pitcher <- lx |> mutate(Side=3-Side) |> select(code, Side, Team, Number, Name)

  cs2 <- counting_stats_all(gs)
  cs <- cs2$stats
  cn <- cs2$names

  vs <- gs |> select(code, about, when, game) |> unnest(game) |>
    select(code, about, when, vs=Team) |> filter(vs!=team)

  if(!missing(number)) {
    batter_cols_here <- c("about", "when", "vs", batter_cols_ind)
    pitcher_cols_here <- c("about", "when", "vs", pitcher_cols_ind)
    batter_doby <- c("code", "about", "when", "vs", "Number", "Name", "Lineup")
    pitcher_doby <- c("code", "about", "when", "vs", "Number", "Name")
    filternum <- \(x) filter(x, Number==number)
  } else {
    batter_cols_here <- c("about", "when", "vs", batter_cols_total[-c(1:2)])
    pitcher_cols_here <- c("about", "when", "vs", pitcher_cols_total[-c(1:2)])
    batter_doby <- pitcher_doby <- c("code", "about", "when", "vs")
    filternum <- identity
  }
  b1 <- cs |> rename(Number=Batter) |> left_join(lx_Batter, by=c('code', 'Side', 'Number')) |>
    filter(Team==team) |>
    filternum() |>
    left_join(vs, by=c("code")) |>
    calc_stats(cn, batter_calculations,
               by=batter_doby,
               total=c(about="Season")) |>
    select(all_of(batter_cols_here)) |>
    mutate(when=MDY_format(when))

  p1 <- cs |> rename(Number=Pitcher) |> left_join(lx_Pitcher, by=c('code', 'Side', 'Number')) |>
    filter(Team==team) |>
    filternum() |>
    left_join(vs, by="code") |>
    calc_stats(cn, pitcher_calculations,
               by=pitcher_doby,
               total=c(about="Season")) |>
    select(all_of(pitcher_cols_here)) |>
    mutate(when=MDY_format(when))

  list(b1, p1) |> setNames(c("Batting", "Pitching"))
}
