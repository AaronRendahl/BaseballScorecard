add_stats <- function(gs, ...) {
  gs$stats <- map(seq_len(nrow(gs)), \(i) game_stats(gs[i,], ...))
  gs
}

game_stats <- function(g, team) {
  stopifnot(is_tibble(g) & nrow(g)==1)
  teams <- g |> select(code, game) |> unnest(game) |> pull(Team)
  stopifnot(team %in% teams)
  vs <- setdiff(teams, team)

  header <- sprintf("%s, %s", paste(teams, collapse=" @ "), MDY_format(g$when))

  if(!is.null(g$scorecard_link)) {
    link <- g$scorecard_link
    names(link) <- "LINK TO SCORECARD"
    class(link) <- "hyperlink"
    header <- list(header, link)
  }

  lx <- gs |> select(code, game) |> unnest(game) |> select(code, Side, Team, Lineup) |> unnest(Lineup)
  lx_Batter <- lx |> select(code, Side, Team, Number, Name)
  lx_Pitcher <- lx |> mutate(Side=3-Side) |> select(code, Side, Team, Number, Name)

  cs2 <- counting_stats_all(g)
  cs <- cs2$stats
  cn <- cs2$names

  b1 <- cs |> rename(Number=Batter) |> left_join(lx_Batter, by=c('code', 'Side', 'Number')) |>
    filter(Team==team) |>
    calc_stats(cn, batter_calculations,
               by=c("code", "Team", "Number", "Name", "Lineup")) |>
    arrange(Lineup) |> select(all_of(batter_cols_ind))

  bx <- cs |> rename(Number=Batter) |> left_join(lx_Batter, by=c('code', 'Side', 'Number')) |>
    calc_stats(cn, batter_calculations,
               by=c("code", "Team", "Side")) |>
    arrange(Side) |>select(all_of(batter_cols_team))

  px <- cs |> rename(Number=Pitcher) |> left_join(lx_Pitcher, by=c('code', 'Side', 'Number')) |>
    calc_stats(cn, pitcher_calculations,
               by=c("code", "Team")) |>
    select(all_of(pitcher_cols_team))

  p1 <- cs |> rename(Number=Pitcher) |> left_join(lx_Pitcher, by=c('code', 'Side', 'Number')) |>
    filter(Team==team) |>
    calc_stats(cn, pitcher_calculations,
               by=c("code", "Team", "Number", "Name"),
               total=c(Name="Team")
               ) |>
    select(all_of(pitcher_cols_ind))

  p2 <- cs |> rename(Number=Pitcher) |> left_join(lx_Pitcher, by=c('code', 'Side', 'Number')) |>
    filter(Team==vs) |>
    calc_stats(cn, pitcher_calculations,
               by=c("code", "Team", "Number", "Name"),
               total=c(Name="Team")) |>
    select(all_of(pitcher_cols_ind))

  out <- list(b1, bx, p1, p2) |>
    setNames(c(paste(team, "Batting"), "Team Batting", paste(c(team, vs), "Pitching")))

  c(list(header=header), out)
}
