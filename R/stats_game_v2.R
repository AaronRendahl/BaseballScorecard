add_stats <- function(gs, ...) {
  gs$stats <- map(seq_len(nrow(gs)), \(i) game_stats(gs[i,], ...))
  gs
}

br_stats <- function(counts, lx, ...,
                     batter_by, batter_counts, batter_cols, batter_calculations,
                     runner_by, runner_counts, runner_cols, runner_calculations,
                     final_cols, arrange_by, arrange_desc) {
  foo <- counts |> rename(Number=Batter) |> left_join(lx, by=c('code', 'Side', 'Number'))
  b1 <- counts |> rename(Number=Batter) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(batter_counts, batter_calculations, by=batter_by) |>
    select(all_of(batter_cols))
  r1 <- cs |> rename(Number=Runner) |> left_join(lx_Batter, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(runner_counts, runner_calculations, by=runner_by) |>
    select(all_of(runner_cols))
  descif <- if(arrange_desc) desc else identity
  full_join(b1, r1, by=intersect(runner_cols, batter_cols)) |>
    arrange(descif(.data[[arrange_by]])) |>
    select(all_of(final_cols))
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

  runner_calculations <- list()
  batter_counting <- setdiff(cn, c("R", "SB"))
  runner_counting <- c(cn, "R", "SB")
  batter_runner_cols <- c("PA", "H", "AB", "BA", "R", "SB",
                          "K", "BB", "HBP", "ROE", "1B", "2B", "3B", "HR")
  br_cols2 <- c("SLG", "OBPE", "K/PA", "SLG + OBPE + notK/PA:\nBatting Sum")
  runner_cols <- c("R", "SB")
  batter_cols <- batter_runner_cols |> setdiff(runner_cols)
  ind_cols <- c("Number", "Name", "Lineup")
  team_cols <- c("Team", "Side")

  br1 <- br_stats(cs, lx_Batter,
                  Team==team,
                  batter_by=c("code", "Team", "Number", "Name", "Lineup"),
                  batter_counts=batter_counting,
                  batter_cols=c("Blank", ind_cols, batter_cols),
                  batter_calculations=batter_calculations,
                  runner_by=c("code", "Team", "Number", "Name"),
                  runner_counts=runner_counting,
                  runner_cols=c("Number", "Name", runner_cols),
                  runner_calculations=runner_calculations,
                  final_cols=c(ind_cols, batter_runner_cols),
                  arrange_by="Lineup", arrange_desc=FALSE)
  ###

  bx <- cs |> rename(Number=Batter) |> left_join(lx_Batter, by=c('code', 'Side', 'Number')) |>
    calc_stats(batter_counting, batter_calculations,
               by=c("code", "Team", "Side")) |>
    arrange(Side) |>
    select(any_of(c("Blank", team_cols, batter_cols, br_cols2)))

  rx <- cs |> rename(Number=Runner) |> left_join(lx_Batter, by=c('code', 'Side', 'Number')) |>
    calc_stats(runner_counting, runner_calculations,
               by=c("code", "Team", "Side")) |>
    select(any_of(c(team_cols, runner_cols)))

  brx <- full_join(bx, rx, by=c("Team", "Side")) |>
    arrange(Side) |>
    select(all_of(c("Blank1"="Blank", "Team", "Blank2"="Blank", batter_runner_cols, br_cols2)))

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

  out <- list(br1, brx, p1, p2) |>
    setNames(c(paste(team, "Batting"), "Team Batting", paste(c(team, vs), "Pitching")))

  c(list(header=header), out)
}
