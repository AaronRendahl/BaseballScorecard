add_stats <- function(gs, ...) {
  gs$stats <- map(seq_len(nrow(gs)), \(i) game_stats(gs[i,], ...))
  gs
}

br_stats <- function(counts, lx, ..., total,
                     batter_by, batter_counts, batter_cols, batter_calculations,
                     runner_by, runner_counts, runner_cols, runner_calculations,
                     final_cols, arrange_by, arrange_desc=TRUE) {
  b1 <- counts |> rename(Number=Batter) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(batter_counts, batter_calculations, by=batter_by, total=total) |>
    select(all_of(batter_cols))
  r1 <- counts |> rename(Number=Runner) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(runner_counts, runner_calculations, by=runner_by, total=total) |>
    select(all_of(runner_cols))
  descif <- if(arrange_desc) desc else identity
  blank <- which(final_cols=="Blank")
  if(length(blank)>0) {
    names(final_cols) <- ""
    names(final_cols)[blank] <- paste0("Blank", blank)
  }
  full_join(b1, r1, by=intersect(runner_cols, batter_cols)) |>
    arrange(descif(.data[[arrange_by]])) |>
    mutate(Blank=NA) |>
    select(all_of(final_cols))
}

p_stats <- function(counts, lx, ..., total,
                    pitcher_by, pitcher_counts, pitcher_cols, pitcher_calculations,
                    final_cols=pitcher_cols, arrange_by, arrange_desc=TRUE) {
  arrangeif <- if(is.null(arrange_by)) \(x, ...) x else arrange
  descif <- if(arrange_desc) desc else identity
  blank <- which(final_cols=="Blank")
  if(length(blank)>0) {
    names(final_cols) <- ""
    names(final_cols)[blank] <- paste0("Blank", blank)
  }
  counts |> rename(Number=Pitcher) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(pitcher_counts, pitcher_calculations, by=pitcher_by, total=total) |>
    select(all_of(pitcher_cols)) |>
    arrangeif(descif(.data[[arrange_by]])) |>
    mutate(Blank=NA) |>
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

  batter_counting <- setdiff(cn, c("R", "SB"))
  runner_counting <- cn

  br_cols1 <- c("PA", "H", "AB", "BA", "R", "SB+", "CS+",
                "K", "BB", "HBP", "ROE", "1B", "2B", "3B", "HR")
  br_cols2 <- c("SLG", "OBPE", "K/PA", "SLG + OBPE + notK/PA:\nBatting Sum")

  runner_cols <- c("R", "SB+", "CS+")
  batter_cols <- br_cols1 |> setdiff(runner_cols)

  ind_cols <- c("Number", "Name", "Lineup")
  team_cols <- c("Team", "Side")

  pitcher_cols_ind <- c("Number", "Name", "IP", "BF", "Strikes", "Pitches", "SR", "Blank",
                        "H", "AB", "K", "BB", "HB", "ROE", "1B", "2B", "3B", "HR", "SR.",
                        "Opp. OBP", "BBHB/BF", "SR + notOB + notBBHB:\nPitching Sum")

  br1 <- br_stats(cs, lx_Batter,
                  Team==team,
                  batter_by=c("code", "Team", "Number", "Name", "Lineup"),
                  batter_counts=batter_counting,
                  batter_cols=c(ind_cols, batter_cols),
                  batter_calculations=batter_calculations,
                  runner_by=c("code", "Team", "Number", "Name"),
                  runner_counts=runner_counting,
                  runner_cols=c("Number", "Name", runner_cols),
                  runner_calculations=runner_calculations,
                  final_cols=c(ind_cols, br_cols1),
                  arrange_by="Lineup", arrange_desc=FALSE)

  brx <- br_stats(cs, lx_Batter,
                  batter_by=c("code", "Team", "Side"),
                  batter_counts=batter_counting,
                  batter_cols=c("Team", "Side", batter_cols, br_cols2),
                  batter_calculations=batter_calculations,
                  runner_by=c("code", "Team", "Side"),
                  runner_counts=runner_counting,
                  runner_cols=c("Team", "Side", runner_cols),
                  runner_calculations=runner_calculations,
                  final_cols=c("Blank1"="Blank", "Team", "Blank2"="Blank", br_cols1, br_cols2),
                  arrange_by="Side", arrange_desc=FALSE)

  p1 <- p_stats(cs, lx_Pitcher, total=c(Name="Team"),
                Team==team,
                pitcher_by = c("code", "Team", "Number", "Name"),
                pitcher_counts = cn,
                pitcher_cols = pitcher_cols_ind,
                pitcher_calculations = pitcher_calculations,
                arrange_by=NULL)

  p2 <- p_stats(cs, lx_Pitcher, total=c(Name="Team"),
                Team==vs,
                pitcher_by = c("code", "Team", "Number", "Name"),
                pitcher_counts = cn,
                pitcher_cols = pitcher_cols_ind,
                pitcher_calculations = pitcher_calculations,
                arrange_by=NULL)

  out <- list(br1, brx, p1, p2) |>
    setNames(c(paste(team, "Batting"), "Team Batting", paste(c(team, vs), "Pitching")))

  c(list(header=header), out)
}
