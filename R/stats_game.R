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
    select(all_of(c(batter_by, batter_cols)))
  r1 <- counts |> rename(Number=Runner) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(runner_counts, runner_calculations, by=runner_by, total=total) |>
    select(all_of(c(runner_by, runner_cols)))
  descif <- if(arrange_desc) desc else identity
  blank <- which(final_cols=="Blank" | final_cols=="" | is.na(final_cols))
  if(length(blank)>0) {
    names(final_cols) <- ""
    names(final_cols)[blank] <- paste0("Blank", blank)
    final_cols[blank] <- "Blank"
  }
  full_join(b1, r1, by=intersect(c(runner_by, runner_cols), c(batter_by, batter_cols))) |>
    arrange(descif(.data[[arrange_by]])) |>
    mutate(Blank=NA) |>
    select(all_of(final_cols))
}

p_stats <- function(counts, lx, ..., total,
                    pitcher_by, pitcher_counts, pitcher_cols, pitcher_calculations,
                    final_cols=pitcher_cols, arrange_by, arrange_desc=TRUE) {
  arrangeif <- if(is.null(arrange_by)) \(x, ...) x else arrange
  descif <- if(arrange_desc) desc else identity
  blank <- which(final_cols=="Blank" | final_cols=="" | is.na(final_cols))
  if(length(blank)>0) {
    names(final_cols) <- ""
    names(final_cols)[blank] <- paste0("Blank", blank)
    final_cols[blank] <- "Blank"
  }
  lx <- lx |> mutate(Side = 3 - Side)
  counts |> rename(Number=Pitcher) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(pitcher_counts, pitcher_calculations, by=pitcher_by, total=total) |>
    select(all_of(c(pitcher_by, pitcher_cols))) |>
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

  lx <- gs |> select(code, game) |> unnest(game) |>
    select(code, Side, Team, Lineup) |> unnest(Lineup) |>
    select(code, Side, Team, Number, Name)

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

  br1_cols <- c(ind_cols, br_cols1)
  brx_cols <- c("Blank", "Team", "Blank", br_cols1, br_cols2)
  p1_cols <- pitcher_cols_ind
  p2_cols <- pitcher_cols_ind

  bb1 <- c("code", "Team", "Number", "Name", "Lineup")
  rb1 <- c("code", "Team", "Number", "Name")
  br1 <- br_stats(cs, lx,
                  Team==team,
                  batter_by=bb1,
                  batter_counts=batter_counting,
                  batter_cols=c(bb1, batter_cols),
                  batter_calculations=batter_calculations,
                  runner_by=rb1,
                  runner_counts=runner_counting,
                  runner_cols=c(rb1, runner_cols),
                  runner_calculations=runner_calculations,
                  final_cols=br1_cols,
                  arrange_by="Lineup", arrange_desc=FALSE)

  bbx <- c("code", "Team", "Side")
  rbx <- c("code", "Team", "Side")
  brx <- br_stats(cs, lx,
                  batter_by=bbx,
                  batter_counts=batter_counting,
                  batter_cols=c(bbx, batter_cols, br_cols2),
                  batter_calculations=batter_calculations,
                  runner_by=rbx,
                  runner_counts=runner_counting,
                  runner_cols=c(rbx, runner_cols),
                  runner_calculations=runner_calculations,
                  final_cols=brx_cols,
                  arrange_by="Side", arrange_desc=FALSE)

  pb1 <- c("code", "Team", "Number", "Name")
  pb2 <- c("code", "Team", "Number", "Name")
  p1 <- p_stats(cs, lx, total=c(Name="Team"),
                Team==team,
                pitcher_by = pb1,
                pitcher_counts = cn,
                pitcher_cols = p1_cols,
                pitcher_calculations = pitcher_calculations,
                arrange_by=NULL)

  p2 <- p_stats(cs, lx, total=c(Name="Team"),
                Team==vs,
                pitcher_by = pb2,
                pitcher_counts = cn,
                pitcher_cols = p2_cols,
                pitcher_calculations = pitcher_calculations,
                arrange_by=NULL)

  out <- list(br1, brx, p1, p2) |>
    setNames(c(paste(team, "Batting"), "Team Batting", paste(c(team, vs), "Pitching")))

  c(list(header=header), out)
}

game_stats2 <- function(g, team, stats_file, calculations, roles) {
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

  out <- make_stats_from_file(g, header, stats_file, calculations, roles, team, vs)
  out
}
