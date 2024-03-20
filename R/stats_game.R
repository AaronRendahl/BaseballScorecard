get_score <- function(game) {
  score <- game$plays |> summarize(R=sum(ToBase==4), .by=c(Inning, Side)) |>
    pivot_wider(names_from=Inning, values_from=R) |>
    arrange(Side) |> select(-Side) |>
    as.matrix()
  score <- cbind(score, R=rowSums(score))
  rownames(score) <- game$teams
  score
}



batter_stats <- function(game, forSide, teamname=TRUE) {
  stopifnot(forSide %in% 1:2)
  x <- game$lineup |> filter(Side==forSide) |> select(-Side)
  d <- game$plays  |> filter(Side==forSide) |> select(-Side)
  teamname <- if(teamname) game$teams[forSide] else "Team"

  stats <- batter_counting_stats(d)
  ind_stats <- stats |> left_join(x, by="Lineup") |>
    calc_stats(batter_calculations, batter_cols_ind)
  team_stats <- stats |> summarize(across(-c(Lineup), sum)) |>
    mutate(Lineup=NA, Number=NA, Name=teamname) |>
    calc_stats(batter_calculations, batter_cols_team)
  out <- bind_rows(ind_stats, team_stats) |> mutate(G=NA)
  out[c("Number", "Name", "G", "Lineup", batter_cols_team)]
}

pitcher_stats <- function(game, forSide, teamname=FALSE) {
  stopifnot(forSide %in% 1:2)
  x <- game$lineup |> filter(Side==forSide) |> select(-Side)
  d <- game$plays  |> filter(Side==3 - forSide) |> select(-Side) ## need to use opposite side!
  teamname <- if(teamname) game$teams[forSide] else "Team"

  stats <- pitcher_counting_stats(d)
  ind_stats <- stats |> left_join(x, by="Number") |>
    calc_stats(pitcher_calculations, pitcher_cols_ind)
  team_stats <- stats |> summarize(across(-c(Number, Order), sum)) |>
    mutate(Number=NA, Order=Inf, Name=teamname) |>
    calc_stats(pitcher_calculations, pitcher_cols_team)
  out <- bind_rows(ind_stats, team_stats) |> arrange(Order) |> mutate(G=NA)
  out[c("Number", "Name", "G", pitcher_cols_team)]
}

game_add_stats <- function(games) {
  for(i in seq_len(nrow(games))) {
    g <- prep_game(games[i,])
    gg <- games$game[[i]]
    games$game[[i]] <-
      bind_cols(gg,
                tibble(Pitcher_Stats = list(pitcher_stats(g, gg$Side[1]),
                                            pitcher_stats(g, gg$Side[2])),
                       Batter_Stats  = list(batter_stats(g, gg$Side[1]),
                                            batter_stats(g, gg$Side[2]))))
  }
  games
}

makestatsfile <- function(game, team, filename) {
  add_title <- function(x, title) {
    x <- rbind(names(x), x)
    if(!missing(title)) {
      x <- rbind(NA, x)
      x[1,1] <- title
    }
    x
  }
  about <- game$about
  stats <- game$game[[1]]
  k_us <- match(team, stats$Team)
  k_them <- setdiff(1:2, k_us)
  team2 <- stats$Team[k_them]

  b0 <- bind_rows(stats$Batter_Stats[[k_us]],
                  stats$Batter_Stats[[k_them]] |> filter(is.na(Lineup)))
  b1x <- b0[1:(nrow(b0)-2), ]
  b2x <- b0[1:2 + (nrow(b0)-2), ] |> rename(Team="Name")
  list.batting <- list(b1x, b2x) |>
    setNames(c(paste(team, "Batting"), "Team Batting"))

  a1x <- stats$Pitcher_Stats[[k_us]]
  a2x <- stats$Pitcher_Stats[[k_them]]
  list.pitching <- list(us=a1x, them=a2x) |>
    setNames(c(paste(c(team, team2), "Pitching")))

  out <- c(list.batting, list.pitching) |>
    map(~select(., -G))
  if(!missing(filename)) {
    statsToExcel(out, filename)
  }
  invisible(out)
}

makeallstatsfiles <- function(gs, team) {
  havelinks <- "scorecard_link" %in% names(gs)
  gs |> mutate(out=map(seq_len(nrow(gs)), function(idx) {
    g <- gs[idx,]
    tmp <- makestatsfile(g, team)
    link <- NULL
    if(havelinks) {
      link <- scorecard_link[idx]
      names(link) <- "LINK TO SCORECARD"
      class(link) <- "hyperlink"
    }
    vs <- setdiff(g$game[[1]]$Team, "Roseville")
    list(header=list(sprintf("%s, %s", vs, when_format(g$when)), link)) |> c(tmp)
  }) |> setNames(code))
}

