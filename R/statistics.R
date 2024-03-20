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

all_stats <- function(games, team) {
  the_game_stats <- games |> select(stats) |> unnest(stats)
  b.all <- the_game_stats |> filter(Team==team) |> select(Batter_Stats) |> unnest(Batter_Stats) |>
    mutate(G=1) |>
    group_by(Number, Name) |> summarize(across(everything(), sum)) |> ungroup() |>
    mutate(Lineup=NA) |>
    calc_stats(batter_calculations, batter_cols_team)
  p.all <- the_game_stats |> filter(Team==team) |> select(Pitcher_Stats) |> unnest(Pitcher_Stats) |>
    mutate(G=1) |>
    group_by(Number, Name) |> summarize(across(everything(), sum)) |> ungroup() |>
    calc_stats(pitcher_calculations, pitcher_cols_team)
  list(Batting=b.all, Pitching=p.all)
}



get_all_stats <- function(gs, team) {
  all_the_stats <- all_stats(gs, team=team)
  the_game_stats <- gs |> select(about, when, code, stats, any_of('scorecard_link')) |> unnest(stats)
  batting_stats <- the_game_stats |> filter(Team==team) |>
    select(about, when, any_of('scorecard_link'), vs, Batter_Stats) |> unnest(Batter_Stats) |>
    mutate(type="Batting") |>
    bind_rows(all_the_stats$Batting %>% mutate(type="Batting", about="Season")) %>%
    mutate(Name=if_else(Name==team, "Team", Name)) %>%
    mutate(NumberX=replace_na(Number, -1)) %>% arrange(NumberX) %>% select(-NumberX) %>%
    mutate(X=as_factor(if_else(!is.na(Number), paste(Number, Name), Name))) %>%
    nest(data=c(-X, -type))
  pitching_stats <- the_game_stats |> filter(Team==team) |>
    select(about, when, any_of('scorecard_link'), vs, Pitcher_Stats) |> unnest(Pitcher_Stats) |>
    mutate(type="Pitching") |>
    bind_rows(all_the_stats$Pitching %>% mutate(type="Pitching", about="Season")) %>%
    mutate(NumberX=replace_na(Number, -1)) %>% arrange(NumberX) %>% select(-NumberX) %>%
    mutate(X=as_factor(if_else(!is.na(Number), paste(Number, Name), Name))) %>%
    nest(data=c(-X, -type))
  x <- bind_rows(batting_stats, pitching_stats)

  all_the_stats$Batting <- all_the_stats$Batting %>%
    mutate(Name=if_else(Name==team, "Team", Name)) |>
    calc_stats(batter_calculations, batter_cols_total)
  all_the_stats$Pitching <- all_the_stats$Pitching %>%
    calc_stats(pitcher_calculations, pitcher_cols_total)

  x$data <- setNames(x$data, x$type)
  xAll <- split(x, x$X) %>% map(pull, "data")
  xAll$Team <- map(xAll$Team, select, -c(Number, Name))

  xAllStats <- c(list(Individual=all_the_stats), xAll)
  names(xAllStats) <- str_remove(names(xAllStats), " NA")

  attr(xAllStats$Team, "freeze") <- 4
  attr(xAllStats$Individual, "freeze") <- 3

  contact <- get_contact_rates(gs, team)
  c(xAllStats[1:2], list('Contact Rate'=list('Contact Rate'=contact)), xAllStats[3:length(xAllStats)])
}

get_contact_rates <- function(gs, team) {
  gg <- map_dfr(seq_len(nrow(gs)), function(idx) {
    g <- gs[idx,] |> select(code, lineup, plays) |> flatten()
    out <- g$plays |> mutate(Team=names(g$lineup)[Side+1]) |> filter(Team==team) |>
      rename(Number=Batter)
    nsx <- rr[[team]] |> select(Number, Name)
    out <- out |>  left_join(nsx, by="Number") |> mutate(code=g$code)
  })
  gg |> count(Name, Contact) |> pivot_wider(names_from=Contact, values_from=n, values_fill = 0) |>
    select(Name, BBHB, K, Soft, Hard) |>
    mutate(Contact=Soft+Hard, AB=Soft+Hard+K, 'Contact/AB'=(Hard+Soft)/AB, 'Hard/AB'=Hard/AB, 'Hard/Contact'=Hard/(Soft+Hard)) |>
    arrange(desc(`Contact/AB`)) |> filter(AB>4)
}
