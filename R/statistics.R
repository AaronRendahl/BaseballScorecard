get_score <- function(game) {
  score <- game$plays |> summarize(R=sum(ToBase==4), .by=c(Inning, Side)) |>
    pivot_wider(names_from=Inning, values_from=R) |>
    arrange(Side) |> select(-Side) |>
    as.matrix()
  score <- cbind(score, R=rowSums(score))
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

game_add_stats <- function(games, rosters) {
  for(i in seq_len(nrow(games))) {
    g <- prep_game(games[i,], rosters)
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

makestatsfile <- function(game, team, filename) {
  add_title <- function(x, title) {
    x <- rbind(names(x), x)
    if(!missing(title)) {
      x <- rbind(NA, x)
      x[1,1] <- title
    }
    x
  }
  game <- flatten(game)
  stats <- game$stats
  about <- game$about
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
      list(header=list(sprintf("%s, %s", g$vs, g$when), link)) |> c(tmp)
    }) |> setNames(code))
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
