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
