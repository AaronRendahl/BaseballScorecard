
readgame <- function(file,
                     rosters = tibble(Team=character(), Number=numeric()),
                     parse_time = \(x) lubridate::mdy_hm(stringr::str_replace(x, "([ap])$", "\\1m"))) {
  message(file)
  ss <- readxl::excel_sheets(file)
  tmp <- readxl::read_excel(file, "Lineup", n_max = 1, col_names = FALSE, col_types="text", .name_repair="minimal")
  when <- parse_time(tmp[[1]])
  about <- if(ncol(tmp) > 1) tmp[[2]] else as.character(NA)
  g1 <- readxl::read_excel(file, "Lineup", skip=1, col_types="numeric")
  teams <- names(g1)[2:3]
  stopifnot(teams %in% ss)
  names(g1)[2:3] <- paste(names(g1)[2:3], 1:2, sep="_")
  lineups <- g1 |>
    pivot_longer(-Lineup, names_to=c("Team", "Side"),
                 names_sep="_", names_transform=list(Side=as.integer),
                 values_to="Number", values_drop_na=TRUE) |>
    left_join(rosters, by=c("Team", "Number")) |>
    arrange(Side, Lineup) |>
    nest(Lineup=c(Lineup, Number, Name)) |>
    select(Side, Team, Lineup)
  #game <- lineups |> mutate(Plays = map2(Team, Lineup, \(team, lineup) {
  #  readxl::read_excel(file, sheet = team) |> makedata() |> left_join(lineup, by="Lineup") |>
  #  select(Row, Inning, Lineup, Batter=Number, Pitcher, everything())
  game <- lineups |> mutate(Plays = map(Team, \(team) {
        readxl::read_excel(file, sheet = team) |> makedata() |>
      select(Row, Inning, Lineup, Pitcher, everything())
  }))
  out <- tibble(when=when, about=about, game=list(game))
  out
}

prep_game <- function(game) {
  stopifnot(is_tibble(game) && nrow(game)==1)
  game <- as.list(game)
  game$teams <- game$game[[1]]$Team
  game$plays <- game$game[[1]] |> select(Side, Plays) |> unnest(Plays)
  game$lineup <- game$game[[1]] |> select(Side, Lineup) |> unnest(Lineup)
  game$game <- NULL
  game
}

prep_game2 <- function(game) {
  stopifnot(is_tibble(game) && nrow(game)==1)

  g <- game$game[[1]]

  lx <- local({
    p1 <- g |> select(Side, Plays) |> unnest(Plays) |>
      arrange(Side, Row) |> select(Side, Number=Pitcher) |> unique() |>
      mutate(Order=1:n(), .by=Side) |> mutate(Side=3-Side)
    g |> select(Side, Team, Lineup) |> unnest(Lineup) |>
      full_join(p1, by=c("Side", "Number")) |>
      mutate(Player=sprintf("%d%3d", Side, Number)) |>
      arrange(Side, Lineup, Order)
  })

  px <- g |> select(Side, Plays) |> unnest(Plays) |>
    arrange(Inning, Side, Row) |>
    left_join(lx |> select(Side, Pitcher=Number, Pitcher_Code=Player) |> mutate(Side=3-Side),
              by=c("Side", "Pitcher")) |>
    left_join(lx |> select(Side, Lineup, Batter_Code=Player),
              by=c("Side", "Lineup")) |>
    select(-Pitcher, -Lineup) |>
    rename(Pitcher=Pitcher_Code, Batter=Batter_Code) |>
    select(Row, Inning, Side, Batter, Pitcher, everything())

  lx <- lx |> select(Player, Team, Number, Name, Lineup, Order)

  game <- as.list(game)
  game$teams <- game$game[[1]]$Team
  game$plays <- px
  game$lineup <- lx
  game$game <- NULL
  game
}

readgames <- function(dir=".", gamecode="^Game_([0-9a-z]+)\\.xlsx$",
                      files=list.files(path="game_data", pattern=gamecode, full.names=TRUE),
                      codes=str_replace(basename(files), gamecode, "\\1"),
                      save.file, resave=!missing(save.file),
                      ...) {

  gs <- tibble(code=codes, datafile=files) |> mutate(mtime.now=file.mtime(datafile))
  if(!missing(save.file) && file.exists(save.file)) {
    gs <- full_join(gs, read_rds(save.file), by=c("code", "datafile"))
  }
  gs <- gs |> bind_rows(tibble(mtime=as.POSIXct(c()))) |>
    mutate(status=case_when(is.na(mtime.now) ~ "remove",
                            is.na(mtime) ~ "new",
                            mtime.now > mtime ~ "update",
                            TRUE ~ "ok"
    ))
  # print(split(gs$datafile, gs$status))
  gs <- gs |> filter(status %in% c("new", "update")) |> select(code, datafile) |>
    mutate(mtime=file.mtime(datafile)) |>
    mutate(map_dfr(datafile, \(x) readgame(x, ...))) |>
    game_add_stats() |>
    bind_rows(filter(gs, status=="ok")) |>
    select(-mtime.now, -status) |>
    arrange(code)

  if(resave && !missing(save.file)) {
    saveRDS(gs, save.file)
  }
  gs
}

readrosters <- function(file) {
  ss <- readxl::excel_sheets(file)
  tibble(Team=ss) |>
    mutate(roster=map(Team, \(x) readxl::read_excel(path=file, sheet=x))) |>
    unnest(roster) |>
    select(Team, Number, Name)
}
