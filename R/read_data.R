
readgame <- function(file,
                     parse_time = \(x) lubridate::mdy_hm(stringr::str_replace(x, "([ap])$", "\\1m"))) {
  message(file)
  ss <- readxl::excel_sheets(file)
  tmp <- readxl::read_excel(file, "Lineup", n_max = 1, col_names = FALSE, col_types="text", .name_repair="minimal")
  when <- parse_time(tmp[[1]])
  about <- if(ncol(tmp) > 1) tmp[[2]] else as.character(NA)
  g1 <- readxl::read_excel(file, "Lineup", skip=1, col_types="numeric")
  teams <- names(g1)[2:3]
  stopifnot(teams %in% ss[2:3])
  lineups <- g1 |> setNames(c("Lineup", "1", "2")) |> # 1=away, 2=home
    pivot_longer(-Lineup,
                 names_to="Side", names_transform=as.numeric,
                 values_to="Number", values_drop_na=TRUE) |>
    nest(.by=Side, .key="Lineup") |>
    left_join(tibble(Side=1:2, Team=teams), by="Side")
  game <- lineups |> mutate(Plays = map2(Team, Lineup, \(team, lineup) {
    readxl::read_excel(file, sheet = team) |> makedata() |> left_join(lineup, by="Lineup") |>
      select(Row, Inning, Lineup, Batter=Number, Pitcher, everything())
  }))
  out <- tibble(when=when, about=about, game=list(game))
  out
}

prep_game <- function(game, rosters) {
  stopifnot(is_tibble(game) && nrow(game)==1)
  game <- as.list(game)
  game$teams <- game$game[[1]]$Team
  game$plays <- game$game[[1]] |> select(Side, Plays) |> unnest(Plays)
  game$lineup <- game$game[[1]] |> select(Side, Team, Lineup) |> unnest(Lineup) |>
    left_join(rosters, by=c("Team", "Number")) |> select(-Team)
  game$game <- NULL
  game
}

readgames <- function(dir=".", gamecode="^Game_([0-9a-z]+)\\.xlsx$",
                      files=list.files(path="game_data", pattern=gamecode, full.names=TRUE),
                      codes=str_replace(basename(files), gamecode, "\\1"),
                      rosters=c(),
                      save.file, resave=!missing(save.file)) {

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
    mutate(map_dfr(datafile, readgame)) |>
    bind_rows(filter(gs, status=="ok")) |>
    select(-mtime.now, -status) |>
    arrange(code) |>
    game_add_stats(rosters)

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
