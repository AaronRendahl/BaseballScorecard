readgame <- function(file,
                     rosters = tibble(Team=character(), Number=numeric(), Name=character()),
                     parse_time = \(x) lubridate::mdy_hm(stringr::str_replace(x, "([ap])$", "\\1m")),
                     cleanplays_fun=cleanplays,
                     plays=TRUE, ...) {
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
  game <- lineups |> mutate(Plays = map(Team, \(team) {
    readxl::read_excel(file, sheet = team) |>
      cleanplays_fun() |>
      mutate(Row = 1:n(), .before=1)
  }))
  ## add Batter from the Lineup, if needed
  if(! "Batter" %in% names(game$Plays[[1]])) {
    game$Plays[[1]] <- game$Plays[[1]] |>
      left_join(game$Lineup[[1]] |> select(Lineup, Batter=Number), by="Lineup") |>
      relocate(Batter, .after="Inning")
  }
  if(! "Batter" %in% names(game$Plays[[2]])) {
    game$Plays[[2]] <- game$Plays[[2]] |>
      left_join(game$Lineup[[2]] |> select(Lineup, Batter=Number), by="Lineup") |>
      relocate(Batter, .after="Inning")
  }
  out <- tibble(when=when, about=about, game=list(game))
  if(plays) {
    out <- add_plays(out, ...)
  }
  out
}

readgames <- function(dir=".", gamecode="^Game_([0-9a-z]+)\\.xlsx$",
                      files=list.files(path="game_data", pattern=gamecode, full.names=TRUE),
                      codes=str_replace(basename(files), gamecode, "\\1"),
                      save.file, resave=!missing(save.file),
                      reload=FALSE,
                      ...) {

  gs <- tibble(code=codes, datafile=files) |> mutate(mtime.now=file.mtime(datafile))
  if(!reload && !missing(save.file) && file.exists(save.file)) {
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
