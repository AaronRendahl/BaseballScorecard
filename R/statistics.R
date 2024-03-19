makedata <- function(d) {
  ## helper function
  get_ToBase <- function(Outcome, B1, B2, B3, B4) {
    f1 <- function(Outcome, B1, B2, B3, B4) {
      B <- c("out", B1, B2, B3, B4)
      to1 <- max(which(!is.na(B))-1)
      max(key$Base[match(Outcome, key$Outcome)], to1) + stringr::str_detect(B[to1+1], "^X")*(-0.5)
    }
    purrr::pmap_dbl(list(Outcome, B1, B2, B3, B4), f1)
  }
  get_OutDuring <- function(B2, B3, B4) {
    f1 <- function(B2, B3, B4) {
      out <- NA
      B <- c(B2, B3, B4)
      ## need to remove any X at the beginning and any pitches at the end
      ## for the following code to find the at-bat correctly
      X <- stringr::str_subset(B, "^X") |> str_remove("-[0-9]+$")
      if(length(X) > 0) {
        out <- stringr::str_replace(X, ".*[^0-9]([0-9]*)$", "\\1") |>
          as.integer() |> replace_na(0L)
      }
      out
    }
    purrr::pmap_int(list(B2, B3, B4), f1)
  }
  get_Outcome <- function(Play, B1) {
    out <- if_else(!is.na(B1), B1, stringr::str_sub(Play, 1, 1)) |> stringr::str_replace("^E.*", "E")
    ## error check
    if(!all(out %in% key$Outcome)) {
      tmp <- out[! out %in% key$Outcome]
      warning(paste(paste0("[",tmp,"]"), collapse=" "))
    }
    out
  }
  get_RunnersOut <- function(Lineup, Inning, OutDuring) {
    out <- tibble(Lineup=Lineup, Inning=Inning, OutDuring=OutDuring)
    out <- out |> mutate(RunnersOut=0, idx=1:n())
    tmp <- out |> filter(!is.na(OutDuring))
    for(i in seq_len(nrow(tmp))) {
      if(tmp$OutDuring[i]==0) {
        out$RunnersOut[tmp$idx[i]] <- out$RunnersOut[tmp$idx[i]] + 1
      } else {
        foo <- filter(out, Lineup==tmp$OutDuring[i] & idx >= tmp$idx[i] & Inning==tmp$Inning[i])
        stopifnot(nrow(foo)>0)
        out$RunnersOut[foo$idx[1]] <- out$RunnersOut[foo$idx[1]] + 1
      }
    }
    pull(out, RunnersOut)
  }
  get_PitchesAtBat <- function(Count, Outcome) {
    Count + (key$Pitch[match(Outcome, key$Outcome)]!="No Pitch")*1L
  }
  get_Contact <- function(Play, B1) {
    ## do have some 1Bs with unknown Play for other teams...
    tibble(Play=Play, B1=B1) |>
      mutate(Play=if_else(B1 %in% c("BB", "HB"), B1, Play)) |>
      mutate(Play=na_if(Play, "_")) |>
      mutate(Play2 = str_replace(Play, "([A-Z]+)([1-9]).*", "\\1-\\2")) |>
      separate(Play2, c("HitType", "HitLocation"), remove = FALSE, convert=TRUE, fill="right") |>
      mutate(HitFar=if_else(HitLocation>=7, "Out", "In"),
             HitHard=if_else(HitFar=="Out" | HitType=="L", "Hard", "Soft"),
             HitType=str_replace(HitType, "K.*", "K"),
             HitType=str_replace(HitType, "[HB]B", "BBHB"),
             Outcome=if_else(is.na(HitHard), HitType, HitHard)) |>
      mutate(Outcome=if_else(is.na(Outcome) & B1=="1B", "Soft",
                             if_else(is.na(Outcome) & B1 %in% c("2B", "3B", "HR"), "Hard", Outcome))) |>
      pull(Outcome)
  }
  ## now process as needed, adding variables
  ## Outcome: to match Outcome column in key
  ## ToBase: which base they got to (use 0.5 to specify out between; eg, 2.5 if out between 2 and 3)
  ## OutDuring: if batter gets out later, during what at-bat did it happen?
  ## RunnersOut: how many runners got out during this at bat?
  ## PitchesAtBat: total pitches during at-bat
  d |>
    mutate(across(c("Balls", "Strikes", "Fouls"), \(x) replace_na(x, 0L))) |>
    mutate(Outcome=get_Outcome(Play, B1),
           ToBase=get_ToBase(Outcome, B1, B2, B3, B4),
           OutDuring=get_OutDuring(B2, B3, B4),
           RunnersOut=get_RunnersOut(Lineup, Inning, OutDuring),
           Contact=get_Contact(Play, B1)) |>
    mutate(across(c("B2", "B3", "B4"), \(x) {
      ## .0$ is in case Google to xlsx adds a .0 to numbers
      x |> stringr::str_remove(pattern="^X") |> stringr::str_remove(pattern="\\.0$")
    })) |>
    mutate(PitchesAtBat=get_PitchesAtBat(Balls + Strikes + Fouls, Outcome)) |>
    mutate(Row = 1:n()) |>
    select(Inning, Row, Lineup, everything())
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

batter_stats <- function(game, rosters, forSide, teamname=TRUE) {
  stopifnot(forSide %in% 1:2)
  x <- game$lineup[c(1, forSide+1)]
  d <- game$plays |> filter(Side==forSide)
  team <- colnames(x)[2]
  teamname <- if(teamname) team else "Team"
  names(x)[2] <- "Number"
  if(team %in% names(rosters)) {
    x <- x |> left_join(select(rosters[[team]], c(Number, Name)), by="Number")
  }
  stats <- batter_counting_stats(d)
  ind_stats <- stats |> left_join(x, by="Lineup") |> #mutate(BA=getBA(H, AB))
    calc_stats(batter_calculations, batter_cols_ind)
  team_stats <- stats |> summarize(across(-c(Lineup), sum)) |>
    mutate(Lineup=NA, Number=NA, Name=teamname) |>
    calc_stats(batter_calculations, batter_cols_team)
  out <- bind_rows(ind_stats, team_stats) |> mutate(G=NA)
  out[c("Number", "Name", "G", "Lineup", batter_cols_team)]
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

pitcher_stats <- function(game, rosters, forSide) {
  stopifnot(forSide %in% 1:2)
  x <- game$lineup[c(1, forSide + 1)]
  d <- game$plays |> filter(Side == 3 - forSide)   ## need to use opposite side!
  team <- colnames(x)[2]
  ## x has Lineup, <Team Name>; rosters has Number, Name
  ## <Team Name> to Number, then join by Number
  names(x)[2] <- "Number"
  if(team %in% names(rosters)) {
    x <- x |> left_join(select(rosters[[team]], c(Number, Name)), by="Number")
  }
  stats <- pitcher_counting_stats(d)
  ind_stats <- stats |> left_join(x, by="Number") |>
    calc_stats(pitcher_calculations, pitcher_cols_ind)
  team_stats <- stats |> summarize(across(-c(Number, Order), sum)) |>
    mutate(Number=NA, Order=Inf, Name="Team") |>
    calc_stats(pitcher_calculations, pitcher_cols_team)
  out <- bind_rows(ind_stats, team_stats) |> arrange(Order) |> mutate(G=NA)
  out[c("Number", "Name", "G", pitcher_cols_team)]
}

readgame <- function(file, rosters=c()) {
  message(file)
  ss <- readxl::excel_sheets(file)
  tmp <- readxl::read_excel(file, "Lineup", n_max = 1, col_names = FALSE, .name_repair="minimal")
  when <- tmp[[1]] |> stringr::str_replace("([ap])$", "\\1m")
  about <- if(ncol(tmp) > 1) tmp[[2]] else "GSBL"
  g1 <- readxl::read_excel(file, "Lineup", skip=1)
  lineup.long <- g1 |> setNames(c("Lineup", "1", "2")) |>
    pivot_longer(-Lineup,
                 names_to="Side", names_transform=as.numeric,
                 values_to="Batter", values_drop_na=TRUE)
  stopifnot(names(g1)[2:3] %in% ss[2:3])
  g1_away <- readxl::read_excel(file, names(g1)[2]) |> makedata() |> mutate(Side = 1, .after = Inning)
  g1_home <- readxl::read_excel(file, names(g1)[3]) |> makedata() |> mutate(Side = 2, .after = Inning)
  plays <- bind_rows(g1_away, g1_home) |>
    left_join(lineup.long, by=c("Side", "Lineup")) |>
    select(Inning, Side, Row, Lineup, Batter, Pitcher, everything())
  out <- tibble(when=when, about=about, lineup=list(g1), plays=list(plays)) |>
    mutate(vs = map_chr(lineup, ~sprintf("%s @ %s", names(.)[2], names(.)[3])),
           datetime=lubridate::mdy_hm(when))
  out$stats <- out |> game_stats(rosters=rosters) |> list()
  out
}

readgames <- function(dir=".", gamecode="^Game_([0-9a-z]+)\\.xlsx$",
                      files=list.files(path="game_data", pattern=gamecode, full.names=TRUE),
                      codes=str_replace(basename(files), gamecode, "\\1"),
                      rosters=c(),
                      save.file, resave=!missing(save.file)) {

  gs <- tibble(code=codes, datafile=files) |> mutate(mtime.now=file.mtime(datafile))
  if(!missing(save.file) && file.exists(save.file)) {
    gs <- full_join(gs, read_rds(save.file), by="datafile")
  }
  gs <- gs |> bind_rows(tibble(mtime=as.POSIXct(c()))) |>
    mutate(status=case_when(is.na(mtime.now) ~ "remove",
                            is.na(mtime) ~ "new",
                            mtime.now > mtime ~ "update",
                            TRUE ~ "ok"
    ))
  print(split(gs$datafile, gs$status))
  gs <- gs |> filter(status %in% c("new", "update")) |> select(code, datafile) |>
    mutate(mtime=file.mtime(datafile)) |>
    mutate(map_dfr(datafile, readgame, rosters=rosters)) |>
    bind_rows(filter(gs, status=="ok")) |>
    select(-mtime.now, -status) |>
    arrange(code)
  ## allow for numbers to be characters
  if(!all(map_lgl(gs$lineup, \(x) is.numeric(pull(x, "Lineup"))))) {
    for(i in 1:nrow(gs)) {
      gs$lineup[[i]][[2]] <- as.character(gs$lineup[[i]][[2]])
      gs$lineup[[i]][[3]] <- as.character(gs$lineup[[i]][[3]])
      gs$plays[[i]]$Pitcher <- as.character(gs$plays[[i]]$Pitcher)
    }
  }
  if(resave) {
    saveRDS(gs, save.file)
  }
  gs
}

readrosters <- function(file) {
  ss <- readxl::excel_sheets(file)
  lapply(ss, readxl::read_excel, path=file) |> setNames(ss)
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

get_score <- function(game) {
  a1 <- game$plays |> summarize(R=sum(ToBase==4), .by=c(Inning, Side)) |>
    pivot_wider(names_from=Inning, values_from=R) |> as.data.frame() |>
    select(-Side) |> as.matrix()
  cbind(a1, R=rowSums(a1))
}

game_stats <- function(game, rosters) {
  game <- flatten(game)
  tibble(Team=names(game$lineup)[2:3], Role=c("away", "home"), Side=1:2,
         vs=names(game$lineup)[3:2],
         Pitcher_Stats=list(pitcher_stats(game, rosters, forSide=1),
                            pitcher_stats(game, rosters, forSide=2)),
         Batter_Stats=list(batter_stats(game, rosters, forSide=1),
                           batter_stats(game, rosters, forSide=2)))
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
  #all_the_stats$Batting <- all_the_stats$Batting[c("Number", "Name", "G", "Lineup", batter_cols_total)]
  all_the_stats$Pitching <- all_the_stats$Pitching %>%
    calc_stats(pitcher_calculations, pitcher_cols_total)
  #all_the_stats$Pitching <- all_the_stats$Pitching[c("Number", "Name", "G", pitcher_cols_total)]

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
