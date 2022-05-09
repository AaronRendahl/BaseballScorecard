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
      X <- stringr::str_subset(B, "^X")
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

  ## now process as needed, adding variables
  ## Outcome: to match Outcome column in key
  ## ToBase: which base they got to (use 0.5 to specify out between; eg, 2.5 if out between 2 and 3)
  ## OutDuring: if batter gets out later, during what at-bat did it happen?
  ## RunnersOut: how many runners got out during this at bat?
  ## PitchesAtBat: total pitches during at-bat
  d |>
    mutate(across(c("Balls", "Strikes", "Fouls"), replace_na, 0L)) |>
    mutate(Outcome=get_Outcome(Play, B1),
           ToBase=get_ToBase(Outcome, B1, B2, B3, B4),
           OutDuring=get_OutDuring(B2, B3, B4),
           RunnersOut=get_RunnersOut(Lineup, Inning, OutDuring)) |>
    mutate(across(c("B2", "B3", "B4"), stringr::str_remove, pattern="^X")) |>
    mutate(PitchesAtBat=get_PitchesAtBat(Balls + Strikes + Fouls, Outcome))
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
## BATTER STATS
batter_counting_stats <- function(d) {
  d |> select(Lineup, Outcome, ToBase) |> left_join(key, by="Outcome") |>
    group_by(Lineup) |> summarize(
      G=NA,
      PA=sum(Outcome!="_"), H=sum(Hit, na.rm=TRUE), AB=sum(!is.na(Hit)), BA=NA,
      R=sum(ToBase==4), Blank=NA,
      K=sum(Outcome=="K"), BB=sum(Outcome=="BB"), HBP=sum(Outcome=="HB"),
      ROE=sum(Outcome=="E"),
      `1B`=sum(Outcome=="1B"), `2B`=sum(Outcome=="2B"), `3B`=sum(Outcome=="3B"), HR=sum(Outcome=="HR"),
      .groups="drop")
}
batter_stats <- function(game, rosters, who=c("away", "home"), teamname=TRUE) {
  who <- match.arg(who)
  i <- match(who, c("away", "home"))
  x <- game$lineup[c(1,i+1)]
  d <- game[[who]]
  team <- colnames(x)[2]
  teamname <- if(teamname) team else "Team"
  names(x)[2] <- "Number"
  if(team %in% names(rosters)) {
    x <- x |> left_join(select(rosters[[team]], c(Number, Name)), by="Number")
  }
  stats <- batter_counting_stats(d)
  ind_stats <- stats |> left_join(x, by="Lineup") |> mutate(BA=getBA(H, AB))
  team_stats <- stats |> summarize(across(-c(Lineup), sum)) |>
    mutate(Lineup=NA, Number=NA, Name=teamname) |>
    mutate(BA=getBA(H, AB)) |>
    mutate("K/PA"=K/PA, OBPE=(H+BB+HBP+ROE)/PA)
  bind_rows(ind_stats, team_stats) |>
    select(any_of(c("Number", "Name", "G", "Lineup")), everything())
}

## PITCHER STATS
## returns data set with Number, Order, and then all the counting stats
pitcher_counting_stats <- function(d) {
  d |> select(Pitcher, Balls, Strikes, Fouls, Outcome, RunnersOut) |> left_join(key, by="Outcome") |>
    mutate(Out = Out + RunnersOut) |> select(-RunnersOut) |>
    mutate(Strikes = Strikes + (Pitch == "Strike"), Balls = Balls + (Pitch == "Ball")) |>
    mutate(Order = as.integer(as_factor(paste(Pitcher)))) |>
    group_by(Order, Pitcher) |> summarize(
      G=NA, IP=NA,
      Outs=sum(Out), BF=sum(Outcome!="_"),
      S=sum(Strikes+Fouls), P=sum(Balls+Strikes+Fouls), SR=NA,
      H=sum(Hit, na.rm=TRUE), AB=sum(!is.na(Hit)),
      K=sum(Outcome=="K"), BB=sum(Outcome=="BB"), HB=sum(Outcome=="HB"),
      ROE=sum(Outcome=="E"),
      `1B`=sum(Outcome=="1B"), `2B`=sum(Outcome=="2B"), `3B`=sum(Outcome=="3B"), HR=sum(Outcome=="HR"),
      .groups="drop") |> select(Pitcher, everything()) |> rename(Number="Pitcher")
}

pitcher_stats <- function(game, rosters, who=c("away", "home")) {
  who <- match.arg(who)
  i <- match(who, c("away", "home"))
  x <- game$lineup[c(1,i+1)]
  d <- game[[setdiff(c("home", "away"), who)]]
  team <- colnames(x)[2]
  ## x has Lineup, <Team Name>; rosters has Number, Name
  ## <Team Name> to Number, then join by Number
  names(x)[2] <- "Number"
  if(team %in% names(rosters)) {
    x <- x |> left_join(select(rosters[[team]], c(Number, Name)), by="Number")
  }
  stats <- pitcher_counting_stats(d)
  ind_stats <- stats |> mutate(SR=S/P, IP=getIP(Outs)) |> left_join(x, by="Number")
  team_stats <- stats |> summarize(across(-c(Number, Order), sum)) |>
    mutate(Number=NA, Order=Inf, Name="Team") |>
    mutate(SR=S/P, IP=getIP(Outs)) |>
    mutate("BBHB/BF"=(BB+HB)/BF, "Opp. OBP"=(H+BB+HB)/BF)
  bind_rows(ind_stats, team_stats) |> arrange(Order) |> select(-Lineup, -Order) |>
    select(any_of(c("Number", "Name")), everything())
}

readgame <- function(file) {
  message(file)
  ss <- readxl::excel_sheets(file)
  tmp <- readxl::read_excel(file, "Lineup", n_max = 1, col_names = FALSE, .name_repair="minimal")
  when <- tmp[[1]] |> stringr::str_replace("([ap])$", "\\1m")
  about <- if(ncol(tmp) > 1) tmp[[2]] else "GSBL"
  g1 <- readxl::read_excel(file, "Lineup", skip=1)
  stopifnot(names(g1)[2:3] %in% ss[2:3])
  g1_away <- readxl::read_excel(file, names(g1)[2]) |> makedata()
  g1_home <- readxl::read_excel(file, names(g1)[3]) |> makedata()
  tibble(when=when, about=about, lineup=list(g1), away=list(g1_away), home=list(g1_home))
}

readrosters <- function(file) {
  ss <- readxl::excel_sheets(file)
  lapply(ss, readxl::read_excel, path=file) |> setNames(ss)
}

all_stats <- function(games, team) {
  the_game_stats <- games |> select(stats) |> unnest(stats)
  b.all <- the_game_stats |> filter(Team=="Roseville") |> select(Batter_Stats) |> unnest(Batter_Stats) |>
    mutate(G=1) |>
    group_by(Number, Name) |> summarize(across(everything(), sum)) |> ungroup() |>
    mutate(Lineup=NA) |>
    mutate(BA=getBA(H,AB)) |>
    mutate("K/PA"=K/PA, OBPE=(H+BB+HBP+ROE)/PA)
  p.all <- the_game_stats |> filter(Team=="Roseville") |> select(Pitcher_Stats) |> unnest(Pitcher_Stats) |>
    mutate(G=1) |>
    group_by(Number, Name) |> summarize(across(everything(), sum)) |> ungroup() |>
    mutate(SR=S/P, IP=getIP(Outs)) |>
    mutate("BBHB/BF"=(BB+HB)/BF, "Opp. OBP"=(H+BB+HB)/BF)
  list(Batting=b.all, Pitching=p.all)
}

get_score <- function(game) {
  a1 <- game$away |> group_by(Inning) |> summarize(R=sum(ToBase==4), .groups="drop") |> rename(away="R")
  a2 <- game$home |> group_by(Inning) |> summarize(R=sum(ToBase==4), .groups="drop") |> rename(home="R")
  Final <- c(sum(a1$away), sum(a2$home))
  out <- full_join(a1, a2, by="Inning") |> as.data.frame()
  rownames(out) <- out$Inning
  out$Inning <- NULL
  colnames(out) <- names(game$lineup[2:3])
  out |> as.matrix() |> t() |> cbind(R=Final)
}

game_stats <- function(game, rosters) {
  tibble(Team=names(game$lineup)[2:3], Role=c("away", "home"),
         vs=names(game$lineup)[3:2],
         Data=list(game$away, game$home),
         Pitcher_Stats=list(pitcher_stats(game, rosters, "away"),
                            pitcher_stats(game, rosters, "home")),
         Batter_Stats=list(batter_stats(game, rosters, "away"),
                           batter_stats(game, rosters, "home")))
}

## calls game_stats
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


