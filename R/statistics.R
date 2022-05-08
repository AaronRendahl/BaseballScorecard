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
  ff <- function(d) {
    d |> select(Lineup, Outcome, ToBase) |> left_join(key, by="Outcome") |>
      group_by(Lineup) |> summarize(
        G=NA,
        PA=sum(Outcome!="_"), H=sum(Hit, na.rm=TRUE), AB=sum(!is.na(Hit)), BA=NA,
        R=sum(ToBase==4), Blank=NA,
        K=sum(Outcome=="K"), BB=sum(Outcome=="BB"), HBP=sum(Outcome=="HB"),
        ROE=sum(Outcome=="E"),
        `1B`=sum(Outcome=="1B"), `2B`=sum(Outcome=="2B"), `3B`=sum(Outcome=="3B"), HR=sum(Outcome=="HR"),
        .groups="drop") |>
      mutate(BA=getBA(H, AB))
  }
  bind_rows(
    x |> right_join(ff(d), by="Lineup"),
    d |> mutate(Lineup=NA) |> ff() |> mutate(Number=NA, Name=teamname) |>
      mutate("K/PA"=K/PA, OBPE=(H+BB+HBP+ROE)/PA)
  ) |> select(any_of(c("Number", "Name","G", "Lineup")), everything())
}

## PITCHER STATS
pitcher_stats <- function(game, rosters, who=c("away", "home")) {
  who <- match.arg(who)
  i <- match(who, c("away", "home"))
  x <- game$lineup[c(1,i+1)]
  d <- game[[setdiff(c("home", "away"), who)]]
  team <- colnames(x)[2]
  names(x)[2] <- "Number"
  if(team %in% names(rosters)) {
    x <- x |> left_join(select(rosters[[team]], c(Number, Name)), by="Number")
  }
  ff <- function(d) {
    d |> select(Pitcher, Balls, Strikes, Fouls, Outcome, RunnersOut) |> left_join(key, by="Outcome") |>
      mutate(Out = Out + RunnersOut) |> select(-RunnersOut) |>
      mutate(Strikes = Strikes + (Pitch == "Strike"), Balls = Balls + (Pitch == "Ball")) |>
      mutate(Order = as.integer(as_factor(paste(Pitcher)))) |>
      group_by(Order, Pitcher) |> summarize(
        G=NA,IP=NA,
        Outs=sum(Out), BF=sum(Outcome!="_"),
        S=sum(Strikes+Fouls), P=sum(Balls+Strikes+Fouls), SR=S/P,
        H=sum(Hit, na.rm=TRUE), AB=sum(!is.na(Hit)),
        K=sum(Outcome=="K"), BB=sum(Outcome=="BB"), HB=sum(Outcome=="HB"),
        ROE=sum(Outcome=="E"),
        `1B`=sum(Outcome=="1B"), `2B`=sum(Outcome=="2B"), `3B`=sum(Outcome=="3B"), HR=sum(Outcome=="HR"),
        .groups="drop") |> select(Pitcher, everything()) |> rename(Number="Pitcher")
  }
  bind_rows(
    d |> mutate(Pitcher=0) |> ff() |> mutate(Number=NA, Name="Team", Order=Inf) |>
      mutate("BBHB/BF"=(BB+HB)/BF, "Opp. OBP"=(H+BB+HB)/BF),
    x |> right_join(ff(d), by="Number")
  ) |> mutate(IP=getIP(Outs)) |> select(any_of(c("Number", "Name")), everything()) |>
  arrange(Order) |> select(-Lineup, -Order)
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

all_stats <- function(games, rosters, team) {
  both_stats <- function(game, rosters, team) {
    k <- match(team, colnames(game$lineup))
    if(!is.na(k)) {
      list(batter=batter_stats(game, rosters, c("away", "home")[k-1], teamname=FALSE),
           pitcher=pitcher_stats(game, rosters, c("away", "home")[k-1]))
    } else {
      NULL
    }
  }
  ss <- lapply(games, both_stats, rosters=rosters, team=team)
  b.all <- bind_rows(lapply(ss, function(x) x$batter)) |>
    group_by(Number, Name) |> mutate(G=n()) |> ungroup() |>
    mutate(Lineup=NA) |> pivot_longer(-c(Lineup, Number, Name, G)) |>
    mutate(across("name", as_factor)) |>
    group_by(Number, Name, G, Lineup, name) |> summarize(value=sum(value), .groups="drop") |>
    pivot_wider() |> mutate(BA=getBA(H,AB)) |>
    mutate("K/PA"=K/PA, OBPE=(H+BB+HBP+ROE)/PA)
  p.all <- bind_rows(lapply(ss, function(x) x$pitcher)) |>
    group_by(Number, Name) |> mutate(G=n()) |> ungroup() |>
    pivot_longer(-c(Number, Name, G)) |>
    mutate(across("name", as_factor)) |>
    group_by(Number, Name, G, name) |> summarize(value=sum(value), .groups="drop") |>
    pivot_wider() |> mutate(SR=S/P) |>
    mutate("BBHB/BF"=(BB+HB)/BF, "Opp. OBP"=(H+BB+HB)/BF, IP=getIP(Outs))
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
  list(about=setNames(list(game$about, colnames(game$lineup)[2], colnames(game$lineup)[3]),
                      c("about", "away", "home")),
       home=list(Batting=batter_stats(game, rosters, "home"),
                 Pitching=pitcher_stats(game, rosters, "home")),
       away=list(Batting=batter_stats(game, rosters, "away"),
                 Pitching=pitcher_stats(game, rosters, "away")))
}

## calls game_stats
makestatsfile <- function(games, rosters, team, filename) {
  add_title <- function(x, title) {
    x <- rbind(names(x), x)
    if(!missing(title)) {
      x <- rbind(NA, x)
      x[1,1] <- title
    }
    x
  }
  game <- games[[length(games)]]
  game_stats <- game_stats(game, rosters)
  about <- game_stats$about$about
  k_us <- match(team, colnames(game$lineup)[2:3])
  k_them <- setdiff(1:2, k_us)
  team2 <- colnames(game$lineup[1+k_them])
  us <- c("away", "home")[k_us]
  them <- c("away", "home")[k_them]

  us_stats <- game_stats[[us]]
  them_stats <- game_stats[[them]]

  b0 <- bind_rows(us_stats$Batting,
                  them_stats$Batting |> filter(is.na(Lineup)))
  b1x <- b0[1:(nrow(b0)-2), ]
  b2x <- b0[1:2 + (nrow(b0)-2), ] |> rename(Team="Name")
  list.batting <- list(b1x, b2x) |>
    setNames(c("Roseville Game Stats", "Team Game Stats"))

  a1x <- us_stats$Pitching
  a2x <- them_stats$Pitching
  list.pitching <- list(us=a1x, them=a2x) |>
    setNames(c(paste(c("Roseville", team2), "Game Stats")))

  out <- list(Batting=list.batting, Pitching=list.pitching)
  if(!missing(filename)) {
    statsToExcel(out, filename)
  }
  invisible(out)
}


