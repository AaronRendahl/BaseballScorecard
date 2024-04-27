# need to have added "Base" to plays to specify how many bases the batter initially made it to
find_runners <- function(plays, pattern.out="^X", after.play=c("P", "E", "FC")) {
  rx <- plays |> filter(!is.na(B1)) |>
    select(AtBatID_Runner=AtBatID, Lineup_Runner=Lineup, Runner=Batter, Pitcher, Row, Inning, Side, Base, B2, B3, B4) |>
    # now pivot longer and remove any rows that they reached due to their at bat
    pivot_longer(c(B2, B3, B4), names_to="B") |>
    mutate(B=as.integer(str_sub(B, 2, 2))) |>
    mutate(value=if_else(B==Base & B > 1, ".", value)) |>
    select(-Base) |>
    rename(Base="B") |>
    # now backfill for any that they moved two bases on the same play
    group_by(AtBatID_Runner) |>
    fill(value, .direction="up") |>
    ungroup() |>
    # and now remove bases they didn't get to
    filter(!is.na(value) & value!=".") |>
    mutate(idx=1:n()) |>
    # now can split the value into isOut/Advance/Lineup/AtBatPitches/Fielders
    # if how not specified, assume it was part of the play
    mutate(isOut=str_detect(value, pattern.out)*1L,
           value=str_remove(value, pattern.out),
           value=str_replace(value, "^$", "?"),
           value=str_replace(value, "^([0-9])", "P\\1")) |>
    separate_wider_regex(value, c(Advance="[^0-9]*", Lineup="[0-9-.]+", Fielders=".*"), too_few="align_start") |>
    separate(Lineup, c("Lineup", "AtBatPitches"), sep="[.-]", fill = "right") |>
    mutate(Fielders=str_remove(Fielders, "^/") |> na_if("")) |>
    mutate(Lineup=as.integer(Lineup)) |>
    mutate(AtBatPitches=as.numeric(AtBatPitches))

  p1 <- plays |> select(AtBatID, Inning, Side, Lineup) |> unique()
  r1 <- rx |> filter(!is.na(Lineup)) |>
    select(idx, AtBatID_Runner, Inning, Side, Lineup) |>
    left_join(p1, by=c("Inning", "Side", "Lineup")) |>
    # if batted through in an inning, there could be two matches,
    # need to get the first one that is at or after the runner
    mutate(ok = AtBatID == min(AtBatID[AtBatID >= AtBatID_Runner]), .by="idx") |>
    (\(x) { # ERROR CHECK: each should have only one that is found
      tmp <- x |> mutate(nok=sum(ok, na.rm=TRUE), .by=idx) |> filter(nok!=1)
      if(nrow(tmp)) {print(tmp); stop("exactly 1 batter not found")}
      x
    })() |>
    filter(is.na(ok) | ok) |>
    (\(x) { # ERROR CHECK: if have Lineup, should have found a RunnerID
      tmp <- x |> filter(!is.na(Lineup) & is.na(AtBatID_Runner))
      if(nrow(tmp)) {print(tmp); stop("> 1 batter found")}
      x
    })() |>
    select(idx, AtBatID)
  rx |> left_join(r1, by="idx") |>
    select(-idx) |>
    select(Inning, Side, AtBatID, AtBatID_Runner, Lineup, Lineup_Runner, everything()) |>
    mutate(AtBatPitches=case_when(!is.na(AtBatPitches) ~ AtBatPitches,
                             is.na(AtBatID) ~ 100L,         # after this play, sometime...
                             Advance %in% after.play ~ 99L, # after this specific play
                             TRUE ~ 0L),                     # before this specific play
           .after=AtBatID) |>
    # if know when made it to a previous base, then everything after that must be after that
    arrange(AtBatID_Runner, Base) |>
    group_by(AtBatID_Runner) |>
    fill(AtBatID_Runner) |> fill(Lineup) |> fill(AtBatID) |>
    ungroup() |>
    # otherwise, must be after they batted
    mutate(AtBatID=if_else(is.na(AtBatID), AtBatID_Runner, AtBatID),
           Lineup=if_else(is.na(Lineup), Lineup_Runner, Lineup)) |>
    (\(x) { # ERROR CHECK: Runner > Batter, or if = AtBatPitches is not 0
      tmp <- x |> filter(AtBatID_Runner > AtBatID | (AtBatID_Runner == AtBatID & AtBatPitches <= 0))
      if(nrow(tmp)) {print(tmp); stop("became runner before batted!")}
      x
    })()
}

make_plays <- function(g, p,
                       pattern.out="^X",
                       noPlay = "_",
                       key.Base = key |> filter(Base>1) |> select(B1=Outcome, Base),
                       Pitches_fun=add_Pitches) {
  if(missing(p)) {
    stopifnot(is_tibble(g) & nrow(g)==1)
    p <- g$game[[1]] |> select(Side, Plays) |> unnest(Plays)
  }

  px <- p |>
    Pitches_fun() |>
    ## add AtBatID
    arrange(Inning, Side, Row) |>
    mutate(.p=is.na(lag(Row)), .x=!(lag(Play) %in% noPlay), .by=c(Inning, Side)) |>
    mutate(AtBatID=cumsum(.x | .p), .after=Row) |> select(-.x, -.p) |>
    ## get Pitches so far in at bat (AtBatPitches)
    mutate(AtBatPitches=cumsum(Pitches), .by=AtBatID) |>
    ## add Base
    left_join(key.Base, by="B1") |>
    mutate(
      Base=case_when(!is.na(Base) ~ Base, !is.na(B1) ~ 1L, TRUE ~0L),
      Base=if_else(Play %in% noPlay, NA, Base)) |>
    ## add isOut
    mutate(isOut=(Base==0L)*1L) |>
    separate_wider_regex(Play, c(Play="[A-Za-z_]*", Fielders=".*")) |>
    mutate(Fielders=na_if(Fielders, ""))

  rx <- find_runners(px, pattern.out)

  bind_rows(rx, px) |>
    # if no runner, set the runner to the batter and the AtBatID to 0
    mutate(
      Runner=if_else(is.na(Runner), Batter, Runner),
      Lineup_Runner=if_else(is.na(Lineup_Runner), Lineup, Lineup_Runner),
      AtBatID_Runner=if_else(is.na(AtBatID_Runner), AtBatID, AtBatID_Runner)) |>
    # fill in the Batter variable, need to be careful in case an at bat had more than one batter
    arrange(AtBatID, AtBatPitches, AtBatID_Runner, Base) |>
    group_by(AtBatID) |> fill(Batter, .direction="downup") |> ungroup() |>
    # now that they're in the right place, can put in the correct AtBatPitches
    mutate(AtBatPitches=na_if(AtBatPitches, 99L)) |>
    fill(AtBatPitches) |>
    # need to change isOut to NA if not their last stop
    mutate(lastbase=Base==max(-1, Base, na.rm=TRUE), .by=AtBatID_Runner) |>
    mutate(R = (Base==4 & !isOut) * 1L,
           LOB = if_else(!lastbase, 0L, (Base!=4 & !isOut) * 1L)) |>
    mutate(Type=case_when(!is.na(Advance) ~ "Runner", TRUE ~ "Play")) |>
    # final selection of variables
    select(Row, Inning, Side, AtBatID, AtBatPitches,
           Lineup, AtBatID_Runner, Lineup_Runner,
           Batter, Pitcher, Runner,
           Type,
           Pitches, Balls, Strikes, Fouls,
           Play, B1, Advance,
           Base, Out=isOut, R, LOB, Fielders)
}

add_plays <- function(gs, ...) {
  gs$plays <- map(seq_len(nrow(gs)), \(i) make_plays(gs[i,], ...))
  gs
}
