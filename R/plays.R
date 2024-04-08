# need to have added "Base" to plays to specify how many bases the batter initially made it to
find_runners <- function(plays, pattern.out="^X", after.play=c("P", "E", "FC")) {
  rx <- plays |> filter(!is.na(B1)) |>
    select(AtBatID_Runner=AtBatID, Runner=Lineup, Pitcher, Row, Inning, Side, Base, B2, B3, B4) |>
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
    # now can split the value into Out/How/Lineup/AtBatPitches/Note
    # if how not specified, assume it was part of the play
    mutate(Out=str_detect(value, pattern.out)*1L,
           value=str_remove(value, pattern.out),
           value=str_replace(value, "^$", "?"),
           value=str_replace(value, "^([0-9])", "P\\1")) |>
    separate_wider_regex(value, c(How="[^0-9]*", Lineup="[0-9-.]+", Note=".*"), too_few="align_start") |>
    separate(Lineup, c("Lineup", "AtBatPitches"), sep="[.-]", fill = "right") |>
    mutate(Note=str_remove(Note, "^/") |> na_if("")) |>
    mutate(Lineup=as.integer(Lineup)) |>
    mutate(AtBatPitches=as.numeric(AtBatPitches))

  p1 <- plays |> select(AtBatID, BatterID, Inning, Side, Lineup) |> unique()
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
    select(Inning, Side, AtBatID, AtBatID_Runner, Lineup, Runner, everything()) |>
    mutate(AtBatPitches=case_when(!is.na(AtBatPitches) ~ AtBatPitches,
                             is.na(AtBatID) ~ 1000L,     # after this play, sometime...
                             How %in% after.play ~ 100L, # after this specific play
                             TRUE ~ 0L),                 # before this specific play
           .after=AtBatID) |>
    # if know when made it to a previous base, then everything after that must be after that
    arrange(AtBatID_Runner, Base) |>
    group_by(AtBatID_Runner) |>
    fill(AtBatID_Runner) |> fill(Lineup) |>
    ungroup() |>
    # otherwise, must be after they batted
    mutate(AtBatID=if_else(is.na(AtBatID), AtBatID_Runner, AtBatID),
           Lineup=if_else(is.na(Lineup), Runner, Lineup)) |>
    (\(x) { # ERROR CHECK: Runner > Batter, or if = AtBatPitches is not 0
      tmp <- x |> filter(AtBatID_Runner > AtBatID | (AtBatID_Runner == AtBatID & AtBatPitches <= 0))
      if(nrow(tmp)) {print(tmp); stop("became runner before batted!")}
      x
    })()
}

make_plays <- function(g,
                       pattern.out="^X",
                       noPlay = "_",
                       key.Base = key |> filter(Base>1) |> select(B1=Outcome, Base),
                       Pitches_fun=add_Pitches) {
  stopifnot(is_tibble(g) & nrow(g)==1)
  p2 <- g$game[[1]] |> select(Side, Plays) |> unnest(Plays)

  px <- p2 |>
    Pitches_fun() |>
    ## add BatterID
    arrange(Inning, Side, Row) |>
    mutate(x=Lineup!=lag(Lineup, default=0), .by=c(Side, Inning)) |>
    mutate(BatterID=cumsum(x), .after=Lineup) |> select(-x) |>
    ## add AtBatID
    mutate(.p=is.na(lag(BatterID)), .x=!(lag(Play) %in% noPlay), .by=c(Side, Inning)) |>
    mutate(AtBatID=cumsum(.x | .p), .after=BatterID) |> select(-.x, -.p) |>
    ## get Pitches so far in at bat (AtBatPitches)
    mutate(AtBatPitches=cumsum(Pitches), .by=AtBatID) |>
    ## rename
    rename(NumOut=Out) |>
    ## add Base
    left_join(key.Base, by="B1") |>
    mutate(
      Base=case_when(!is.na(Base) ~ Base, !is.na(B1) ~ 1L, TRUE ~0L),
      Base=if_else(Play %in% noPlay, NA, Base)) |>
    ## add Out
    mutate(Out=(Base==0L)*1L)

  rx <- find_runners(px, pattern.out)

  bind_rows(rx, px) |>
    mutate(
      Runner=if_else(is.na(Runner), Lineup, Runner),
      AtBatID_Runner=replace_na(AtBatID_Runner, 0L)) |>
    arrange(AtBatID, AtBatPitches, AtBatID_Runner, Base) |>
    select(Side, Row, Inning, AtBatID, AtBatPitches,
           Pitcher, Pitches, Balls, Strikes, Fouls,
           BatterID, Lineup, Play, B1,
           AtBatID_Runner, Runner, How, Note,
           Base, Out) |>
    # now that they're in the right place, can put in the correct AtBatPitches
    mutate(AtBatPitches=na_if(AtBatPitches, 100L)) |>
    fill(AtBatPitches) |>
    mutate(AtBatPitches=if_else(AtBatPitches==1000L, 100L, AtBatPitches))
}

add_plays <- function(gs, ...) {
  gs$plays <- map(seq_len(nrow(gs)), \(i) make_plays(gs[i,], ...))
  gs
}
