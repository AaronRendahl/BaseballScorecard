# need to have added "Base" to plays to specify how many bases the batter initially made it to
find_runners <- function(plays, pattern.out="^X") {
  rx <- plays |> filter(!is.na(B1)) |>
    select(RunnerID=BatterID, Runner=Lineup, Pitcher, Row, Inning, Side, Base, B2, B3, B4) |>
    # now pivot longer and remove any rows that they reached due to their at bat
    pivot_longer(c(B2, B3, B4), names_to="B") |>
    mutate(B=as.integer(str_sub(B, 2, 2))) |>
    mutate(value=if_else(B==Base & B > 1, ".", value)) |>
    select(-Base) |>
    rename(Base="B") |>
    # now backfill for any that they moved two bases on the same play
    group_by(RunnerID) |>
    fill(value, .direction="up") |>
    ungroup() |>
    # and now remove bases they didn't get to
    filter(!is.na(value) & value!=".") |>
    # now can split the value into Out/How/When/Note
    # if how not specified, assume it was part of the play
    mutate(Out=str_detect(value, pattern.out)*1L,
           value=str_remove(value, pattern.out),
           value=str_replace(value, "^([0-9])", "P\\1")) |>
    separate_wider_regex(value, c(How="[^0-9]*", Lineup="[0-9]+", Note=".*"), too_few="align_start") |>
    mutate(Lineup=as.integer(Lineup)) |>
    mutate(idx=1:n())

  p1 <- px |> select(BatterID, Inning, Side, Lineup) |> unique()
  r1 <- rx |> filter(!is.na(Lineup)) |>
    select(idx, RunnerID, Inning, Side, Lineup) |>
    left_join(p1, by=c("Inning", "Side", "Lineup")) |>
    # if batted through in an inning, there could be two matches,
    # need to get the first one that is at or after the runner
    mutate(ok = BatterID == min(BatterID[BatterID >= RunnerID]), .by="idx") |>
    (\(x) { # ERROR CHECK: each should have only one that is found
      tmp <- x |> mutate(nok=sum(ok), .by=idx) |> filter(nok>1)
      if(nrow(tmp)) {print(tmp); stop("> 1 batter found")}
      x
    })() |>
    filter(is.na(ok) | ok) |>
    (\(x) { # ERROR CHECK: if have Lineup, should have found a RunnerID
      tmp <- x |> filter(!is.na(Lineup) & is.na(RunnerID))
      if(nrow(tmp)) {print(tmp); stop("> 1 batter found")}
      x
    })() |>
    select(idx, BatterID)
  rx |> left_join(r1, by="idx") |>
    select(-idx) |>
    select(Inning, Side, BatterID, Lineup, Runner, everything()) |>
    # TODO: allow this to be a parameter, and also add option to force them
    mutate(onPitch=case_when(is.na(BatterID) ~ 1000,           # after this play, sometime...
                             How %in% c("P", "E", "FC") ~ 100, # after this specific play
                             TRUE ~ 0),                        # before this specific play
           .after=BatterID) |>
    # if know when made it to a previous base, then everything after that must be after that
    arrange(RunnerID, Base) |>
    group_by(RunnerID) |>
    fill(BatterID) |> fill(Lineup) |>
    ungroup() |>
    # otherwise, must be after they batted
    mutate(BatterID=if_else(is.na(BatterID), RunnerID, BatterID),
           Lineup=if_else(is.na(Lineup), Runner, Lineup)) |>
    (\(x) { # ERROR CHECK: RunnerID > BatterID, or if = onPitch is not 0
      tmp <- x |> filter(RunnerID > BatterID | (RunnerID == BatterID & onPitch <= 0))
      if(nrow(tmp)) {print(tmp); stop("became runner before batted!")}
      x
    })()
}

make_plays <- function(g,
                       pattern.out="^X",
                       noPlay = "_",
                       key.Base = key |> filter(Base>1) |> select(B1=Outcome, Base)) {
  stopifnot(is_tibble(g) & nrow(g)==1)
  p2 <- g$game[[1]] |> select(Side, Plays) |> unnest(Plays)

  p2 |>
    ## add BatterID
    arrange(Inning, Side, Row) |>
    mutate(x=Lineup!=lag(Lineup, default=0), .by=c(Side, Inning)) |>
    mutate(BatterID=cumsum(x), .after=Lineup) |> select(-x) |>
    ## rename
    rename(NumOut=Out, onPitch=PitchesAtBat) |>
    ## add Base
    left_join(key.Base, by="B1") |>
    mutate(
      Base=case_when(!is.na(Base) ~ Base, !is.na(B1) ~ 1L, TRUE ~0L),
      Base=if_else(Play %in% noPlay, NA, Base)) |>
    ## add Out
    mutate(Out=(Base==0)*1L)

  rx <- find_runners(px, pattern.out)

  bind_rows(rx, px) |>
    arrange(BatterID, onPitch) |>
    select(Side, Row, Inning, BatterID, onPitch, Lineup, Pitcher,
           Runner, RunnerID,
           Balls, Strikes, Fouls,
           Play, B1, How, Note, Base, Out)
}
