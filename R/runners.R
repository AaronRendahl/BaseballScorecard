addBatterID <- function(x) {
  x |> arrange(Inning, Side, Row) |>
    mutate(x=Lineup!=lag(Lineup, default=0), .by=c(Side, Inning)) |>
    mutate(BatterID=cumsum(x), .after=Lineup) |> select(-x)
}

find_runners <- function(plays, key.Base) {
  rx <- plays |> filter(!is.na(B1)) |>
    select(RunnerID=BatterID, Runner=Lineup, Pitcher, Inning, Side, B1, B2, B3, B4) |>
    # how many bases did they get on their at bat
    left_join(key.Base, by="B1") |>
    mutate(Base=case_when(!is.na(Base) ~ Base, !is.na(B1) ~ 1L, TRUE ~0L)) |>
    select(-B1) |>
    # now pivot longer and remove any rows that they reached due to their at bat
    pivot_longer(c(B2, B3, B4), names_to="B") |>
    mutate(B=as.integer(str_sub(B, 2, 2))) |>
    mutate(value=if_else(B==Base & B > 1, ".", value)) |>
    # now backfill for any that they moved two bases on the same play
    group_by(RunnerID) |>
    fill(value, .direction="up") |>
    ungroup() |>
    # and now remove bases they didn't get to
    filter(!is.na(value) & value!=".") |>
    select(-Base) |>
    # now can split the value into How/When/Note
    mutate(value=value |> str_replace("^([0-9])", "P\\1")) |>
    separate_wider_regex(value, c(How="[A-Z]+", Lineup="[0-9]+", Note=".+"), too_few="align_start") |>
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
    arrange(RunnerID, B) |>
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
