get_individual_stats <- function(g, stats, team,
                                 role = c("Batter/Runner", "Pitcher"),
                                 calcs, roles, key=BaseballScorecard::codes,
                                 grp_by = c("Number", "Name")) {
  role <- match.arg(role)
  lx <- g |> select(code, game) |> unnest(game) |>
    select(code, Side, Team, Lineup) |> unnest(Lineup) |>
    select(code, Side, Team, Number, Name)

  ab <- g |> select(code, about, when, game) |> unnest(game) |>
    select(code, about, when, Team) |>
    mutate(when=MDY_format(when)) |>
    filter(Team!=team) |> rename(vs=Team)
  lx <- left_join(lx, ab, by="code")

  cs <- counting_stats_all(g, key)$stats

  grps <- c("Number", "Name", "Lineup", "Order", "Team", "Side", "about", "when", "vs", "code")
  foov <- tibble(Stat=c(grp_by, stats)) |> mutate(Order=1:n(), .before=1) |>
    left_join(roles, by="Stat") |>
    mutate(Group=case_when(Stat %in% grps ~ "Group", is.na(Stat) ~ "Blank", TRUE ~ "Stat"))

  grp <- foov |> filter(Group=="Group") |> pull("Stat")
  final <- foov |> filter(Order>0) |> pull(Stat)

  if(role=="Batter/Runner") {
    need_r <- foov |> filter(Role=="Runner" & Group=="Stat") |> pull("Stat") |> get_calc_vars(calcs)
    need_b <- foov |> filter(Role=="Batter" & Group=="Stat") |> pull("Stat") |> get_calc_vars(calcs)
    br_stats(cs, lx, total=c(Name="Team"),
             Team==team,
             batter_by=grp_by,
             batter_counts=need_b$Counts,
             batter_cols=need_b$Need,
             batter_calculations=need_b$Calculations,
             runner_by=grp_by,
             runner_counts=need_r$Counts,
             runner_cols=need_r$Need,
             runner_calculations=need_r$Calculations,
             final_cols=final,
             arrange_by=grp_by[1], arrange_desc = FALSE)
  } else {
    need_p <- foov |> filter(Group=="Stat") |> pull("Stat") |> get_calc_vars(calcs)
    p_stats(cs, lx, total=c(Name="Team"),
            Team==team,
            pitcher_by = grp,
            pitcher_counts = need_p$Counts,
            pitcher_cols = need_p$Need,
            pitcher_calculations = need_p$Calculations,
            final_cols=final,
            arrange_by=grp_by[1], arrange_desc = FALSE)
  }

}
