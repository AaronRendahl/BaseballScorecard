streaks <- function(gs, team, roll_num=4, calcs,
                            var="SLG + OBPE + notK/PA:\nBatting Sum",
                            title="Summary",
                            full_title,
                            role=c("Batter/Runner", "Pitcher")) {
  stopifnot(is_tibble(gs))
  role <- match.arg(role)

  lx <- gs |> select(code, game) |> unnest(game) |> select(code, Side, Team, Lineup) |> unnest(Lineup) |>
    select(code, Side, Team, Number, Name)

  cs2 <- counting_stats_all(gs)

  if(role=="Batter/Runner") {
    cs <- cs2$stats |> mutate(idx=as.integer(factor(code)), .before=1) |>
      rename(Number=Batter) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
      filter(Team==team)
  } else {
    lx$Side <- 3 - lx$Side
    cs <- cs2$stats |> mutate(idx=as.integer(factor(code)), .before=1) |>
      rename(Number=Pitcher) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
      filter(Team==team)
  }
  cv <- get_calc_vars(var, calcs)

  X.all <- calc_stats(cs, cv$Counts, cv$Calculations, by="Name", total=c(Name="Team")) |>
    select("Name", all_of(var)) |> rename(y=!!var)
  X.indall <- X.all |> filter(Name!="Team") |> arrange(desc(y)) |> mutate(Name=as_factor(Name))
  X.teamall <- X.all |> filter(Name=="Team") |> select(-Name)

  csx1 <- cs |> select(idx, code, Name, all_of(cv$Counts)) |>
    summarize(across(all_of(cv$Counts), \(x) sum(x, na.rm=TRUE)), .by=all_of(c("idx", "Name")))
  csx2 <- csx1 |> mutate(.x=1L) |> mutate(across(all_of(c(cv$Counts, ".x")), \(x) zoo::rollsum(x, k=roll_num, fill=NA, align="right")), .by="Name") |>
    filter(!is.na(.x)) |> select(-.x)
  X.team <- calc_stats(csx2 |> mutate(code=1), cv$Counts, cv$Calculations, by="idx", total=NA) |>
    select(idx, all_of(var)) |> rename(y=!!var)

  for(n in names(cv$Calculations)) { csx2[[n]] <- with(csx2, eval(cv$Calculations[[n]])) }
  X.ind <- csx2 |> select(idx, Name, all_of(var)) |> rename(y=!!var) |> mutate(Name=factor(Name, levels=levels(X.indall$Name)))

  coli <- "black"
  colt <- "gray60"
  subt <- "Black shows player, gray shows team average; straight lines are season averages"

  if(missing(full_title)) {
    roll_txt <- if(roll_num > 1) sprintf(" (Average for last %d games)", roll_num) else ""
    full_title <- sprintf("%s%s", title, roll_txt)
  }

  ggplot(X.ind) + aes(idx, y) +
    facet_wrap(~Name, nrow=2) +
    geom_hline(aes(yintercept=y), data=X.indall, col=coli) +
    geom_hline(aes(yintercept=y), data=X.teamall, col=colt) +
    geom_line(data=X.team, col=colt) + geom_point(data=X.team, fill=colt, pch=21) +
    geom_line(col=coli) + geom_point(fill=coli, pch=21) +
    xlab("Game Number") +
    scale_x_continuous() +
    ggtitle(full_title, subtitle=subt) +
    ylab(var)
}
