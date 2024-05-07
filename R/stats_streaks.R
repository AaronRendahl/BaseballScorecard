batting_streaks <- function(gs, team, roll_num=4) {
  stopifnot(is_tibble(gs))

  lx <- gs |> select(code, game) |> unnest(game) |> select(code, Side, Team, Lineup) |> unnest(Lineup)
  lx_Batter <- lx |> select(code, Side, Team, Number, Name)

  cs2 <- counting_stats_all(gs)
  cn <- cs2$names

  cs <- cs2$stats |> mutate(idx=as.integer(factor(code)), .before=1) |>
    rename(Number=Batter) |> left_join(lx_Batter, by=c('code', 'Side', 'Number')) |>
    filter(Team==team)

  calcs <- batter_calculations[c("SLG", "OBPE", "K/PA", "SLG + OBPE + notK/PA:\nBatting Sum")]
  names(calcs)[4] <- "y"
  need <- c("TB", "AB", "H", "BB", "HBP", "ROE", "PA", "K")
  remove <- c("Games", setdiff(c(names(calcs), need), "y"))

  X.all <- calc_stats(cs, need, calcs, by="Name", total=c(Name="Team")) |> select(-all_of(remove))
  X.indall <- X.all |> filter(Name!="Team") |> arrange(desc(y)) |> mutate(Name=as_factor(Name))
  X.teamall <- X.all |> filter(Name=="Team") |> select(-Name)

  csx1 <- cs |> select(idx, code, Name, all_of(need)) |>
    summarize(across(all_of(need), \(x) sum(x, na.rm=TRUE)), .by=all_of(c("idx", "Name")))
  csx2 <- csx1 |> mutate(across(all_of(need), \(x) zoo::rollsum(x, k=roll_num, fill=NA, align="right")), .by="Name") |>
    filter(!is.na(TB))
  X.team <- calc_stats(csx2 |> mutate(code=1), need, calcs, by="idx") |> select(-any_of(remove))

  for(n in names(calcs)) { csx2[[n]] <- with(csx2, eval(calcs[[n]])) }
  X.ind <- csx2 |> select(-any_of(remove)) |> mutate(Name=factor(Name, levels=levels(X.indall$Name)))

  coli <- "black"
  colt <- "gray60"
  subt <- "Black shows player, gray shows team average; straight lines are season averages"

  ggplot(X.ind) + aes(idx, y) +
    facet_wrap(~Name, nrow=2) +
    geom_hline(aes(yintercept=y), data=X.indall, col=coli) +
    geom_hline(aes(yintercept=y), data=X.teamall, col=colt) +
    geom_line(data=X.team, col=colt) + geom_point(data=X.team, fill=colt, pch=21) +
    geom_line(col=coli) + geom_point(fill=coli, pch=21) +
    xlab("Game Number") +
    scale_x_continuous(breaks=1:3) + #seq(0, 40, by=10)) +
    ggtitle(sprintf("Batting Summary (Average for last %d games)", roll_num), subtitle=subt) +
    ylab("Batting Sum: SLG + OBPE + notK/PA")
}
