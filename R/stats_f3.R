make_stats_from_file <- function(g, header, file, calcs, roles, team, vs, key=BaseballScorecard::codes) {

  f <- file |>
    mutate(across(where(is.character), \(x) str_replace(x, "\\\\n", "\n")))


  lx <- g |> select(code, game) |> unnest(game) |>
    select(code, Side, Team, Lineup) |> unnest(Lineup) |>
    select(code, Side, Team, Number, Name)

  if(!missing(team)) {
    ab <- g |> select(code, about, when, game) |> unnest(game) |>
      select(code, about, when, Team) |>
      mutate(when=MDY_format(when)) |>
      filter(Team!=team) |> rename(vs=Team)
    lx <- left_join(lx, ab, by="code")
  }

  cs <- counting_stats_all(g, key)$stats

  us <- team
  them <- vs

  read_idx <- function(idx) {
    foo <- local({
      nn <- names(f)[idx]
      fi <- f |> select(Order, Stat=all_of(nn))
      rr <- fi |> filter(Order=="Role") |> pull("Stat")
      ff <- fi |> filter(Order=="Filter") |> pull("Stat")
      tt <- fi |> filter(Order=="Total") |> pull("Stat")
      x <- fi |> filter(!Order %in% c("Role", "Filter", "Total"))
      sort_X <- str_subset(x$Stat, "^[><]")
      sort_by <- sort_X |> str_remove("^[><] *")
      sort_desc <- if(length(sort_X)>0) str_sub(sort_X, 1, 1) == ">" else FALSE
      x <- x |> mutate(Stat=str_remove(Stat, "^[><] *")) |>
        mutate(Order=if_else(Order=="Hide", "0", Order) |> as.numeric())
      #vv <- x |> filter(Order>0) |> arrange(Order) |> pull(Stat)
      #nn <- x |> filter(!is.na(Stat)) |> pull(Stat)
      list(name=glue::glue(nn), role=rr, filter=ff, total=tt, sort_by=sort_by, sort_desc=sort_desc, vars=x)
    })

    grps <- c("Number", "Name", "Lineup", "Order", "Team", "Side",
              "about", "when", "vs", "code")
    foov <- foo$vars |> left_join(roles, by="Stat") |>
      mutate(Group=case_when(Stat %in% grps ~ "Group", is.na(Stat) ~ "Blank", TRUE ~ "Stat"))

    grp <- foov |> filter(Group=="Group") |> pull("Stat")
    final <- foov |> filter(Order>0) |> pull(Stat)
    sort_by <- foo$sort_by
    sort_desc <- foo$sort_desc
    filter_txt <- foo$filter |> replace_na("TRUE")
    total_txt <- foo$total

    outi <- if(foo$role=="Batter/Runner") {
      oops <- foov |> filter(Group=="Stat" & is.na(Role))
      if(nrow(oops) > 1) {
        stop("Role unknown for ", paste(oops$Stat, collapse=", "))
      }
      need_r <- foov |> filter(Role=="Runner" & Group=="Stat") |> pull("Stat") |> get_calc_vars(calcs)
      need_b <- foov |> filter(Role=="Batter" & Group=="Stat") |> pull("Stat") |> get_calc_vars(calcs)
      grp_r <- setdiff(grp, "Lineup")
      br_stats(cs, lx, total=eval(parse(text=total_txt)),
               eval(parse(text=filter_txt)),
               batter_by=grp,
               batter_counts=need_b$Counts,
               batter_cols=need_b$Need,
               batter_calculations=need_b$Calculations,
               runner_by=grp_r,
               runner_counts=need_r$Counts,
               runner_cols=need_r$Need,
               runner_calculations=need_r$Calculations,
               final_cols=final,
               arrange_by=sort_by, arrange_desc=sort_desc)
    } else {
      need_p <- foov |> filter(Group=="Stat") |> pull("Stat") |> get_calc_vars(calcs)
      p_stats(cs, lx, total=eval(parse(text=total_txt)),
              eval(parse(text=filter_txt)),
              pitcher_by = grp,
              pitcher_counts = need_p$Counts,
              pitcher_cols = need_p$Need,
              pitcher_calculations = need_p$Calculations,
              final_cols=final,
              arrange_by=sort_by, arrange_desc=sort_desc)
    }
    out <- list(outi)
    names(out) <- foo$name
    out
  }

  out <- NULL
  for(idx in 2:ncol(f)) out <- c(out, read_idx(idx))
  c(list(header=header), out)
}
