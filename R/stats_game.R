add_stats <- function(gs, ...) {
  gs$stats <- map(seq_len(nrow(gs)), \(i) game_stats(gs[i,], ...))
  gs
}

br_stats <- function(counts, lx, ..., total,
                     batter_by, batter_counts, batter_cols, batter_calculations,
                     runner_by, runner_counts, runner_cols, runner_calculations,
                     final_cols, arrange_by, arrange_desc=TRUE) {
  b1 <- counts |> rename(Number=Batter) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(batter_counts, batter_calculations, by=batter_by, total=total) |>
    select(all_of(c(batter_by, batter_cols)))
  r1 <- counts |> rename(Number=Runner) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(runner_counts, runner_calculations, by=runner_by, total=total) |>
    select(all_of(c(runner_by, runner_cols)))
  descif <- if(arrange_desc) desc else identity
  blank <- which(final_cols=="Blank" | final_cols=="" | is.na(final_cols))
  if(length(blank)>0) {
    names(final_cols) <- ""
    names(final_cols)[blank] <- paste0("Blank", blank)
    final_cols[blank] <- "Blank"
  }
  full_join(b1, r1, by=intersect(c(runner_by, runner_cols), c(batter_by, batter_cols))) |>
    arrange(descif(.data[[arrange_by]])) |>
    mutate(Blank=NA) |>
    select(all_of(final_cols))
}

p_stats <- function(counts, lx, ..., total,
                    pitcher_by, pitcher_counts, pitcher_cols, pitcher_calculations,
                    final_cols=pitcher_cols, arrange_by, arrange_desc=TRUE) {
  arrangeif <- if(is.null(arrange_by)) \(x, ...) x else arrange
  descif <- if(arrange_desc) desc else identity
  blank <- which(final_cols=="Blank" | final_cols=="" | is.na(final_cols))
  if(length(blank)>0) {
    names(final_cols) <- ""
    names(final_cols)[blank] <- paste0("Blank", blank)
    final_cols[blank] <- "Blank"
  }
  lx <- lx |> mutate(Side = 3 - Side)
  counts |> rename(Number=Pitcher) |> left_join(lx, by=c('code', 'Side', 'Number')) |>
    filter(...) |>
    calc_stats(pitcher_counts, pitcher_calculations, by=pitcher_by, total=total) |>
    select(all_of(c(pitcher_by, pitcher_cols))) |>
    arrangeif(descif(.data[[arrange_by]])) |>
    mutate(Blank=NA) |>
    select(all_of(final_cols))
}

game_stats <- function(g, team, stats_file, calculations, roles) {
  stopifnot(is_tibble(g) & nrow(g)==1)
  teams <- g |> select(code, game) |> unnest(game) |> pull(Team)
  stopifnot(team %in% teams)
  vs <- setdiff(teams, team)

  header <- sprintf("%s, %s", paste(teams, collapse=" @ "), MDY_format(g$when))

  if(!is.null(g$scorecard_link)) {
    link <- g$scorecard_link
    names(link) <- "LINK TO SCORECARD"
    class(link) <- "hyperlink"
    header <- list(header, link)
  }

  out <- make_stats_from_file(g, header, stats_file, calculations, roles, team, vs)
  out
}
