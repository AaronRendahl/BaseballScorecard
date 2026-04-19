upper <- function(game, side,
                  name_style="Name",
                  team = if(!missing(game)) game$teams[[name_style]][side] else NA,
                  logo = if(!missing(game)) game$teams$Logo[[side]] else NULL,
                  header = c("none", "about", "score"),
                  margin.top = 0,
                  teamnamesize = 14,
                  headertextsize = 10,
                  ninnings = 7,
                  inningtextsize = 9,
                  when_format = MDY_format) {
  header <- match.arg(header)
  dt <- if(header == "about") {
    if(missing(game)) {
      textGrob("Game Date/Time:",
               x = 0.5,
               y = unit(1, "npc") - unit(margin.top, "inches"),
               just = c("left", "top"),
               gp = gpar(fontsize = headertextsize))
    } else {
      textGrob(sprintf("%s: %s", game$about, when_format(game$when)),
               x = 0.95,
               y = 0.55,
               just = c("right", "center"),
               gp = gpar(fontsize=14))
    }
  } else if(header == "score"){
    if(missing(game)) nextra <- 1 else nextra <- 0
    np1 <- ninnings + nextra + 1
    xw <- 0.25 / 7 * np1
    xx <- (1 - xw) + xw / np1 * (1:np1) - xw / (2 * np1)
    yy <- c(0.675, 0.425)

    inning_headers <- c( 1:ninnings, rep("", nextra), "R")
    a2 <- textGrob(inning_headers, x = xx, y = 0.85, just = "bottom",
                   gp = gpar(fontsize = inningtextsize))
    xs <- (1 - xw) + xw / np1 * (0:np1)
    a3 <- segmentsGrob(x0 = xs, x1 = xs, y0 = 0.3, y1 = 0.8, gp = gpar(lwd = 0.25))
    ys <- c(0.3, 0.55, 0.8)
    a4 <- segmentsGrob(x0 = 0.45, x1 = 1, y0 = ys, y1 = ys, gp = gpar(lwd = 0.25))
    out <- gList(a2, a3, a4)
    if(!missing(game)) {
      score <- get_score(game, "Short") |> as.data.frame()
      names(score)[ncol(score)] <- ninnings + 1
      teams <- rownames(score)
      score <- score |> mutate(team=1:2) |> pivot_longer(-team, names_to="inning") |>
        mutate(inning = as.integer(inning)) |>
        mutate(xx = xx[inning], yy=yy[team]) |>
        mutate(value = replace_na(as.character(value), "-"))
      out2 <- textGrob(score$value, x = xx[score$inning], y = yy[score$team],
                       gp = gpar(fontsize = inningtextsize))
      out3 <- textGrob(teams, x = unit(1 - xw, "npc") - unit(6, "pt"), y = yy, just="right",
                       gp = gpar(fontsize = inningtextsize))
      out <- gList(out, out2, out3)
    }
    out
  }
  name <- if(!is.na(team)) {
    haslogo <- !is.null(logo)
    vs <- case_when(header == "score" ~ "",
                    side == 2 ~ "@ ",
                    side == 1 ~ "v. ",
                    TRUE ~ "")
    rr <- if(haslogo) dim(logo)[2]/dim(logo)[1] else 1
    teamtext <- textGrob(paste0(vs, team),
                         x = unit(0.8 * rr, "snpc"),
                         y = 0.55,
                         just=c("left", "center"),
                         gp = gpar(fontsize = teamnamesize))
    if(haslogo) {
      teamlogo <- rasterGrob(logo, x = 0, y = 1, height = 0.8,
                             just = c("left", "top"))
      gTree(children = gList(teamlogo, teamtext))
    } else {
      teamtext
    }
  } else if(is.na(team)) {
    textGrob("@\nvs.",
             x = 0,
             y = unit(1, "npc") - unit(margin.top, "inches"),
             just = c("left", "top"),
             gp = gpar(fontsize = headertextsize))
  }
  gTree(children = gList(dt, name))
}
