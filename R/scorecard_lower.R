lower <- function(game = NULL,
                  blank = is.null(game),
                  n_pitchers = 6,
                  n_innings = c(7, 2),
                  footer_text = "",
                  panel.left = 1,
                  main.width = 6,
                  inningtextsize = 9,
                  leftlabelsize = 9, leftlabelcolor = "gray50",
                  footertextsize = 8, footertextcolor = "gray20") {
  ninnings <- n_innings[1]
  nextra <- n_innings[2]
  ncol <- ninnings + nextra
  if(!blank) {
    pitch_count <- function(x) {
      x <- x |> mutate(Pitcher=fct_inorder(as.character(Pitcher)))
      bind_rows(
        x |> group_by(Inning, Pitcher) |> summarize(N = sum(Pitches), .groups = "drop"),
        x |> group_by(Pitcher) |> summarize(N = sum(Pitches), .groups = "drop") |> mutate(Inning = 0)
      ) |> mutate(Order = as.integer(Pitcher))
    }
    x1 <- pitch_count(game) |> mutate(N = as.character(N))
    x2 <- filter(x1, Inning==0) |> mutate(N = paste0("#", levels(Pitcher)),
                                          Inning = -1, Pitcher = NA)
    ins <- seq_len(max(x1$Inning))
    x3 <- tibble(Inning = c(-1, 0, ins), N = c("P", "Total", as.character(ins)), Order = 0)
    x <- bind_rows(x1, x2, x3) |>
      mutate(x = (Inning + 2) / 24,
             y = 1 - (Order + 0.5) / max(c(7, Order+1)))
    gf <- frameGrob(grid.layout())
    gf <- placeGrob(gf,
                    textGrob(paste(x$N), x = unit(x$x,"npc"), y = unit(x$y, "npc"),
                             just = "right"),
                    row = 1, col = 1)
    return(gf)
  } else {
    heights2 <- c(0.2, rep(1 / n_pitchers, n_pitchers), 0.2)
    #leftcols <- panel.left * c(0.5, 0.4, 0.1)
    leftcols <- panel.left * c(1.1, 0.3, 0.1)
    nleftcols <- length(leftcols)
    npinnings <- ninnings
    pwidth <- 1 / ncol / 2 * npinnings
    widths2 <- c(leftcols, rep(main.width * pwidth / npinnings, npinnings), main.width * (1 - pwidth))
    lx2 <- grid.layout(ncol = length(widths2),
                       nrow = length(heights2),
                       heights = heights2,
                       widths = widths2)
    gf2 <- frameGrob(layout = lx2)
    gf2 <- placeGrob(gf2,
                     textGrob("Pitcher", x = 0, y = unit(3, "pt"), just = c("left", "bottom"),
                              gp=gpar(fontsize = inningtextsize)),
                     row = 1, col = 1)
    gf2 <- placeGrob(gf2,
                     textGrob("Total", x = 0.5, y = unit(3, "pt"), just = "bottom",
                              gp=gpar(fontsize = inningtextsize)),
                     row = 1, col = 2)
    for(i in 1:n_pitchers) {
      gf2 <- placeGrob(gf2,
                       textGrob("#", x = 0.05, just = "left",
                                gp = gpar(fontsize = leftlabelsize, col = leftlabelcolor)),
                       row = i + 1, col = 1)
      box <- rectGrob(gp = gpar(lwd = 0.25))
      gf2 <- placeGrob(gf2, box, row = i + 1, col=1)
      gf2 <- placeGrob(gf2, box, row = i + 1, col=2)
    }
    for(i in 1:npinnings) for(j in 1:n_pitchers) {
      box <- rectGrob(gp = gpar(lwd = 0.25))
      gf2 <- placeGrob(gf2, box, row = j + 1, col = nleftcols + i)
    }
    for(i in 1:npinnings) {
      gf2 <- placeGrob(gf2, row = 1, col = nleftcols + i,
                       grob=textGrob(i, y = unit(3, "pt"), just = "bottom",
                                     gp = gpar(fontsize = inningtextsize)))
    }
    xx1 <- textGrob(footer_text,
                    x = 0, y = 0.9,  just = c("left", "top"),
                    gp = gpar(fontsize = footertextsize, col = footertextcolor))
    gf2 <- placeGrob(gf2, xx1, row = length(heights2), col = 1)
    return(gf2)
  }
}
