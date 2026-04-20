left <- function(lineup = NULL,
                 blank = is.null(lineup),
                 nrow = 9,
                 lineupnumsize = 14,
                 numbersize = 12,
                 numbercolor = "gray50") {
  gf <- frameGrob(layout = grid.layout(ncol = 1, nrow = nrow))
  if(blank) {
    for(i in 1:nrow) {
      for(y in c(0.2, 0.5, 0.8)) {
        s3 <- textGrob("#", x = 0.15, y = y, just = c("center", "center"),
                        gp = gpar(fontsize = numbersize, col = numbercolor))
        gf <- placeGrob(gf, row = i, col = 1, grob = s3)
      }
    }
  } else {
    nrow <- max(lineup$Lineup)
    lineup <- lineup |> filter(!is.na(Number)) |> mutate(Name=replace_na(Name, "")) |>
      mutate(NN=sprintf("#%d %s", Number, Name))
    if(!"Sub" %in% names(lineup)) lineup$Sub <- 1
    for(i in 1:nrow(lineup)) {
      s1 <- textGrob(lineup$NN[i], x = 0.15, y = 1-0.3*lineup$Sub[i], just = c("left", "bottom"))
      gf <- placeGrob(gf, row = lineup$Lineup[i], col = 1, grob = s1)
    }
  }
  for(i in 1:nrow) {
    s1 <- segmentsGrob(x0 = 0, x1 = 1, y0 = 1, y1 = 1, gp = gpar(lwd=0.5))
    s2 <- textGrob(i, x = 0, y = 0.85, just = c("left", "top"),
                   gp = gpar(fontsize = lineupnumsize))
    gf <- placeGrob(gf, row = i, col = 1, grob = gTree(children = gList(s1, s2)))
  }
  # gf <- placeGrob(gf, row = nrow, col = 1,
  #                 grob = segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0,
  #                                     gp = gpar(lwd = 0.5)))
  gf
}
