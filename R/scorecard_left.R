left <- function(lineup, nrow,
                 lineupnumsize = 14,
                 numbersize = 12,
                 numbercolor = "gray50") {
  gf <- frameGrob(layout = grid.layout(ncol = 1, nrow = nrow))
  if(missing(lineup)) {
    players <- nrow
  } else {
    lineup <- lineup |> filter(!is.na(Number)) |> mutate(Name=replace_na(Name, ""))
    players <- max(lineup$Lineup)
    for(i in 1:nrow(lineup)) {
      s1 <- textGrob(lineup$Number[i], x = 0.25, y = 0.1, just = c("left", "bottom"))
      if(!("Name" %in% names(lineup))) {s2 <- NULL} else {
        s2 <- textGrob(lineup[["Name"]][i], x = 0.25, y = 0.85, just=c("left", "top"))
      }
      ss <- gTree(children = gList(s1, s2))
      gf <- placeGrob(gf, row = lineup$Lineup[i], col = 1, grob = ss)
    }
  }
  for(i in 1:players) {
    s1 <- segmentsGrob(x0 = 0, x1 = 1, y0 = 1, y1 = 1, gp = gpar(lwd=0.5))
    s2 <- textGrob(i, x = 0, y = 0.85, just = c("left", "top"),
                   gp = gpar(fontsize = lineupnumsize))
    s3a <- textGrob("#", x = 0.15, y = 0.8, just = c("center", "center"),
                    gp = gpar(fontsize = numbersize, col = numbercolor))
    s3b <- textGrob("#", x = 0.15, y = 0.5, just = c("center", "center"),
                    gp = gpar(fontsize = numbersize, col = numbercolor))
    s3c <- textGrob("#", x = 0.15, y = 0.2, just = c("center", "center"),
                    gp = gpar(fontsize = numbersize, col = numbercolor))
    ss <- gTree(children = gList(s1, s2, s3a, s3b, s3c))
    ## FIX 1
    gf <- placeGrob(gf, row = i, col = 1, grob = ss)
  }
  gf <- placeGrob(gf, row = players, col = 1,
                  grob = segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                      gp = gpar(lwd = 0.5)))
  gf
}
