boxes <- function(d, nrow=9, ncol=8,
                  ninnings=7,
                  inningtextsize=9) {
  blank <- missing(d)
  pitchsize <- if(blank) 0.12 else 0.10
  basesize <- 0.13 - (nrow-12)*0.005
  gf <- NULL
  gf <- frameGrob(layout=grid.layout(nrow = nrow, ncol=ncol))
  if(missing(d)) {
    inning_list=data.frame(Inning=1:ninnings, X=1:ninnings)
    onebox <- makebox(basesize=basesize, pitchsize=pitchsize,
                      blank=TRUE)
    for(i in seq_len(nrow)) for (j in seq_len(ncol)) {
      gf <- placeGrob(gf, onebox, row=i, col=j)
    }
  } else {
    # make the graphics...
    get_X <- function(Lineup, Inning) {
      data.frame(Lineup=Lineup, Inning=Inning) |>
        ## this part tries to notice if anyone is out of order
        mutate(diff=(Lineup-lag(Lineup)), gap=diff%%max(Lineup),
               .by="Inning") |>
        mutate(Xorder=cumsum(!is.na(diff) & (gap>2 & diff<0))) |>
        select(-diff, -gap) |>
        ## this part looks for if they batted around
        group_by(Lineup, Inning) |> mutate(X=1:n()) |>
        group_by(Inning) |> mutate(X=cummax(X) - 1) |>
        nest() |> ungroup() |>
        mutate(X3=purrr::map_dbl(data, ~max(.$X)), X4=lag(cumsum(X3), default = 0)) |>
        unnest(data) |> mutate(X=X+X4+Xorder) |> select(-X3, -X4, -Xorder) |>
        ungroup() |> pull(X)
    }
    d <- d |> mutate(X=get_X(Lineup, Inning)) |>
      group_by(Pitcher, Inning) |> mutate(PitchesSoFar=cumsum(Pitches), LastPitch=1:n()==n()) |>
      group_by(Inning) |> mutate(top=(1:n()==1)) |>
      rowwise() |>
      mutate(box=list(
        makebox(ToBase=ToBase, count=c(Balls, Strikes, Fouls),
                pitchcount=c(Pitches, PitchesSoFar), LastPitch=LastPitch,
                play=Play, bybase=c(B1, B2, B3, B4),
                out=Out, basesize=basesize, pitchsize=pitchsize, blank=blank, top=top)
      )) |> ungroup()
    inning_list <- d |> group_by(Inning) |> summarize(X=min(Inning+X), .groups="drop")
    for(idx in seq_len(nrow(d))) {
      gf <- placeGrob(gf, d$box[[idx]], row=d$Lineup[idx], col=d$Inning[idx]+d$X[idx])
    }
  }
  ## add inning numbers
  for(idx in seq_len(nrow(inning_list))) {
    gf <- placeGrob(gf, row=1, col=inning_list$X[idx],
                    textGrob(inning_list$Inning[idx],
                             x=0.5, y=unit(1,"npc") + unit(3, "pt"),
                             just = c("center", "bottom"),
                             gp=gpar(fontsize=inningtextsize)))
  }
  gf
}
