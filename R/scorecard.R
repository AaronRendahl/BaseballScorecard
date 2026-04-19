## ## variables needed from Game file:
## Balls, Strikes, Fouls, Play, Out, B1, B2, B3, B4
##
## ## also provide functions that add these
## ToBase, Pitches
##
## ## computed in this file:
## X: if extra column needed for an inning
## PitchesSoFar: pitches so far by this pitcher
## LastPitch: TRUE/FALSE if is last batter for this pitcher

scorecard <- function(game, file="_scorecard_tmp.pdf",
                      pages = c("one", "two"),
                      team_name = "",
                      footer_text = "",
                      page_size = c(8.5, 11),
                      margins = c(0.12, 0.2, 0.1, 0.2), # top, right, bottom, left
                      panels = c(0.65, 1.5, 1), # top, bottom, left
                      n_players = 12, n_innings = c(7, 2), n_pitchers = 6,
                      when_format=MDY_format,
                      start_count=c(0,0)
                      ) {
  blank <- missing(game)

  pages <- match.arg(pages)

  if(length(n_players) == 1) n_players <- rep(n_players, 2)
  if(length(n_innings) == 1) n_innings <- c(n_innings, 2)

  ninnings <- n_innings[1]
  nextra   <- n_innings[2]

  page.width    <- page_size[1]
  page.height   <- page_size[2]

  margin.top    <- margins[1]
  margin.right  <- margins[2]
  margin.bottom <- margins[3]
  margin.left   <- margins[4]

  panel.top     <- panels[1]
  panel.bottom  <- panels[2]
  panel.left    <- panels[3]

  ncol <- ninnings + nextra
  main.width  <- page.width  - (margin.left + margin.right + panel.left)
  main.height <- page.height - (margin.top + margin.bottom + panel.top + panel.bottom)

  inningtextsize <-  9

  boxes <- function(d, nrow) {
    pitchsize <- if(blank) 0.12 else 0.10
    basesize <- 0.13 - (nrow-12)*0.005
    gf <- NULL
    gf <- frameGrob(layout=grid.layout(nrow = nrow, ncol=ncol))
    if(missing(d)) {
      inning_list=tibble(Inning=1:ninnings, X=1:ninnings)
      onebox <- makebox(basesize=basesize, pitchsize=pitchsize,
                        blank=TRUE)
      for(i in seq_len(nrow)) for (j in seq_len(ncol)) {
        gf <- placeGrob(gf, onebox, row=i, col=j)
      }
    } else {
      # make the graphics...
      get_X <- function(Lineup, Inning) {
        tibble(Lineup=Lineup, Inning=Inning) |>
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

  mainbox <- function(plays, lineup, nrow=11) {
    gf <- frameGrob(layout=grid.layout(ncol=2, nrow=1,
                                        widths=c(panel.left, main.width)))
    gf <- placeGrob(gf, row=1, col=1, grob=left(lineup, nrow=nrow))
    gf <- placeGrob(gf, row=1, col=2, grob=boxes(plays, nrow=nrow))
    gf
  }

  makeside <- function(game, side, team=NA, header, nrow, name_style="Name") {
    if(!missing(game)) {
      header.grob <- upper(game, side,
                           header = header, name_style = name_style,
                           margin.top = margin.top,
                           ninnings = ninnings,
                           inningtextsize = inningtextsize,
                           when_format = when_format)
      main.grob <- mainbox(game$plays |> filter(Side==side),
                           game$lineup |> filter(Side==side), nrow=nrow)
      footer.grob <- lower(game$plays |> filter(Side==side),
                           n_pitchers = n_pitchers,
                           n_innings = c(ninnings, nextra),
                           panel.left = panel.left,
                           main.width = main.width,
                           inningtextsize = inningtextsize,
                           footer_text = footer_text)
    } else {
      header.grob <- upper(header = header, side = side,
                           team = team, name_style = name_style,
                           margin.top = margin.top,
                           ninnings = ninnings,
                           inningtextsize = inningtextsize,
                           when_format = when_format)
      main.grob <- mainbox(nrow=nrow)
      footer.grob <- lower(n_pitchers = n_pitchers,
                           n_innings = c(ninnings, nextra),
                           panel.left = panel.left,
                           main.width = main.width,
                           inningtextsize = inningtextsize,
                           footer_text = footer_text)
    }

    ## do final layout
    page.grid <- frameGrob(layout=grid.layout(nrow=3, ncol=3,
                             widths=c(margin.left, panel.left + main.width, margin.right),
                             heights=c(margin.top,
                                       panel.top + main.height + panel.bottom,
                                       margin.bottom)))
    main.grid <- frameGrob(layout=grid.layout(nrow=3, ncol=1,
                             heights = c(panel.top, main.height, panel.bottom)))
    main.grid <- placeGrob(main.grid, row=1, col=1, grob=header.grob)
    main.grid <- placeGrob(main.grid, row=2, col=1, grob=main.grob)
    main.grid <- placeGrob(main.grid, row=3, col=1, grob=footer.grob)
    page.grid <- placeGrob(page.grid, row=2, col=2, grob=main.grid)
    page.grid
  }

  if(missing(game)) {
    gf1 <- makeside(header="score", side=1, nrow=n_players[1], team=team_name)
    gf2 <- makeside(header="about", side=2, nrow=n_players[2])
  } else {
    nr <- max(c(11, game$lineup$Lineup))
    sides <- 1:2
    if(team_name==game$teams$Team[2]) sides <- rev(sides)
    gf1 <- makeside(game, sides[1], nrow=nr, header="score")
    gf2 <- makeside(game, sides[2], nrow=nr, header="about")
  }

  on.exit(dev.off())
  dir <- dirname(file)
  if(!dir.exists(dir)) { dir.create(dir) }
  if(pages=="one") {
    pdf(file, width=page.width*2, height=page.height)
    gf <- frameGrob(layout=grid.layout(ncol=2))
    gf <- placeGrob(gf, row=1, col=1, grob=gf1)
    gf <- placeGrob(gf, row=1, col=2, grob=gf2)
    grid.draw(gf)
  } else if(pages=="two") {
    pdf(file, width=page.width, height=page.height)
    foo <- gridExtra::marrangeGrob(list(gf1, gf2), nrow=1, ncol=1, top=NULL)
    grid.draw(foo)
  }
}

