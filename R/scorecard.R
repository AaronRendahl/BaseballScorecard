## ## variables needed from Game file:
## Balls, Strikes, Fouls, Play, Out, B1, B2, B3, B4

scorecard <- function(game, file="_scorecard_tmp.pdf",
                      pages = c("one", "two"),
                      team_name = "",
                      logo = NULL,
                      footer_text = "",
                      page_size = c(8.5, 11),
                      margins = c(0.12, 0.2, 0.1, 0.2), # top, right, bottom, left
                      panels = c(0.65, 1.5, 1.5), # top, bottom, left
                      n_players = 12, n_innings = c(7, 2), n_pitchers = 6,
                      when_format = MDY_format,
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

  makeside <- function(game, side, team = NA, logo = NULL,
                       header = c("none", "about", "score"),
                       nrow, name_style = "Name") {
    if(!missing(game)) {
      lineup <- game$lineup |> filter(Side==side)
      plays <- game$plays |> filter(Side==side)
    } else {
      lineup <- NULL
      plays <- NULL
      game <- NULL
    }
    header <- match.arg(header)
    header.grob <- upper(game, team=team, logo=logo,
                         header = header, side = side,
                         name_style = name_style,
                         margin.top = margin.top,
                         ninnings = ninnings,
                         when_format = when_format)
    left.grob <- left(lineup,
                      nrow = nrow)
    boxes.grob <- boxes(plays,
                        nrow = nrow, ncol = ncol,
                        ninnings = ninnings)
    footer.grob <- lower(plays,
                         n_pitchers = n_pitchers,
                         n_innings = c(ninnings, nextra),
                         panel.left = panel.left,
                         main.width = main.width,
                         footer_text = footer_text)
    ## do final layout
    page.grid <- frameGrob(layout=grid.layout(
      nrow=3, ncol=3,
      widths=c(margin.left, panel.left + main.width, margin.right),
      heights=c(margin.top,
                panel.top + main.height + panel.bottom,
                margin.bottom)
    ))
    main.grid <- frameGrob(layout=grid.layout(
      nrow=3, ncol=1,
      heights = c(panel.top, main.height, panel.bottom)
    ))
    middle.grid <- frameGrob(layout=grid.layout(
      ncol=2, nrow=1,
      widths=c(panel.left, main.width)
    ))
    middle.grid <- placeGrob(middle.grid, row=1, col=1, grob=left.grob)
    middle.grid <- placeGrob(middle.grid, row=1, col=2, grob=boxes.grob)
    main.grid <- placeGrob(main.grid, row=1, col=1, grob=header.grob)
    main.grid <- placeGrob(main.grid, row=2, col=1, grob=middle.grid)
    main.grid <- placeGrob(main.grid, row=3, col=1, grob=footer.grob)
    page.grid <- placeGrob(page.grid, row=2, col=2, grob=main.grid)
    page.grid
  }

  if(missing(game)) {
    gf1 <- makeside(header="score", side=1, nrow=n_players[1], team=team_name, logo=logo)
    gf2 <- makeside(header="about", side=2, nrow=n_players[2])
  } else {
    nr <- max(c(11, game$lineup$Lineup))
    sides <- 1:2
    if(team_name==game$teams$Team[2]) sides <- rev(sides)
    gf1 <- makeside(game, side=sides[1], nrow=nr, header="score")
    gf2 <- makeside(game, side=sides[2], nrow=nr, header="about")
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

