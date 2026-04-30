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
                      when_format = MDY_format
                      ) {
  blank <- missing(game)

  pages <- match.arg(pages)

  page.width    <- page_size[1]
  page.height   <- page_size[2]

  if(missing(game)) {
    if(length(n_players) == 1) n_players <- rep(n_players, 2)
    gf1 <- makeside(header="score", side=1, nrow=n_players[1], team=team_name, logo=logo,
                    n_innings = n_innings,
                    n_pitchers = n_pitchers,
                    footer_text = footer_text,
                    margins = margins,
                    panels = panels,
                    page_size = page_size,
                    when_format = when_format)
    gf2 <- makeside(header="about", side=2, nrow=n_players[2],
                    n_innings = n_innings,
                    n_pitchers = n_pitchers,
                    footer_text = footer_text,
                    margins = margins,
                    panels = panels,
                    page_size = page_size,
                    when_format = when_format)
  } else {
    nr <- max(c(11, game$lineup$Lineup))
    sides <- 1:2
    if(team_name==game$teams$Team[2]) sides <- rev(sides)
    gf1 <- makeside(game, side=sides[1], nrow=nr, header="score",
                    n_innings = n_innings,
                    footer_text = footer_text,
                    margins = margins,
                    panels = panels,
                    page_size = page_size,
                    when_format = when_format)
    gf2 <- makeside(game, side=sides[2], nrow=nr, header="about",
                    n_innings = n_innings,
                    footer_text = footer_text,
                    margins = margins,
                    panels = panels,
                    page_size = page_size,
                    when_format = when_format)
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

