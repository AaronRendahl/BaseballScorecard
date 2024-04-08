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
                      team_name = "", logos = list(),
                      footer_text = "",
                      page_size = c(8.5, 11),
                      margins = c(0.1, 0.2, 0.12, 0.2), # bottom, left, top, right
                      panels = c(1.5, 1, 0.65), # bottom, left, top
                      n_players = 12, n_innings = c(7, 2), n_pitchers = 6,
                      when_format=MDY_format,
                      toBase_fun=add_ToBase,
                      Pitches_fun=add_Pitches
                      ) {
  blank <- missing(game)

  if(!blank) {
    stopifnot(is_tibble(game) && nrow(game)==1)
    game <- as.list(game)
    game$teams <- game$game[[1]]$Team
    game$plays <- game$game[[1]] |> select(Side, Plays) |> unnest(Plays)
    game$lineup <- game$game[[1]] |> select(Side, Lineup) |> unnest(Lineup)
    game$game <- NULL
    ## ToBase: which base they got to (use 0.5 to specify out between; eg, 2.5 if out between 2 and 3)
    ## Pitches: total pitches during at-bat
    game$plays <- game$plays |> toBase_fun() |> Pitches_fun()
  }

  pages <- match.arg(pages)

  if(length(n_players) == 1) n_players <- rep(n_players, 2)
  if(length(n_innings) == 1) n_innings <- c(n_innings, 2)

  ninnings <- n_innings[1]
  nextra   <- n_innings[2]

  page.width    <- page_size[1]
  page.height   <- page_size[2]

  margin.bottom <- margins[1]
  margin.left   <- margins[2]
  margin.top    <- margins[3]
  margin.right  <- margins[4]

  panel.bottom  <- panels[1]
  panel.left    <- panels[2]
  panel.top     <- panels[3]

  ncol <- ninnings + nextra
  main.width  <- page.width  - (margin.left + margin.right + panel.left)
  main.height <- page.height - (margin.top + margin.bottom + panel.top + panel.bottom)

  pitchsize       <- if(missing(game)) 0.12 else 0.10
  pitchboxcolor   <- "gray30"
  pitchslashcolor <- "gray30"


  teamnamesize   <- 14
  footertextsize <-  8; footertextcolor <- "gray20"
  headertextsize <- 10
  inningtextsize <-  9
  lineupnumsize  <- 14
  numbersize     <- 12; numbercolor     <- "gray50"
  leftlabelsize  <-  9; leftlabelcolor  <- "gray50"
  pitchcountsize <-  8

  basedotcolor <- "gray50"

  makebox <- function(ToBase=NA, count=c(0,0), pitchcount=NA, LastPitch=FALSE,
                      out=NA, bybase, play=NA, basesize, top=FALSE) {
    basesize  <- unit(basesize,  "inches")
    pitchsize <- unit(pitchsize, "inches")
    basex <- unit(0.55, "npc")
    basey <- unit(if(blank) 0.5 else 0.6, "npc")
    xs <- (c(1, 2, 3, 1, 2) - 1) * pitchsize
    ys <- unit(1, "npc") - (c(1, 1, 1, 2, 2) - 1) * pitchsize
    pitchboxes <- do.call(gList, mapply(function(x, y) {
      rectGrob(x = x, y = y,
               width  = pitchsize,
               height = pitchsize,
               just = c("left","top"),
               gp = gpar(fill = "white", lwd = 0.25, col = pitchboxcolor))
    }, xs, ys, SIMPLIFY = FALSE))
    fouls <- if(length(count) == 3 && count[3] > 0) {
      xx <- pitchsize * (seq_len(count[3]) - 0.5)
      yy <- unit(1, "npc") - pitchsize * 2.67
      pointsGrob(x = xx, y=rep(yy, count[3]), pch = 19,
                 size = unit(2, "pt"))
    } else { NULL }
    countX <- if(any(count[1:2] > 0)) {
      X <- rep(FALSE, 5)
      X[seq_len(count[1])  ] <- TRUE
      X[seq_len(count[2])+3] <- TRUE
      segmentsGrob(x0=xs[X],
                   y0=ys[X] - pitchsize,
                   x1=xs[X] + pitchsize,
                   y1=ys[X])
    } else { NULL }
    box <- rectGrob(gp = gpar(lwd = 0.5))
    if(top) {
      box <- gList(box, segmentsGrob(x0 = 0, x1 = 1, y0 = 1, y1 = 1, gp=gpar(lwd = 2)))
    }
    xs <- basex + c(0, 1, 0, -1, 0) * basesize
    ys <- unit(1, "npc") - basey + c(-1, 0, 1, 0, -1) * basesize
    bases <- if(!is.na(ToBase)) { NULL } else {
      pointsGrob(x = xs[1:4],
                 y = ys[1:4],
                 pch = 19, size = unit(0.25, "pt"), gp = gpar(col=basedotcolor))
    }
    lines <- if(is.na(ToBase)) { NULL } else {
      ex <- ToBase - floor(ToBase + 0.01)  ## add 0.01, caution about floating point
      to <- floor(ToBase + 0.01)
      xx <- xs[seq_len(ToBase + 1)]
      yy <- ys[seq_len(ToBase + 1)]
      outbar <- NULL
      if(ex > 0.1) {
        xs2 <- c(0.25, -0.75, -0.25)[ToBase] * basesize + basex
        ys2 <- c(0.75,  0.25, -0.75)[ToBase] * basesize + unit(1, "npc") - basey
        xa <- xs2 + c(-1,  1,  1)[ToBase] * c(-1, 1) * 0.2 * basesize
        ya <- ys2 + c(-1, -1,  1)[ToBase] * c(-1, 1) * 0.2 * basesize
        outbar <- linesGrob(x = xa, y = ya, gp = gpar(lwd = 2))
        xx <- unit.c(xx, xs2)
        yy <- unit.c(yy, ys2)
      }
      gList(linesGrob(x = xx, y = yy, gp = gpar(col="black")),
            outbar)
    }
    fill <- if(!is.na(ToBase) && ToBase == 4) {
      polygonGrob(x = xs, y = ys, gp = gpar(fill = "gray50"))
    } else { NULL }
    # slash for pitches atbat/total
    sh <- segmentsGrob(x0 = unit(0.5, "npc") + pitchsize * 1.5 - pitchsize * 3 / 16 * 1.5,
                       x1 = unit(0.5, "npc") + pitchsize * 1.5 + pitchsize * 3 / 16 * 1.5,
                       y0 = unit(1.0, "npc") - pitchsize * 1.5, # how tall is slash
                       y1 = unit(1.0, "npc"),
                       gp = gpar(col = pitchslashcolor, lwd = 0.25))
    pitchnum <- if(any(is.na(pitchcount))) { NULL } else {
      a2x <- (unit(1, "npc") + unit(0.5, "npc") + pitchsize*1.5)/2
      a2y <-  unit(1, "npc") - pitchsize*3/4
      a1 <- textGrob(pitchcount[1],
                     x = unit(0.5, "npc") + pitchsize * (1.5 - 3 / 16 * 1.5),
                     y = a2y,
                     gp = gpar(fontsize=pitchcountsize), just = "right")
      a2 <- textGrob(pitchcount[2],
                     x = a2x, y = a2y,
                     gp = gpar(fontsize = pitchcountsize))
      bb <- if(!LastPitch) { NULL } else {
        rectGrob(x = a2x, y = a2y,
                 width = unit(1.5, "grobwidth", a2), height = unit(1.5, "grobheight", a2))
      }
      gList(a1, a2, bb)
    }
    out <- if(!is.na(out)) {
      xx <- yy <- unit(1, "strheight", data = paste(out))
      gList(textGrob(out, x = xx, y = yy),
            circleGrob(x = xx, y = yy, r = xx * 0.85))
    } else { NULL }
    play <- if(is.na(play)) { NULL } else {
      play <- str_replace(play, "-", "")
      if(ToBase == 0) {
        textGrob(play, basex, unit(1, "npc") - basey)
      } else {
        textGrob(play,
                 # ## for in action spot
                 # x=basex, y=unit(0.1,"npc"),
                 ## for above B1 text
                 x = unit(1, "npc"), y = unit(1, "npc") - basey, just = "right",
                 gp = gpar(fontsize=8))
      }
    }
    bybase <- if(missing(bybase)) { NULL } else {
      bybase[bybase == "?"] <- ""
      bybase <- str_remove(bybase, "-.*")
      textGrobNA <- function(label, x, y, ...) {
        if(is.na(label)) return(NULL)
        textGrob(label, x, y, ...)
      }
      yy <- unit(1, "strheight", data=bybase[1])
      xx <- unit(1,  "strwidth", data=bybase[1])/2 + yy/3
      gList(textGrobNA(bybase[1], x = unit(1,"npc") - xx,
                       y = yy),
                       # ## to move up above action
                       # y = yy + unit(0.2, "npc")),
            textGrobNA(bybase[2], x = basex + basesize,
                       y=unit(1,"npc") - basey + basesize, gp = gpar(fontsize=8)),
            textGrobNA(bybase[3], x = basex - basesize,
                       y=unit(1,"npc") - basey + basesize, gp = gpar(fontsize=8)),
            textGrobNA(bybase[4], x = basex - basesize,
                       y=unit(1,"npc") - basey - basesize, gp = gpar(fontsize=8))
      )
    }
    action <- if(!blank) { NULL } else {
      segmentsGrob(x0 = unit(0.25, "npc"),
                   x1 = unit(1.00, "npc"),
                   y0 = unit(0.20, "npc"),
                   y1 = unit(0.20, "npc"),
                   gp = gpar(col=pitchslashcolor, lwd=0.25))
    }
    gTree(children=gList(bases, lines, fill, action,
                         pitchboxes, countX, fouls,
                         play, out, bybase,
                         sh, pitchnum, box))
  }

  left <- function(lineup, nrow) {
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
        gf <- placeGrob(gf, row = i, col = 1, grob = ss)
      }
    }
    for(i in 1:players) {
      s1 <- segmentsGrob(x0 = 0, x1 = 1, y0 = 1, y1 = 1, gp = gpar(lwd=0.5))
      s2 <- textGrob(i, x = 0.1, y = 0.85, just = c("center", "top"),
                     gp = gpar(fontsize = lineupnumsize))
      s3 <- textGrob("#", x = 0.1, y = 0.1, just = c("center", "bottom"),
                     gp = gpar(fontsize = numbersize, col = numbercolor))
      ss <- gTree(children = gList(s1, s2, s3))
      gf <- placeGrob(gf, row = i, col = 1, grob = ss)
    }
    gf <- placeGrob(gf, row = players, col = 1,
                    grob = segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                      gp = gpar(lwd = 0.5)))
    gf
  }

  lower <- function(game) {
    if(!missing(game)) {
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
        mutate(x = (Inning + 2) / 16,
               y = 1 - (Order + 0.5) / 7)
      gf <- frameGrob(grid.layout())
      gf <- placeGrob(gf,
                      textGrob(paste(x$N), x = unit(x$x,"npc"), y = unit(x$y, "npc"),
                               just = "right"),
                      row = 1, col = 1)
      return(gf)
    } else {
      heights2 <- c(0.2, rep(1 / n_pitchers, n_pitchers), 0.2)
      leftcols <- panel.left * c(0.5, 0.4, 0.1)
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

  upper <- function(game, side,
                    team = if(!missing(game)) game$teams[side] else NA,
                    header = c("none", "about", "score")) {
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
      np1 <- ninnings + 1
      xw <- 0.25 / 7 * np1

      xx <- (1 - xw) + xw / np1 * (1:np1) - xw / (2 * np1)
      yy <- c(0.675, 0.425)
      a2 <- textGrob(c( 1:ninnings, "R"), x = xx, y = 0.85, just = "bottom",
                     gp = gpar(fontsize = inningtextsize))
      xs <- (1 - xw) + xw / np1 * (0:np1)
      a3 <- segmentsGrob(x0 = xs, x1 = xs, y0 = 0.3, y1 = 0.8, gp = gpar(lwd = 0.25))
      ys <- c(0.3, 0.55, 0.8)
      a4 <- segmentsGrob(x0 = 0.65, x1 = 1, y0 = ys, y1 = ys, gp = gpar(lwd = 0.25))
      out <- gList(a2, a3, a4)
      if(!missing(game)) {
        score <- get_score(game) |> as.data.frame()
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
      haslogo <- team %in% names(logos)
      vs <- case_when(header == "score" ~ "",
                      side == 2 ~ "@ ",
                      side == 1 ~ "v. ",
                      TRUE ~ "")
      teamtext <- textGrob(paste0(vs, team),
                           x = unit(1 * haslogo, "snpc"),
                           y = 0.55,
                           just=c("left", "center"),
                           gp = gpar(fontsize = teamnamesize))
      if(haslogo) {
        teamlogo <- rasterGrob(logos[[team]], x = 0, y = 1, height = 0.9,
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

  boxes <- function(d, nrow) {
    basesize <- 0.13 - (nrow-12)*0.005
    gf <- NULL
    gf <- frameGrob(layout=grid.layout(nrow = nrow, ncol=ncol))
    if(missing(d)) {
      inning_list=tibble(Inning=1:ninnings, X=1:ninnings)
      onebox <- makebox(basesize=basesize)
      for(i in seq_len(nrow)) for (j in seq_len(ncol)) {
        gf <- placeGrob(gf, onebox, row=i, col=j)
      }
    } else {
      # make the graphics...
      get_X <- function(Lineup, Inning) {
        tibble(Lineup=Lineup, Inning=Inning) |>
          group_by(Lineup, Inning) |> mutate(X=1:n()) |>
          group_by(Inning) |> mutate(X=cummax(X) - 1) |>
          nest() |> ungroup() |>
          mutate(X3=purrr::map_dbl(data, ~max(.$X)), X4=lag(cumsum(X3), default = 0)) |>
          unnest(data) |> mutate(X=X+X4) |> select(-X3, -X4) |>
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
                  out=Out, basesize=basesize, top=top)
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

  makeside <- function(game, side, team=NA, header, nrow) {
    if(!missing(game)) {
      header.grob <- upper(game, side, header=header)
      main.grob <- mainbox(game$plays |> filter(Side==side),
                           game$lineup |> filter(Side==side), nrow=nrow)
      footer.grob <- lower(game$plays |> filter(Side==side))
    } else {
      header.grob <- upper(header=header, team=team)
      main.grob <- mainbox(nrow=nrow)
      footer.grob <- lower()
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
    gf1 <- makeside(header="score", nrow=n_players[1], team=team_name)
    gf2 <- makeside(header="about", nrow=n_players[2])
  } else {
    nr <- max(c(11, game$lineup$Lineup))
    sides <- 1:2
    if(team_name==game$teams[2]) sides <- rev(sides)
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

