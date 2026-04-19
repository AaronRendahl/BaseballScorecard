makebox <- function(ToBase=NA, count=c(0,0), pitchcount=NA, LastPitch=FALSE,
                    out=NA, bybase, play=NA,
                    basesize=0.13,
                    pitchsize=if(blank) 0.12 else 0.10,
                    blank=TRUE, top=FALSE,
                    basedotcolor="gray50",
                    pitchslashcolor="gray30",
                    pitchboxcolor="gray30",
                    pitchcountsize=8,
                    count0=c(0,0)) {
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
  countX_start <- if(any(count0 > 0)) {
    X <- rep(FALSE, 5)
    X[seq_len(count0[1])] <- TRUE
    X[seq_len(count0[2])+3] <- TRUE
    segmentsGrob(x0=xs[X] + pitchsize*0.1,
                 y0=ys[X] - pitchsize*0.5,
                 x1=xs[X] + pitchsize*0.9,
                 y1=ys[X] - pitchsize*0.5)
  } else { NULL }
  countX <- if(any(count[1:2] > 0)) {
    X <- rep(FALSE, 5)
    X[seq_len(count[1])+count0[1]] <- TRUE
    X[seq_len(count[2])+3+count0[2]] <- TRUE
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
    play <- str_replace_all(play, "-", "")
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
                       pitchboxes, countX_start, countX, fouls,
                       play, out, bybase,
                       sh, pitchnum, box))
}


