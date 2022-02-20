makedata <- function(d) {
  ## helper function
  tobase <- function(Outcome, B1, B2, B3, B4) {
    B <- c("out", B1, B2, B3, B4)
    to1 <- max(which(!is.na(B))-1)
    max(key$Base[match(Outcome, key$Outcome)], to1) + stringr::str_detect(B[to1+1], "^X")*(-0.5)
  }
  whoout <- function(B2, B3, B4) {
    out <- NA
    B <- c(B2, B3, B4)
    X <- stringr::str_subset(B, "^X")
    if(length(X) > 0) {
      out <- str_replace(X, ".*[^0-9]([0-9]*)$", "\\1") %>%
        as.numeric() %>% replace_na(0)
    }
    out
  }
  ## now process as needed
  out <- d %>%
    mutate(Outcome=if_else(!is.na(B1), B1, str_sub(Play, 1, 1))) %>%
    mutate(Outcome=str_replace(Outcome, "^E.*", "E")) %>%
    rowwise() %>%
    mutate(to=tobase(Outcome, B1, B2, B3, B4),
           out_during=whoout(B2, B3, B4)) %>%
    ungroup() %>%
    mutate(across(c("B2", "B3", "B4"), stringr::str_remove, pattern="^X")) %>%
    mutate(across(c("Balls", "Strikes", "Fouls"), replace_na, 0)) %>%
    mutate(pitch_batter=Balls+Strikes+Fouls+(Outcome!="_")*1L) %>%
    group_by(Pitcher, Inning) %>% mutate(pitch_pitcher=cumsum(pitch_batter),
                                 lastpitch=1:n()==n()) %>%
    ungroup() %>%
    ## move innings over as needed
    group_by(Lineup, Inning) %>% mutate(X=1:n()) %>%
    group_by(Inning) %>% mutate(X=cummax(X) - 1) %>%
    nest() %>% ungroup() %>%
    mutate(X3=map_dbl(data, ~max(.$X)), X4=lag(cumsum(X3), default = 0)) %>%
    unnest(data) %>% mutate(X=X+X4) %>% select(-X3, -X4) %>%
    ungroup()
    # mutate(X=max(Lineup)) %>%
    # group_by(Inning) %>% mutate(X=(1:n()-1) %/% X, X=X-lag(X, default=0)) %>%
    # ungroup() %>% mutate(X=cumsum(X))
  out <- out %>% mutate(whoout=NA, idx=1:n())
  tmp <- out %>% filter(!is.na(out_during))
  for(i in seq_len(nrow(tmp))) {
    if(tmp$out_during[i]==0) {
      out$whoout[tmp$idx[i]] <- 0
    } else {
      foo <- filter(out, Lineup==tmp$out_during[i] & idx >= tmp$idx[i] & Inning==tmp$Inning[i])
      stopifnot(nrow(foo)>0)
      out$whoout[foo$idx[1]] <- tmp$Lineup[i]
    }
  }
  out <- out %>% select(-idx)

  ## error check
  tmp <- out$Outcome[! out$Outcome %in% key$Outcome]
  if(!all(out$Outcome %in% key$Outcome)) {
    warning(paste(paste0("[",tmp,"]"), collapse=" "))
  }
  out
}

## RBI, sort of; who was at bat when run scored, which isn't quite the same thing
# d1 %>% select(Lineup, Outcome, B4) %>% mutate(RBI_by=if_else(Outcome=="HR", Lineup, B4)) %>%
#   filter(!is.na(RBI_by)) %>% select(Lineup=RBI_by) %>%
#   group_by(Lineup) %>% summarize(RBI=n(), .groups="drop")

getBA <- function(H, AB) {
  if_else(!is.na(AB) & (AB > 1), H/AB, as.numeric(NA))
}
getIP <- function(x) {
  a <- x %/% 3
  b <- x %% 3
  a + b/10
}
## BATTER STATS
batter_stats <- function(game, who=c("away", "home"), teamname=TRUE) {
  who <- match.arg(who)
  i <- match(who, c("away", "home"))
  x <- game$lineup[c(1,i+1)]
  d <- game[[who]]
  team <- colnames(x)[2]
  teamname <- if(teamname) team else "Team"
  names(x)[2] <- "Number"
  if(team %in% names(rr)) {
    x <- x %>% left_join(select(rr[[team]], c(Number, Name)), by="Number")
  }
  ff <- function(d) {
    d %>% select(Lineup, Outcome, to) %>% left_join(key, by="Outcome") %>%
      group_by(Lineup) %>% summarize(
        G=NA,
        PA=sum(Outcome!="_"), H=sum(Hit, na.rm=TRUE), AB=sum(!is.na(Hit)), BA=NA,
        R=sum(to==4), Blank=NA,
        K=sum(Outcome=="K"), BB=sum(Outcome=="BB"), HBP=sum(Outcome=="HB"),
        ROE=sum(Outcome=="E"),
        `1B`=sum(Outcome=="1B"), `2B`=sum(Outcome=="2B"), `3B`=sum(Outcome=="3B"), HR=sum(Outcome=="HR"),
        .groups="drop") %>%
      mutate(BA=getBA(H, AB))
  }
  bind_rows(
    x %>% right_join(ff(d), by="Lineup"),
    d %>% mutate(Lineup=NA) %>% ff() %>% mutate(Number=NA, Name=teamname) %>%
      mutate("K/PA"=K/PA, OBPE=(H+BB+HBP+ROE)/PA)
  ) %>% select(any_of(c("Number", "Name","G", "Lineup")), everything())
}

## PITCHER STATS
pitcher_stats <- function(game, who=c("away", "home")) {
  who <- match.arg(who)
  i <- match(who, c("away", "home"))
  x <- game$lineup[c(1,i+1)]
  d <- game[[setdiff(c("home", "away"), who)]]
  team <- colnames(x)[2]
  names(x)[2] <- "Number"
  if(team %in% names(rr)) {
    x <- x %>% left_join(select(rr[[team]], c(Number, Name)), by="Number")
  }
  ff <- function(d) {
    d %>% select(Pitcher, Balls, Strikes, Fouls, Outcome, whoout) %>% left_join(key, by="Outcome") %>%
      mutate(Out=Out+!is.na(whoout)) %>% select(-whoout) %>% ## NEED TO FIX FOR BATTER, NOT PLAYER!!
      mutate(Strikes=Strikes+(Pitch=="Strike"), Balls=Balls+(Pitch=="Ball")) %>%
      mutate(Order=as.integer(as_factor(paste(Pitcher)))) %>%
      group_by(Order, Pitcher) %>% summarize(
        G=NA,IP=NA,
        Outs=sum(Out), BF=sum(Outcome!="_"),
        S=sum(Strikes+Fouls), P=sum(Balls+Strikes+Fouls), SR=S/P,
        H=sum(Hit, na.rm=TRUE), AB=sum(!is.na(Hit)),
        K=sum(Outcome=="K"), BB=sum(Outcome=="BB"), HB=sum(Outcome=="HB"),
        ROE=sum(Outcome=="E"),
        `1B`=sum(Outcome=="1B"), `2B`=sum(Outcome=="2B"), `3B`=sum(Outcome=="3B"), HR=sum(Outcome=="HR"),
        .groups="drop") %>% select(Pitcher, everything()) %>% rename(Number="Pitcher")
  }
  bind_rows(
    d %>% mutate(Pitcher=0) %>% ff() %>% mutate(Number=NA, Name="Team", Order=Inf) %>%
      mutate("BBHB/BF"=(BB+HB)/BF, "Opp. OBP"=(H+BB+HB)/BF),
    x %>% right_join(ff(d), by="Number")
  ) %>% mutate(IP=getIP(Outs)) %>% select(any_of(c("Number", "Name")), everything()) %>%
  arrange(Order) %>% select(-Lineup, -Order)
}

readgame <- function(file) {
  message(file)
  ss <- readxl::excel_sheets(file)
  tmp <- readxl::read_excel(file, "Lineup", n_max = 1, col_names = FALSE, .name_repair="minimal")
  when <- tmp[[1]] %>% str_replace("([ap])$", "\\1m")
  about <- if(ncol(tmp) > 1) tmp[[2]] else "GSBL"
  g1 <- readxl::read_excel(file, "Lineup", skip=1)
  stopifnot(names(g1)[2:3] %in% ss[2:3])
  g1_away <- readxl::read_excel(file, names(g1)[2]) %>% makedata()
  g1_home <- readxl::read_excel(file, names(g1)[3]) %>% makedata()
  tibble(when=when, about=about, lineup=list(g1), away=list(g1_away), home=list(g1_home))
}

readrosters <- function(file) {
  ss <- readxl::excel_sheets(file)
  lapply(ss, readxl::read_excel, path=file) %>% setNames(ss)
}

all_stats <- function(games, team) {
  both_stats <- function(game, team) {
    k <- match(team, colnames(game$lineup))
    if(!is.na(k)) {
      list(batter=batter_stats(game, c("away", "home")[k-1], teamname=FALSE),
           pitcher=pitcher_stats(game, c("away", "home")[k-1]))
    } else {
      NULL
    }
  }
  ss <- lapply(games, both_stats, team)
  b.all <- bind_rows(lapply(ss, function(x) x$batter)) %>%
    group_by(Number, Name) %>% mutate(G=n()) %>% ungroup() %>%
    mutate(Lineup=NA) %>% pivot_longer(-c(Lineup, Number, Name, G)) %>%
    mutate(across("name", as_factor)) %>%
    group_by(Number, Name, G, Lineup, name) %>% summarize(value=sum(value), .groups="drop") %>%
    pivot_wider() %>% mutate(BA=getBA(H,AB)) %>%
    mutate("K/PA"=K/PA, OBPE=(H+BB+HBP+ROE)/PA)
  p.all <- bind_rows(lapply(ss, function(x) x$pitcher)) %>%
    group_by(Number, Name) %>% mutate(G=n()) %>% ungroup() %>%
    pivot_longer(-c(Number, Name, G)) %>%
    mutate(across("name", as_factor)) %>%
    group_by(Number, Name, G, name) %>% summarize(value=sum(value), .groups="drop") %>%
    pivot_wider() %>% mutate(SR=S/P) %>%
    mutate("BBHB/BF"=(BB+HB)/BF, "Opp. OBP"=(H+BB+HB)/BF, IP=getIP(Outs))
  list(Batting=b.all, Pitching=p.all)
}

get_score <- function(game) {
  a1 <- game$away %>% group_by(Inning) %>% summarize(R=sum(to==4), .groups="drop") %>% rename(away="R")
  a2 <- game$home %>% group_by(Inning) %>% summarize(R=sum(to==4), .groups="drop") %>% rename(home="R")
  Final <- c(sum(a1$away), sum(a2$home))
  out <- full_join(a1, a2, by="Inning") %>% as.data.frame()
  rownames(out) <- out$Inning
  out$Inning <- NULL
  colnames(out) <- names(game$lineup[2:3])
  out %>% as.matrix() %>% t() %>% cbind(R=Final)
}

game_stats <- function(game) {
  list(about=setNames(list(game$about, colnames(game$lineup)[2], colnames(game$lineup)[3]),
                      c("about", "away", "home")),
       home=list(Batting=batter_stats(game, "home"),
                 Pitching=pitcher_stats(game, "home")),
       away=list(Batting=batter_stats(game, "away"),
                 Pitching=pitcher_stats(game, "away")))
}

makestatsfile <- function(games, team, filename) {
  add_title <- function(x, title) {
    x <- rbind(names(x), x)
    if(!missing(title)) {
      x <- rbind(NA, x)
      x[1,1] <- title
    }
    x
  }

  game <- games[[length(games)]]

  game_stats <- game_stats(game)
  all_stats <- all_stats(games, team)

  about <- game_stats$about$about
  k_us <- match(team, colnames(game$lineup)[2:3])
  k_them <- setdiff(1:2, k_us)
  team2 <- colnames(game$lineup[1+k_them])
  us <- c("away", "home")[k_us]
  them <- c("away", "home")[k_them]

  us_stats <- game_stats[[us]]
  them_stats <- game_stats[[them]]

  b0 <- bind_rows(us_stats$Batting,
                  them_stats$Batting %>% filter(is.na(Lineup)))
  b1x <- b0[1:(nrow(b0)-2), ]
  b2x <- b0[1:2 + (nrow(b0)-2), ] %>% rename(Team="Name")
  b3x <- all_stats$Batting
  list.batting <- list(b1x, b2x, b3x) %>%
    setNames(c("Roseville Game Stats", "Team Game Stats", "Roseville Season Stats"))

  a1x <- us_stats$Pitching
  a2x <- them_stats$Pitching
  a3x <- all_stats$Pitching
  list.pitching <- list(us=a1x, them=a2x, season=a3x) %>%
    setNames(c(paste(c("Roseville", team2), "Game Stats"), "Roseville Season Stats"))

  out <- list(Batting=list.batting, Pitching=list.pitching)
  if(!missing(filename)) {
    statsToExcel(out, filename)
  }
  invisible(out)
}


######
## output to Excel file

addData <- function(wb, sheet, dat, header, row) {
  writeData(wb, sheet, header, startRow=row, startCol=1)
  addStyle(wb, sheet, createStyle(textDecoration="bold"), cols=1, rows=row)
  writeData(wb, sheet, dat, startRow=row+1, startCol=1)
  isNum <- sapply(dat, is.numeric)
  addStyle(wb, sheet, createStyle(textDecoration="bold"),
           cols=which(!isNum), rows=row+1)
  addStyle(wb, sheet, createStyle(textDecoration="bold", halign="right"),
           cols=which(isNum), rows=row+1)
  if("Name" %in% names(dat)) {
    k <- which(dat$Name=="Team")
    if(length(k)==1) {
      addStyle(wb, sheet, createStyle(textDecoration="bold"),
               cols = seq_len(ncol(dat)), rows = 1 + row + k)
    }
  }
  if("about" %in% names(dat)) {
    k <- which(dat$about=="Season")
    if(length(k)==1) {
      addStyle(wb, sheet, createStyle(textDecoration="bold"),
               cols = seq_len(ncol(dat)), rows = 1 + row + k)
    }
  }
  addStyle(wb, sheet, createStyle(numFmt="0.0"),
           cols=which(names(dat) %in% c("IP")),
           rows=(1 + row + 1:nrow(dat)),
           gridExpand=TRUE, stack=TRUE)
  addStyle(wb, sheet, createStyle(numFmt="0.000"),
           cols=which(names(dat) %in% c("BA", "Opp. OBP", "OBPE") | stringr::str_detect(names(dat), "Sum$")),
           rows=(1 + row + 1:nrow(dat)),
           gridExpand=TRUE, stack=TRUE)
  addStyle(wb, sheet, createStyle(numFmt="0%"),
           cols=which(names(dat) %in% c("SR", "K/PA", "BBHB/BF")),
           rows=(1 + row + 1:nrow(dat)),
           gridExpand=TRUE, stack=TRUE)
  wb
}


addDataList <- function(wb, sheet, x) {
  ## count how many rows needed for each part
  ## for data frame need to count title and header row (so +2)
  nr <- sapply(x, function(x) {if(is.data.frame(x)) nrow(x) + 2 else length(x) })
  ## the +1 is for a space between them
  row <- lag(1 + cumsum(nr + 1), default=1)
  for(i in seq_along(x)) {
    xi <- x[[i]]
    if(is.data.frame(xi)) {
      ## remove Outs from output now that have IP
      xi$Outs <- NULL
      ## set name blank for anything that is all NAs. Specifically should be these:
      ## c("Lineup", "Number", "Name", "G", "BA")
      link <- NULL
      if(!is.null(xi$scorecard_link)) {
        link <- xi$scorecard_link
        xi$scorecard_link <- NULL
      }
      kk <- which(map_lgl(xi, ~all(is.na(.))))
      xi <- mutate(xi, across(any_of(c("HBP", "HB", "1B", '2B', '3B', 'HR','ROE')), function(x) {x[x==0] <- NA; x}))
      names(xi)[kk] <- "" # map_chr(seq_along(kk), ~paste(rep(" ",.), collapse="")) ## if need to be unique
      wb <- addData(wb, sheet, xi, names(x)[i], row=row[i])
      if("when" %in% names(xi)) {
        k <- which(names(xi)=="when")
        names(link) <- xi$when
        for(j in seq_along(link)) {
          linkx <- link[j]
          if(!is.na(linkx)) {
            class(linkx) <- "hyperlink"
            writeData(wb, sheet, linkx, startRow=row[i]+1+j, startCol=k)
          }
        }
      }
    } else {
      for(j in seq_along(xi)) {
        writeData(wb, sheet, xi[[j]], startRow=row[i]+j-1, startCol=1)
        if(j==1) addStyle(wb, sheet, createStyle(textDecoration="bold"), cols=1, rows=row[i]+j-1)
      }
    }
  }
  w0 <- 5
  wx <- bind_rows(tibble(name=c("Lineup", "Number", "Name", "BA", "OBP"), width=8),
                  tibble(name=c("SR", "K/PA"), width=7),
                  tibble(name=c("Opp. OBP", "BBHB/BF"), width=9),
                  tibble(name=c("about", "when", "vs"), width=c(10, 20,15)))
  xdf <- x[sapply(x, is.data.frame)]
  ws <- map(xdf, ~tibble(col=1:ncol(.), name=names(.))) %>% bind_rows() %>%
    left_join(wx, by="name") %>% mutate(width=replace_na(width, w0),
                                        width=if_else(stringr::str_detect(name, "Sum$"), 20, width)) %>%
    group_by(col) %>% summarize(width=max(width), .groups="drop") %>% arrange(col)
  setColWidths(wb, sheet, cols = ws$col, widths = ws$width)
  wb
}

statsToExcel <- function(out, filename) {
  wb <- createWorkbook()
  for(n in names(out)) {
    addWorksheet(wb, n)
    wb <- addDataList(wb, n, out[[n]])
  }
  saveWorkbook(wb, filename, overwrite = TRUE)
}

toGoogle <- function(file, newfile=stringr::str_remove(basename(file), "\\.xlsx$"),
                     dir="Roseville 10U AA 2021 Stats",
                     subdir) {
  if(!missing(subdir)) { dir <- file.path(dir, subdir) }
  if(str_detect(file, "xlsx$")) {
    dimxl <- function(path, sheet) {
      x <- readxl::read_excel(path=path, sheet=sheet, col_names=FALSE)
      dim(x) %>% setNames(c("rows", "cols"))
    }
    out <- drive_put(file, file.path(dir, newfile), type="spreadsheet")
    dims <- tibble(sheet=readxl::excel_sheets(file)) %>%
      mutate(map_dfr(sheet, dimxl, path=file))
    for(i in 1:nrow(dims)) {
      sheet_resize(out, sheet=dims$sheet[i],
                   nrow=dims$rows[i]+1, ncol=dims$cols[i]+1, exact=TRUE)
    }
  } else {
    out <- drive_put(file, file.path(dir, newfile))
  }
  out
}
