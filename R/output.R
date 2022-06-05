######
## output to Excel file
addData <- function(wb, sheet, dat, header, row, numFmt=NA) {
  about <- dat |> select(starts_with("."))
  dat <- dat |> select(!starts_with("."))
  ## put header in first desired row and bold it
  writeData(wb, sheet, header, startRow=row, startCol=1)
  addStyle(wb, sheet, createStyle(textDecoration="bold"), cols=1, rows=row)
  ## add data starting on the next row
  writeData(wb, sheet, dat, startRow=row+1, startCol=1)
  ## make header row bold, with text justified left and numeric justified right
  isNum <- sapply(dat, is.numeric)
  addStyle(wb, sheet, createStyle(textDecoration="bold"),
           cols=which(!isNum), rows=row+1)
  addStyle(wb, sheet, createStyle(textDecoration="bold", halign="right"),
           cols=which(isNum), rows=row+1)
  ## add textDecoration
  if(!is.null(about$.textDecoration) && any(!is.na(about$.textDecoration))) {
    for(k in which(!is.na(about$.textDecoration))) {
      addStyle(wb, sheet, createStyle(textDecoration=about$.textDecoration[k]),
               cols = seq_len(ncol(dat)), rows = row + 1 + k)
    }
  }
  ## add hyperlinks
  h <- about |> select(starts_with(".hyperlink"))
  if(ncol(h)>0) {
    names(h) <- str_remove(names(h), fixed(".hyperlink."))
    nh <- intersect(names(h), names(dat))
    for(n in nh) {
      col <- which(names(dat)==n)
      for(rowi in seq_len(nrow(dat))) {
        val <- dat[[n]][rowi]
        link <- h[[n]][rowi]
        if(!is.na(val) && !is.na(link)) {
          names(link) <- val
          class(link) <- "hyperlink"
          writeData(wb, sheet, link, startRow=row + 1 + rowi, startCol=col)
        }
      }
    }
  }
  ## format columns
  for(idx in which(!is.na(numFmt))) {
    addStyle(wb, sheet, createStyle(numFmt=numFmt[idx]),
             cols=idx, rows=(1 + row + 1:nrow(dat)),
             gridExpand=TRUE, stack=TRUE)
  }
  wb
}

## get desired number format for each column
tmp1 <- tibble(name=c("IP",
                       "BA", "Opp. OBP", "OBPE",
                       "BA + OBPE + notK/PA:\nBatting Sum", "SR + notOB + notBBHB:\nPitching Sum",
                       "Contact/AB", "Hard/AB", "Hard/Contact",
                       "SR", "K/PA", "BBHB/BF"),
                 numFmt=c("0.0", rep("0.000", 8), rep("0%", 3)))

tmp2 <- bind_rows(tibble(name=c("Lineup", "Number", "Name", "BA", "OBP"), width=8),
                tibble(name=c("SR", "K/PA"), width=7),
                tibble(name=c("Opp. OBP", "BBHB/BF"), width=9),
                tibble(name=c("Contact", "Contact/AB", "Hard/AB", "Hard/Contact"), width=c(8, 10, 10, 12)),
                tibble(name=c("BA + OBPE + notK/PA:\nBatting Sum", "SR + notOB + notBBHB:\nPitching Sum"), width=20),
                tibble(name=c("about", "when", "vs"), width=c(10, 20,15)))

fmt <- full_join(tmp1, tmp2)
rm(tmp1, tmp2)

addDataList <- function(wb, sheet, x, format=fmt) {
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
      ## add scorecard links as hyperlinks to "when" column
      #link <- NULL
      if(!is.null(xi$scorecard_link)) {
        xi$.hyperlink.when <- xi$scorecard_link
        #link <- xi$scorecard_link
        xi$scorecard_link <- NULL
        # if("when" %in% names(xi)) {
        #   names(link) <- xi$when
        #   xi$when <- link
        #   class(xi$when) <- "hyperlink"
        # }
      }
      ## fix any duplicated names
      xi <- as_tibble(xi, .name_repair="unique")
      ## Determine which columns are all missing. Specifically should be these:
      ## c("Lineup", "Number", "Name", "G", "BA")
      kk <- which(purrr::map_lgl(xi, ~all(is.na(.))))
      ## for certain counting stats replace zeros with blanks
      xi <- mutate(xi, across(any_of(c("HBP", "HB", "1B", '2B', '3B', 'HR','ROE')), function(x) {x[x==0] <- NA; x}))
      ## now set name blank for anything that is all missing.
      ## this needs to be last because can't use tidyverse functions with duplicate names
      names(xi)[kk] <- map_chr(seq_along(kk), ~paste(rep(" ",.), collapse="")) ## if need to be unique
      ## save the resulting data sheet in the original list
      x[[i]] <- xi
      ## which rows should be bold
      boldrows <- NULL
      if("Name" %in% names(xi)) {
        k <- which(xi$Name=="Team")
        if(length(k)==1) {
          boldrows <- k
        }
      }
      if("about" %in% names(xi)) {
        k <- which(xi$about=="Season")
        if(length(k)==1) {
          boldrows <- k
        }
      }
      xi$.textDecoration <- NA
      xi$.textDecoration[k] <- "bold"
      numFmts <- fmt$numFmt[match(names(xi), fmt$name)]
      ## now add to the spreadsheet
      wb <- addData(wb, sheet, xi, names(x)[i], row=row[i],
                    numFmt=numFmts)
    } else {
      for(j in seq_along(xi)) {
        writeData(wb, sheet, xi[[j]], startRow=row[i]+j-1, startCol=1)
        if(j==1) addStyle(wb, sheet, createStyle(textDecoration="bold"), cols=1, rows=row[i]+j-1)
      }
    }
  }
  w0 <- 5
  xdf <- x[sapply(x, is.data.frame)]
  ws <- purrr::map(xdf, ~tibble(col=1:ncol(.), name=names(.))) |> bind_rows() |>
    left_join(fmt, by="name") |> mutate(width=replace_na(width, w0)) |>
    group_by(col) |> summarize(width=max(width), .groups="drop") |> arrange(col)
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
                     dir, subdir) {
  if(!missing(subdir)) { dir <- file.path(dir, subdir) }
  if(str_detect(file, "xlsx$")) {
    dimxl <- function(path, sheet) {
      x <- readxl::read_excel(path=path, sheet=sheet, col_names=FALSE, .name_repair="minimal")
      dim(x) |> setNames(c("rows", "cols"))
    }
    out <- drive_put(file, file.path(dir, newfile), type="spreadsheet")
    dims <- tibble(sheet=readxl::excel_sheets(file)) |>
      mutate(purrr::map_dfr(sheet, dimxl, path=file))
    for(i in 1:nrow(dims)) {
      sheet_resize(out, sheet=dims$sheet[i],
                   nrow=dims$rows[i]+1, ncol=dims$cols[i]+1, exact=TRUE)
    }
  } else {
    out <- drive_put(file, file.path(dir, newfile))
  }
  out
}
