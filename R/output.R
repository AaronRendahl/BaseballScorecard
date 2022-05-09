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
      kk <- which(purrr::map_lgl(xi, ~all(is.na(.))))
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
  ws <- purrr::map(xdf, ~tibble(col=1:ncol(.), name=names(.))) |> bind_rows() |>
    left_join(wx, by="name") |> mutate(width=replace_na(width, w0),
                                       width=if_else(stringr::str_detect(name, "Sum$"), 20, width)) |>
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
                     dir="Roseville 10U AA 2021 Stats",
                     subdir) {
  if(!missing(subdir)) { dir <- file.path(dir, subdir) }
  if(str_detect(file, "xlsx$")) {
    dimxl <- function(path, sheet) {
      x <- readxl::read_excel(path=path, sheet=sheet, col_names=FALSE)
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