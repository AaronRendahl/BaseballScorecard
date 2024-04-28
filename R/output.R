######
## output to Excel file
addData <- function(wb, sheet, dat, header, row) {
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
  textD <- about |> bind_rows(tibble(.textDecoration=character())) |>
    select(.textDecoration) |>
    mutate(row=row + 1 + 1:n()) |>
    filter(!is.na(.textDecoration))
  for(idx in seq_len(nrow(textD))) {
    addStyle(wb, sheet, createStyle(textDecoration=textD$.textDecoration[idx]),
             cols = seq_len(ncol(dat)), rows = textD$row[idx])
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
  getattr <- function(x, n) {
    out <- sapply(x, function(v) attr(v, n))
    out[sapply(out, is.null)] <- NA
    unlist(out)
  }
  #numFmts <- sapply(dat, function(v) attr(v, "numFmt"))
  #numFmts[sapply(numFmts, is.null)] <- NA
  #numFmts <- unlist(numFmts)
  numFmts <- getattr(dat, "numFmt")
  for(idx in which(!is.na(numFmts))) {
    addStyle(wb, sheet, createStyle(numFmt=numFmts[idx]),
             cols=idx, rows=(1 + row + 1:nrow(dat)),
             gridExpand=TRUE, stack=TRUE)
  }
  ## return widths
  getattr(dat, "width")
}

addDataList <- function(wb, sheet, x, width.default=5) {
  ## count how many rows needed for each part
  ## for data frame need to count title and header row (so +2)
  nr <- sapply(x, function(x) {if(is.data.frame(x)) nrow(x) + 2 else length(x) })
  ## the +1 is for a space between them
  row <- lag(1 + cumsum(nr + 1), default=1)
  widths <- NULL
  for(i in seq_along(x)) {
    xi <- x[[i]]
    if(is.data.frame(xi)) {
      ## now add to the spreadsheet
      wi <- addData(wb, sheet, xi, names(x)[i], row=row[i])
      ## save widths so can get best width later
      widths <- bind_rows(widths, tibble(col=seq_along(wi), width=wi))
    } else {
      for(j in seq_along(xi)) {
        writeData(wb, sheet, xi[[j]], startRow=row[i]+j-1, startCol=1)
        if(j==1) addStyle(wb, sheet, createStyle(textDecoration="bold"), cols=1, rows=row[i]+j-1)
      }
    }
  }
  ws <- widths |> mutate(width=replace_na(width, width.default)) |>
    group_by(col) |> summarize(width=max(width), .groups="drop") |> arrange(col)
  setColWidths(wb, sheet, cols = ws$col, widths = ws$width)
  wb
}

statsToExcel <- function(out, filename) {
  wb <- createWorkbook()
  for(n in names(out)) {
    addWorksheet(wb, n)
    addDataList(wb, n, out[[n]])
    freeze <- attr(out[[n]], "freeze")
    if(!is.null(freeze)) freezePane(wb, n, firstActiveCol=freeze)
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
    out <- googledrive::drive_put(file, file.path(dir, newfile), type="spreadsheet")
    dims <- tibble(sheet=readxl::excel_sheets(file)) |>
      mutate(purrr::map_dfr(sheet, dimxl, path=file))
    for(i in 1:nrow(dims)) {
      googlesheets4::sheet_resize(out, sheet=dims$sheet[i],
                   nrow=dims$rows[i]+1, ncol=dims$cols[i]+1, exact=TRUE)
    }
  } else {
    out <- googledrive::drive_put(file, file.path(dir, newfile))
  }
  out
}
