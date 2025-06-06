## get desired number format for each column
tmp1 <- tibble(name=c("IP",
                      "BA", "Opp. OBP", "OBPE", "SLG",
                      "SLG + OBPE + notK/PA:\nBatting Sum", "SR + notOB + notBBHB:\nPitching Sum",
                      "Contact/AB", "Hard/AB", "Hard/Contact",
                      "SR", "SR.", "K/PA", "BBHB/BF",
                      "BIP/AB", "Hard/BIP",
                      "Strike Rate", "BIP/Strikes"),
               numFmt=c("0.0", rep("0.000", 9), rep("0%", 8)))

tmp2 <- bind_rows(tibble(name=c("Lineup", "Number", "Name", "BA", "OBP", "SLG"), width=8),
                  tibble(name=c("SR", "SR.", "K/PA"), width=7),
                  tibble(name=c("Opp. OBP", "BBHB/BF"), width=9),
                  tibble(name=c("BIP", "BIP/AB", "Hard/BIP", "Strike Rate", "BIP/Strike"), width=c(7, 8, 8, 10, 10)),
                  tibble(name=c("SLG + OBPE + notK/PA:\nBatting Sum", "SR + notOB + notBBHB:\nPitching Sum"), width=20),
                  tibble(name=c("about", "when", "vs"), width=c(10, 20,15)))

fmt <- full_join(tmp1, tmp2)
rm(tmp1, tmp2)

prepDataList <- function(x, format=fmt) {
  fix1 <- function(xi) {
    if(!is.data.frame(xi)) return(xi)
    ## turn to tibble and fix any duplicated blank names quietly
    unique_Blank <- function(x) {
      k <- which(x == "Blank")
      if(length(k) > 1) {
        x[k] <- paste0(x[k], "...", k)
      }
      x
    }
    xi <- as_tibble(xi, .name_repair=unique_Blank)
    ## add scorecard links as hyperlinks to "when" column
    xi <- xi |> rename(any_of(c(.hyperlink.when="scorecard_link")))
    ## Determine which columns are all missing. Specifically should be these:
    ## c("Lineup", "Number", "Name", "G", "BA")
    kk <- which(purrr::map_lgl(xi, ~all(is.na(.))))
    ## for certain counting stats replace zeros with blanks
    xi <- mutate(xi, across(any_of(c("HBP", "HB", "1B", '2B', '3B', 'HR','ROE')), function(x) {x[x==0] <- NA; x}))
    ## now set name blank for anything that is all missing.
    ## this needs to be last because can't use tidyverse functions with duplicate names
    names(xi)[kk] <- map_chr(seq_along(kk), ~paste(rep(" ",.), collapse="")) ## if need to be unique
    ## which rows should be bold
    boldrows <- NULL
    if("Name" %in% names(xi)) {
      k <- which(xi$Name=="Team")
      if(length(k)==1) {
        boldrows <- k
      }
      xi$.textDecoration <- NA
      xi$.textDecoration[k] <- "bold"
    }
    if("about" %in% names(xi)) {
      k <- which(xi$about=="Season")
      if(length(k)==1) {
        boldrows <- k
      }
      xi$.textDecoration <- NA
      xi$.textDecoration[k] <- "bold"
    }
    for(n in names(xi)) {
      if(n %in% fmt$name) {
        attr(xi[[n]], "numFmt") <- fmt$numFmt[fmt$name==n]
        attr(xi[[n]], "width") <- fmt$width[fmt$name==n]
      }
    }
    xi
  }
  lapply(x, function(y) {
    freeze <- attr(y, "freeze")
    y2 <- lapply(y, fix1)
    attr(y2, "freeze") <- freeze
    y2
  })
}
