#########
get_calc_list <- function(calculations) {
  bb <- calculations |> filter(!is.na(Formula)) |>
    mutate(Need=Formula |>
             str_remove_all("[A-Za-z0-9._]*\\(") |>        # remove functions
             str_remove_all("[`()!]") |>                # remove backticks and parentheses
             str_replace_all("[-+/*<>,=&|%]", "•") |>        # convert math and commas to breaks
             str_replace_all("[ 0-9]*•[ •0-9]*", "•") |> # simplify breaks and remove numerals
             str_replace("•$", "")
    ) |>
    separate_longer_delim(Need, "•") |>
    filter(is.na(Need) | Need!="NA") |> unique() |>
    mutate(Order=case_when(!Need %in% Stat ~ 1L, TRUE ~ NA))
  step <- 1
  while(any(is.na(bb$Order))) {
    step <- step + 1
    stopifnot(step<100)
    bmax <- bb |> summarize(Order=max(Order), .by=Stat)
    k <- which(is.na(bb$Order))
    m <- match(bb$Need[k], bmax$Stat)
    stopifnot(any(!is.na(m)))
    bb$Order[k] <- bmax$Order[m]+1
  }
  bb |> mutate(Order=max(Order), .by=Stat) |> nest(Need=Need) |> arrange(Order)
}

get_calc_vars <- function(need, calc_list) {
  vv <- calc_list |> unnest(Need) 
  vx1 <- vx2 <- vv |> filter(Stat %in% need)
  for(i in 1:100) {
    stopifnot(i<100)
    vx2 <- vv |> filter(Stat %in% vx2$Need & !Stat %in% vx2$Stat)
    if(nrow(vx2)==0) break;
    vx1 <- bind_rows(vx1, vx2) 
  }
  calc <- vx1 |> nest(Need=Need) |> arrange(Order)
  need2 <- calc |> unnest(Need) |> pull(Need)
  counts <- setdiff(c(need, need2), calc$Stat)
  calcs <- calc$Formula |> setNames(calc$Stat) |> 
    lapply(function(x) parse(text=x))
  list(Counts=counts, Need=need, Calculations=calcs)
}
  
