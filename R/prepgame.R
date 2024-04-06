prep_game <- function(game) {
  stopifnot(is_tibble(game) && nrow(game)==1)
  game <- as.list(game)
  game$teams <- game$game[[1]]$Team
  game$plays <- game$game[[1]] |> select(Side, Plays) |> unnest(Plays)
  game$lineup <- game$game[[1]] |> select(Side, Lineup) |> unnest(Lineup)
  game$game <- NULL
  game
}

prep_game2 <- function(game) {
  stopifnot(is_tibble(game) && nrow(game)==1)

  g <- game$game[[1]]

  lx <- local({
    p1 <- g |> select(Side, Plays) |> unnest(Plays) |>
      arrange(Side, Row) |> select(Side, Number=Pitcher) |> unique() |>
      mutate(Order=1:n(), .by=Side) |> mutate(Side=3-Side)
    g |> select(Side, Team, Lineup) |> unnest(Lineup) |>
      full_join(p1, by=c("Side", "Number")) |>
      mutate(Player=sprintf("%d%3d", Side, Number)) |>
      arrange(Side, Lineup, Order)
  })

  px <- g |> select(Side, Plays) |> unnest(Plays) |>
    arrange(Inning, Side, Row) |>
    left_join(lx |> select(Side, Pitcher=Number, Pitcher_Code=Player) |> mutate(Side=3-Side),
              by=c("Side", "Pitcher")) |>
    left_join(lx |> select(Side, Lineup, Batter_Code=Player),
              by=c("Side", "Lineup")) |>
    select(-Pitcher, -Lineup) |>
    rename(Pitcher=Pitcher_Code, Batter=Batter_Code) |>
    select(Row, Inning, Side, Batter, Pitcher, everything())

  lx <- lx |> select(Player, Team, Number, Name, Lineup, Order)

  game <- as.list(game)
  game$teams <- game$game[[1]]$Team
  game$plays <- px
  game$lineup <- lx
  game$game <- NULL
  game
}
