detach("package:BaseballScorecard", unload = TRUE)
library(tidyverse)
library(BaseballScorecard)
## googledrive, googlesheets4, OpenImageR, zoo, patchwork

google_user <- "rendahla@gmail.com"
googledrive::drive_auth(google_user)
googlesheets4::gs4_auth(google_user)
Google_dir <- "Sports/Baseball/Roseville 13U Silver 2024/Roseville 13U Silver 2024 Stats"
google_user <- "NO"

logos <- list(Roseville=OpenImageR::readImage("logos/roseville.png"),
              Shoreview=OpenImageR::readImage("logos/shoreview.png"),
              "East Ridge"=OpenImageR::readImage("logos/eastridge.png"),
              "Oakdale"=OpenImageR::readImage("logos/oakdale.png"),
              "Andover"=OpenImageR::readImage("logos/andover.png"),
              "Spring Lake Park"=OpenImageR::readImage("logos/springlakepark.png"),
              "St. Anthony"=OpenImageR::readImage("logos/stanthony.png"),
              "Blaine"=OpenImageR::readImage("logos/blaine.png"),
              "Cambridge Isanti"=OpenImageR::readImage("logos/cambridge.jpeg"))

# gi <- readgame("game_data/Scrimmage_0421.xlsx", rosters=readrosters("Roster.xlsx"))
# gi$about <- "Scrimmage"
# file.score <- file.path("scorecards", "Scorecard_0421.pdf")
# scorecard(gi, file.score, logos=logos, team_name="Roseville",
#           panels = c(1.75, 1, 0.65),
#           start_count=c(1,1),
#           team_display="Roseville Raiders 13U Silver (AA) 2024")
# toGoogle(file.score, dir=Google_dir, subdir="Scorecards")

#############################
rr <- readrosters("Roster.xlsx")

team <- "Roseville"

gs <- readgames("game_data", rosters=rr, save.file="tmp.RDS", reload=FALSE) |>
  mutate(about=replace_na(about, "GSBL"))

a <- readxl::read_excel("forstats.xlsx", "List") |>
  mutate(across(where(is.character), \(x) str_replace(x, "\\\\n", "\n")))
calcsX <- a |> select(Stat, Formula) |> get_calc_list()
rolesX <- a |> select(Stat, Role) |> filter(!is.na(Role))
rm(a)

#############################################
## SCORECARD FOR LAST GAME

all.scorecards <- FALSE
i <- nrow(gs)
for(i in seq_len(nrow(gs))) {
  file.score <- file.path("scorecards", sprintf("Scorecard_%s.pdf", gs$code[i]))
  mtime.score <- file.mtime(file.score)
  if(all.scorecards || is.na(mtime.score) || (mtime.score < gs$mtime[i])) {
    message("Scorecard ", gs$code[i])
    scorecard(gs[i,], file.score, logos=logos, team_name="Roseville",
              team_display="Roseville Raiders 13U Silver (AA) 2024")
    if(google_user=="rendahla@gmail.com") {
      toGoogle(file.score, dir=Google_dir, subdir="Scorecards")
    }
  }
}

#drive_ls(Google_dir)
#############################################
## GAME STATS

## get scorecard links
# ## can either get by dirname (if have access) or by hardcoded id
#tmp0 <- googledrive::drive_ls(file.path(Google_dir, "Scorecards"))
tmp0 <- googledrive::drive_ls(googledrive::as_id("1SV9oCAs_WFQde5IBiaoCZjdVlIrNSQCm"))
scorecard_list <- tmp0 |> mutate(link=map_chr(drive_resource, ~.$webViewLink)) |>
  mutate(code=name |> str_remove("Scorecard_") |> str_remove("\\.pdf")) |>
  select(code, scorecard_link=link) |> arrange(code)
scorecard_list |> arrange(desc(code))
gs <- gs |> left_join(scorecard_list, by="code")

# THEN ADD STATS, AFTER THE SCORECARD LINKS
game_stats_file <- readxl::read_excel("forstats.xlsx", "Game Stats")
gs <- add_stats(gs, team="Roseville", stats_file=game_stats_file, calculations=calcsX, roles=rolesX)

################################################################################

gs$group <- gs$about
# ## CODE FOR SETTING NAMES OF SHEETS OF GAME DATA
# ## these were previously stored as gs$name
# names(all.files$out) <- all.files$name

# # a little function to separate the GSBL games into 1-8 and 9-16
# tog <- function(x, n) {
#   if(max(x) <=n) return("")
#   k <- ((x-1) %/% n)*n
#   sprintf(" %d-%d", k+1, k+n)
# }
# team="", bydate=c(), maxg=8,
# mutate(group=paste0(about, tog(1:n(), maxg)), .by=about) |>
#   mutate(name=sprintf("%s %s", if_else(about %in% bydate,
#                                        format(datetime, "%m-%e") |> str_remove("^0") |> str_remove_all(" "),
#                                        paste("Game", 1:n())),
#                       vs |> str_remove(sprintf("^%s @ ", team)) |>
#                         str_remove(sprintf(" @ %s$", team))), .by=about)

# ### also add names (for sheet names)
# this should be different for tournaments!!
team <- "Roseville"
names(gs$stats) <- sprintf("%s %s", 
                           format(gs$when, "%m-%e") |> str_remove("^0") |> str_remove(" "),
                           gs |> mutate(x=map_chr(game, \(x) x$Team |> setdiff(team))) |> pull(x))

unique(gs$group)
dofor <- unique(gs$group)
dofor
# dofor <- "GSBL"
# dofor <- "GSBL 1-8"
# dofor <- "GSBL 9-16"
for(s in dofor) {
  message(s)
  tmp <- filter(gs, group==s)
  sx <- paste0("Game Stats ", s)
  file.xlsx <- file.path("stats", sprintf("%s.xlsx", sx))
  tmp$stats |> prepDataList() |> statsToExcel(file.xlsx)
  if(google_user=="rendahla@gmail.com") {
    toGoogle(file.xlsx, newfile=sx, dir=Google_dir)
  }
}

#############################################
## PLAYER STATS

file.xlsx <- file.path("stats", "Season Stats.xlsx")

individual_stats_file <- readxl::read_excel("forstats.xlsx", "Individual")
ii <- make_stats_from_file(gs, header=NULL, individual_stats_file, 
                             calcs=calcsX, 
                             roles=rolesX, 
                             team="Roseville", vs=NA)

season_stats_file <- readxl::read_excel("forstats.xlsx", "Season")
ss <- make_stats_from_file(gs, header=NULL, season_stats_file, 
                             calcs=calcsX, 
                             roles=rolesX, 
                             team="Roseville", vs=NA)

player_stats_file <- readxl::read_excel("forstats.xlsx", "Player")
fk <- which(player_stats_file$Order=="Filter")
rteam <- rr |> filter(Team==team) |>
  mutate(filter = sprintf('Team == "%s" & Number == %d', Team, Number),
         player = sprintf("%d %s", Number, Name))
pout <- lapply(setNames(rteam$filter, rteam$player), function(ff) {
  player_stats_file$Batting[fk] <- ff
  player_stats_file$Pitching[fk] <- ff
  make_stats_from_file(gs, header=NULL, f=player_stats_file, 
                       calcs=calcsX, 
                       roles=rolesX, 
                       team="Roseville", vs=NA)
})

c(list(Individual=ii, Season=ss), pout) |>
  prepDataList() |> statsToExcel(file.xlsx)

if(google_user=="rendahla@gmail.com") {
  toGoogle(file.xlsx, newfile="Season Stats", dir=Google_dir)
}


#############################################
## CONTACT
contact <- ii$Batting |> select(Number, Name, AB, K, BBHB, BIP, H, Soft, Hard) 

p1 <- ggplot(contact) + aes(BIP/AB, Hard/pmax(BIP,1), label=Name) + 
  geom_point(position=ggbeeswarm::position_beeswarm()) + ggrepel::geom_text_repel() +
  ggtitle("Balls in Play (BIP) and Hard Hit Rates",
          subtitle=paste("through", format(max(gs$when), "%B %e") |> str_replace(" +", " "))) +
  scale_x_continuous(labels=scales::percent, name="BIP/AB") +
  scale_y_continuous(labels=scales::percent, name="Hard/BIP")
#p1

file.pdf <- file.path("stats", "ContactRates.pdf")
ggsave(file.pdf, p1, width=5, height=5)
if(google_user=="rendahla@gmail.com") {
  toGoogle(file.pdf, dir=Google_dir)
}

pitches <- ii$Pitching |> select(Number, Name, Strikes, Pitches, BIP)

p2 <- ggplot(pitches) + aes(Strikes/Pitches, BIP/Strikes, label=Name) + 
  geom_point(position=ggbeeswarm::position_beeswarm()) + ggrepel::geom_text_repel() +
  ggtitle("Balls in Play (BIP) and Strike Rates",
          subtitle=paste("through", format(max(gs$when), "%B %e") |> str_replace(" +", " "))) +
  scale_x_continuous(labels=scales::percent, name="Strike Rate (Strikes/Pitches)") +
  scale_y_reverse(labels=scales::percent, name="BIP Rate (BIP/Strikes)")
p2

file.pdf <- file.path("stats", "StrikeRates.pdf")
ggsave(file.pdf, p2, width=5, height=5)
if(google_user=="rendahla@gmail.com") {
  toGoogle(file.pdf, dir=Google_dir)
}
#############################################
## STREAKS

bp <- streaks(gs, "Roseville", 3, calcs=calcsX, title="Batting Summary")
pp <- streaks(gs, "Roseville", 1, calcs=calcsX, title="Pitching Summary", 
              var = "SR + notOB + notBBHB:\nPitching Sum",
              role = "Pitcher")

sumplots <- patchwork::wrap_plots(bp, pp, ncol=1)
#sumplots
file.pdf <- file.path("stats", "StreaksAndSlumps.pdf")
ggsave(file.pdf, sumplots, width=8, height=8)
if(google_user=="rendahla@gmail.com") {
  toGoogle(file.pdf, dir=Google_dir)
}

p1 <- streaks(gs, "Roseville", 3, calcs=calcsX, title="OBPE", var="OBPE")
p2 <- streaks(gs, "Roseville", 3, calcs=calcsX, title="BIP / AB", var="BIP/AB")
sumplots2 <- patchwork::wrap_plots(p1, p2, ncol=1)
file.pdf <- file.path("stats", "StreaksAndSlumps_OBPandBIP.pdf")
ggsave(file.pdf, sumplots2, width=8, height=8)
if(google_user=="rendahla@gmail.com") {
  toGoogle(file.pdf, dir=Google_dir)
}