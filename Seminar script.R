library(cfbscrapR)
library(ggrepel)
library(tidyverse)
library(gt)
library(ggimage)

#create play-by-play data (just run this stuff, don't worry about it)
pbp_2020 <- data.frame()
seasons <- 2020
pbp_2020 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})

#Filter out 1) special teams plays 2) Games involving FCS teams and 3) create columns for garbage time and successful plays
plays_2020 <- pbp_2020 %>% filter(rush == 1| pass == 1) %>% 
  filter(!is.na(offense_conference) & !is.na(defense_conference)) %>%
  mutate(abs_diff = abs(score_diff),
         garbage = ifelse(period == 1 & abs_diff > 43, 1, 
                          ifelse(period == 2 & abs_diff > 37, 1,
                                 ifelse(period == 3 & abs_diff > 27, 1,
                                        ifelse(period == 4 & abs_diff > 22, 1, 0)))),
         success = ifelse(down == 1 & yards_gained > .5*distance, 1,
                          ifelse(down == 2 & yards_gained > .7*distance, 1,
                                 ifelse((down == 3 | down == 4) & yards_gained >=distance, 1, 0))))
#filters out garbage time plays
plays_2020 <- plays_2020 %>% filter(garbage == 0)

#Creating a dataset and using group_by and summarize (must use BOTH)
SECPassEPA <- plays_2020 %>% filter(offense_conference == "SEC" & pass == 1) %>% group_by(pos_team) %>%
  summarize(
    pass_epa = mean(EPA, na.rm = TRUE),
    total_epa = sum(EPA, na.rm = TRUE),
    passes = n()
  )
#In case you want to sort a dataset by a column (put a minus sign in front of the column name if you want it sorted in the opposite direction)
SECPassEPA <- SECPassEPA %>% arrange(pass_epa)

#filter out teams that hadn't thrown many passes just in case you want it
SECPassEPA <- SECPassEPA %>% filter(passes >= 250)


#New dataset - Group of 5 teams!
#Create a filter of Group of 5 conferences
Groupof5 <- c("Mid-American", "FBS Independents", "Mountain West", "American Athletic", "Conference USA", "Sun Belt")

#Let's do some EPA stuff, create a dataset and make some stats
Gof5 <- plays_2020 %>% filter(offense_conference %in% Groupof5) %>% group_by(pos_team, offense_conference) %>%
  summarize(
    epa_per_play = mean(EPA, na.rm = TRUE),
    epa_per_rush = mean(EPA[rush == 1], na.rm = TRUE),
    epa_per_pass = mean(EPA[pass == 1], na.rm = TRUE),
    success_rate = mean(success),
    pass_success = mean(success[pass == 1]),
    rush_success = mean(success[rush == 1]),
    rush = sum(rush),
    pass = sum(pass),
    ratio = pass/rush
  )
#filter out Notre Dame because they're not really a G5 team
Gof5 <- Gof5 %>% filter(pos_team != "Notre Dame")

#You probably want some logos on your chart! Here's how!
cfblogos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv") %>% select(school, logo)
GofFive <- Gof5 %>% left_join(cfblogos, by = c("pos_team" = "school"))


#Let's make a chart!
#This looks like a bunch of nonsense, but here's basically what happened:
#1. Tell ggplot which dataset you want to graph +
#2. Give it some coordinates +
#3. Put the logos in +
#4. Draw up some lines showing the average for both the y-axis and the x-axis +
#5. Add labels
ggplot(GofFive, aes(x=epa_per_rush, y=epa_per_pass)) + geom_image(image = GofFive$logo, asp = 16/9) + 
  geom_hline(yintercept = mean(GofFive$epa_per_pass)) + geom_vline(xintercept = mean(GofFive$epa_per_rush)) +
  labs(x = "EPA per rush", y = "EPA per pass", title = "Group of Five offensive efficiency", caption = "Chart by Brendan Farrell | Data via @cfbscrapR")
#And now save it
ggsave("FirstChart.jpg", dpi = 300) 