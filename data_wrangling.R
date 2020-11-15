library(tidyverse)
library(here)

#==============================================================================================#
##                                      read data                                             ##
#==============================================================================================#

season_0910<-read_csv("data/season-0910.csv") %>% mutate(Season = as.factor(rep("09/10")))
season_1011<-read_csv("data/season-1011.csv") %>% mutate(Season = as.factor(rep("10/11")))
season_1112<-read_csv("data/season-1112.csv") %>% mutate(Season = as.factor(rep("11/12")))
season_1213<-read_csv("data/season-1213.csv") %>% mutate(Season = as.factor(rep("12/13")))
season_1213 <- season_1213[1:380,]
season_1314<-read_csv("data/season-1314.csv") %>% mutate(Season = as.factor(rep("13/14")))
season_1415<-read_csv("data/season-1415.csv") %>% mutate(Season = as.factor(rep("14/15")))
season_1415 <- season_1415[1:380,]
season_1516<-read_csv("data/season-1516.csv") %>% mutate(Season = as.factor(rep("15/16")))
season_1516 <- season_1516[1:380,]
season_1617<-read_csv("data/season-1617.csv") %>% mutate(Season = as.factor(rep("16/17")))
season_1718<-read_csv("data/season-1718.csv") %>% mutate(Season = as.factor(rep("17/18")))
season_1819<-read_csv("data/season-1819.csv") %>% mutate(Season = as.factor(rep("18/19")))

#==============================================================================================#
##                                      cleandata                                             ##
#==============================================================================================#

season_0910 <- season_0910 %>%
  select(
    Season,
    Date,
    HomeTeam,
    AwayTeam,
    FTHG,
    FTAG,
    FTR,
    HTHG,
    HTAG,
    HTR,
    HS,
    AS,
    HST,
    AST,
    HF,
    AF,
    HC,
    AC,
    HY,
    AY,
    HR,
    AR
  ) %>%
  rename(
    "FT_Home_Goals" = "FTHG",
    "FT_Away_Goals" = "FTAG",
    "FT_Result" = "FTR",
    "HT_Home_Goals" = "HTHG",
    "HT_Away_Goals" = "HTAG",
    "HT_Result" = "HTR",
    "Home_Shots" = "HS",
    "Away_Shots" = "AS",
    "Home_Shots_on_Target" = "HST",
    "Away_Shots_on_Target" = "AST",
    "Home_Fouls" = "HF",
    "Away_Fouls" = "AF",
    "Home_Corners" = "HC",
    "Away_Corners" = "AC",
    "Home_Yellow" = "HY",
    "Away_Yellow" = "AY",
    "Home_Red" = "HR",
    "Away_Red" = "AR"
  ) %>%
  mutate(
    FT_Result = case_when(
      str_starts(FT_Result, "H") ~ "Home Win",
      str_starts(FT_Result, "D") ~ "Draw",
      str_starts(FT_Result, "A") ~ "Away Win"
    ),
    HT_Result = case_when(
      str_starts(HT_Result, "H") ~ "Home Win",
      str_starts(HT_Result, "D") ~ "Draw",
      str_starts(HT_Result, "A") ~ "Away Win"
    ),
    Home_Shot_Acc = round(Home_Shots_on_Target/Home_Shots,digits = 2),
    Away_Shot_Acc = round(Away_Shots_on_Target/Away_Shots, digits =2)
  )


clean <- function(x) {
  x$Date <- dmy(x$Date)
  x %>% select(
    Season,
    Date,
    HomeTeam,
    AwayTeam,
    FTHG,
    FTAG,
    FTR,
    HTHG,
    HTAG,
    HTR,
    HS,
    AS,
    HST,
    AST,
    HF,
    AF,
    HC,
    AC,
    HY,
    AY,
    HR,
    AR
  ) %>%
    rename(
      "FT_Home_Goals" = "FTHG",
      "FT_Away_Goals" = "FTAG",
      "FT_Result" = "FTR",
      "HT_Home_Goals" = "HTHG",
      "HT_Away_Goals" = "HTAG",
      "HT_Result" = "HTR",
      "Home_Shots" = "HS",
      "Away_Shots" = "AS",
      "Home_Shots_on_Target" = "HST",
      "Away_Shots_on_Target" = "AST",
      "Home_Fouls" = "HF",
      "Away_Fouls" = "AF",
      "Home_Corners" = "HC",
      "Away_Corners" = "AC",
      "Home_Yellow" = "HY",
      "Away_Yellow" = "AY",
      "Home_Red" = "HR",
      "Away_Red" = "AR"
    ) %>%
    mutate(
      FT_Result = case_when(
        str_starts(FT_Result, "H") ~ "Home Win",
        str_starts(FT_Result, "D") ~ "Draw",
        str_starts(FT_Result, "A") ~ "Away Win"
      ),
      HT_Result = case_when(
        str_starts(HT_Result, "H") ~ "Home Win",
        str_starts(HT_Result, "D") ~ "Draw",
        str_starts(HT_Result, "A") ~ "Away Win"
      ),
      Home_Shot_Acc = round(Home_Shots_on_Target/Home_Shots, digits=2),
      Away_Shot_Acc = round(Away_Shots_on_Target/Away_Shots, digits=2)
    )
}


season_1011 <- clean(season_1011)
season_1112 <- clean(season_1112)
season_1213 <- clean(season_1213)
season_1314 <- clean(season_1314)
season_1415 <- clean(season_1415)
season_1516 <- clean(season_1516)
season_1617 <- clean(season_1617)
season_1718 <- clean(season_1718)
season_1819 <- clean(season_1819)

all_season <- rbind(
  season_0910,
  season_1011,
  season_1112,
  season_1213,
  season_1314,
  season_1415,
  season_1516,
  season_1617,
  season_1718,
  season_1819
)

write_csv(all_season, here::here("data/all_season.csv"))


#==============================================================================================#
##                                      Standings/Table                                       ##
#==============================================================================================#

seriea_home <- all_season %>%
  select(Season, HomeTeam, FT_Result, FT_Home_Goals, FT_Away_Goals) %>%
  rename("Team" = HomeTeam) %>%
  mutate(
    Win = ifelse(FT_Result == "Home Win", rep(1), 0),
    Draw = ifelse(FT_Result == "Draw", rep(1), 0),
    Lose = ifelse(FT_Result == "Away Win", rep(1), 0),
    GAway = rep(0),
    GConAway = rep(0)
  ) %>%
  mutate(
    W = ifelse(FT_Result == "Home Win", rep(3), 0),
    D = ifelse(FT_Result == "Draw", rep(1), 0),
    L = ifelse(FT_Result == "Away Win", rep(0), 0),
    Points = W + D + L
  ) %>%
  rename("GHome" = "FT_Home_Goals",
         "GConHome" = "FT_Away_Goals")

seriea_away <- all_season %>%
  select(Season, AwayTeam, FT_Result, FT_Away_Goals, FT_Home_Goals) %>%
  rename("Team" = AwayTeam) %>%
  mutate(
    Win = ifelse(FT_Result == "Away Win", rep(1), 0),
    Draw = ifelse(FT_Result == "Draw", rep(1), 0),
    Lose = ifelse(FT_Result == "Home Win", rep(1), 0),
    GHome = rep(0),
    GConHome = rep(0)
  ) %>%
  mutate(
    W = ifelse(FT_Result == "Away Win", rep(3), 0),
    D = ifelse(FT_Result == "Draw", rep(1), 0),
    L = ifelse(FT_Result == "Home Win", rep(0), 0),
    Points = W + D + L
  )%>%
  rename("GAway" = "FT_Away_Goals",
         "GConAway" = "FT_Home_Goals")

seriea <- rbind(seriea_home, seriea_away) %>%
  mutate(GF = GHome+GAway,
         GA = GConHome+GConAway,
         GD = GF-GA,
         MP = Win+Draw+Lose)
write_csv(seriea, "data/seriea.csv")

#==============================================================================================#
##                                      Match stats                                           ##
#==============================================================================================#

detail_stat <- function(team){
  team_FT <- all_season %>%
    filter(HomeTeam == team | AwayTeam == team) %>%
    mutate(Full_Time = ifelse(
      HomeTeam == team & FT_Result == "Home Win",
      rep("Win"),
      ifelse(
        AwayTeam == team & FT_Result == "Away Win",
        rep("Win"),
        ifelse(
          HomeTeam == team & FT_Result == "Away Win",
          rep("Lose"),
          ifelse(
            AwayTeam == team & FT_Result == "Home Win",
            rep("Lose"),
            rep("Draw")
          )
        )
      )
    )) %>%
    pivot_wider(names_from = "Full_Time",
                values_from = "Full_Time")
  
  # inter_FT$Away_Shot_Acc[is.nan(inter_FT$Away_Shot_Acc)] <- 0
  # inter_FT[is.na(inter_FT)]= "None"
  
  team_FT %>%
    mutate(
      Draw = ifelse(Draw == "Draw", rep(1), 0),
      Win = ifelse(Win == "Win", rep(1), 0),
      Lose = ifelse(Lose == "Lose", rep(1), 0)) %>%
    replace_na(list(Win=0,
                    Draw=0,
                    Lose=0)) %>% mutate(
                      W = ifelse(Win == 1, rep(3), 0),
                      D = ifelse(Draw == 1, rep(1), 0),
                      L = ifelse(Lose == 1, rep(0), 0),
                      Points = W+D+L)
}


summary_stat <- function(team){
  detail_stat(team)  %>%
    pivot_longer(
      cols = c(5, 6, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24),
      names_to = "MatchIndicator",
      values_to = "Value"
    ) %>%
    mutate(format = case_when(
      str_detect(MatchIndicator, "Home") ~ "Home",
      str_detect(MatchIndicator, "Away") ~ "Away",
    )) %>%
    pivot_longer(cols = c(3, 4)) %>%
    rename("Team" = "value") %>%
    filter(Team == team) %>%
    select(-HT_Result) %>%
    mutate(
      key = case_when(
        str_detect(name, "Home") & format == "Away" ~ 0,
        str_detect(name, "Away") & format == "Away" ~ 1,
        str_detect(name, "Away") & format == "Home" ~ 0,
        str_detect(name, "Home") & format == "Home" ~ 1)
    ) %>% filter(key==1) %>%
    group_by(Season, Team, format, MatchIndicator) %>%
    summarise(TotalWin = sum(Win),
              TotalDraw = sum(Draw),
              TotalLose = sum(Lose),
              TotalPoints = sum(Points),
              Value = sum(Value))
}


summary_all_team <- function(x){
  allteam <- as.list(unique(all_season$HomeTeam))
  tes <- sapply(allteam,summary_stat)
  as.data.frame(tes[,x])
}

summary_all_team_final <- 
  rbind(summary_all_team(1),
        summary_all_team(2),
        summary_all_team(3),
        summary_all_team(4),
        summary_all_team(5),
        summary_all_team(6),
        summary_all_team(7),
        summary_all_team(8),
        summary_all_team(9),
        summary_all_team(10),
        summary_all_team(11),
        summary_all_team(12),
        summary_all_team(13),
        summary_all_team(14),
        summary_all_team(15),
        summary_all_team(16),
        summary_all_team(17),
        summary_all_team(18),
        summary_all_team(19),
        summary_all_team(20),
        summary_all_team(21),
        summary_all_team(22),
        summary_all_team(23),
        summary_all_team(24),
        summary_all_team(25),
        summary_all_team(26),
        summary_all_team(27),
        summary_all_team(28),
        summary_all_team(29),
        summary_all_team(30),
        summary_all_team(31),
        summary_all_team(32),
        summary_all_team(33),
        summary_all_team(34))

write_csv(summary_all_team_final, here::here("data/summary_all_team_final.csv"))
