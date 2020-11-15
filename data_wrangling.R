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