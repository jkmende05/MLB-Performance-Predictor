library(Lahman)
library(dplyr)

batting_data <- Batting %>%
  filter(yearID >= 1995) %>%
  group_by(playerID, yearID) %>%
  summarize(BA = sum(H) / sum(AB), HR = sum(HR), H = sum(H), X2B = sum(X2B),
            X3B = sum(X3B), AB = sum(AB), R = sum(R), RBI = sum(RBI),
            BB = sum(BB), SO = sum(SO), SB = sum(SB),
            OBP = sum(H, BB, IBB, HBP) / sum(AB),
            SLG = (sum(X2B) * 2 + sum(X3B) * 3 +
                     sum(HR) * 4 + (sum(H) - sum(X2B, X3B, HR))) / sum(AB)) %>%
  na.omit()
head(batting_data)

player_data <- People %>%
  select(playerID, nameFirst, nameLast, height, weight, bats, throws, birthYear)
head(player_data)

complete_player_data <- merge(batting_data, player_data, by = "playerID")
complete_player_data$age <- complete_player_data$yearID -
  complete_player_data$birthYear
head(complete_player_data)

saveRDS(complete_player_data, "data//player_data.rds")
