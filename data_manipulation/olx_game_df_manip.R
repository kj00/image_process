# check change point of year
game_df$date %>%
  str_extract("[0-9]+") %>% 
  as.numeric() %>% 
  diff() %>%
  {which(. == min(.))}


  

game_df %>%
  mutate(date = date %>% 
  str_replace("）", "") %>% 
  str_split("（") %>% 
  map_chr(~ .x[1]) %>% 
  str_c("/", c(rep(2016, 947), rep(2017, nrow(game_df) - 947))) %>% 
  lubridate::mdy()
  ) %>% 
  filter(team_a == "オリックス" | team_b == "オリックス") -> olx_game_df

olx_game_df %>% 
  filter(!is.na(point_a)) %>% 
  mutate(point_a = as.numeric(point_a),
         point_b = as.numeric(point_b),
         start_time = hm(start_time),
         opp_ab = if_else(team_a == "オリックス", "b", "a"),
         opp = if_else(opp_ab == "a", team_a, team_b),
         diff_point = if_else(opp_ab == "a", point_b - point_a, point_a - point_b),
         wld = if_else(diff_point > 0, "w",
                      if_else(diff_point == 0, "d", "l"))
         ) %>%
  select(date, ball_park, start_time:wld) -> olx_game_df

saveRDS(olx_game_df, "olx_game_df")
