library(magrittr)
library(tidyverse)
library(stringr)
library(lubridate)


### prepare scraping ------------------------------------------------------------
# 
# ## start a chrome browser
# #rD <- rsDriver()
# remDr <- rD[["client"]]
# 
# remDr$navigate("http://npb.jp/games/2016/schedule_03_detail.html")
# 
# wl = remDr$findElements(using = "xpath",
#                         value = "//div[@class='table_normal summary_table']")


3:10 %>%   
 {map_if(., str_count(.) == 1, ~ paste0("0", .x))} %>% 
  as.character() -> month

paste0(
  "http://npb.jp/games/",
  rep(c(2016, 2017), each = 8),
  "/schedule_",
  month,
  "_detail.html") -> urls




map_df(urls, function(x) {
  
  # read html table into data.frame
  xml2::read_html(x) %>%
      rvest::html_table() %>%
      .[[1]] -> temp
  
    # clean up enters
    map_df(
      temp,
      ~ str_replace_all(.x, "\n", "") %>%
        str_replace_all(" +", ",")
    ) -> temp
    
    # tidying variables
    temp %>%
      separate(
        "対戦カード",
        c("team_a", "point_a", "dump", "point_b", "team_b"),
        ","
      ) %>%
      separate("球場・開始時間",
               c("ball_park", "start_time"),
               ",") -> temp
    # dump needless variables
    temp %>%
      select(-c(dump,  備考, starts_with("予告"))) %>%
      rename(date =  月日) -> temp
}) -> game_df

saveRDS(game_df, "game_df.rds")


