
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)

tw <-read_rds("for_bc/object/tw_ponjour_20170301_2017_0430")
  
  
tw$account %>% 
  str_extract("@.+ ") %>% 
  str_trim("both") %>% 
  str_split(" ") %>% 
  map_chr(~ tail(.x, 1))



### save df
tw_df <- tibble(
  id = seq_along(tw$content),
  account = tw$account %>% 
    str_extract("@.+ ") %>% 
    str_trim("both") %>% 
    str_split(" ") %>% 
    map_chr(~ tail(.x, 1)),
  hs = tw$time %>%  unlist() %>% 
    str_extract("title=.+data-aria") %>% 
    str_extract("[0-9].+日") %>% 
    str_split(" - ") %>% 
    map_chr(~.[[1]]) %>% 
    hm(),
  ymd = tw$time %>% 
    unlist() %>% 
    str_extract("title=.+data-aria") %>% 
    str_extract("[0-9].+日") %>% 
    str_split(" - ") %>% 
    map_chr(~.[[2]]) %>% 
    ymd(),
  content = tw$content,
  num_retweet = tw$num_retweeted,
  num_fav = tw$num_faved,
  img_source = tw$img_source
)


saveRDS(tw_df, "for_bc/object/tw_ponjour_df2")
