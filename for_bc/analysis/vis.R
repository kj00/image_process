library(tidyverse)
library(stringr)
library(lubridate)

tw_df <- bind_rows(read_rds("for_bc/object/tw_ponjour_df1"),
                   read_rds("for_bc/object/tw_ponjour_df2"))




tw_df %>% 
  select(-id) %>% 
  filter(ymd <= "2017-04-01" & ymd >= "2017-02-20") %>% 
  replace_na(replace = list("num_retweet" = 0,
                            "num_fav" = 0)) %>% 
  mutate(tag_ponta = str_detect(content,
                                regex("#Ponta|#ポンタ", ignore_case = T)) %>% 
           as.integer(),
         tag_ponjour = str_detect(content,
                                  regex("#Pontacafe|#ポンタカフェ|Ponjour|ポンジュール", ignore_case = T)) %>% 
           as.integer(),
         at_Ponta = str_detect(account, regex("@Ponta", ignore_case = F)) %>%
           as.integer(),
         at_bs_ponta = str_detect(account, regex("@bs_ponta", ignore_case = F)) %>% 
           as.integer(),
         at_ponta_play = str_detect(account, regex("@PontaPLAY", ignore_case = F)) %>% 
           as.integer(),
         at_ponta_connect = str_detect(account, regex("@PontaConnect", ignore_case = F)) %>% 
           as.integer()) %>%
  filter(tag_ponjour == 1) %>% 
  group_by(ymd) %>%
  summarise(num_tweet = n(),
            num_retweet = sum(num_retweet, na.rm = T),
            num_fav = sum(num_fav, na.rm = T)) %>% 
  ungroup()-> tw_agg_df



ggplot(tw_agg_df ) +
  geom_line(aes(ymd, num_tweet)) +
#  geom_line(aes(ymd, num_retweet)) +
  geom_point(aes(ymd, num_tweet)) +
  scale_x_date(date_labels = "%b %d") +
  geom_vline(xintercept = as_date(c("2017-03-23", "2017-03-26")) %>%
               as.numeric(),
             colour = "green", alpha = 0.8) 


(tw_agg_df$num_tweet %>% sum ) +
(tw_agg_df$num_retweet %>% sum)


