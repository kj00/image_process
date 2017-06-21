left_join(olx_game_df,
          tw_df,
          c("date" = "ymd")) %>%
  filter(wld %in% c("w", "l") & !is.na(num_retweet)) -> tmp


ggplot(tmp) + geom_point(aes(num_fav, num_retweet))

ggplot(tmp) + geom_point(aes(wld, num_retweet, colour = as.numeric(e_day)))

ggplot(tmp %>% group_by(wld) %>% mutate(nr_mean_wld = mean(num_retweet))) + 
  geom_histogram(aes(num_retweet, fill = as.numeric(e_day))) +
  geom_vline(aes(xintercept = nr_mean_wld, colour = wld)) + 
  facet_grid(wld~.) 

  
ggplot(tmp) + geom_point(aes(e_day %>% as.numeric, num_retweet))


ggplot(tmp) + geom_point(aes(diff_point, num_retweet))


## point difference
tmp %>%
  group_by(diff_point) %>%
  summarize(dp_count= n()) 


lm(num_retweet ~ log(as.numeric(e_day)) + wld, tmp) %>% summary

lm(num_retweet ~ log(as.numeric(e_day)) + factor(diff_point), tmp) %>% summary
lm(num_retweet ~ log(as.numeric(e_day)) + factor(diff_point), tmp) %>% coef %>% 
  .[-1:-2] %>%
  cbind(-12:10, .) %>% 
  plot(type = "l")
  
ggplot(tmp) + geom_point(aes(factor(diff_point), num_retweet))


## renpai, rensho

## sayonara



