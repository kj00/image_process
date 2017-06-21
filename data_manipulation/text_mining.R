#devtools::install_github("richfitz/remoji")

library(tidyverse)
library(stringr)
library(magrittr)
library(RMeCab)

tw_df = read_rds("objects/tw_df.rds")

tw_df$content %>%
  str_replace_all("\n|／|＼|", "") %>% 
  str_split(boundary("sentence")) %>% 
  map(~ str_replace_all(.x, "#.+[:space:]|#.+$", "") %>% 
        str_trim("both")) %>% 
   
  #絵文字や記号はNAになってしまう
  map(~ iconv(.x, from = "UTF-8")) %>%
  map(~ .x[.x != "" & !is.na(.x)]) %>% 
  
  # keitaiso decomposition
  at_depth(2, RMeCabC)

    
for (ii in seq_along(test)) {
  message(ii)
  map(test[[ii]], RMeCabC)
}


test[[64]] %>% .[3] 
  map(RMeCabC)
