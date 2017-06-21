library(tidyverse)
library(EBImage)


tw_df = read_rds("objects/tw_df.rds")

# scale image as 500*278
# change scale by factor of f

tw_df %>% 
  mutate(
    img_file = map_chr(id, ~ paste0("ponta_image/pi_", .x, ".jpeg")),
    img = map(img_file,
              ~ failwith(NA,
                         function(x) {
                           out <-  readImage(x)
                           colorMode(out) =  Grayscale # Gray color
                           out = resize(out, w = 5, h = 5) 
                           return(out)
                           })(.x)
              ),
    e_day = lubridate::today() - ymd
    ) -> tw_df



# exclude tweets without image
tw_df$img %>%
{which(!is.na(.))} -> not_na_row


tw_df$img[not_na_row] %>% 
  map(~ .x@.Data %>% as.vector) %>% 
  reduce(rbind) %>% 
  as_tibble() -> all_df

all_df <- bind_cols(tw_df[not_na_row, ] %>% select(num_retweet, e_day),
                    all_df)


write_rds(all_df, "objects/all_df.rds")

