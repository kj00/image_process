library(RSelenium)
library(magrittr)
library(tidyverse)
library(stringr)
library(lubridate)


### prepare selenium =========================================================

## start a chrome browser
## check if you installed java
rD <- rsDriver(verbose = T, browser = "firefox")
remDr <- rD[["client"]]
remDr$navigate("https://twitter.com/search-home")

# hash tag search :Ponta Pontacafe Ponjour ポンタ ポンタカフェ ポンジュール
# language = ja
# until:2017-06-25


for (ii in 1:400) {
  remDr$executeScript("scroll(0, 100000000);", args = list())
  Sys.sleep(1)
}




## tweet contents
system.time({
wl = remDr$findElements(using = "xpath",
                        value = "//div[@class='js-tweet-text-container']")
})
system.time({
  tw_content <-
    map(seq_along(wl), ~ wl[[.x]]$getElementText()) %>% unlist()
})


## account name
## you must check account name of the first tweet
wl = remDr$findElements(using = "xpath",
                        value = "//div[@class='stream-item-header']")


system.time({
  tw_account <-
    map(seq_along(wl), ~ wl[[.x]]$getElementText()) %>% unlist()
})



## tweet language
wl = remDr$findElements(using = "xpath",
                        value = "//p[@class='TweetTextSize  js-tweet-text tweet-text']")

system.time({
  tw_lang <-
    map(seq_along(wl), ~ wl[[.x]]$getElementAttribute("lang")) %>% unlist()
})


## tweet time
wl = remDr$findElements(using = "xpath",
                        value = "//a[@class='tweet-timestamp js-permalink js-nav js-tooltip']")

# extract HTML source of elements.
# getAlementAttribute("data-origin-time") doesen't work,
# probably due to back slash in the source: \...\
system.time({
  tw_time <-
    map(seq_along(wl), ~ wl[[.x]]$getElementAttribute("outerHTML"))
})

## data time ms

# extract time from html
# tw_time %>%
#   unlist() %>%
#   stringr::str_extract('[0-9]:(.+?)日') -> tw_time_1
#
# tw_time %>%
#   unlist() %>%
#   stringr::str_extract('[0-9]:(.+?)日') -> tw_time_2



## number of retweets
wl = remDr$findElements(using = "xpath",
                        value = "//button[@class='ProfileTweet-actionButton  js-actionButton js-actionRetweet']")

system.time({
  tw_retweet <- map(seq_along(wl), ~ wl[[.x]]$getElementText())
})

tw_retweet %>%
  unlist() %>%
  stringr::str_replace(",", "") %>%
  stringr::str_extract("[0-9]+") %>%
  as.numeric() -> tw_retweet


## number of facorite
wl = remDr$findElements(using = "xpath",
                        value = "//button[@class='ProfileTweet-actionButton js-actionButton js-actionFavorite']")

system.time({
  tw_fav <- map(seq_along(wl), ~ wl[[.x]]$getElementText())
})

tw_fav %>%
  unlist() %>%
  stringr::str_replace(",", "") %>%
  stringr::str_extract("[0-9]+") %>%
  as.numeric() -> tw_fav

## image source
wl = remDr$findElements(using = "xpath",
                        value = "//div[@class='content']")

system.time({
  img_source <-
    map(seq_along(wl), ~ wl[[.x]]$getElementAttribute("outerHTML"))
})


img_source %>%
  unlist() %>%
  stringr::str_extract('src=\"https://pbs.twimg.com/media.+jpg') %>%
  stringr::str_replace('^src=\"', "") -> img_source



## geo information
wl = remDr$findElements(using = "xpath",
                        value = "//a[@class='js-nav u-textUserColorHover js-geo-pivot-link']")

system.time({
  tw_geo <- map(seq_along(wl), ~ list(wl[[.x]]$getElementText() %>% unlist,
                                      wl[[.x]]$getElementAttribute("data-place-id") %>% unlist,
                                      wl[[.x]]$getElementAttribute("href") %>% unlist)
  )
})

tw_fav %>%
  unlist() %>%
  stringr::str_replace(",", "") %>%
  stringr::str_extract("[0-9]+") %>%
  as.numeric() -> tw_fav


#=========================================================================

tw_tmp <- list(
  "content" = tw_content,
  "account" = tw_account,
  "lang" = tw_lang,
  "time" =  tw_time,
  "num_retweeted" =  tw_retweet,
  "num_faved" = tw_fav,
  "img_source" = img_source,
  "geo_inf" = tw_geo
)




# 3/13まで
saveRDS(tw_tmp,
        paste0("scraped/tw_hashtag_byhand_2.rds"))


map(tw_tmp, length)
# ### save df
# tw_df <- tibble(
#   id = seq_along(wl),
#   hs = tw_time %>%
#     str_split(" - ") %>%
#     map_chr(~.[[1]]) %>%
#     hm(),
#   ymd = tw_time %>%
#     str_split(" - ") %>%
#     map_chr(~.[[2]]) %>%
#     ymd(),
#   content = tw_content,
#   num_retweet = tw_retweet,
#   num_fav = tw_fav,
#   img_source = img_source
# )
#
# saveRDS(tw_df, "tw_df.rds")
