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


### prepare loop =============================================================

# 検索条件
# https://twitter.com/search-advanced
# 次のキーワードのいずれかを含む
# Ponta ポンタ Ponjour ポンジュール ポンタカフェ
# 日本語
# 2017-03-01 ~ 2017-04-30
# url : https://twitter.com/search?l=ja&q=Ponta%20OR%20%E3%83%9D%E3%83%B3%E3%82%BF%20OR%20Ponjour%20OR%20%E3%83%9D%E3%83%B3%E3%82%B8%E3%83%A5%E3%83%BC%E3%83%AB%20OR%20%E3%83%9D%E3%83%B3%E3%82%BF%E3%82%AB%E3%83%95%E3%82%A7%20since%3A2017-03-01%20until%3A2017-04-30&src=typd
# Ponta OR ポンタ OR Ponjour OR ポンジュール OR ポンタカフェ since:2017-03-01 until:2017-04-30



#common_search_word <- "Ponta OR ポンタ OR Ponjour OR ポンジュール OR ポンタカフェ"
since <- "2017-03-01"
period_len <- 60
period <- as.Date(0:period_len , origin = as.Date(since))
period_set <- list(period[-(period_len + 1)],
                   period %>% lead(1) %>% .[-(period_len + 1)])

common_url <-
  "https://twitter.com/search?f=tweets&q=Ponta%20OR%20%E3%83%9D%E3%83%B3%E3%82%BF%20OR%20Ponjour%20OR%20%E3%83%9D%E3%83%B3%E3%82%B8%E3%83%A5%E3%83%BC%E3%83%AB%20OR%20%E3%83%9D%E3%83%B3%E3%82%BF%E3%82%AB%E3%83%95%E3%82%A7%20OR%20Pontacafe%20since%3A"


map2_chr(period_set[[1]],
         period_set[[2]],
         ~ paste0(.x,
                  "_17%3A00%3A00_JST%20until%3A",
                  .y,
                  "_17%3A00%3A00_JST&src=typd")) %>%
  paste0(common_url, .) -> loop_url

#ii <- 1

#==============================================================================

for (ii in seq_along(loop_url)) {
  remDr$navigate(loop_url[ii])
  
  ## scroll down until all tweets are appeared
  
  for (iii in 1:320) {
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
  
  
  ## number of favorite
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
  
  saveRDS(tw_tmp,
          paste0("for_bc/object/tw_list_",
                 period_set[[1]][ii],
                 "_",
                 period_set[[2]][ii],
                 ".rds"))
  
  message(paste0(round(ii / period_len), "%"))
}





tw_tmp$time %>% unlist %>% head




