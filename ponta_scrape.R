library(RSelenium)
library(magrittr)
library(tidyverse)

## Not run: 
# start a chrome browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$open()
remDr$navigate("https://twitter.com/bs_ponta")



#total tweets
tt = 346
# scroll down until all tweets are appeared
remDr$executeScript("scroll(0, 100000000);", args = list())


# tweet contents
wl =remDr$findElements(using = "xpath",
                   value = "//p[@class='TweetTextSize TweetTextSize--normal js-tweet-text tweet-text']")
length(wl) == tt

system.time({
  tw_content <- map(seq_along(wl), ~ wl[[.x]]$getElementText()) %>% unlist()
  })

length(tw_content) == tt

# tweet time
wl =remDr$findElements(using = "xpath",
                       value = "//a[@class='tweet-timestamp js-permalink js-nav js-tooltip']")
length(wl) == tt

tmp <- wl[[4]]$getElementAttribute("outerHTML")

tmp[[1]] %>%
  stringr::str_extract('[0-9]:(.+?)日')

guess_encoding(html[[1]])

tmp[[1]] %>% 
  html() %>% 
  html_attr(name = "data-original-itle")

library(rvest)
rvest::html_attrs(html[[1]])
?rvest::html()


wl[[12]]$getElementAttribute("data-original-title")

wl[[1]]$


system.time({
  tw_content <- map(seq_along(wl), ~ wl[[.x]]$getElementText()) 
})


library(magrittr)
wl






# stop the selenium server
rD[["server"]]$stop() 
