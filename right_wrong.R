library(zoo)
library(plyr)
library(dplyr)

a <- read_html('http://www.j-archive.com/showgameresponses.php?game_id=6187')

rbind(
cbind(a %>% 
  html_nodes('#jeopardy_round a , #jeopardy_round .right') %>%
  html_text() %>% rollapply(data = ., 2, c) %>% cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% t %>% data.frame()) %>% `colnames<-`(c("X1", "X2", "NCX1","NCX2")) %>% 
  filter(., !grepl('[0-9][0-9]|[0-9]', X2)) %>% select(X1,X2),1)
,
a %>% 
  html_nodes('.clue_order_number a , #double_jeopardy_round .wrong , #jeopardy_round .wrong') %>%
  html_text() %>% rollapply(data = ., 3, c) %>% cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% t %>% data.frame()) %>% `colnames<-`(c("X1", "X2","X3", "NCX1","NCX2","NCX3")) %>% filter(NCX1 <= 2 & NCX2 > 3 ) %>% select(X1,X2)
,

a %>% 
  html_nodes('.clue_order_number a , #double_jeopardy_round .wrong , #jeopardy_round .wrong') %>%
  html_text() %>% rollapply(data = ., 3, c) %>% cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% t %>% data.frame()) %>% `colnames<-`(c("X1", "X2","X3", "NCX1","NCX2","NCX3")) %>% filter(NCX1 <= 2 & NCX2 > 3 & NCX3 > 2) %>% 
  select(X1,X3) %>% `colnames<-`(c("X1", "X2"))
)
