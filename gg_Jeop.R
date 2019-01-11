#

link_text <- paste('http://www.j-archive.com/showgameresponses.php?game_id=',sample(x = c(1:9000),size = 1, replace = T),sep = '',collapse = '')

link <- read_html(link_text)

#link_text

###ROUND1###
correctData <-   link %>% 
  html_nodes('#jeopardy_round a , #jeopardy_round .right') %>%
  html_text() %>% 
  rollapply(data = ., 2, c) %>% 
  cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% 
          t %>% 
          data.frame) %>% 
  `colnames<-`(c("QuestionNumber", "Contestant", "NCX1","NCX2")) %>% 
  filter(., !grepl('[0-9][0-9]|[0-9]', Contestant)) %>% 
  select(QuestionNumber,Contestant)

tripStump <- data.frame(
  link %>% 
    html_nodes('#jeopardy_round table+ table .wrong , #jeopardy_round .clue_order_number') %>%
    html_text() %>% 
    rollapply(data = ., 2, c) %>% 
    cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% 
            t %>% 
            data.frame()) ) %>% filter(X1 <= 2, X2 > 2) %>% select(X1.1,X2.1) %>% 
  `colnames<-`(c("QuestionNumber", "Contestant")
  )

wrongData <- data.frame(
  link %>% 
    html_nodes('#jeopardy_round .clue_order_number , #jeopardy_round br+ table .wrong') %>%
    html_text() %>% 
    rollapply(data = ., 4, c) %>% 
    cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% 
            t %>% 
            data.frame()) 
) 

wrongData <- rbind(
  data.frame(
    link %>% 
      html_nodes('#jeopardy_round .clue_order_number , #jeopardy_round br+ table .wrong') %>%
      html_text() %>% 
      rollapply(data = ., 2, c) %>% 
      cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% 
              t %>% 
              data.frame()) 
  ) %>% filter(X1 <= 2, X2 > 2) %>% select(X1.1,X2.1) %>% 
    `colnames<-`(c("QuestionNumber", "Contestant")),
  wrongData %>% 
    filter(X1 <= 2, X2 > 2, X3 <= 2, X4 <= 2) %>% 
    select(X1.1,X2.1)  %>% 
    `colnames<-`(c("QuestionNumber", "Contestant")),
  
  rbind(
    wrongData %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 <= 2) %>% 
      select(X1.1,X2.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant")),
    wrongData %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 <= 2) %>% 
      select(X1.1,X3.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant"))
  ),
  
  rbind(
    wrongData %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 > 2) %>% 
      select(X1.1,X2.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant")),
    wrongData %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 > 2) %>% 
      select(X1.1,X3.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant")),
    wrongData %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 > 2) %>% 
      select(X1.1,X4.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant"))
  ),
  
  wrongData %>% 
    filter(X1 > 2, X2 <= 2, X3 <= 2, X4 > 2) %>% 
    select(X3.1,X4.1) %>% 
    `colnames<-`(c("QuestionNumber", "Contestant")),
  
  wrongData %>% 
    filter(X1 <= 2, X2 > 2, X3 <= 2, X4 > 2) %>% 
    select(X3.1,X4.1) %>% 
    `colnames<-`(c("QuestionNumber", "Contestant")),
  
  tripStump) %>% 
  unique() %>% 
  `rownames<-`(NULL)

d1 <- rbind(
  data.frame(correctData,Round = "Round 1",Response='RIGHT'),
  data.frame(wrongData,Round = "Round 1",Response='WRONG')
)

d1$QuestionNumber <- as.numeric(as.character(d1$QuestionNumber))
d1$Contestant <- as.character(d1$Contestant)

###Round 2###

correctData2 <-     link %>% 
  html_nodes('.clue_order_number a , #double_jeopardy_round .right') %>%
  html_text() %>% 
  rollapply(data = ., 2, c) %>% 
  cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% 
          t %>% 
          data.frame) %>% 
  `colnames<-`(c("QuestionNumber", "Contestant", "NCX1","NCX2")) %>% 
  filter(., !grepl('[0-9][0-9]|[0-9]', Contestant)) %>% 
  select(QuestionNumber,Contestant)

tripStump2 <- data.frame(
  link %>% 
    html_nodes('#double_jeopardy_round .clue_order_number a , #double_jeopardy_round table+ table .wrong') %>%
    html_text() %>% 
    rollapply(data = ., 2, c) %>% 
    cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% 
            t %>% 
            data.frame()) ) %>% filter(X1 <= 2, X2 > 2) %>% select(X1.1,X2.1) %>%  
  `colnames<-`(c("QuestionNumber", "Contestant"))

wrongData2 <- data.frame(
  link %>% 
    html_nodes('#double_jeopardy_round .clue_order_number a , #double_jeopardy_round br+ table .wrong') %>%
    html_text() %>% 
    rollapply(data = ., 4, c) %>% 
    cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% 
            t %>% 
            data.frame()) 
)

wrongData2 <- rbind(
  data.frame(
    link %>% 
      html_nodes('#double_jeopardy_round .clue_order_number a , #double_jeopardy_round br+ table .wrong') %>%
      html_text() %>% 
      rollapply(data = ., 2, c) %>% 
      cbind(., apply(X = ., MARGIN = 1., FUN = nchar) %>% 
              t %>% 
              data.frame()) 
  ) %>% filter(X1 <= 2, X2 > 2) %>% select(X1.1,X2.1) %>% 
    `colnames<-`(c("QuestionNumber", "Contestant")),
  wrongData2 %>% 
    filter(X1 <= 2, X2 > 2, X3 <= 2, X4 <= 2) %>% 
    select(X1.1,X2.1)  %>% 
    `colnames<-`(c("QuestionNumber", "Contestant")),
  
  rbind(
    wrongData2 %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 <= 2) %>% 
      select(X1.1,X2.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant")),
    wrongData2 %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 <= 2) %>% 
      select(X1.1,X3.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant"))
  ),
  
  rbind(
    wrongData2 %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 > 2) %>% 
      select(X1.1,X2.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant")),
    wrongData2 %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 > 2) %>% 
      select(X1.1,X3.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant")),
    wrongData2 %>% 
      filter(X1 <= 2, X2 > 2, X3 > 2, X4 > 2) %>% 
      select(X1.1,X4.1) %>% 
      `colnames<-`(c("QuestionNumber", "Contestant"))
  ),
  
  wrongData2 %>% 
    filter(X1 > 2, X2 <= 2, X3 <= 2, X4 > 2) %>% 
    select(X3.1,X4.1) %>% 
    `colnames<-`(c("QuestionNumber", "Contestant")),
  
  wrongData2 %>% 
    filter(X1 <= 2, X2 > 2, X3 <= 2, X4 > 2) %>% 
    select(X3.1,X4.1) %>% 
    `colnames<-`(c("QuestionNumber", "Contestant")),
  
  tripStump2) %>% 
  unique() %>% 
  `rownames<-`(NULL)

d2 <- rbind(
  data.frame(correctData2,Round = "Round 2",Response='RIGHT'),
  data.frame(wrongData2,Round = "Round 2",Response='WRONG')
)

d2$QuestionNumber <- as.numeric(as.character(d2$QuestionNumber))
d2$Contestant <- as.character(d2$Contestant)

#####

d1 <- data.table(d1)
d2 <- data.table(d2)

r1_cash <- link %>% html_nodes('#jeopardy_round .clue_header') %>% html_text(trim = T) %>% str_split(string = ., pattern = '\n            ') %>% do.call(rbind,.) %>% gsub(pattern = "[\\$,]",replacement = '') %>% data.frame %>% `colnames<-`(c('money', 'QuestionNumber')) %>% select(QuestionNumber, money)

r1_cash$QuestionNumber <- as.numeric(as.character(r1_cash$QuestionNumber))

r2_cash <- link %>% html_nodes('#double_jeopardy_round .clue_header') %>% html_text(trim = T) %>% str_split(string = ., pattern = '\n            ') %>% do.call(rbind,.) %>% gsub(pattern = "[\\$,]",replacement = '') %>% data.frame %>% `colnames<-`(c('money', 'QuestionNumber')) %>% select(QuestionNumber, money)

r2_cash$QuestionNumber <- as.numeric(as.character(r2_cash$QuestionNumber))

d1 <- inner_join(x = d1, y = r1_cash, by = 'QuestionNumber')

d2 <- inner_join(x = d2, y = r2_cash, by = 'QuestionNumber')

#####

finalJDATA <- rbind(d1,d2)
finalJDATA$Round <- as.factor(finalJDATA$Round)
finalJDATA$money <- as.numeric(as.character(finalJDATA$money))

levels(finalJDATA$Response) <- c(levels(finalJDATA$Response), "Triple Stumper")
finalJDATA$Response[finalJDATA$Contestant == 'Triple Stumper']  <- 'Triple Stumper' 

finalJDATA <- within(finalJDATA, money[Response=='WRONG'] <- ((money[Response=='WRONG'])*-1))  
 

#####

j_canvas <- ggplot(finalJDATA, aes(x = QuestionNumber, y = as.numeric(as.character(money)), color = Contestant, group = Round, shape = Response)) 

j_canvas + 
  geom_line(size=3,col='black') + 
  geom_point(size=8) + 
  facet_wrap(~Round, nrow = 2) + 
  scale_y_continuous(limits = c(-6000, 6000)) + 
  theme(
    plot.title = element_text(color="blue", size=30, face="bold.italic"),
    axis.title.x = element_text(color="black", size=15, face="bold"),
    axis.title.y = element_text(color="black", size=15, face="bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, size=12,color="black"),
    axis.text.y = element_text(angle = 90, hjust = 1, size=12,color="black")
  ) +
  labs(title = link %>%
                    html_node('h1 a') %>%
                    html_text() %>%
                    toupper(),
       subtitle = link_text) +
  xlab('Question Number') +
  ylab('Question Money')

#####

# list(link_text,
#      finalJDATA,
#   c(nrow(d1),link %>% html_nodes('#jeopardy_round .clue_text td') %>% length()),
#      c(nrow(d2),link %>% html_nodes('#double_jeopardy_round .clue_text td') %>% length())
#   )










