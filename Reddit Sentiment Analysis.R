#climate change comments on reddit
#loading packages
library(rvest)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(textdata)
library(stringr)
library(wordcloud)
library(reshape2)

#reading article comments
article <- read_html("https://viterbischool.usc.edu/news/2020/01/sea-level-rise-could-reshape-the-united-states-trigger-migration-inland/") 

#getting article title
articletitle <- article%>%
  html_nodes("title")%>%
  html_text()

#viewing article title
articletitle

#getting text from the article webpage
articletxt <- article%>%
  html_nodes("p")%>%
  html_text()

#viewing article text  
articletxt

#reading opinions about the article from reddit webpage
climate_change <- read_html("https://www.reddit.com/r/science/comments/etue8q/climate_changedriven_sealevel_rise_could_trigger/")

#getting the title of the discussion
climate_change%>%
  html_node("title")%>%
  html_text()

#getting comments from the discussion
comments_data <-climate_change%>%
  html_nodes("p._1qeIAgB0cPwnLhDF9XSiJM")%>%
  html_text()

#changing the data to a vector
comments_data1 <- c(comments_data)

#making a vector from the article
articledf <- data.frame(line= 1, text= articletxt, stringsAsFactors = FALSE)
articledf

#making a data frame from comments 
commentdf <-  data.frame(id = 1:19, text = comments_data1, stringsAsFactors = FALSE)


#saving dataframe as a csv file
#write_csv(commentsdf, path ="/Users/sallymoywaywa/Documents/school work/text analytics/personal assignment /reddit comments.csv")

#tokenizing comments dataframe
comments_token <-commentdf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word,sort =TRUE)
  
#printing frequent comment tokens
comments_token

#tokenizing article dataframe
article_token <- articledf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)

#printing frequent comment tokens
article_token

#plotting token frequency histograms

#articles token frequency histogram
article_hist <- articledf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE) %>%
  filter(n > 4) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#printing the comment frequency token histogram
print(article_hist)

#comments token frequency histogram
comment_hist <- commentdf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE) %>%
  filter(n > 2) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#printing the comment frequency token histogram
comment_hist

#removing stop words
tidyarticle<- articledf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

tidycomments <-commentdf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

  
#combining the two data sets 
combo_data <- bind_rows(mutate(tidycomments, author="Comments"),
                        mutate(tidyarticle, author= "Article")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Comments`)

combo_data

#plotting the correlograms
ggplot(combo_data, aes(x=proportion, y=`Article`, 
                       color = abs(`Article`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Article", x=NULL)

#doing the corelation test
cor.test(data=combo_data[combo_data$author == "Comments",],
         ~proportion + `Article`)

#SENTIMENT ANALYSIS

#getting the sentiments
afinn<- get_sentiments("afinn")
nrc<- get_sentiments("nrc")
bing<- get_sentiments("bing")

#####ARTICLE

tidyarticle %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidyarticle %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

articlesentiment <-tidyarticle %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

articlesentiment  %>%
  group_by(sentiment) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


##### article word cloud

tidyarticle %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.5,0.5),
                   fixed.asp = TRUE,
                   title.size = 1)

tidyarticle %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(1.0,1.0),
                   fixed.asp = TRUE,
                   title.size = 1)

###sentiment analysis of the comments

tidycomments %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidycomments %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

commentsentiment <-tidycomments%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

commentsentiment  %>%
  group_by(sentiment) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



##### comments word cloud

tidycomments %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.5,0.5),
                   fixed.asp = TRUE,
                   title.size = 1)

tidycomments %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(1.0,1.0),
                   fixed.asp = TRUE,
                   title.size = 1)

####Bigrams

###Article
Article_bigram <- articledf %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)
  
Article_bigram

# sentiment analysis on the article bigram
articlesentiment2 <-Article_bigram %>%
  inner_join(get_sentiments("nrc"), by = c(word2 = "word")) %>%
  count(word1, word2, sentiment, sort=T) %>%
  ungroup()

articlesentiment2

### comments 
Comments_bigram <- commentdf %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

Comments_bigram

# sentiment analysis on the article bigram
commentsentiment2 <-Comments_bigram %>%
  inner_join(get_sentiments("nrc"), by = c(word2 = "word")) %>%
  count(word1, word2, sentiment, sort=T) %>%
  ungroup()

commentsentiment2



