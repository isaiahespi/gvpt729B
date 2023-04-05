# R script following text analysis tutorial


install.packages('tidytext')
install.packages('pdftools')
install.packages('tm')
install.packages('wordcloud2')

# load these 
library(tidyverse)
library(rvest)
library(tidytext)
library(pdftools) #Text Extraction, Rendering and Converting of PDF Documents
library(tm)
library(broom)
library(wordcloud2)
library(devtools)


# this is the url that I first pulled from
https://www.nytimes.com/2023/02/21/us/politics/barbara-lee-senate-california.html

paragraphs <-
  read_html(
    "https://www.nytimes.com/2023/02/21/us/politics/barbara-lee-senate-california.html"
  ) %>%
  # html_elements refers to the HTML element on the webpage, e.g., <p>
  html_elements(".css-at9mc1") %>%
  
  # html_text returns the raw underlying text
  html_text()

paragraphs <-
  read_html(
    "https://www.nytimes.com/2023/02/21/us/politics/barbara-lee-senate-california.html"
  ) %>%
  html_elements(".css-at9mc1") %>% # this refers to the HTML element on the HTML webpage
  
  # html_text2() simulates how text looks in a browser, using an approach
  # inspired by JavaScript's innerText(). Roughly speaking, it converts ⁠<br />⁠
  # to "\n", adds blank lines around ⁠<p>⁠ tags, and lightly formats tabular
  # data
  html_text2()


# this creates a function 'scraper'
scraper <- function(url, elements) {
  #This reads the html of the url provided
  read_html(url) %>% 
    
    #This selects the specific node you're looking to scrape
    html_elements(elements) %>% 
    
    #This then scrapes the text from that note
    html_text() %>% 
    
    #This puts it into a data frame, col 1 = "id", col 2 = "text"
    enframe("id", "text") 
}


# this is using the 'scraper' function to scrape text from an article in the NYT
# This places all the elements into a dataframe, with each individual element
# placed in rows
article2 <- scraper(
  url = "https://www.nytimes.com/2023/02/21/us/homelessness-us-california.html",
  # this element seems specific to NYT article
  # NYT is usually pay-walled, so I first view the HTML page by disabling
  # javascript, then I inspect the elements I want
  elements = ".css-at9mc1") 

# view the data frame to determine what rows you want to keep
View(article2)


# I don't need any of the rows below row 12
# this simply empties out all the rows identified, replacing them with 'NA'
article2[13:26,] <- NA

# this function from the dplyr package 'tidyr::drop_na' 
# drops rows where any column specified by ... contains a missing value
article2 <- article2 %>% 
  tidyr::drop_na()

# this 'tokenizes' the text
# which means it separates the paragraphs into sentences and words
article2_tokenized <- article2 %>%
  
  # technically what this does is splits a column into tokens
  # flattening the table into one-token-per-row
  unnest_tokens(word, text)


# This removes the most common 'stop words' from the corpus
article2_sansstop <- article2_tokenized %>% 
  anti_join(stop_words,
            by = join_by(word))

# run this to see the top ten most frequent words
article2_sansstop %>% 
  count(word, sort = TRUE)  %>%
  head(n = 10)

# this is a barplot of the top ten most frequent words
article2_sansstop %>% 
  count(word, sort = TRUE) %>% 
  head(n = 10) %>% 
  ggplot(aes(x = word, y = n))+
  geom_col()


# this is only for the words that will appear in the word cloud
article2_words <- article2_sansstop %>% 
  count(word, sort = TRUE)


# This is just to render the word cloud
# from the wordcloud2 package
wordcloud2::wordcloud2(article2_words)


wordcloud2::wordcloud2(article2_words,
                       color = 'random-dark',
                       backgroundColor = "black",
                       shape = 'circle')

#####
## Rudimentary Sentiment analysis of text


# making another function with html_text2 instead
scraper2 <- function(url, elements) {
  read_html(url) %>%
    html_elements(elements) %>% 
    html_text2() %>% 
    enframe("id", "text") 
}

# article from NYT
nyt <- scraper2(
  url = "https://www.nytimes.com/2023/02/21/us/politics/barbara-lee-senate-california.html",
  elements = ".css-at9mc1"
)

# tokenize
nyt_tokenized <- nyt %>%
  unnest_tokens(word, text)


# This removes the most common 'stop words' from the corpus
# also, use filter to add in words to be removed
nyt_sansstop <- nyt_tokenized %>% 
  anti_join(stop_words,
            by = join_by(word))%>% 
  filter(!word %in% c("ms", "lee"))

# run this to see the top ten most frequent words
nyt_sansstop %>% 
  count(word, sort = TRUE)  %>%
  head(n = 10)

# this is a barplot of the top ten most frequent words
nyt_sansstop %>% 
  count(word, sort = TRUE) %>% 
  head(n = 10) %>% 
  ggplot(aes(x = word, y = n))+
  geom_col()


# this is only for the words that will appear in the word cloud
nyt_words <- nyt_sansstop %>% 
  count(word, sort = TRUE)


# This is just to render the word cloud
# from the wordcloud2 package
wordcloud2::wordcloud2(nyt_words,
                       color = 'random-dark',
                       backgroundColor = "black",
                       shape = 'circle')


# creating a sentiment data-frame
nyt_sentiment <- nyt_sansstop %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# bar plot of sentiments by word with positive and negative sentiment
nyt_sentiment %>%  
  ggplot(aes(x = word, y = sentiment))+
  geom_col()+
  coord_flip()







