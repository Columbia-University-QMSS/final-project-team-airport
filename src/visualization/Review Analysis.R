#################
# Airline Project
# Review Visualizations
# 06/27/2017
#################

library(tm)       
library(qdap)     
library(qdapDictionaries)
library(tidytext)
library(quanteda)
library(readxl)
Airlines_Review <- read_excel(".../.../data/raw/Airlines_Review.xlsx")
Airlines_Review<-as.data.frame(Airlines_Review)
Review <- Airlines_Review[,3]
review<- VectorSource(Review)
review_corpus <- VCorpus(review)
meta(review_corpus, type="local", tag="country") <- Airlines_Review$Author_country
meta(review_corpus, type="local", tag="class") <- Airlines_Review$Cabin_Flown
meta(review_corpus, type="local", tag="airline") <- Airlines_Review$airline

removeNumPunct <- function(x){gsub("[^[:alpha:][:space:]]*", "", x)}
#clean
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(replace_symbol))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, c(stopwords("english")))  
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(corpus, content_transformer(replace_number))
  corpus <- tm_map(corpus, content_transformer(removeNumPunct))
  return(corpus)
}
review_clean<-clean_corpus(review_corpus)
#stem
library(SnowballC)    
review_stemmed <- tm_map(review_clean, stemDocument)
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
review_comp <- lapply( review_clean, stemCompletion2, 
                       dictionary=review_clean)
#tdm
for(i in 1:1993){
  Airlines_Review$ID[i]<-i
}
Airlines_Review$ID<-as.character(Airlines_Review$ID)
review_comp_corp <- as.VCorpus(review_comp)
meta(review_comp_corp, type="local", tag="id")<-Airlines_Review$ID
review_tdm <- TermDocumentMatrix(review_comp_corp)
#td
library(dplyr)
library(tidytext)
review_td <- tidy(review_tdm)


review_tdd<- review_td %>%
  inner_join(Airlines_Review, by = c(document = "ID"))


library(ggplot2)
library(ggthemes)


library(wordcloud)
library(tidyr)
set.seed(2103)
RdBu <- brewer.pal(10, "RdBu")
set1 <- brewer.pal(5, "Set1")
cloud_tf <- review_tdd %>%
  group_by(airline,term) %>%
  summarise(num = sum(count))%>%
  spread(airline, num)

cloud_tf <- as.data.frame(cloud_tf)
rownames(cloud_tf) <- cloud_tf$term
cloud_tf <- cloud_tf[ , -1]
cloud_tf[is.na(cloud_tf)] <- 0


## Common words used by authors

commonality.cloud(cloud_tf, color = RdBu, max.words = 200)


## Different words used by authors

comparison.cloud(cloud_tf, color = set1, max.words = 200, title.size = 1)

# We can tell the most common things the passengers care about are plane, crew, seat, time and food. Comparing different airlines, delta have some positive words like good, great, nice, excellent. But the other two have negative words like old, late, delayed and cancelled.


##positive vs. negative analysis

pos <- read.table(".../.../data/raw/dictionaries/positive-words.txt", as.is=T)
neg <- read.table(".../.../data/raw/dictionaries/negative-words.txt", as.is=T)
sentiment <- function(words){
  require(quanteda)
  tok <- quanteda::tokenize(words)
  pos.count <- sum(tok[[1]]%in%pos[,1])
  neg.count <- sum(tok[[1]]%in%neg[,1])
  out <- (pos.count - neg.count)/(pos.count+neg.count)
  return(out)
}
for(i in 1:1993){
  Airlines_Review$senti[i]<-sentiment(Airlines_Review$Review[i])
}
Airlines_Review$citizen<-"US"
Airlines_Review$Author_country[is.na(Airlines_Review$Author_country)]<-"United States"
Airlines_Review$citizen[Airlines_Review$Author_country !="United States"]<-"non-US"
Airlines_Review$Cabin_Flown[is.na(Airlines_Review$Cabin_Flown)]<-"Economy"

ggplot(data=Airlines_Review,
       aes(x=airline, y=senti, color=citizen))  +geom_boxplot() +ylab("Auttitude")


# Delta airline have positive comments while the other two are more negative. Also US citizen are more likely to give negative comments. It might due to international filght service are better.

ggplot(data=Airlines_Review,
       aes(x=Cabin_Flown, y=senti, color=airline))  +geom_boxplot() +
  scale_x_discrete(limit=c("Economy","Premium Economy","Business Class","First Class")) + ylab("Auttitude")


# Overall, delta are better than the other two. Interestingly, people with economy seats are more likely to give negative feedbacks, while business and first class passengers mostly give positive comments.


# Regressive Imagery dictionary
# primordial / conceptual thinking
RID_dictionary <- dictionary(file=".../.../data/raw/dictionaries/RID.cat",
                             format = "wordstat")
review_corpus_corp<-corpus(review_corpus)
# make a dfm based on the dictionary
DTM_RIDdict <- dfm(review_corpus_corp, dictionary=RID_dictionary)
library(reshape2)
library(stringr)
RIDdf <- melt(as.matrix(DTM_RIDdict))
RIDdf$id <- str_sub(RIDdf$docs,5)
RIDdff<- RIDdf %>%
  inner_join(Airlines_Review, by = c(id = "ID"))


##Customer Sadness by Airlines

library(ggrepel)
# Has politics become more aggressive over time?
RIDdff$idd<-as.numeric(RIDdff$id)
ggplot(filter(RIDdff, features=="EMOTIONS.SADNESS._",value>0), 
       aes(x=idd, y=value, color=airline)) + geom_point() + 
  ylab("SAD")+xlab(NULL)  + theme_tufte() 

# American and United airline have more passengers with higher sadness level. Delta have only a few passengers with sadness level greater than 2.

# As a conclusion, Delta airline provide more pleasant flight experience than American and United airlines. International flight provide better service than domestic. Those suffer the most are economy class passengers. But delta on average still have above neutral comments from economy class. This might explain why delta tickets are always more expensice. But if you want a better flight experience, choose delta.
