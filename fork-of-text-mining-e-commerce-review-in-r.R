
library(qdap)
library(dplyr)
library(ggthemes)
library(RWeka)
library(reshape2)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)



setwd("/Users/saileshraturi/Desktop/wordcloudandassociation")
review = read.csv("Womens Clothing E-Commerce Reviews.csv")

##stringsAsFactors - logical argument (strings in a data frame should be treated as factor variables or as just plain strings. 
#For text mining, the characters are treated as strings  to use all the text mining techniques
#names(review)

## vector source to corpus
corpus_r = Corpus(VectorSource(review$Review.Text)) 
corpus_r=tm_map(corpus_r, tolower)
corpus_r=tm_map(corpus_r, removePunctuation)
corpus_r=tm_map(corpus_r, removeWords, stopwords("english"))
corpus_r=tm_map(corpus_r, removeWords,c("also", "get", "both","company", "just","made", "can", "im", "dress", "just", "i"))

## Stem document
corpus_r=tm_map(corpus_r, stemDocument)

##corpus content
corpus_r[[3]][1]

   ##------ Pie Chart ------##
# most frequent terms
term_freq <- freq_terms(corpus_r, 10)

Review_df <- term_freq %>%
arrange(desc(WORD)) %>%
mutate(prop = round(FREQ*100/sum(FREQ), 1),lab.ypos = cumsum(prop) - 0.5*prop)
head(Review_df, 4)

ggplot(Review_df, aes(x = "", y = prop, fill = WORD)) +
labs(x = NULL, y = NULL, fill = NULL, title = "Top 10 frequesnt used Words",caption="Source: Kaggle")+
geom_bar(width = 5, stat = "identity", color = "grey") +
geom_text(aes(y = lab.ypos, label = paste(round(prop), Sep=" %")), 
color = "white")+coord_polar("y", start = 0)+ggpubr::fill_palette("jco")+
theme_classic() +
theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

            ##------ Bar Chart ------##

term_freq <- freq_terms(corpus_r, 20)
ggplot(term_freq,aes(x=reorder(WORD, -FREQ),y =FREQ)) + 
geom_bar(fill = heat.colors(20),stat = "identity") +geom_text(aes(label = FREQ), vjust = -0.3) 


review_dtm <- DocumentTermMatrix(corpus_r)
review_tdm <- TermDocumentMatrix(corpus_r)

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# top 20 most common words
review_term_freq[1:20]

# barchart of the 20 most common words
barplot(review_term_freq[1:20],col = cm.colors(20),las = 2)


                    ####----WORDCLOUD------###

review_word_freq <- data.frame(term = names(review_term_freq),num = review_term_freq)
# Create a wordcloud for the values in word_freqs
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = "aquamarine")


# Print the word cloud with the specified colors
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = c("aquamarine","blue","red"))

# spliting dataset in Reccomendion in Yes & No

(review_yes <- review[review$Recommended.IND == 1, ])
(review_no <- review[review$Recommended.IND == 0, ])

## Combine both corpora: all reviews
all_pos <- paste(review_yes$Review.Text, collapse = "")

all_neg <- paste(review_no$Review.Text, collapse = "")
all_combine <- c(all_pos, all_neg)

## Creating corpus for combination
corpus_review_all=Corpus(VectorSource(all_combine)) 
## Pre-processing corpus - all

corpus_review_all=tm_map(corpus_review_all, tolower)
corpus_review_all=tm_map(corpus_review_all, removePunctuation)
corpus_review_all=tm_map(corpus_review_all, removeWords, stopwords("english"))
corpus_review_all=tm_map(corpus_review_all, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress","just","i"))
#Stem document
corpus_review_all=tm_map(corpus_review_all, stemDocument)

review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m=as.matrix(review_tdm_all)
colnames(all_m)=c("Yes","No")
#Sum rows and frequency data frame
review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term=names(review_term_freq_all), num = review_term_freq_all)
#Make commonality cloud
commonality.cloud(all_m, colors = "blue",  max.words = 50)
# Create comparison cloud
comparison.cloud(all_m,colors = c("green", "red"),max.words = Inf)
# Identify terms shared by both documents
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)
# calculate common words and difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)
top25_df <- data.frame(x = common_words[1:25, 1],y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))

# Make pyramid plot
#pyramid.plot(top25_df$x, top25_df$y, labels = top25_df$labels, main = "Words in Common", gap = 3500, space = 1.5, ppmar = c(4,2,4,2),laxlab = NULL,
#top.labels = c("Yes","Words","No"))

pyramid.plot(top25_df$x,top25_df$y, labels = top25_df$labels, top.labels = c("Yes","Words","No"))

review_tdm2 <- removeSparseTerms(review_tdm, sparse = 0.9)
hc <- hclust(d = dist(review_tdm2, method = "euclidean"), method = "complete")
plot(hc)


associations <- findAssocs(review_tdm, "fit", 0.06)
associations_df <- list_vect2df(associations)[, 2:3]
# Plot the associations_df values 
ggplot(associations_df, aes(y = associations_df[, 1])) + 
geom_point(aes(x = associations_df[, 2]), 
data = associations_df, size = 3) + 
ggtitle("Word Associations to 'fit'") + 
theme_gdocs()


