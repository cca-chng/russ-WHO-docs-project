
## This script is used for creating document feature matrix and some exploratory
## analyis using the foia_df data frame created in separate_attachments.R.

library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(stringi)
library(stringr)
library(ggplot2)

# Load foia_df
foia = readRDS("russ_who_docs/outputs/foia_df.rds")

## Begin some preprocesing before creating the document feature matrix
# Remove frequent words found in emails
foia$text = str_remove_all(foia$text, "\\[EXTERNAL]\\s?|RE:\\s?|Re:\\s?|FW:\\s?|Fw:\\s?")

# Set all text data to lower case
foia$text = char_tolower(foia$text)

# Remove stopwords, digits, punctuation, symbols
tokens_object = tokens(foia$text, remove_punct = T, remove_numbers = T, remove_symbols = T)
tokens_object = tokens_remove(tokens_object, stopwords("en"))
tokens_object = tokens_select(tokens_object, min_nchar = 3) # Not letters

## Create document feature matrix of all documents
foia_dfm = dfm(tokens_object)
head(foia_dfm) # check to see if the results look alright

# Summary of document lengths
summary(tokens_object)

# Vocabulary size
# Vocabulary size for each text
ntype(tokens_object) # using ntype()

# Vocabulary size for the dfm
ncol(foia_dfm) # using the number of columns of the dfm

# Top 50 most frequent words
mostfreq_50 = textstat_frequency(foia_dfm, n = 50)

# Plots
# Wordcloud
png("russ_who_docs/outputs/wordcloud.png") # write out the path
textplot_wordcloud(foia_dfm, max_words = 50)
dev.off() # Run dev.off() again in case the plot isn't saved

# Rank and frequency
# Sort by reverse frequency order
mostfreq_50$feature <- with(mostfreq_50, reorder(feature, -frequency))

ggplot(mostfreq_50, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Feature", y = "Frequency",
       title = "Top 50 Most Frequent Words for All Documents")

ggsave("russ_who_docs/outputs/frequency.png", width = 6, height = 6, scale = 1)


## Email chains

# Subset for email chains
email_dfm = foia_dfm[foia$email==T,]
head(email_dfm) # check to see if the results look alright

# Summary of document lengths for emails
summary(tokens_object[foia$email==T,])

# Vocabulary size
# Vocabulary size for each email chain
ntype(tokens_object[foia$email==T,]) # using ntype()

# Vocabulary size for the dfm
ncol(email_dfm) # using the number of columns of the dfm

# Top 50 most frequent words
mostfreq_50_email = textstat_frequency(email_dfm, n = 50)

# Plots
# Wordcloud
png("russ_who_docs/outputs/email_wordcloud.png")
textplot_wordcloud(email_dfm, max_words = 50)
dev.off() # Run it twice if plot isn't saved

# Rank and frequency
# Sort by reverse frequency order
mostfreq_50_email$feature <- with(mostfreq_50_email, reorder(feature, -frequency))

ggplot(mostfreq_50_email, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Feature", y = "Frequency",
       title = "Top 50 Most Frequent Words for Emails")

ggsave("russ_who_docs/outputs/email_freq.png", width = 6, height = 6, scale = 1)

## Attachments
# Subset for attachments
attach_dfm = foia_dfm[foia$email==F,]
head(attach_dfm) # check to see if the results look alright

# Summary of document lengths for attachments
summary(tokens_object[foia$email==F,])

# Vocabulary size
# Vocabulary size for each attachment
ntype(tokens_object[foia$email==F,]) # using ntype()

# Vocabulary size for the dfm
ncol(attach_dfm) # using the number of columns of the dfm

# Top 50 most frequent words
mostfreq_50_attach = textstat_frequency(attach_dfm, n = 50)

# Plots
# Wordcloud
png("russ_who_docs/outputs/attachment_wordcloud.png")
textplot_wordcloud(attach_dfm, max_words = 50)
dev.off()

# Rank and frequency
# Sort by reverse frequency order
mostfreq_50_attach$feature <- with(mostfreq_50_attach, reorder(feature, -frequency))

ggplot(mostfreq_50_attach, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Feature", y = "Frequency", 
       title = "Top 50 Most Frequent Words for Attachments")

ggsave("russ_who_docs/outputs/attach_freq.png", width = 6, height = 6, scale = 1)


