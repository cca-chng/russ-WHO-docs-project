
## This script preprocess the email attachments of the FOIA documents using
## the foia_df data frame. The end result is a token object of the email
## attachments and its document feature matrix.

library(quanteda)
library(quanteda.textstats)
library(stringi)
library(stringr)
library(hunspell)

# Load foia_df
foia = readRDS("russ_who_docs/outputs/foia_df.rds")

## Begin some preprocesing before creating the document feature matrix
# Remove frequent words found in emails
foia$text = str_remove_all(foia$text, "\\[EXTERNAL]\\s?|RE:\\s?|Re:\\s?|FW:\\s?|Fw:\\s?")

# Set all text data to lower case
foia$text = char_tolower(foia$text)

# Remove stopwords, digits, punctuation, symbols, select tokens of at least 3
# characters
tokens_object = tokens(foia$text, remove_punct = T, remove_numbers = T, remove_symbols = T)
tokens_object = tokens_remove(tokens_object, stopwords("en"))
tokens_object = tokens_select(tokens_object, min_nchar = 3) # Not letters

# Obtain a list of bigrams from tokens object
colls = textstat_collocations(tokens_object)
bigrams = colls$collocation[1:50]

# Replace tokens in the bigrams list
tokens_object = tokens_compound(tokens_object, phrase(bigrams))

# Filter out non-English attachments
attachments_tokens = tokens_object[foia$email==F,]

result = sapply(attachments_tokens, hunspell_check, dict = "en_US")
sums = sapply(result, sum)
ratio = sums/ntoken(attachments_tokens)
plot(ratio)
names(head(sort(ratio), 3))

attachments_tokens = tokens_subset(attachments_tokens, 
                    subset = names(attachments_tokens) != "text120")
attachments_tokens = tokens_subset(attachments_tokens, 
                                   subset = names(attachments_tokens) != "text121")
attachments_tokens = tokens_subset(attachments_tokens, 
                                   subset = names(attachments_tokens) != "text141")

## Create document feature matrix of attachments
attachments_dfm = dfm(attachments_tokens)
tail(attachments_dfm, 10) # check to see if the results look alright

# Save the attachment_token object
saveRDS(attachments_tokens, "russ_who_docs/outputs/attachments_tokens")
