
# This script is attempting to separate email chains from attachments in the 
# FOIA documents.

library(pdftools)
library(stringr)
library(tesseract)

# Make sure the working directory is set in 'raw_data' folder
# setwd("russ_who_docs/raw_data")

# Load in data
foia_87 = "USTR_FY21-87_Release in Part_Redacted.pdf"
foia_93 = "USTR_FY21-93_Release in Part_Redacted.pdf"

# Use 'pdf_text()' to extract out the text blobs
foia_87text = pdf_text("../raw_data/USTR_FY21-87_Release in Part_Redacted.pdf")
foia_93text = pdf_text("../raw_data/USTR_FY21-93_Release in Part_Redacted.pdf")


# WORKED: Separate pdf pages by page numbers

# The function 'page_number_sep' takes in two arguments: the pdf and its extracted
# text blobs version after it goes through 'pdf_text()'. It goes through each text
# blob (page in the document) and assigns a number in the empty vector group()
# according to its page number. If it's the first page of a group of documents,
# a new number is assigned. If it's not, then the number assigned is the number of
# the previous page. The vector group() is then used to split up the text blobs
# so we get a list of groups of documents separated by page numbers.

# To account for pages where page numbers aren't picked up by 'pdf_text()', OCR is
# performed for those pages to extract their page numbers.

# The function is returning the splited-up documents and the group() vector that
# tells us on which pages a new document begins.

page_number_sep = function(x,x_text) {
  group = c()
  for (i in 1:length(x_text)) {
    if (str_detect(x_text[i], regex("(?<!\\d)\\d{1,2}\n$"))) {
      if (str_detect(x_text[i], regex("(?<!\\d)1\n$"))) {
        group[i] = i
      } else {
        group[i] = group[i-1]
      }
    } else {
      pagen = pdf_ocr_text(pdf = x, pages = i)
      if (str_detect(pagen, regex("(?<!\\d)\n?1\n$"))) {
        group[i] = i
      } else if (str_detect(pagen, regex("(?<!\\d)\\d{1,2}\n?$"))) {
        group[i] = group[i-1]
      } else { # in the case where OCR doesn't pick up the page numbers
        group[i] = i
      }
    }
  }
  result = split(x_text, group)
  return(list(result, group))
}

# Apply the function 'page_number_sep' to each pdf
foia_87_sep = page_number_sep(foia_87, foia_87text)
# Take out the group vector from the list save it as a vector for later use
group87 = unlist(foia_87_sep[2])

foia_93_sep = page_number_sep(foia_93, foia_93text)
group93 = unlist(foia_93_sep[2])

# Combine two pdfs into one
combined = c(foia_87_sep[[1]], foia_93_sep[[1]])


## WORKED: Separates emails from attachments

# This loop goes into each list created by splitting the text data in the previous
# loop and checks whether each page contains "From:", that is usually found in
# email chains. It then adds that chunk of text to either the email list or
# the attachment list.

# email_vec is a vector that contains TRUE(the document is an email) or
# FALSE(the document is an attachment). This is for later use when constructing
# the dataframe.

email_list = c()
attachment_list= c()
email_vec = c()
for (i in 1:length(combined)) {
  if (str_detect(combined[[i]][1], regex("From:", TRUE))==T) {
    email = combined[i]
    email_list = append(email_list, email)
    email_vec[i] = TRUE
  } else {
    attachment = combined[i]
    attachment_list = append(attachment_list, attachment)
    email_vec[i] = FALSE
  }
}


## WORKED: Extracts the titles of each email chain use them to name documents.

# Extract titles of email chains
# Create new vector titles_all() that separates email chain titles with attachments
titles_all = c()
for (i in 1:length(combined)) {
  if (email_vec[i] == T) {
    cleaned = str_replace_all(combined[[i]][1], "\\n|\\n\\s*", " ")
    cleaner = str_remove_all(cleaned,
                             "\\[EXTERNAL]\\s?|RE:\\s?|Re:\\s?|FW:\\s?|Fw:\\s?")
    titles_all[i] = str_extract(cleaner, ".+?(?=From:)")
  } else {
    titles_all[i] = "attachment"
  }
}

# This for loop creates a vector titles_vec() that names each document, based on
# whether it's an email chain or attachment. If the document is an attachment,
# then the loop will look whether the document before it is an email chain or another
# document. If it's not the first attachment after the email chain, the while loop
# will count how many positions it is behind its corresponding email chain and assigns
# the attachment that number.

titles_vec = c()
for (i in 1:length(titles_all)) {
  if (titles_all[i] == "attachment") { 
    if (str_detect(titles_all[i-1], "attachment")==F) {
      titles_vec[i] = paste(titles_vec[i-1], "_attachment_1", sep = "")
    } else if (str_detect(titles_all[i-1], "attachment")) {
      count = 0
      while (str_detect(titles_all[i-1], "attachment")) {
        count = count + 1
        i = i - 1
      }
      i = i + count # goes back to original position for indexing
      stripped = str_replace_all(titles_vec[i-1], "[:punct:]*\\d+$", replacement = "")
      titles_vec[i] = paste(stripped, "_", count+1, sep = "")
    }
  } else {
    titles_vec[i] = titles_all[i]
  }
}


## WORKED: Create ids for each group of documents
ID = paste("foia_", 1:length(combined), sep = "")


## WORKED: Create source vector for the source of each segmentation
source_87 = rep("USTR_FY21-87_Release in Part_Redacted.pdf", length(unique(group87)))
source_93 = rep("USTR_FY21-93_Release in Part_Redacted.pdf", length(unique(group93)))
source_vec = c(source_87, source_93)


## WORKED: Create start and end vectors

# unique() takes the unique numbers from group87 and group93, which are the start
# of a segmentation
start_vec = c(unique(group87), unique(group93))

# To obtain the ending page numbers, we take the page number of the start of a new
# segmentation and subtract by 1. We then remove the first entry and add the last
# page of the pdf.
end87 = unique(group87) -1
end87 = end87[-1] # remove first entry because it's 0
end87 = append(end87, length(group87)) # add the last page of the pdf
end93 = unique(group93) -1
end93 = end93[-1]
end93 = append(end93, length(group93))
end_vec = c(end87, end93)


## WORKED: Create text vector for data frame
text_col = c()
for (i in 1:length(combined)) {
  text_col[i] = paste(combined[[i]], collapse = "")
}


## Combine into a data frame foia_df
foia_df = data.frame(ID, titles_vec, source_vec, start_vec, end_vec, email_vec, text_col)
names(foia_df) = c("ID","name", "source", "start", "end", "email", "text")


## Create .rds file for foia_df in outputs folder
saveRDS(foia_df, "../outputs/foia_df.rds")
