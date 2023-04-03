# Russ WHO Docs Project

PI: Katheryn Russ

DataLab Lead: 
Arthur Koehl | avkoehl@ucdavis.edu

DataLab Team: 
Angel Tseng | hyitseng@ucdavis.edu
Catherine Cheng | ccacheng@ucdavis.edu

[Google Drive Link](https://drive.google.com/drive/folders/1ZHKQMK2Kdbp8DWCcqu345c9tHFQz5u2W?usp=sharing)

For communication: Slack Channel Name is startup_who_2022

### Background

The PI is interested in studying how industry lobbying affects global health policy.
They have collected 700 pages of documents through a Freedom of Information Act request.
The pages contain industry lobbying positions on measures proposed at the WHO. 
The PI would like to shed light into the contents of these positions, and use text
analysis methods to compare the content of those industry lobbying position documents with
the positions of the member governments at the World Health Assembly.

### Goals

1. Prepare the data for text analysis (can include segmenting, labeling, running ocr ...)
2. Perform Exploratory data analysis on the texts of the corpus
3. Explore the link between the industry documents and the policy documents

### Plan

Phase 1:

1. Create a manifest of documents and their metadata, would require codebook to link emails to attachments
2. Extract the text from the documents
3. Create a document-term matrix and preprocessing plan
4. Run an exploratory text report

If there is time left -

Phase 2:

1. Refine document segmentation and labeling
2. Explore nlp methods for comparing lobby positions with policy positions on an industry and country level

### File and Directory Structure

Data source: Documents that will serve as data sources come from Freedom of Information Act Request. Data includes lobbing documents such as email chains
and attachments and WHA Assembly position documents. Sources are downloaded onto local machines and any work is tracked through branches. 

```
README.md
.gitignore 
R/    		R source code 
raw_data/	Project Data (download from google drive folder)
outputs/	Data and Figures generated by the R code
```