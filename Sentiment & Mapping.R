
# ========================================================================================================

setwd("C:/Users/sasha/Downloads/NTU NBS/Y2S1/bc2406/grp 4 project/Report Datasets")
war <- read.csv("geopoliticalevents.sasha.csv")

install.packages("tm")
install.packages("slam")
install.packages("syuzhet")
install.packages("NLP")
install.packages("sentimentr")

library(sentimentr)
library(syuzhet)
library(tm)
library(slam)
library(NLP)

#---------------------------------------------------------------------------------------------------------

# Combine the 'Headline' and 'Description' columns into a single 'Text' column
war$Text <- paste(war$Headline, war$Description, sep = " ")

# Specify the encoding as UTF-8 (or any other suitable encoding)
war$Text <- iconv(war$Text, to = "UTF-8", sub = "byte") 

# Create a Corpus from the cleaned 'Text' column
corpus <- Corpus(VectorSource(war$Text))


# Text Preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

inspect(corpus[8:15])

# Initialize an empty vector to store sentiment scores
sentiment_scores <- numeric(0)

# Perform sentiment analysis using VADER for each text
for (i in 1:length(corpus)) {
  text <- content(corpus[[i]])
  sentiment_score <- sentiment(text, method = "vader")$sentiment
  sentiment_scores <- c(sentiment_scores, sentiment_score)
}

# Adjusted the sentiment_to_severity_mapping function
sentiment_to_severity_mapping <- function(sentiment_score) {
  if (sentiment_score < -0.7) return(1)
  if (sentiment_score >= -0.7 && sentiment_score < -0.4) return(2)
  if (sentiment_score >= -0.4 && sentiment_score < -0.1) return(3)
  if (sentiment_score >= -0.1 && sentiment_score < 0.1) return(4)
  if (sentiment_score >= 0.1 && sentiment_score < 0.4) return(5)
  if (sentiment_score >= 0.4) return(6)
}


# Apply the mapping to get severity scores (same as before)
severity_scores <- sapply(sentiment_scores, sentiment_to_severity_mapping)

# Add the severity scores to the existing dataframe
war$SeverityIndex <- severity_scores


# Define a mapping for conflict type combinations
conflict_type_mapping <- c(
  "Trigger" = 0,
  "Weapon" = 1,
  "Casualty" = 2,
  "Political" = 3
)

# Use the mapping to create a new column with unique numeric values for ConflictTypes
war$ConflictTypeNumeric <- conflict_type_mapping[war$ConflictTypes]


#---------------------------------------------------------------------------------------------------------

# Specify the same path and filename where you originally read the CSV
output_file <- "C:/Users/sasha/Downloads/NTU NBS/Y2S1/bc2406/grp 4 project/Report Datasets/geopoliticalevents.sasha.csv"

# Write the dataframe to the same CSV file
write.csv(war, file = output_file, row.names = FALSE)


#---------------------------------------------------------------------------------------------------------

# End ==============================================================================
