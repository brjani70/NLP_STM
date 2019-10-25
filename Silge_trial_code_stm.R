## Set your working directory to where you've saved your primary input file.
setwd("C:\\Users\\brjani70\\Documents\\Documents from 2019 laptop\\Paul's Code")

## Load the tidyverse library as you'll need the functions that come with it. 
## Use install.packages if you get an error loading the tidyverse library.
library(tidyverse)

## Load the input data.
## Get rid of an columns you don't need.
## Use read_csv as opposed to read.csv because read_csv does a better job reading-in 
## data with the right class.
## I.e. your column of text will actually be read in as a character class as opposed to factor.
clean <- read_csv("cleaned_dataset.csv")
colnames(clean)
clean <- clean[-1]
summary(clean)

## Checking to see if the text column was read in properly - it should say 'character'.
class(clean$CleanBody)

## Load tidytext library for the functions necessary for cleaning your text.
## Again use install.packages if you get an error loading this library.
library(tidytext)

## The following code tokenizes the words in all the CleanBody text brought in from the input data.
## "tokenize" = convert each word into a separate column. 

## %>% is a piping operator: often times functions will require 
## you to enter your dataset's name as a reference to the dataset to 
## which you are cleaning, changing, making modifications, etc.  
## For instance, without 'clean %>% ', you would need to manually enter the dataset name as the 1st 
## argument of the unnest_tokens function.
## "unnest_tokens" is creating a column called 'word' and for each document it creates a separate 
## row for each unique word it finds per document.
## "anti_join" removes stop words, like 'the', 'but', 'a', and so on.
tidy_clean <- clean %>% 
  unnest_tokens(word, CleanBody) %>%
  anti_join(stop_words)

## The following code is just exploratory analysis to see what the most common words are in our 
## corpus after cleaning it.
## This may be useful if you notice other extremely common words that may not be too informative 
## when topic modeling.
tidy_clean %>% 
  count(word, sort = TRUE)

## Load quanteda library for the functions necessary for converting your DTM into a sparse matrix. 
## Just think of a sparse matrix as an efficient way to store matrix data in your computer.
## Again use install.packages if you get an error loading this library.
## Load the stm package to run the stm...yup, not much else to say here.
library(quanteda)
library(stm)

## The count function in the block below is helping to create the initial DTM, while cast_sparse 
## converts the DTM into a sparse matrix so that it can serve as an input for the actual STM.
clean_sparse <- tidy_clean %>% 
  count(Doc_Id, word, sort = TRUE) %>% 
  cast_sparse(Doc_Id, word, n)


## Running the actual STM function, make sure to set the seed to whatever number you want 
## - that way if you want to reproduce your results, you can use that very same seed.

## Other arguments in the function:

## 1. "K", this is the number of topic you wish for the STM function to identify from your corpus.
## Determining the K isn't something that is done by default, you as the researcher must choose 
## a number. You can determine the number multiple ways, but you can follow through an example 
## outlined in another R file titled "____" which is located __________.

## 2. "verbose" is just whether or not you want text to show up in the console that lets you know 
## the progress of the STM process.

## 3. "init.type" you should just leave as Spectral - the specifics of this are lost to me but it 
## is recommended that you use this.
topic_model <- stm(clean_sparse, K = 90, verbose = TRUE, init.type = "Spectral", seed = 123)

## The tidy function allows us to convert the STM model results into something that we can more 
## easily view, either in tabular format or graphically.
td_beta <- tidy(topic_model)

## This block of code creates a table that shows the top 10 words associated with each of the 90 
## topics generated from your corpus. By 'top' I mean the words will be displayed based off of 
## their probabilities of showing up in a particular topic, also known as their 'betas'.
td_beta_new <- td_beta %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  group_by(term) %>% 
  arrange(topic, beta)

## This block of code creates a table that shows the top 3 topics associated with each document 
## in your corpus based off of each topic's prevalence in the document, otherwise known as 
## 'gamma' via this function (or 'theta' in other literature).
td_gamma <- tidy(topic_model, matrix = "gamma", document_names = rownames(clean_sparse))
td_gamma_new <- td_gamma %>% 
  group_by(document) %>% 
  top_n(3, gamma) %>% 
  ungroup() 

## Save a copy of the actual STM model that was trained/fit on your corpus so you can refer back to it if you didn't set a seed
save(topic_model, file = "Silge_topic_model1.zip", compress = "xz", compression_level = 1)
write.csv(td_gamma_new, "Silge1_doc_topic.csv")
write.csv(td_beta_new, "Silge1_topic_word.csv")
