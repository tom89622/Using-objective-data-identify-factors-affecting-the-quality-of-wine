rm(list=ls())
library(readr)
library(arules)
library(plyr)
library(shiny)
library(stringr)

setwd("/Users/z-renhong/Desktop/CYICE/senior/DataMining/finalProject") # please change this path with our file dir.
winequality_white <- read_delim("winequality-white.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) # read the dataset
winequality_red <- read_delim("winequality-red.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE)

View(winequality_red) # check the dataset
View(winequality_white)


#---------------------------white_frequent estimating---------------------------

whiteCopy <- winequality_white # copy the dataset
for ( col in colnames(whiteCopy) ) { 
  # pre-processing the dataset trying to process the dataset become relatively
  thisMedian <- median(whiteCopy[[col]])
  tmp <- ( whiteCopy[[col]]-thisMedian ) / ( max(whiteCopy[[col]])-thisMedian )
  print( thisMedian )
  process <- c()
  for ( index in tmp ) {
    if ( index >= 0 )
      process <- append( process, "1" )
    else
      process <- append( process, "-1" )
  }
  whiteCopy[[col]] <- process
}

# using apriori to find the frequent set
freq <- apriori( whiteCopy, parameter=list(supp=0.01, target="frequent",minlen=2))
freq=sort(freq,decreasing=T,by="support")
out <- cbind(labels = labels(freq), quality(freq))
result1<-out[str_detect(out$labels, "quality"), ]
result1<-result1[str_detect(result1$labels, "alcohol"), ]
nrow(result1)
result1[c(1:30),]


#---------------------------red_frequent estimating---------------------------

redCopy <- winequality_red
for ( col in colnames(redCopy) ) {
  # pre-processing the dataset trying to process the dataset become relatively
  # The purpose is easy to find the tendency of the dataset
  thisMedian <- median(redCopy[[col]])
  tmp <- ( redCopy[[col]]-thisMedian ) / ( max(redCopy[[col]])-thisMedian )
  print( thisMedian )
  process <- c()
  for ( index in tmp ) {
    if ( index >= 0 )
      process <- append( process, "1" )
    else
      process <- append( process, "-1" )
  }
  redCopy[[col]] <- process
}

# using apriori to find the frequent set
freq <- apriori( redCopy, parameter=list(supp=0.01, target="frequent",minlen=2))
freq=sort(freq,decreasing=T,by="support")
out <- cbind(labels = labels(freq), quality(freq))
result1<-out[str_detect(out$labels, "quality"), ]
result1<-result1[str_detect(result1$labels, "alcohol"), ]
nrow(result1)
result1[c(1:30),]
