---
  title: "PredictionModel"
author: "Steven Wang"
date: "Feb 26, 2021"
output: html_document
---
  
  #Load the data into R
  ```{R,warning=FALSE,message=FALSE}
library(NLP)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(slam)
library(wordcloud)

# Download the zip file, unzip it and load the target data into R

setwd("C:/Users/Steve/Desktop/Coursera Data Analysis/Data Science Capstone/DataAnalysisFinalproject")
if(!file.exists("./data")){
  dir.create("./data")
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",destfile = "Coursera-SwiftKey.zip",mode = "wb")
  
  unzip(zipfile = "./data/Coursera-SwiftKey.zip",exdir = "./data")
}

#Check if the data is downloaded and unzipped
dir("./Coursera-SwiftKey/final/",recursive = TRUE)

#Load the data into R
twitterraw<-readLines("./Coursera-Swiftkey/final/en_US/en_US.twitter.txt",encoding = "UTF-8",skipNul = TRUE)
blograw<-readLines("./Coursera-Swiftkey/final/en_US/en_US.blogs.txt",encoding = "UTF-8",skipNul = TRUE)
newsraw<-readLines("./Coursera-Swiftkey/final/en_US/en_US.news.txt",encoding = "UTF-8",skipNul = TRUE)

#Sample size is too large to be analysed directly, collect a proportion of original sample as new sample
sampletext<-function(file,proportion){
  collect<-sample(1:length(file),length(file)*proportion)
  sampletext<-file[collect]
  sampletext
}

#Collecting samples from each file and combine them
twisample<-sampletext(twitterraw,1/500)
blogsample<-sampletext(blograw,1/500)
newssample<-sampletext(newsraw,1/500)
allsample<-c(twisample,blogsample,newssample)

#Save the sample text into document
writeLines(allsample,"./allsample/allsample.txt")
```

##Data Cleaning
```{R,warning=FALSE,message=FALSE}

#Build the function of data cleaning

cleandata<-function(text){
  text<-tm_map(text,tolower)
  text<-tm_map(text,removeNumbers)
  text<-tm_map(text,removePunctuation)
  text<-tm_map(text,stripWhitespace)
  text<-tm_map(text,removeWords,stopwords(kind = "en"))
  text
}

#Clean the collected sample data

allsample<-VectorSource(allsample)
allsamplecorpus<-Corpus(allsample)
ASclean<-cleandata(allsamplecorpus)
```

##Build up the Ngram Function and perform analysis
```{R,warning=FALSE,message=FALSE}

#Build up the Ngram function 

TDM_ngram<-function(file,n){
  NGramTokenizer<-function(x){RWeka::NGramTokenizer(x,RWeka::Weka_control(min=n, max=n))}
  tdm_ngram<-TermDocumentMatrix(file,control=list(tokenizer=NGramTokenizer))
  tdm_ngram
}

##Function to transform the Ngram file into data frame

TDM_ngram_DF<-function(TDM_ngram){
  TDM_ngram_matrix<-as.matrix(TDM_ngram)
  TDM_ngram_DF<-as.data.frame(TDM_ngram_matrix)
  colnames(TDM_ngram_DF)<-"count"
  TDM_ngram_DF<-TDM_ngram_DF[order(-TDM_ngram_DF$count),,drop=FALSE]
  TDM_ngram_DF
}

#Calculate Ngram models and transform them to data frame

#Build up 1~4 Ngram Models
TDM_ngram1<-TDM_ngram(ASclean,1)
TDM_ngram2<-TDM_ngram(ASclean,2)
TDM_ngram3<-TDM_ngram(ASclean,3)
TDM_ngram4<-TDM_ngram(ASclean,4)

#Transform the ngram model into data frame
ngram1_DF<-TDM_ngram_DF(TDM_ngram1)
ngram2_DF<-TDM_ngram_DF(TDM_ngram2)
ngram3_DF<-TDM_ngram_DF(TDM_ngram3)
ngram4_DF<-TDM_ngram_DF(TDM_ngram4)
```

##Save the Data frames to Independent R files for Shinyapp usage

```{R}
#4 
quadgram<-data.frame(rows=rownames(ngram4_DF),count=ngram4_DF$count)
quadgram$rows<-as.character(quadgram$rows)
quadgram_split<-strsplit(as.character(quadgram$rows),split = " ")
quadgram<-transform(quadgram,first=sapply(quadgram_split,"[",1),second=sapply(quadgram_split,"[",2),third=sapply(quadgram_split,"[",3),fourth=sapply(quadgram_split,"[",4))
quadgram<-data.frame(unigram=quadgram$first,bigram=quadgram$second,trigram=quadgram$third,quadgram=quadgram$fourth,freq = quadgram$count,stringsAsFactors = FALSE)
write.csv(quadgram[quadgram$freq > 1,],"./allsample/quadgram.csv",row.names = FALSE)
quadgram<-read.csv("./allsample/quadgram.csv",stringsAsFactors = FALSE)
saveRDS(quadgram,"./Rdata/quadgram.RData")

#3
trigram<-data.frame(rows=rownames(ngram3_DF),count=ngram3_DF$count)
trigram$rows<-as.character(trigram$rows)
trigram_split<-strsplit(as.character(trigram$rows),split = " ")
trigram<-transform(trigram,first=sapply(trigram_split,"[",1),second=sapply(trigram_split,"[",2),third=sapply(trigram_split,"[",3))
trigram<-data.frame(unigram=trigram$first,bigram=trigram$second,trigram=trigram$third,freq = trigram$count,stringsAsFactors = FALSE)
write.csv(trigram[trigram$freq > 1,],"./allsample/trigram.csv",row.names = FALSE)
trigram<-read.csv("./allsample/trigram.csv",stringsAsFactors = FALSE)
saveRDS(trigram,"./Rdata/trigram.RData")

#2
bigram<-data.frame(rows=rownames(ngram2_DF),count=ngram2_DF$count)
bigram$rows<-as.character(bigram$rows)
bigram_split<-strsplit(as.character(bigram$rows),split = " ")
bigram<-transform(bigram,first=sapply(bigram_split,"[",1),second=sapply(bigram_split,"[",2))
bigram<-data.frame(unigram=bigram$first,bigram=bigram$second,freq = bigram$count,stringsAsFactors = FALSE)
write.csv(bigram[bigram$freq > 1,],"./allsample/bigram.csv",row.names = FALSE)
bigram<-read.csv("./allsample/bigram.csv",stringsAsFactors = FALSE)
saveRDS(bigram,"./Rdata/bigram.RData")
```

## Build up shiny app with 
```{R}
install.packages("shiny")
library(shiny)
library(tm)
install.packages("stringr")
library(stringr)

message<-" "
#Clean the input data
prdct<-function(x){
  cleanx<-removeNumbers(removePunctuation(tolower(x)))
  xsplit<-strsplit(cleanx, " ") [[1]]
  
  if(length(xsplit)>= 3){
    xsplit<-tail(xsplit,3)
    if(identical(character(0),head(quadgram[quadgram$unigram == xsplit[1] & quadgram$bigram == xsplit[2] & quadgram$trigram == xsplit[3],4],1))){
      prdct(paste(xs[2],xs[3], sep = " "))
    }else {message<-"Next word predicted with 4 gram"; head(quadgram[quadgram$unigram == xsplit[1] & quadgram$bigram == xsplit[2] & quadgram$trigram == xsplit[3],4],1)}
  }
  else if(length(xsplit) == 2){
    xsplit<-tail(xsplit,2)
    if(identical(character(0),head(trigram[trigram$unigram == xsplit[1] & trigram$bigram== xsplit[2],3],1))){
      prdct(xsplit[2])
    }
    else{message<-"Next word predicted with 3 gram";head(trigram[trigram$unigram ==xsplit[1] & trigram$bigram== xsplit[2],3],1)}
  }
  else if(length(xsplit)==1){
    xsplit<-tail(xsplit,1)
    if(identical(character(0),head(bigram[bigram$unigram == xsplit[1],2],1))){
      message<-"No match"
    }else{message<-"Next word predicted with 2 gram";head(bigram[bigram$unigram==xsplit[1],2],1)}
  }
}


#Build Shiny server

shinyServer(function(input,output){
  output$prediction<-renderPrint({
    result<-prdct(input$inputString)
    output$text1<-renderText({message})
    result
  });
  output$text2<-renderText({input$inputString});
})
```