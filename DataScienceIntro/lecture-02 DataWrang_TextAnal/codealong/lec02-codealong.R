#1 
sum(df$revenue)
#2
max(df$social_media)
#3
sum(df$elec_bill[which(df$quarter %in% c(2,3))])
#or
sum(df$elec_bill[which(df$quarter == 2 | df$quarter == 3)])
#4
df$elec_bill[8:12, c(3,5)]
# 3 and 5 represent the two variables in question 
#5
df[min(df$social_media) == df$social_media, 1]
# 1 represents month? 
#6
df[max(df$revenue) == df$revenue, 1]
#7
sum(df$revenue > d$rev_target)
# Sum the Boolean
#8 
df[df$revenue > df$rev_target, "month"]
#9 
# Look back at answer key
#10
df[df$month %% 3 ==1, 5]

salary <- c("$100k ","$10,000","None")
salary <- gsub("k", "000", salary)
salary <- gsub("None", NA, salary)

# The below is shorthand for give me all punctuation.
salary <- gsub("[[:punct:]]","", salary)
# The below is for specific punctuation ($ and , here)
salary <- gsub("[$,","",salary)
salary <- as.numeric(salary)

a <- "Please call me at 678-567-9847 at 12pm"
gsub("\\d{3}-\\d{3}-\\d{4}", "XXX-XXX-XXXX", a)
# OR
gsub("\\d", "X", a)

a <- "1,2,4,10"
b <- "duck, duck, goose"
c <- strsplit(b, ",")

case <-  c(	"Jerry v. Newman", " Tom vs. Jerry", " Donald versus Media")
case <- gsub("vs\\.| v\\.", "versus", case)
b <- strsplit(case, "versus")
matrix(trimws(unlist(b)), nrow=3, byrow=T)
# In the matrix above, we lay the matrix out by row, meaning it will go with the first two values in the first row as 1 and 2 and so on. 

x <- data.frame( id = c(1,1,2,2), 
                 t = c(1,2,1,2), income = c(50,55,101,123), 
                 vote = c(8,7,4,3))
wide <- reshape(x, 
                idvar="id", 
                timevar="t", 
                direction="wide")

long <- reshape(wide, 
                 idvar = "id", 
                 timevar = "t", 
                 direction = "long")

wide <- data.frame(	id = c(1,2,3),
                    income = rnorm(3,100,5),
                    debt = rnorm(3,-10,20))
long <- reshape(wide, 
                varying = c("income", "debt"),
                v.names = "amount",
                timevar = "financials", 
                time = c("income", "debt"),
                direction = "long")

x <- data.frame(var1 = 	round(rnorm(100,1000,100)),
                group1 = round(runif(100)*5))
aggregate(x$var1, by = list(x$group1), FUN = length)
aggregate(x$var1, by = list(x$group1), FUN = mean)
# If we want to use sum for some variables and mean for others, we'd want to use a custom function to tell it to so sum on somethings and mean on others. 

# Merges
# Service user + Tax Returns + Marital Status



#Lecture 2- Codealong

setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro/lecture-02/codealong")

####How many planned applause breaks in 2010 versus 2016?

#2010
#read in lines from the text
  speech10 <- readLines("sotu_2010.txt")

#remove any blank lines
  speech10 <- speech10[speech10!=""]

#get string position of each Applause (returns positive values if matched)
  ind <- regexpr("Applause", speech10)
  sum(attr(ind,"match.length")>1)

#2016
  speech16 <- readLines("sotu_2016.txt")
  speech16 <- speech16[speech16!=""]
  ind <- regexpr("Applause", speech16)
  sum(attr(ind,"match.length")>1)


####Which words were more important in each year?

#2010
#Clean up and standardize values
  clean10 <- gsub("[[:punct:]]","",speech10)
  clean10 <- gsub("[[:digit:]]","",clean10)
  clean10 <- gsub("[^[:graph:]]"," ",clean10)

#convert into bag of words
  bag10 <- strsplit(clean10," ")
  bag10 <- tolower(trimws(unlist(bag10)))

#Count the number of times a word shows up
  counts10 <- aggregate(bag10, by=list(bag10), FUN=length)
  colnames(counts10) <- c("word","freq")
  counts10$len <- nchar(as.character(counts10$word))
  counts10 <- counts10[counts10$len>2,]
  counts10 <- counts10[order(-counts10$freq),]
  head(counts10, 10)

#2016
  clean16 <- gsub("[[:punct:]]","",speech16)
  clean16 <- gsub("[[:digit:]]","",clean16)
  clean16 <- gsub("[^[:graph:]]"," ",clean16)

  bag16 <- strsplit(clean16," ")
  bag16 <- tolower(trimws(unlist(bag16)))
  
  counts16 <- aggregate(bag16, by = list(bag16), FUN=length)
  colnames(counts16) <- c("word","freq")
  counts16$len <- nchar(as.character(counts16$word))
  counts16 <- counts16[counts16$len > 2,]
  counts16 <- counts16[order(-counts16$freq),]
  head(counts16,10)

#Remove stopwords
  
  library(rvest)
  #Get stopwords
  stop1 <- read_html("http://www.lextek.com/manuals/onix/stopwords1.html")
  
  stopwords <- stop1 %>% 
    html_nodes("pre") %>%
    html_text() 
  
  stoplist <- unlist(strsplit(stopwords,"\n"))
  stoplist <- stoplist[stoplist!="" & nchar(stoplist)>1]
  stoplist <- stoplist[4:length(stoplist)]
  
#Remove stopwrods
  counts10 <- counts10[!(counts10$word %in% stoplist),]
  counts16 <- counts16[!(counts16$word %in% stoplist),]
  
  head(counts10,10)
  head(counts16,10)

##TFIDF - Term Frequency Inverse Document Frequency
  #TF =  (# of times term t appears in a document) / (Total number of terms in the document).
  #IDF = exp(# of documents / # documents with term t in it) 
  
  master <- merge(counts10, counts16, by = "word", all.x=T, all.y = T)
  colnames(master) <- c("word", "freq10", "len10", "freq16","len16")
  master[is.na(master)]<-0
  
  
  master$tf10 <- master$freq10/sum(master$freq10) 
  master$tf16 <- master$freq16/sum(master$freq16) 
  master$docs_term <- (master$freq10 > 0) + (master$freq16 > 0)
  master$idf <- exp(2/master$docs_term)
  master$tfidf10 <- master$tf10 * master$idf
  master$tfidf16 <- master$tf16 * master$idf
  
  master <- master[,c(1,2,4,10,11)]
  
  
  library(wordcloud)
  set.seed(123)
  par(mfrow=c(2,1))
  master <- master[order(-master$tfidf10),]
  wordcloud(master$word[2:100], master$tfidf10[2:100], rot.per = 0)
  
  master <- master[order(-master$tfidf16),]
  wordcloud(master$word[2:100], master$tfidf16[2:100], rot.per = 0)
  
  
