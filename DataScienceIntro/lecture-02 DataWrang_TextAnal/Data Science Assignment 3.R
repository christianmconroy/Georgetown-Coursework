## Data Science Assignment 3 ##
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")

wmata$timedate <- paste(wmata$date, wmata$time.occur)
wmata$timedate2 <- strptime((paste(wmata$date, wmata$time.occur)), '%B %d, %Y %H:%M%p')

wmata$hour <- format(strptime(wmata$time.occur,'%H:%M%p'),'%H')
ggplot(data=wmata, aes(x=hour, y=delay)) +
  geom_bar(stat="identity")

stadelays <- wmata$in.station[order(wmata$delay, decreasing = TRUE)]

ggplot(data=subset(wmata, in.station %in% stadelays [1:5]), aes(x=in.station, y=delay)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

linedelays <- wmata$line[order(wmata$delay, decreasing = TRUE)]

ggplot(data=subset(wmata, line %in% linedelays [1:5]), aes(x=line, y=delay)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

wmata$date2 <- as.Date(wmata$date, '%B %d, %Y')
ggplot(wmata, aes(date2, delay)) + geom_line() + xlab("Date") + ylab("Delays") + facet_wrap(~line)



library(plyr)
reasoncounts <- as.data.frame(ldply(wmata[,10:17],function(x) t(rbind(names(table(x)),table(x)))))
reasoncounts  <- subset(reasoncounts,`1` == 1)
reasoncounts$`1` <- NULL
names(reasoncounts) <- c("reason", "count")
reasoncounts$reason <- sub("reason.","", reasoncounts$reason)
reasoncounts$count <- as.numeric(reasoncounts$count)


library(wordcloud)
set.seed(123)
par(mfrow=c(2,1))
reasoncounts <- reasoncounts[order(-reasoncounts$count),]
wordcloud(reasoncounts$reason, reasoncounts$count, rot.per = 0, c(3,.1), FALSE)

# List out the lines
levels(wmata$line)
wmatasub <- wmata[wmata$line=="Green",]
delbydate <- aggregate(delay ~ date2, data = wmatasub, sum)
delbydate <- delbydate[order(delbydate$date2),]


linesurvcurve <- function (line) {
wmatasub <- wmata[wmata$line==line,]
delbydate <- aggregate(delay ~ date2, data = wmatasub, sum)
delbydate <- delbydate[order(delbydate$date2),]
delbadd <- data.frame(date2=as.POSIXct(Sys.time(), origin="1970-01-01"), delay = 0)
delbydate <- rbind(delbadd, delbydate)
delbydate$time <- rep(0:(length(delbydate$date2)-1))
delbydate <- subset(delbydate, delbydate$time <= 15)

delbydate$diff <- sum(delbydate$delay)
n <- nrow(delbydate)
if (n > 1) for(i in 2:n) delbydate$diff[i] <- (delbydate$diff[i-1] - delbydate$delay[i-1])

delbydate$dn <- delbydate$delay/delbydate$diff
delbydate$dnsub1 <- 1-delbydate$dn

delbydate$s <- delbydate$dnsub1[1]
delbydate$s[2] <- delbydate$dnsub1[2]
n <- nrow(delbydate)
if (n > 1) for(i in 3:n) delbydate$s[i] <- prod(delbydate$dnsub1[1:i])

p <- ggplot(delbydate, aes(time, s)) + geom_line() + xlab("time t") + ylab("s(probability of lasting beyond time t")
return(p)
}

linesprob10 <- function (line) {
  wmatasub <- wmata[wmata$line==line,]
  delbydate <- aggregate(delay ~ date2, data = wmatasub, sum)
  delbydate <- delbydate[order(delbydate$date2),]
  delbadd <- data.frame(date2=as.POSIXct(Sys.time(), origin="1970-01-01"), delay = 0)
  delbydate <- rbind(delbadd, delbydate)
  delbydate$time <- rep(0:(length(delbydate$date2)-1))
  delbydate <- subset(delbydate, delbydate$time <= 15)
  
  delbydate$diff <- sum(delbydate$delay)
  n <- nrow(delbydate)
  if (n > 1) for(i in 2:n) delbydate$diff[i] <- (delbydate$diff[i-1] - delbydate$delay[i-1])
  
  delbydate$dn <- delbydate$delay/delbydate$diff
  delbydate$dnsub1 <- 1-delbydate$dn
  
  delbydate$s <- delbydate$dnsub1[1]
  delbydate$s[2] <- delbydate$dnsub1[2]
  n <- nrow(delbydate)
  if (n > 1) for(i in 3:n) delbydate$s[i] <- prod(delbydate$dnsub1[1:i])
  
  q <- delbydate$s[delbydate$time == 10]
  return(q)
}

max(c(linesprob10('Blue'), linesprob10('Green'), linesprob10('Orange'), linesprob10('Red'), linesprob10('Silver'), linesprob10('Yellow')))
# The highest probability at time t is blue. 
linesurvcurve('Blue')
linesurvcurve('Green')
linesurvcurve('Orange')
linesurvcurve('Red')
linesurvcurve('Silver')
linesurvcurve('Yellow')





####How many planned applause breaks in 2010 versus 2016?

#2010
#read in lines from the text

List <- strsplit(wmata$text, " ")
advisory <- data.frame(Id=rep(wmata$line, sapply(List, length)), Words=unlist(List))
advisory <- advisory[advisory$Id == "Red" | advisory$Id == "Silver",]
advisory <- advisory[-grep("[[:digit:]]", advisory$Words),]
advisory$Words <- gsub("[[:punct:]]","", advisory$Words)
advisory$Words <- gsub("[^[:graph:]]", "", advisory$Words)
advisory$Words <- gsub("Â", "", advisory$Words)

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
advisory <- advisory[!(advisory$Words %in% stoplist),]


#Count the number of times a word shows up
countsadv <- aggregate(advisory$Words, by=list(advisory$Id, advisory$Words), FUN=length)
colnames(countsadv) <- c("line", "word","freq")



tfidf <- function(df, count.field, term, document){
  
  # Returns TFIDF value 
  #
  # Args:
  #   df: data frame
  #   count.field: string name of field containing frequency of a every unique word for each document
  #   term: string name of the field containing words
  #   document: string name of the field containing document ids
  #
  # Returns:
  #   Vector of TFIDF values for each word-document combination
  
  df <- df[,c(count.field, term, document)]
  colnames(df) <- c("count","term","doc")
  
  #Total frequency of terms
  df$tot_freq <- sum(df$count)
  
  #Number of documents
  df$num_docs <- length(unique(df$doc))
  
  #Number of documents with a given term
  datatemp <- df[!duplicated(df[,c("doc", "term")]),]
  num_doc_term <- aggregate(datatemp$term, 
                            by = list(term = datatemp$term),
                            FUN = length)
  colnames(num_doc_term)[2] <- "num_doc_term"
  
  #merge
  df <- merge(df, num_doc_term, by = "term", all.x = T)
  
  #compute
  df$tfidf <- (df$count/df$tot_freq) * log(df$num_docs / df$num_doc_term)
  df <- df[, c("count","term", "doc", "tfidf")]
  colnames(df) <- c(count.field, term, document, "tfidf")
  return(df)
}

p2forwordc <- as.data.frame(tfidf(countsadv, count.field = "freq", term = "word", document = "line"))
p2forwordc <- subset(p2forwordc, p2forwordc$tfidf>0)

library(wordcloud)
set.seed(123)
par(mfrow=c(2,1))
p2forwordc <- p2forwordc[order(-p2forwordc$tfidf),]
wordcloud(p2forwordc$word[2:100], p2forwordc$tfidf[2:100], rot.per = 0)