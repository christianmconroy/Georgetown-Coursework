# Week 0 Warm Up Exercise #

# Creating new objects
vec1 <- c(88, 51, 129, 2788, 6518)
length(vec1)

vec2 <- c("Aragorn", "Frodo", "Bilbo", "Arwen", "Elrond")
length(vec2)

lotr <- data.frame(vec1, vec2)
dim(lotr)
nrow(lotr)
ncol(lotr)
names(lotr) <- c("ages", "people")
lotr$people <- as.character(lotr$people)

# High Level Calculations 
ancient <- lotr$ages >= 1000
young <- lotr$ages <=100
table(ancient, young)

lotr$age.bracket <- c("old")
lotr$age.bracket <- ifelse(ancient == T, "ancient", ifelse(young == T, "youngish", "old"))
# May be quicker/different way to do above. 

mean <- mean(lotr$ages)
sd <- sd(lotr$ages)
min <- min(lotr$ages)
max <- max(lotr$ages)

chartot <- mean(nchar(lotr$people))
totan <- sum(ancient)
totyo <-sum(young)

hero <- data.frame(mean, sd, min, max, chartot, totan, totyo)
names(hero) <- c("heroagemean", "heroagesd", "heromin", "heromax", "heronamemean", "ancientotal", "yountotal")
print(hero)

# Add new values 
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")
baddies <- read.csv("baddies.csv")
head(baddies)

names(baddies) <- c("people","ages")
baddies$age.bracket <- NA
lotr <- rbind(lotr, baddies)

lotr$allegiance <- NA
lotr$allegiance[1:5] <- "Goodie"
lotr$allegiance[4:9] <- "Baddie"

lotr$ages[3]
lotr$allegiance[6]
lotr[c(4,5),]
lotr[c(1,8),]
lotr[4:nrow(lotr),]
lotr[,1]

match.up <- lotr[lotr$ages == ave(lotr$ages, lotr$allegiance, FUN = "max"), "people"]
print(match.up)

