# Final Exam Question 1 #
## Question 1

# Import Data
b <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(b)

# Run logit model 
admitlog = glm(admit ~ gre, data=b, family=binomial) 
summary(admitlog)

# Create the four datasets
rank1 <- subset(b, rank == 1)
rank1log = glm(admit ~ gre, data=rank1, family=binomial) 
mean(predict(rank1log, rank1, type="resp"))
summary(rank1log)
# AIC is 85.046

rank1gpa325 <- subset(b, rank == 1 & gpa > 3.25)
rank1gpa325log = glm(admit ~ gre, data=rank1gpa325, family=binomial) 
mean(predict(rank1gpa325log, rank1gpa325, type="resp"))
summary(rank1gpa325log)
# AIC is 57.127

rank3gpa33 <- subset(b, rank == 3 & gpa > 3.3)
rank3gpa33log = glm(admit ~ gre, data=rank3gpa33, family=binomial) 
mean(predict(rank3gpa33log, rank3gpa33, type="resp"))
summary(rank3gpa33log)
# AIC is 96.468

rank4 <- subset(b, rank == 4)
rank4log = glm(admit ~ gre, data=rank4, family=binomial) 
mean(predict(rank4log, rank4, type="resp"))
summary(rank4log)
# AIC is 65.775
