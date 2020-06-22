library(caTools)
library(ROCR) 
library(ggplot2)
library(reshape2)
library(corrplot)

############################### DATA PREPARATION ######################################

set.seed(0017)
spam=read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/SPAM17.csv")
head(spam)

# Split data into training and testing
split = sample.split(spam$spam, SplitRatio = 0.7) 
spamTrain = subset(spam, split == TRUE) # training set
spamTest = subset(spam, split == FALSE) # testing set
nrow(spamTrain)
nrow(spamTest)

################################### PART 1 ##########################################

# Get general statistics of the dataset
summary(spamTrain)
sapply(spamTrain, var)
sapply(spamTrain, sd)
correlationMatrix = round(cor(spamTrain),2)
correlationMatrix

# Create visualizations
spamTrain0 = subset(spamTrain, spam == 0) # training set with only non-spam
spamTrain1 = subset(spamTrain, spam == 1) # training set with only spam
par(mar=c(5.1, 6.1, 6.1, 2.1))

# Pie chart 
slices = c(nrow(spamTrain1)/nrow(spamTrain),nrow(spamTrain0)/nrow(spamTrain)) 
labels = c("Spam", "Not Spam")
colours = c("blue","violet")
percentage = round(slices/sum(slices)*100)
labels = paste(labels, percentage)
labels = paste(labels,"%",sep="")
pie(slices,labels, main="Email Type", col=colours)

# Bar chart for word variables for actual spam 
colsToDrop = c("spam", "ch.","crl.long")
spamTrain0Words = spamTrain0[ , !(names(spamTrain0) %in% colsToDrop)]
spamTrain1Words = spamTrain1[ , !(names(spamTrain1) %in% colsToDrop)]
spamTrain0Means = sort(sapply(spamTrain0Words, mean))
spamTrain1Means = sort(sapply(spamTrain1Words, mean))
barplot(spamTrain0Means, main="Non-Spam Emails Mean Word Percentages", xlab="Word Used in Email", ylab = "Mean Percentage of
Words in Non-Spam Email that Match the Word of Focus", col=rgb(0.2,0.4,0.7,0.6), cex.names=0.85)

# Bar graph for actually not spam means
barplot(spamTrain1Means, main="Spam Emails Mean Word Percentages", xlab="Word Used in Email", ylab = "Mean Percentage of
Words in Spam Email that Match the Word of Focus", col=rgb(0.2,0.4,0.7,0.6), cex.names=0.85)

# Histograms
ggplot(spamTrain0, aes(x=crl.long)) + geom_histogram(binwidth=75, color="black", fill="blue") +
  ggtitle ("Histogram of Longest Uninterrupted Sequence of Capital Letters in Non-Spam Emails") + 
  theme(plot.title = element_text(hjust = 0.5,face="bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  xlab("Longest Uninterrupted Sequence of Capital Letters")  +
  ylab("Count of Non-Spam Emails")
        
ggplot(spamTrain1, aes(x=crl.long)) + geom_histogram(binwidth=75, color="black", fill="blue") +
  ggtitle ("Histogram of Longest Uninterrupted Sequence of Capital Letters in Spam Emails") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  xlab("Longest Uninterrupted Sequence of Capital Letters")  +
  ylab("Count of Spam Emails")

ggplot(spamTrain0, aes(x=ch.)) + geom_histogram(binwidth=0.1, color="black", fill="blue") +
  ggtitle ("Histogram of Percentage of Characters in Non-Spam Emails that Match $ Character") + 
  theme(plot.title = element_text(hjust = 0.5,face="bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  xlab("Percentage of Characters in Email that Match $ Character")  +
  ylab("Count of Non-Spam Emails")

ggplot(spamTrain1, aes(x=ch.)) + geom_histogram(binwidth=0.1, color="black", fill="blue") + 
  ggtitle ("Histogram of Percentage of Characters in Spam Emails that Match $ Character") + 
  theme(plot.title = element_text(hjust = 0.5,face="bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  xlab("Percentage of Characters in Email that Match $ Character")  +
  ylab("Count of Spam Emails")

# Scatterplots
ggplot(data=spamTrain, aes(x=crl.long,y=spam))+geom_point(color="blue", size=0.5)+
  ggtitle ("Spam Classification vs Email's Longest Uninterrupted Sequence of Capital Letters")+ 
  theme(plot.title = element_text(hjust = 0.5,face="bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  labs(y= "Spam Classification (1=Spam, 0=Not Spam)", x = "Length of Longest Uninterrupted Sequence of Capital Letters") 

ggplot(data=spamTrain, aes(x=ch.,y=spam))+geom_point(color="blue", size=0.5) + 
  ggtitle ("Spam Classification vs Percentage of Email's Characters that Match $ Character") + 
  theme(plot.title = element_text(hjust = 0.5,face="bold"), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  labs(y= "Spam Classification (1=Spam, 0=Not Spam)", x = "Percentage of Characters in the Email that Match $ Character")

# Heatmap for correlation matrix 
corrplot(correlationMatrix, title ="Correlation Heatmap for Variables in Data Set", mar=c(0,0,3,0))

################################### PART 2 ##########################################

SpamLog = glm(spam ~ internet + order + mail + report + free + business + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)
# All independent variables are signficant and p-values are all the same. So we can 
# try taking each one out seperately and go from there. 


# Try without internet
SpamLog = glm(spam ~ order + mail + report + free + business + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without order
SpamLog = glm(spam ~ internet + mail + report + free + business + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without mail
SpamLog = glm(spam ~ internet + order + report + free + business + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without report
SpamLog = glm(spam ~ internet + order + mail + free + business + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without free
SpamLog = glm(spam ~ internet + order + mail + report + business + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without business
SpamLog = glm(spam ~ internet + order + mail + report + free + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without email 
SpamLog = glm(spam ~ internet + order + mail + report + free + business + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without you 
SpamLog = glm(spam ~ internet + order + mail + report + free + business + email +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without money 
SpamLog = glm(spam ~ internet + order + mail + report + free + business + email + you +
               george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without george
SpamLog = glm(spam ~ internet + order + mail + report + free + business + email + you +
                money + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without meeting
SpamLog = glm(spam ~ internet + order + mail + report + free + business + email + you +
                money + george + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without re
SpamLog = glm(spam ~ internet + order + mail + report + free + business + email + you +
                money + george + meeting + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try without conference
SpamLog = glm(spam ~ internet + order + mail + report + free + business + email + you +
                money + george + meeting + re + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try witout ch.
SpamLog = glm(spam ~ internet + order + mail + report + free + business + email + you +
                money + george + meeting + re + conference  + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Try wihout crl.long
SpamLog = glm(spam ~ internet + order + mail + report + free + business + email + you +
                money + george + meeting + re + conference + ch. , 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Lowest aic is 1932.9 (occurs for model without 'report' and for model without 'email')

# Try without report and mail (mail has highest p value in model without report)
SpamLog = glm(spam ~ internet + order + free + business + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)
# AIC increases to 2004.4

# Try without email and mail (mail has highest p value in model without email)
SpamLog = glm(spam ~ internet + order + report + free + business + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)
# AIC increases to 1973.9

# Stop since AIC has increased in both trials. We can use the model without report or the model without
# email. Both yield the same AIC. Arbitrarily choose to use model without report. 

# Final model - with report taken out 
SpamLog = glm(spam ~ internet + order + mail + free + business + email + you +
                money + george + meeting + re + conference + ch. + crl.long, 
              data = spamTrain, family = binomial)
summary(SpamLog)

# Predicting the outcome for the trained data
predictTrain = predict(SpamLog, type = "response")
head(predictTrain) 

# Get prediction for 50th email in the training set
predictTrain[50]

# Get predictions for testing set
predictTest = predict(SpamLog, type = "response", newdata = spamTest)
# Get confusion matrix for testing set
table(spamTest$spam, predictTest>0.5) 

# Try different threshold values
table(spamTest$spam, predictTest>0.4) 
table(spamTest$spam, predictTest>0.3) 
table(spamTest$spam, predictTest>0.2) 

# Get number of true negatives and positives in test set to calculate baseline accuracy
table(spamTest$spam) 

# Plot ROC for testing set
ROCRpred = prediction(predictTest, spamTest$spam)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,2.0), main="Receiver Operator Characteristic Curve") 

# Calculate AUC for testing set
auc_ROCR = performance(ROCRpred, measure = "auc")
auc_ROCR = auc_ROCR@y.values[[1]]
auc_ROCR
