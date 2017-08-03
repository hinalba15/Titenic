train <- read.csv("G:\\Hinal BA\\titenic\\train.csv", stringsAsFactors = F)
test  <- read.csv("G:\\Hinal BA\\titenic\\test.csv", stringsAsFactors = F)
str(train)
str(test)
#import packages
install.packages("ggplot2")
library('ggplot2') # visualization
install.packages("ggthemes")
library('ggthemes') # visualization
install.packages("scales")
library('scales') # visualization
install.packages("dplyr")
library(dplyr) # data manipulation
summary(train)
summary(test)
colSums(is.na(train))
colSums(is.na(test))
View(train)
#missing Value Tretment
train$Age<- ifelse(is.na(train$Age),28,train$Age)
train$Embarked<- ifelse(is.na(train$Embarked),"s",train$Embarked)
test$Age<- ifelse(is.na(test$Age),27,test$Age)
test$Fare<-ifelse(is.na(test$Fare),14.54,test$Fare)
colSums(is.na(train))
colSums(is.na(test))
#outlier Tretment 
quantile(train$Fare,c(0.10,0.20,0.30,0.40,0.50,0.70,0.80,0.90,0.95,0.99,0.999,1.0))
train$Fare_capped<-ifelse(train$Fare>249.0062,249.0062,train$Fare)
quantile(test$Fare,c(0.10,0.20,0.30,0.40,0.50,0.70,0.80,0.90,0.95,0.99,0.999,1.0))
test$Fare_capped<-ifelse(test$Fare>262.37500,262.37500,test$Fare)
View(test)
full <- bind_rows(train, test) 
str(full)
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# Show title counts by sex
table(full$Sex, full$Title)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
# Show title counts by sex again
table(full$Sex, full$Title)
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') 
# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram()



# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
# Show counts
table(full$Child, full$Survived)
# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
# Show counts
table(full$Mother, full$Survived)
# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)
View(full)
write.csv(full,"full.csv")
#Changing Categorical data to Numerical Data
full$Sex_new<-ifelse(full$Sex=="male",1,0)
full$Embarked_new<-ifelse(full$Embarked=="S",1,ifelse(full$Embarked=="c",2,3))
full$Title_new<-ifelse(full$Title=="Mr",1,ifelse(full$Title=="Mrs",2,ifelse(full$Title=="Miss",3,4)))
full$Child_new<-ifelse(full$Child=="Child",1,2)
full$FsizeD_new<-ifelse(full$FsizeD=="small",1,ifelse(full$FsizeD=="large",2,3))
full$Mother_new<-ifelse(full$Mother=="Mother",1,2)
#deleting Categorical colum 
full_1<-full[,-c(1,4,5,9,10,11,12,14,16,17,18)]
View(full_1)
write.csv(full_1,"full_1.csv")
#finding correlation Matrix 
correlation1<-cor(full_1[1:891,],full_1[1:891,])
correlation1
write.csv(correlation1,"correlation1.csv")

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

# Divide Train data into Traing_data and Validation data.
set.seed(3)
train_1 =  sample(1:nrow(train),nrow(train)*0.80)
Validation = -train_1
training_data = train[train_1,]
testing_data = train[Validation,]


# Logist Regression Model
install.packages("glmnet")
library(glmnet)
logit1<- glm(Survived ~ Pclass + Age + SibSp + Parch + Fare_capped
             + Fsize + Mother_new + Sex_new + Embarked_new + Title_new + Child_new + FsizeD_new, family = binomial("logit") ,data=training_data)
summary(logit1)
logit2<- glm(Survived ~ Pclass + Age + SibSp + Parch 
             + Sex_new + Title_new +  FsizeD_new, family = binomial("logit") ,data=training_data)
summary(logit2)
#predicting on validation data
Predicted<- predict(logit2,testing_data, type="response")
head(Predicted)
Predicted
Predicted_class<-ifelse(Predicted>= 0.5,1,0)
head(Predicted_class)

#confusion Matix or Misclassification
table(Predicted= Predicted_class, Actual= testing_data$Survived)
#ROC
library(gplots)
library(ROCR)
Titenic_ROC_pred= prediction(Predicted,testing_data$Survived)
Titenic_RoCR_performance= performance(Titenic_ROC_pred,"tpr","fpr")
#Roc curve
plot(Titenic_RoCR_performance,main="Roc Curve")
# Finding Cutoff value or Threshold

View(cbind.data.frame(Cutoff=Titenic_RoCR_performance@alpha.values[[1]],
                      FPR = Titenic_RoCR_performance@x.values[[1]],
                      TPR = Titenic_RoCR_performance@y.values[[1]]))

Predicted_class1<-ifelse(Predicted>= 0.366,1,0)
head(Predicted_class1)

#confusion Matix or Misclassification
table(Predicted= Predicted_class1, Actual= testing_data$Survived)

#Prediction on test data
Predicted_test<- predict(logit2,test, type="response")
head(Predicted_test)
Predicted_class_test<-ifelse(Predicted_test>= 0.366,1,0)
head(Predicted_class_test)
write.csv(Predicted_class_test,"Predicted_test.csv")