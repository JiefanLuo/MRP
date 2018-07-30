

# Methodology, experiments and results code for MRP
# Jiefan Luo
# Read normal users and bots from Github and users bots were active during election 2016
library(readr)
library(rio)
library(ggplot2)
library(NLP)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(gridExtra)
library(lubridate)
library(scales)
library(corrplot)
library(reshape2)

######
# Read the dataset 1 containing the general bots
general_data <- read_csv("/Users/Luojiefan/Desktop/training_data_en_csv_UTF.csv")

#For reproducibility
set.seed(1)

# Randomize data points in general dataset 1
general_data <-  general_data[sample(nrow(general_data)),]

# Convert the binary labels 1 and 0 to yes and no
general_data$bot[general_data$bot == 1 ] <- "yes"
general_data$bot[general_data$bot == 0 ] <- "no"

# Normalize the temporal feature created_at and extract the account creation year
for (i in 1:nrow(general_data)) {
  timestamp <- general_data[i,]$created_at
  if (startsWith( timestamp, '"')) {
    year <- strsplit(timestamp, ' ')[[1]][6]
    general_data[i,]$created_at <- gsub('[[:punct:]]','',year)
  } else if(startsWith( timestamp, '1') | startsWith( timestamp, '2') | startsWith( timestamp, '3') | startsWith( timestamp, '4')
            | startsWith( timestamp, '5') | startsWith( timestamp, '6') | startsWith( timestamp, '7') | startsWith( timestamp, '8')| startsWith( timestamp, '9')){
    year <- strsplit(timestamp, '/')[[1]][3]
    general_data[i,]$created_at <- strsplit(year, ' ')[[1]][1]
  } else{
    str <- as.POSIXct(general_data[i,]$created_at, format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")
    general_data[i,]$created_at<- as.character( year(str))
  }
}


# Convert the categorical features as factor 
general_data$bot <- factor(general_data$bot)
general_data$created_at <- factor(general_data$created_at)
general_data$default_profile <- factor(general_data$default_profile)
general_data$default_profile_image <- factor(general_data$default_profile_image)


# Include necessary packages
library(e1071)
library(caret)
library(klaR)
library(RTextTools)


# Only use the numerical and categorical features
general_data <- data.frame(general_data$followers_count, general_data$friends_count, general_data$listed_count, general_data$favourites_count,
                           general_data$statuses_count,general_data$created_at,general_data$default_profile, general_data$default_profile_image,
                          general_data$bot)
colnames(general_data) <- c("followers_count", "friends_count", "listed_count", "favourites_count","statuses_count","created_at",
                            "default_profile", "default_profile_image", "isBot")

# Only use the textual feature
general_data <- data.frame(
                           general_data$description_en, general_data$bot)
colnames(general_data) <- c("description_en", "isBot")


# Use the forward feature selection to add features one by one and check whether they contribute to the classification accuracy
general_data <- data.frame(general_data$followers_count, general_data$friends_count,general_data$favourites_count, general_data$statuses_count,
                           general_data$created_at, general_data$default_profile,general_data$default_profile_image, 
                           general_data$description_en, general_data$bot)
colnames(general_data) <- c("followers_count",  "friends_count", "favourites_count","statuses_count","created_at", "default_profile", 
                            "default_profile_image","description_en", "isBot")


# Convert the description as text data 
general_data$description_en <- as.character(general_data$description_en)


# Replace the NA in description feature with "no description"
for (i in 1:nrow(general_data)) {
  if (is.na(general_data[i,]$description_en))
    general_data[i,]$description_en <- "no description"
}



# Transform the textual attribute description_en into the Document-term matrix
matrix <- create_matrix(general_data[,8], language="english", 
                        removeStopwords=TRUE, removeNumbers=TRUE, 
                        stemWords=FALSE)
# Remove sparse terms from the document-term matrix
matrix <- removeSparseTerms(matrix, 0.98)
mat = as.matrix(matrix)
# Combine the DTM and other features
general_data <- cbind(general_data,mat)
general_data$description_en <- NULL


# Factorize the bot label
general_data$isBot <- factor(general_data$isBot)


# Strartified split the data into training and test set
index <- createDataPartition(general_data$isBot, p =0.7, list = FALSE)
general_trainning <- general_data[index,]
general_test <- general_data[-index,]



# Experiment1 - Naive Bayes Model for Dataset 1 (General Bots)
# Calculate correlation matrix and remove Redundant Features
correlationMatrix <- cor(general_trainning[,1:4])

#Summarize the correlation matrix
print(correlationMatrix)
corrplot(correlationMatrix, method = "circle")

#Find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

#Print indexes of highly correlated attributes
print(highlyCorrelated)


# Parallel Processing
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)


# Prepare training scheme applying 10-fold cross-validation and random search
control <- trainControl(method="repeatedcv", number=10, repeats=1, search = "random", classProbs = TRUE )
#Train the naive bayes model
naiveBayes_model <- train(isBot ~.,  data=general_trainning, method="nb", trControl=control, tuneLength=10, preProc=c("zv"))
print(naiveBayes_model)


# Predict the account identification on the training set
general_trainning$predict <- predict(naiveBayes_model, general_trainning)
# Calculate the confusionMatrix for training
confusionMatrix(data = general_trainning$predict, reference = general_trainning$isBot)


general_test$predict <- NULL
# Detect the account identification on the test set
general_test$predict <- predict(naiveBayes_model, general_test)
# Calculate the confusionMatrix on test data set
confusionMatrix(data = general_test$predict, reference = general_test$isBot)

# Estimate variable importance
importance <- varImp(naiveBayes_model, scale=FALSE)
# Summarize importance
print(importance)
# Plot importance
plot(importance)



# Experiment 2 – Decision Tree Model for Dataset 1
library(rpart)
general_trainning$predict <- NULL
# Train the Naive Bayes model and try 10 different cp values to tune the model
decisionTree_model <- train(isBot ~., data=general_trainning, method="rpart", trControl=control,tuneLength=10)
# Get a summary of the optimal hyperparameters
print(decisionTree_model)
# Plot the line chart of accuracy as a function of complexity parameter
plot(decisionTree_model)


general_test$predict <- NULL
# Detect the account identification on the test set
general_test$predict <- predict(decisionTree_model, general_test)
# Calculate the confusionMatrix on test data set
confusionMatrix(data = general_test$predict, reference = general_test$isBot)


# Estimate variable importance
importance <- varImp(decisionTree_model, scale=FALSE)
# Summarize importance
print(importance)
# Plot importance
plot(importance)



# Experiment 3 – Random Forest Model for Dataset 1
library(randomForest)
general_trainning$predict <- NULL
# Train the Random Forest model and try 10 different mtry values to tune the model
randomForest_model <- train(isBot~., data=general_trainning, method="rf", tuneLength=10, trControl=control)
# Get a summary of the optimal hyperparameters
print(randomForest_model)
# Plot the line chart of accuracy as a function of mtry
plot(randomForest_model)

general_test$predict <- NULL
# Detect the account identification on the test set
general_test$predict <- predict(randomForest_model, general_test)
# Calculate the confusionMatrix on test data set
confusionMatrix(data = general_test$predict, reference = general_test$isBot)


# Experiment 4 – Linear SVM Model for Dataset 1
library(kernlab) 
#Train the model
general_trainning$predict <- NULL
# Train the Linear SVM Model and try 10 different cost values to tune the model
svmLinear_model <- train(isBot ~., data=general_trainning, method="svmLinear", trControl=control,tuneLength=10)
# Get a summary of the optimal hyperparameters
print(svmLinear_model)
# Plot the line chart of accuracy as a function of cost value
plot(svmLinear_model)

general_test$predict <- NULL
# Detect the account identification on the test set
general_test$predict <- predict(svmLinear_model, general_test)
# Calculate the confusionMatrix on test data set
confusionMatrix(data = general_test$predict, reference = general_test$isBot)


#Estimate variable importance
importance <- varImp(svmLinear_model, scale=FALSE)
#Summarize importance
print(importance)
#Plot importance
plot(importance)


# Experiment 5 - Radial Basis Function SVM Model for Dataset 1
general_trainning$predict <- NULL
# Train the Radial Basis Function SVM Model and try 10 different sigma and cost values to tune the model
svmRadial_model <- train(isBot ~., data=general_trainning, method="svmRadial", trControl=control,tuneLength=10)
print(svmRadial_model)


general_test$predict <- NULL
# Detect the account identification on the test set
general_test$predict <- predict(svmRadial_model, general_test)
# Calculate the confusionMatrix on test data set
confusionMatrix(data = general_test$predict, reference = general_test$isBot)


#Estimate variable importance
importance <- varImp(svmRadial_model, scale=FALSE)
#Summarize importance
print(importance)
#Plot importance
plot(importance)


# Stop the parellal processing
stopCluster(cl)



# Generate the ROC curve and calculate the AUC for different models
library(pROC)

decisionTree_model_prob <- predict(decisionTree_model, general_test, type = 'prob')
auc(general_test$isBot, decisionTree_model_prob[,2] )
plot(roc(general_test$isBot, decisionTree_model_prob[,2] ),cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of Decision Tree")

naiveBayes_model_prob <- predict(naiveBayes_model, general_test, type = 'prob')
auc(general_test$isBot, naiveBayes_model_prob[,2] )
plot(roc(general_test$isBot, naiveBayes_model_prob[,2] ),cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of Naive Bayes")

randomForest_model_prob <- predict(randomForest_model, general_test, type = 'prob')
auc(general_test$isBot, randomForest_model_prob[,2] )
plot(roc(general_test$isBot, randomForest_model_prob[,2] ),cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of Random Forest")

svmLinear_model_prob <- predict(svmLinear_model, general_test, type = 'prob')
auc(general_test$isBot, svmLinear_model_prob[,2] )
plot(roc(general_test$isBot, svmLinear_model_prob[,2] ),cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of SVM Linear")

svmRadial_model_prob <- predict(svmRadial_model, general_test, type = 'prob')
auc(general_test$isBot, svmRadial_model_prob[,2] )
plot(roc(general_test$isBot, svmRadial_model_prob[,2] ),cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of SVM Radial")




######
# Read the dataset 2 containing election bots which were active in the 2016 US presidential election 
election_data <- read_csv("/Users/Luojiefan/Desktop/bots_users_election_en.csv")

# Filter out the missing values
election_data_missing <- complete.cases(election_data[, "favourites_count"])
election_data <- election_data[election_data_missing, ]

# Convert the binary labels 1 and 0 to yes and no
election_data$isBot[election_data$isBot == 1 ] <- "yes"
election_data$isBot[election_data$isBot == 0 ] <- "no"

#For reproducibility
set.seed(1)

# Randomize data points in election dataset 2
election_data <-  election_data[sample(nrow(election_data)),]


# Normalize the temporal feature created_at and extract the account creation year
for (i in 1:nrow(election_data)) {
  str <- as.POSIXct(election_data[i,]$created_at, format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")
  election_data[i,]$created_at<- as.character( year(str))
}


# Convert the labels as factor 
election_data$isBot <- factor(election_data$isBot)
election_data$created_at <- factor(election_data$created_at)


# Replace the NA in description_en feature to "no description"
for (i in 1:nrow(election_data)) {
  if (is.na(election_data[i,]$description_en))
    election_data[i,]$description_en <- "no description"
}


# Include the necessary packages
library(e1071)
library(caret)
library(klaR)
library(RTextTools)

#Only use the numerical and categorical features
election_data <- data.frame(election_data$followers_count, election_data$friends_count, election_data$listed_count, election_data$favourites_count,
                            election_data$statuses_count,election_data$created_at, election_data$isBot)
colnames(election_data) <- c("followers_count", "friends_count", "listed_count", "favourites_count","statuses_count","created_at",
                             "isBot")

#Use all features
election_data <- data.frame(election_data$followers_count, election_data$friends_count, election_data$listed_count, election_data$favourites_count,
                            election_data$statuses_count,election_data$created_at,
                            election_data$description_en,election_data$isBot)
colnames(election_data) <- c("followers_count", "friends_count", "listed_count", "favourites_count","statuses_count","created_at",
                            "description_en", "isBot")

# Use the forward selection with adding attributes one by one
election_data <- data.frame(election_data$followers_count, election_data$friends_count, election_data$favourites_count, election_data$statuses_count,
                            election_data$created_at, election_data$description_en,election_data$isBot)
colnames(election_data) <- c("followers_count", "friends_count", "favourites_count","statuses_count","created_at","description_en", "isBot")


# Transform the textual data into the Document-term matrix
matrix <- create_matrix(election_data[,6], language="english", 
                        removeStopwords=TRUE, removeNumbers=TRUE, 
                        stemWords=FALSE)

# Remove sparese terms in DTM
matrix <- removeSparseTerms(matrix, 0.98)
mat <- as.matrix(matrix)
election_data <- cbind(election_data,mat)
election_data$description_en <- NULL


# Factorize the bot label
election_data$isBot <- factor(election_data$isBot)
election_data$created_at <- factor(election_data$created_at)


# Strartified split the data into training and test set
index <- createDataPartition(election_data$isBot, p =0.7, list = FALSE)
election_trainning <- election_data[index,]
election_test <- election_data[-index,]


# Experiment 6 - Naive Bayes Model for Dataset 2 (Election Bots)
# Calculate the correlation matrix and remove highly correlated features
correlationMatrix <- cor(election_trainning[,1:4])

#Summarize the correlation matrix
print(correlationMatrix)
corrplot(correlationMatrix, method = "circle")

#Find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

#Print indexes of highly correlated attributes
print(highlyCorrelated)


#Parallel Processing
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)


# Prepare training scheme applying 10-fold cross-validation and random search
control <- trainControl(method="repeatedcv", number=10, repeats=1, search = "random", classProbs = TRUE )
# Train the naive bayes model
naiveBayes_model <- train(isBot ~.,  data=election_trainning, method="nb", trControl=control,tuneLength=10, preProc=c("zv"))
print(naiveBayes_model)

# Predict the account identification on the test set
election_test$predict <- predict(naiveBayes_model, election_test)
# Calculate the confusionMatrix on test data set
confusionMatrix(data = election_test$predict, reference = election_test$isBot)

# Estimate variable importance
importance <- varImp(naiveBayes_model, scale=FALSE)
# Summarize importance
print(importance)
# Plot importance
plot(importance)



# Experiment 7 – Decision Tree Model for Dataset 2
library(rpart)
# Train the decision tree model
decisionTree_model <- train(isBot ~., data=election_trainning, method="rpart", trControl=control,tuneLength=10)
print(decisionTree_model)
plot(decisionTree_model, scales=list(x=list(cex=1), y=list(cex=1)))

election_test$predict <- NULL
# Predict the account identification on the test set
election_test$predict <- predict(decisionTree_model, election_test)
# Calculate the confusionMatrix on test data set
confusionMatrix(data = election_test$predict, reference = election_test$isBot)

#Estimate variable importance
importance <- varImp(decisionTree_model, scale=FALSE)
#Summarize importance
print(importance)
#Plot importance
plot(importance)



# Experiment 8 – Random Forest Model for Dataset2
library(randomForest)
randomForest_model <- train(isBot~., data=election_trainning, method="rf", tuneLength=10, trControl=control)
print(randomForest_model)
plot(randomForest_model,scales=list(x=list(cex=1), y=list(cex=1)))

election_test$predict <- NULL
#Predict the account identification on the test set
election_test$predict <- predict(randomForest_model, election_test)
#Calculate the confusionMatrix on test data set
confusionMatrix(data = election_test$predict, reference = election_test$isBot)


# Experiment 9 - Linear SVM Model for Dataset 2
library(kernlab) 
# Train the Linear SVM Model
svmLinear_model <- train(isBot ~., data=election_trainning, method="svmLinear", trControl=control,tuneLength=8)
print(svmLinear_model)
plot(svmLinear_model,scales=list(x=list(cex=1), y=list(cex=1)))

# Predict the account identification on the test set
election_test$predict <- predict(svmLinear_model, election_test)
confusionMatrix(data = election_test$predict, reference = election_test$isBot)


# Experiment 10 - Radial Basis Function SVM Model for Dataset 2
svmRadial_model <- train(isBot ~., data=election_trainning, method="svmRadial", trControl=control,tuneLength=5)
print(svmRadial_model)

# Predict the account identification on the test set
election_test$predict <- predict(svmRadial_model, election_test)
confusionMatrix(data = election_test$predict, reference = election_test$isBot)

# Stop the parellal processing
stopCluster(cl)

# Generate the ROC curve and calculate the AUC
library(pROC)
naiveBayes_model_prob <- predict(naiveBayes_model, election_test, type = 'prob')
auc(election_test$isBot, naiveBayes_model_prob[,2] )
plot(roc(election_test$isBot, naiveBayes_model_prob[,2]),cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of Naive Bayes")

decisionTree_model_prob <- predict(decisionTree_model, election_test, type = 'prob')
auc(election_test$isBot, decisionTree_model_prob[,2] )
plot(roc(election_test$isBot, decisionTree_model_prob[,2] ),cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of Decision Tree")

randomForest_model_prob <- predict(randomForest_model, election_test, type = 'prob')
auc(election_test$isBot, randomForest_model_prob[,2] )
plot(roc(election_test$isBot, randomForest_model_prob[,2] ),cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of Random Forest")

svmLinear_model_prob <- predict(svmLinear_model, election_test, type = 'prob')
auc(election_test$isBot, svmLinear_model_prob[,2] )
plot(roc(election_test$isBot, svmLinear_model_prob[,2] ), cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of SVM Linear")

svmRadial_model_prob <- predict(svmRadial_model, election_test, type = 'prob')
auc(election_test$isBot, svmRadial_model_prob[,2] )
plot(roc(election_test$isBot, svmRadial_model_prob[,2] ), cex.axis = 1.2, cex.lab=1.5, main = "ROC curve of SVM Radial")

