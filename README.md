# Twitter Bot Detection Utilizing Multiple Machine Learning Algorithms

The purpose of this project is to apply multiple machine learning algorithms to develop bot-detection
models for Twitter. Using exploratory analysis, we explored the Twitter metadata and found useful
behavior features to distinguish between regular users and bots. For the training models, I used Random
Search to select the optimal hyperparameters used to tune the different classifiers. I applied five
algorithms including Naive Bayes, Decision Tree, Random Forest, Linear Support Vector Machine
(SVM), and Radial Basis Function SVM to train the classifiers. The results of the classification are the
account identities and I measured the classification performance by accuracy, sensitivity, specificity,
and area under the receiver operating characteristic curve (AUC). According to the results, the Random
Forest algorithm was most effective in detecting bots and identifying normal users.

R Libraries used: ggplot2, NLP, wordcloud, rpart, randomForest, kernlab, caret, e1071, doParallel
