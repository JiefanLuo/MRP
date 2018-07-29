
# MPR Exploration Analysis
# Jiefan Luo

# Included the relevant packages
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

# Read the dataset 1 (general bots) 
general_data <- read_csv("/Users/Luojiefan/Desktop/training_data_en_csv_UTF.csv")


# Normalize the temporal created_at feature and extract the creation year
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


# Obtain the general bots and normal users account data respectively
general_nonbots <- general_data[ which(general_data$bot == 0), ]
general_bots <- general_data[ which(general_data$bot == 1), ]


# Generate bar charts showing the distribution of account created date over time:
ggplot(general_bots,aes(x=general_bots$created_at)) + geom_bar(aes(y = (..count..))) + xlab("Creation Time") + ylab("Count") + 
  ggtitle( "Bar Chart: Creation Time of General Bots (Dataset1)") + theme(plot.title = element_text(hjust=0.5, face="bold", size = 22)) +
  theme(text = element_text(size=20)) 
ggplot(general_nonbots,aes(x=general_nonbots$created_at))+geom_bar(aes(y = (..count..))) + xlab("Creation Time") + ylab("Count") +
  ggtitle( "Bar Chart: Creation Time of Normal Users (Dataset1)") + theme(plot.title = element_text(hjust=0.5, face="bold", size = 22)) +
  theme(text = element_text(size=20)) 


# Get the statistic summary of general bots/non-bots data
summary(general_bots)
summary(general_nonbots)

# Calculate the standard deviation of numerical features general bots/non-bots data
sd(general_bots$followers_count)
sd(general_bots$friends_count)
sd(general_bots$listed_count)
sd(general_bots$favourites_count)
sd(general_bots$statuses_count)

sd(general_nonbots$followers_count)
sd(general_nonbots$friends_count)
sd(general_nonbots$listed_count)
sd(general_nonbots$favourites_count)
sd(general_nonbots$statuses_count)


# Correlation analysis for the general dataset.
general_numeric_features <- data.frame(general_data$followers_count,general_data$friends_count, general_data$listed_count,
                                       general_data$favourites_count, general_data$statuses_count)
names(general_numeric_features) <- c("followers_count", "friends_count", "listed_count","favourites_count","statuses_count")
# Calculate the correlation matrix 
cormatrix_generalfeature <- round( cor(general_numeric_features), 2)
# Generate the correlation visualization
corrplot(cormatrix_generalfeature, method = "circle")



# Scatter plots of the number of followers and friends of normal users and bots
ggplot(general_bots, aes(x=general_bots$friends_count, y=general_bots$followers_count)) +
  geom_point(shape=19,color ="firebrick1" ) + ylab("Number of Followers") +xlab("Number of Friends") +ggtitle( "Scatter Plot: Followers Vs Friends Count for General Bots (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 20)) +
  theme(text = element_text(size=20)) + xlim(0, 1000) + ylim(0, 1000)   

ggplot(general_nonbots, aes(x=general_nonbots$friends_count, y=general_nonbots$followers_count)) +
  geom_point(shape=19, color="dodgerblue4") + ylab("Number of Followers") +xlab("Number of Friends") +ggtitle( "Scatter Plot: Followers Vs Friends Count for Normal Users (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 20)) +
  theme(text = element_text(size=20)) + xlim(0, 1000) + ylim(0, 1000) 


# Filter out the outliers of bots which listed_count are too high
general_bots_outlier<-general_bots[general_bots$listed_count <= 500,]

# Create the histogram of the listed_count distribution of general bots
ggplot(data = general_bots_outlier, aes(x = general_bots_outlier$listed_count)) +
  geom_histogram(bins = 50) +
  xlab("List Count") + ylab("Frequency") + ylim(0,600) +
  ggtitle( "Histogram: Listed Count Distribution of General Bots (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 20)) + theme(text = element_text(size=20))
# Add the fitting line of the histogram
ggplot(data = general_bots_outlier, aes(x = general_bots_outlier$listed_count)) +
  geom_histogram(aes(y=..density..),bins = 50) +
  xlab("List Count") + ylab("Frequency") + 
  ggtitle( "List Count Distribution of General Bots") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  geom_density()  


# Filter out the outliers of normal users
general_nonbots_outlier<-general_nonbots[general_nonbots$listed_count <= 8000,]

# Create the histogram of the listed_count distribution of general bots
ggplot(data = general_nonbots_outlier, aes(x = general_nonbots_outlier$listed_count)) +
  geom_histogram(bins = 25) +
  xlab("List Count") + ylab("Frequency") + #ylim(0,700) +
  ggtitle( "Histogram: Listed Count Distribution of Normal Users (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 20)) + theme(text = element_text(size=20))
# Add the fitting line of the histogram
ggplot(data = general_nonbots_outlier, aes(x = general_nonbots_outlier$listed_count)) +
  geom_histogram(aes(y=..density..),bins = 23) +
  xlab("List Count") + ylab("Density") + 
  ggtitle( "List Count Distribution of Non-bots (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  geom_density() 



# Filter out the outliers of bots which favourites_count are too high
general_bots_outlier<-general_bots[general_bots$favourites_count <= 100,]
# Create the histogram of the favourites_count distribution of general bots
ggplot(data = general_bots_outlier, aes(x = general_bots_outlier$favourites_count)) +
  geom_histogram(bins = 30) +
  theme(legend.position= "none") +
  xlab("Favourites Count") + ylab("Frequency") + #ylim(0,900) +
  ggtitle( "Favourites Count Distribution of General Bots (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 20)) + theme(text = element_text(size=20))

# Filter out the outliers of narmal users
general_nonbots_outlier<-general_nonbots[general_nonbots$favourites_count <= 2000,]
# Create the histogram of the favourites_count distribution of normal users
ggplot(data = general_nonbots_outlier, aes(x = general_nonbots_outlier$favourites_count)) +
  geom_histogram(bins = 30) +
  theme(legend.position= "none") +
  xlab("Favourites Count") + ylab("Frequency") + #ylim(0,400) +
  ggtitle( "Favourites Count Distribution of Normal Users (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 20)) + theme(text = element_text(size=20))



# Filter out the outliers of bots
general_bots_outlier<-general_bots[general_bots$statuses_count <= 11000,]
# Create the histogram of the statuses_count distribution of general bots
ggplot(data = general_bots_outlier, aes(x = general_bots_outlier$statuses_count)) +
  geom_histogram(bins = 50) +
  xlab("Statuses Count") + ylab("Frequency") + ylim(0,400) +
  ggtitle( "Statuses Count Distribution of General Bots (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 20)) + theme(text = element_text(size=20))



# Filter out the outliers of normal user
general_nonbots_outlier<-general_nonbots[general_nonbots$statuses_count <= 18000,]
# Create the histogram of the statuses_count distribution of normal users
ggplot(data = general_nonbots_outlier, aes(x = general_nonbots_outlier$statuses_count)) +
  geom_histogram(bins = 50) +
  xlab("Statuses Count") + ylab("Frequency") + #ylim(0,300) +
  ggtitle( "Statuses Count Distribution of Normal Users (Dataset1)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 20)) + theme(text = element_text(size=20))


# Compare users and bots whether they use the default profile themes or backgrounds
# When true, indicates that the user has not altered the theme or background of their user profile. 
ggplot(general_bots, aes(x = general_bots$default_profile)) + geom_bar(stat = "count") + ylab("Number of Accounts") +xlab("") + ggtitle( "Bot Accounts Use the Default Profile Themes or Backgrounds") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 
ggplot(general_nonbots, aes(x = general_nonbots$default_profile)) + geom_bar(stat = "count") + ylab("Number of Accounts") +xlab("") + ggtitle( "Non-bot Account use the Default Profile Themes or Backgrounds") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 


# Compare users and bots whether they use the default profile image
# When true, indicates that the user has not uploaded their own profile image and a default image is used instead. Example:
ggplot(general_bots, aes(x = general_bots$default_profile_image)) + geom_bar(stat = "count") + ylab("Number of Accounts") +xlab("") + ggtitle( "Bot Accounts use the Default Profile Image") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 
ggplot(general_nonbots, aes(x = general_nonbots$default_profile_image)) + geom_bar(stat = "count") + ylab("Number of Accounts") +xlab("") + ggtitle( "Non-bot Accounts use the Default Profile Image") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 



# Text Analysis for account description
# Function to remove the emoticons
removeEmoticon <- function(dataframe) {
  dataframe$description <-  sapply(dataframe$description,function(row) iconv(row, "latin1", "ASCII", sub=""))
  return(dataframe)
}

general_bots <- removeEmoticon(general_bots)
general_nonbots <- removeEmoticon(general_nonbots)

# Function to create term-document matrix
tdmCreator <- function(dataframe, addstopwords, stemDoc = F){
  tdm <- Corpus(VectorSource(dataframe$description))
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
# Remove redundant space
  removeURL <- function(x) gsub("https[^[:space:]]*", "",x)
  tdm <- tm_map(tdm, content_transformer(removeURL))
# Only consider the term with wordLengths greater than 3
  tdm <- TermDocumentMatrix(tdm, control = list(wordLengths = c(3, Inf),removePunctuation = TRUE,tolower = TRUE,  
                                                stopwords = c(addstopwords,stopwords("english"))))
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}


# Add stopwords base on noise words
add_stopwords <- c()
tdm_general_bots <- tdmCreator(general_bots, add_stopwords)
tdm_general_nonbots <- tdmCreator(general_nonbots, add_stopwords)


# Create word clouds for description features for bots and non-bots
wc_general_bots <- wordcloud(words = tdm_general_bots$term,freq = tdm_general_bots$freq, scale=c(3,.6),min.freq=3,
                             max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))
wc_general_nonbots <- wordcloud(words = tdm_general_nonbots$term,freq = tdm_general_nonbots$freq, scale=c(2,.2),min.freq=3,
                                max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))




###########
# Read the dataset 2 (election bots) 
# Contains regular and bot accounts that were active in the 2016 U.S election
election_data <- read_csv("/Users/Luojiefan/Desktop/bots_users_election_en.csv")

# Remove the missing values
election_data <- election_data[complete.cases(election_data[ , 10:14]),]

# Normalize the temporal created_at feature and extract the creation year
for (i in 1:nrow(election_data)) {
  str <- as.POSIXct(election_data[i,]$created_at, format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")
  election_data[i,]$created_at<- as.character( year(str))
}

# Obtain the election bots and normal users account data respectively
election_nonbots <- election_data[ which(election_data$isBot == 0), ]
election_bots <- election_data[ which(election_data$isBot == 1), ]


# Temporal analysis for creation time
# Generate bar charts showing the distribution of account created date over time:
ggplot(election_bots,aes(x=election_bots$created_at))+geom_bar(aes(y = (..count..))) + xlab("Creation Time") + ylab("Count") + 
  ggtitle( "Bar Chart: Creation Time of Election Bots (Dataset2)") + theme(plot.title = element_text(hjust=0.5, face="bold", size = 24)) +
  theme(text = element_text(size=24)) 

ggplot(election_nonbots,aes(x=election_nonbots$created_at))+geom_bar(aes(y = (..count..))) + xlab("Creation Time") + ylab("Count") + 
  ggtitle( "Bar Chart: Creation Time of Normal Users (Dataset2)") + theme(plot.title = element_text(hjust=0.5, face="bold", size = 24)) +
  theme(text = element_text(size=24)) 



# Get the statistic summary of election bots/non-bots data
summary(election_bots)
summary(election_nonbots)

# Calculate the standard deviation of numerical features election bots/non-bots data
sd(election_bots$followers_count)
sd(election_bots$friends_count)
sd(election_bots$listed_count)
sd(election_bots$favourites_count)
sd(election_bots$statuses_count)

sd(election_nonbots$followers_count)
sd(election_nonbots$friends_count)
sd(election_nonbots$listed_count)
sd(election_nonbots$favourites_count)
sd(election_nonbots$statuses_count)


# Correlation analysis for the general dataset.
# Generate the correlation visualization
election_numeric_features <- data.frame(election_data$followers_count,election_data$friends_count, election_data$listed_count,
                                       election_data$favourites_count, election_data$statuses_count)
names(election_numeric_features) <- c("followers_count", "friends_count", "listed_count","favourites_count","statuses_count")
cormatrix_electionfeature <- cor(election_numeric_features)
corrplot(cormatrix_electionfeature, method = "circle")


# Scatter plots of the number of followers and friends of normal users and bots
ggplot(election_bots, aes(x=election_bots$friends_count, y=election_bots$followers_count)) +
  geom_point(shape=19,color ="firebrick1") + ylab("Number of Followers") +xlab("Number of Friends") +ggtitle( "Scatter Plot: Followers Vs Friends Count for Election Bots (Dataset2)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18)) +
  theme(text = element_text(size=20)) + xlim(0, 10000) + ylim(0, 10000)   

ggplot(election_nonbots, aes(x=election_nonbots$friends_count, y=election_nonbots$followers_count)) +
  geom_point(shape=19, color ="dodgerblue4") + ylab("Number of Followers") +xlab("Number of Friends") +ggtitle( "Scatter Plot: Followers Vs Friends Count for Normal Users in U.S Election (Dataset2)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18)) +
  theme(text = element_text(size=20)) + xlim(0, 20000) + ylim(0, 50000) 



# Filter out the outliers of bots
election_bots_outlier <- election_bots[election_bots$listed_count <= 400,]

# Create the histogram of the listed_count distribution of election bots
ggplot(data = election_bots_outlier , aes(x = election_bots_outlier$listed_count)) +
  geom_histogram(bins = 50) +
  theme(legend.position= "none") +
  xlab("List Count") + ylab("Frequency") + #xlim(0, 1000)  + #ylim(0, 150) +
  ggtitle( "Histogram: Listed Count Distribution of Election Bots (Dataset2)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18)) +
  theme(text = element_text(size=18))
#Add Fitting line
ggplot(data = election_bots_outlier, aes(x = election_bots_outlier$listed_count)) + 
  geom_histogram(aes(y=..density..),bins = 50) + 
  xlab("List Count") +ylab("Density") + 
  geom_density() +
  ggtitle( "List Count Distribution of Election Non-bots")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))


# Filter out the outliers of normal users
election_nonbots_outlier <- election_nonbots[election_nonbots$listed_count <= 500,]
# Create the histogram of the listed_count distribution of normal users
ggplot(data = election_nonbots_outlier, aes(x = election_nonbots_outlier$listed_count)) +
  geom_histogram(bins = 50) +
  theme(legend.position= "none") +
  xlab("List Count") + ylab("Frequency") + #xlim(0, 1000)  + #ylim(0, 50) +
  ggtitle( "Histogram: Listed Count Distribution of Normal Users in U.S Election") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18)) +
  theme(text = element_text(size=18))



# Filter out the outliers of election bots 
election_bots_outlier <- election_bots[election_bots$favourites_count <= 10000,]
# Create the histogram of the favourites_count distribution of election bots
ggplot(data = election_bots_outlier , aes(x = election_bots_outlier$favourites_count)) +
  geom_histogram(bins = 50) +
  theme(legend.position= "none") +
  xlab("Favourites Count") + ylab("Frequency") + #xlim(0, 1000)  + #ylim(0, 150) +
  ggtitle( "Favourites Count Distribution of Election Bots (Dataset2)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18)) +
  theme(text = element_text(size=18))


#Filter out outliers of normal users
election_nonbots_outlier <- election_nonbots[election_nonbots$favourites_count <= 20000,]
# Create the histogram of the favourites_count distribution of normal users
ggplot(data = election_nonbots_outlier , aes(x = election_nonbots_outlier$favourites_count)) +
  geom_histogram(bins = 50) +
  theme(legend.position= "none") +
  xlab("Favourites Count") + ylab("Frequency") + #xlim(0, 1000)  + #ylim(0, 150) +
  ggtitle( "Favourites Count Distribution of Normal Users in U.S Election (Dataset2)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18)) +
  theme(text = element_text(size=18))



# Filter outliers of election bots
election_bots_outlier <- election_bots[election_bots$statuses_count <= 10000,]
# Create the histogram of the statuses_count distribution of election bots
ggplot(data = election_bots_outlier , aes(x = election_bots_outlier$statuses_count)) +
  geom_histogram(bins = 50) +
  theme(legend.position= "none") +
  xlab("Statuses Count") + ylab("Frequency") + #xlim(0, 1000)  + #ylim(0, 150) +
  ggtitle( "Statuses Count Distribution of Election Bots (Dataset2)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18)) +
  theme(text = element_text(size=18))


# Filter outliers of normal users
election_nonbots_outlier <- election_nonbots[election_nonbots$statuses_count <= 100000,]
# Create the histogram of the statuses_count distribution of normal users
ggplot(data = election_nonbots_outlier , aes(x = election_nonbots_outlier$statuses_count)) +
  geom_histogram(bins = 50) +
  theme(legend.position= "none") +
  xlab("Statuses Count") + ylab("Frequency") + #xlim(0, 1000)  + #ylim(0, 150) +
  ggtitle( "Statuses Count Distribution of Normal Users in U.S Election (Dataset2)") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18)) +
  theme(text = element_text(size=18))



# Text analysis for account description  

#Function to remove emoticon
removeEmoticon <- function(dataframe) {
  dataframe$description_en <-  sapply(dataframe$description_en,function(row) iconv(row, "latin1", "ASCII", sub=""))
  return(dataframe)
}
election_bots <- removeEmoticon(election_bots)
election_nonbots <- removeEmoticon(election_nonbots)


# Create term-document matrix
tdmCreator <- function(dataframe, addstopwords, stemDoc = F){
  tdm <- Corpus(VectorSource(dataframe$description_en))
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
  # Remove redundant spaces 
  removeURL <- function(x) gsub("https[^[:space:]]*", "",x)
  tdm <- tm_map(tdm, content_transformer(removeURL))
  # Only consider the term with wordLength is greater than 3
  tdm <- TermDocumentMatrix(tdm, control = list(wordLengths = c(3, Inf),removePunctuation = TRUE,tolower = TRUE,  
                                                stopwords = c(addstopwords,stopwords("english"))))
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}


# Add term "news" as the noise 
add_stopwords <- c("news")
tdm_election_nonbots <- tdmCreator(election_nonbots, add_stopwords)
tdm_election_bots <- tdmCreator(election_bots, add_stopwords)



# Create word clouds for description features for bots and non-bots 
wc_election_bots <- wordcloud(words = tdm_election_bots$term,freq = tdm_election_bots$freq, scale=c(2,.3),min.freq=3,
                              max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))

wc_election_nonbots <- wordcloud(words = tdm_election_nonbots$term,freq = tdm_election_nonbots$freq, scale=c(2,.3),min.freq=3,
                                 max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))

