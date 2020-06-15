##Installing required packages
install.packages("forcats")
install.packages("kableExtra")

#Loading the libraries for the project
library(ggplot2)
library(kableExtra)
library(stringr)
library(tidyr)
library(tibble)
library(tidyverse)
library(dslabs)
library(dbplyr)
library(caret)
library(broom)
library(naivebayes)
library(pdftools)
library(rvest)
library(timeDate)
library(readr)
library(purrr)
library(lubridate)
library(labeling)
library(dplyr)
library(e1071)
library(data.table)

# Create edx set, validation set, and final file

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Exploratory Data Analysis

## Inital data Exploration

#The 10 Millions dataset is divided into two dataset: "edx" for training purpose and "validation" for the validation phase. 


#**edx dataset**
#Defining the root mean square error function
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  +     sqrt(mean((true_ratings - predicted_ratings)^2))}

#Viewing fisrt 6 rows of edx and validation dataframes
head(edx)
head(validation)

#Finding the number of unique users and movies in the edx dataframe
edx %>% summarize(Users = n_distinct(userId), Movies = n_distinct(movieId))

#***********Dataset Pre-Processing and Feature Engineering************************


# Convert timestamp to a human readable date


#Adding a date column to both frames with time zone as Greenwich Mean Time
validation$date <- as.POSIXct(validation$timestamp, origin='1970-01-01',tz="GMT")
edx$date <- as.POSIXct(edx$timestamp,origin='1970-01-01',tz="GMT")

#Viewing first 6 rows of edx and validation dataframes
head(validation)
head(edx)

#Adding month and year columns to both dataframes
validation$month_rated <- format(validation$date,"%m")
validation$year_rated <- format(validation$date,"%y")
edx$year_rated <- format(edx$date,"%y")
edx$month_rated <- format(edx$date,"%m")

#validation <- validation %>% mutate(title = str_squish(title)) %>% extract(title,c("titleTemp", "release"),regex = "^(.*) \\(([0-9 \\-]*)\\)$",remove = F) %>%mutate(release = if_else(str_length(release) > 4,as.integer(str_split(release, "-",simplify = T)[1]),as.integer(release)))  

#Adding release date column to validation
validation <- validation %>%mutate(title = str_trim(title)) %>%extract(title, c("titleTemp", "release"),regex = "^(.*) \\(([0-9 \\-]*)\\)$",remove = F) %>%mutate(release = if_else(str_length(release) > 4,as.numeric(str_split(release, "-",simplify = T)[1]),as.numeric(release))) %>%mutate(title = if_else(is.na(titleTemp),title,titleTemp))
validation<-validation %>% select(-titleTemp)

#Viewing fisrt 6 rows of edx and validation dataframes
head(validation)
head(edx)

#Adding release date column to edx
edx <- edx %>% mutate(title = str_squish(title)) %>% extract(title,c("titleTemp", "release"),regex = "^(.*) \\(([0-9 \\-]*)\\)$",remove = F) %>%mutate(release = if_else(str_length(release) > 4,as.integer(str_split(release, "-",simplify = T)[1]),as.integer(release))) 
edx<-edx %>% select(-titleTemp)
head(edx)

# Extract the genre in validation datasets
validation <- validation %>%mutate(genre = fct_explicit_na(genres,na_level = "(no genres listed)")) %>%separate_rows(genre, sep = "\\|")

# Extract the genre in edx datasets
edx <- edx %>% mutate(genre = fct_explicit_na(genres,na_level = "(no genres listed)")) %>% separate_rows(genre,sep = "\\|")

#Viewing fisrt 6 rows of edx and validation dataframes
head(edx)
head(validation)

#Removing the unnecessary columns in both data frames
validation <- validation %>% select(-genres,-timestamp,-date)
edx <- edx %>% select(-genres,-timestamp,-date)

# Convert the columns into the desired data type

validation$month_rated <- as.numeric(validation$month_rated)

validation$year_rated <- as.numeric(validation$year_rated)

edx$year_rated <- as.numeric(edx$year_rated)

edx$month_rated <- as.numeric(edx$month_rated)

#******************Processed edx datadaset****************************

head(edx)

#******************Processed validation datadaset***************************

head(validation)

#**************END OF DATA PROCESSING************************************

#*******MOVIE AND RATING HISTOGRAMS************************************


#movies released per year
hist(edx$release)

#rating distribution
hist(edx$rating, main="Distribution of User's Ratings", xlab="Rating")

#********RATING FREQUENCY DISTRIBUTION HISTOGRAMS*****

### Numbers of Ratings per Movie

ggplot(edx, aes(movieId)) + theme_grey()  + geom_histogram(bins=700) + labs(title = "Ratings Frequency Distribution Per Title (MovieID)",x = "Title (MovieID)",y = "Frequency")


#Number of ratings per user id

ggplot(edx, aes(userId)) +theme_grey()  +geom_histogram(bins=200) +labs(title = "Ratings Frequency Distribution Per Title (MovieID)",x = "Title (MovieID)",y = "Frequency")

#********************RATING FREQUENCY DISTRIBUTION PLOTS***********
#Rating frequency distribution

ggplot(edx, aes(rating)) + theme_grey()  + geom_histogram() +labs(title = "Ratings Frequency Distribution ", x = "Rating", y = "Frequency")

#Ratings Frequency Distribution Per Title - TOP 20 Movies

edx %>% group_by(title) %>% summarise(count = n()) %>% arrange(desc(count)) %>%head(n=20) %>% ggplot(aes(title, count)) +theme_gray()  +geom_col() +theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +labs(title = "Ratings Frequency Distribution Per Title - TOP 20 Movies",x = "Title",y = "Frequency")

#Ratings Frequency Distribution Per Year - TOP 10

edx %>% group_by(release) %>% summarise(count = n()) %>%arrange(desc(count)) %>%head(n=10) %>%ggplot(aes(release, count)) +theme_gray()  +geom_col() +theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +labs(title = "Ratings Frequency Distribution Per Year - TOP 10",x = "Title", y = "Frequency")

#Ratings Frequency Distribution Per genre - TOP 15

edx %>%group_by(genre) %>%summarise(count = n()) %>%arrange(desc(count)) %>% head(n=15) %>%ggplot(aes(genre, count)) +theme_gray()  +geom_col() +theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +labs(title = "Ratings Frequency Distribution Per genre - TOP 15",x = "Title",y = "Frequency")
head(edx)

#**************RATING FREQUENCY DISTRIBUTION TABLES*******************
#Rating frequency distribution per title

edx %>% group_by(title) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(n=25)

#Rating frequency distribution per release year

edx %>% group_by(release) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(n=25)

#*****************MEDIAN DISTRIBUTION HISTOGRAMS*******************
### Median Distribution per Title (Movie) histogram

edx %>%
  group_by(title) %>%
  summarise(median = median(rating)) %>%
  ggplot(aes(median)) +
  theme_gray()  +
  geom_histogram(bins=12) +
  labs(title = "Median Distribution per Title",x = "Median",y = "Frequency")

### Median distribution per release (year) histogram

edx %>%
  group_by(release) %>%
  summarise(median = median(rating)) %>%
  ggplot(aes(median)) +
  theme_gray()  +
  geom_histogram(bins=12) +
  labs(title = "Median Distribution per Release",x = "Median",y = "Frequency")

### Median distribution per user histogram

edx %>%
  group_by(userId) %>%
  summarise(median = median(rating)) %>%
  ggplot(aes(median)) +
  theme_gray()  +
  geom_histogram(bins=12) +
  labs(title = "Median Distribution per user",x = "Median",y = "Frequency")



#*************************MEDIAN DISTRIBUTION TABLES*******************

### Median distribution per title(movie) table

edx %>%
  group_by(title) %>%
  summarise(median = median(rating)) %>%
  arrange(desc(median)) %>%
  head(n=25)

### Median distribution per release year table

edx %>%
  group_by(release) %>%
  summarise(median = median(rating)) %>%
  arrange(desc(median)) %>%
  head(n=25)

### Median distribution per user table

edx %>%
  group_by(userId) %>%
  summarise(median = median(rating)) %>%
  arrange(desc(median)) %>%
  head(n=25)

#*************************MEAN DISTRIBUTION HISTOGRAMS*****************

###Mean distribution per title histogram

edx %>%
  group_by(title) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(mean)) +
  theme_light()  +
  geom_histogram(bins=12) +
  labs(title = "Mean Distribution per Title",x = "Mean",y = "Frequency")

###Mean distribution per release histogram

edx %>%
  group_by(release) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(mean)) +
  theme_bw()  +
  geom_histogram(bins=12) +
  labs(title = "Mean Distribution per release",x = "Mean", y = "Frequency")

###Mean distribution per user histogram

edx %>%
  group_by(userId) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(mean)) +
  theme_bw()  +
  geom_histogram(bins=12) +
  labs(title = "Mean Distribution per user",x = "Mean",y = "Frequency")


#*************************MEAN DISTRIBUTION TABLES***********************

##MEAN DISTRIBUTION PER TITLE TABLE

edx %>%
  group_by(title) %>%
  summarise(mean = mean(rating)) %>%
  arrange(desc(mean)) %>%
  head(n=10)

##MEAN DISTRIBUTION PER RELEASE YEAR TABLE

edx %>%
  group_by(release) %>%
  summarise(mean = mean(rating)) %>%
  arrange(desc(mean)) %>%
  head(n=10)

##MEAN DISTRIBUTION PER USER TABLE
edx %>%
  group_by(userId) %>%
  summarise(mean = mean(rating)) %>%
  arrange(desc(mean)) %>%
  head(n=10)



##*****************************Rating distributions****************************

### Rating Distribution per Genre

#**Overview of Rating distribution over Genre**

edx %>%
  group_by(genre) %>%
  summarise(count = n()) %>%
  ggplot(aes(genre, count)) +
  theme_gray()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Ratings Frequency Distribution Per Genre",x = "Genre",y = "Frequency")

#**Overview of Rating distribution over months**

edx %>%
  group_by(month_rated) %>%
  summarise(count = n()) %>%
  ggplot(aes(month_rated, count)) +
  theme_gray()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Ratings Frequency Distribution Per month",x = "month",y = "Frequency")

#**************************RATING FREQUENCY TABLES*******************

#rating per genre

edx %>%
  group_by(genre) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#rating per month

edx %>%
  group_by(month_rated) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#*********************Rating distribution plots**********************************
### Mean Distribution per Genre

edx %>%
  group_by(genre) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(genre, mean)) +
  theme_bw()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mean Distribution per Genre",x = "Genre",y = "Mean")

### Median Distribution per Genre

edx %>%
  group_by(genre) %>%
  summarise(median = median(rating)) %>%
  ggplot(aes(genre, median)) +
  theme_grey()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Median Distribution per Genre",x = "Genre",y = "Median")

### Mode Distribution per Genre
edx %>%
  group_by(genre) %>%
  summarise(mode = mode(rating)) %>%
  ggplot(aes(genre, mode)) +
  theme_gray()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mode Distribution per Genre",x = "Genre",y = "Mode")

#*************Distribution tables*********************************

####median distribution per genre

edx %>%
  group_by(genre) %>%
  summarise(median = median(rating)) %>%
  arrange(desc(median)) %>%
  head(n=10)

###mean distribution per genre

edx %>%
  group_by(genre) %>%
  summarise(mean = mean(rating)) %>%
  arrange(desc(mean)) %>%
  head(n=10)

#************* Analysis - Model Building and Evaluation********************

## Naive Baseline Model
mean(edx$rating)

### Naive Mean-Baseline Model

# Calculate the average of all movies
mu_hat <- mean(edx$rating)
mu_hat

# Predict the RMSE on the validation set
rmse_mean_result <- RMSE(validation$rating, mu_hat)
rmse_mean_result

# Creating a results dataframe that contains all RMSE results
rmse_results <- data.frame(model="Naive Mean-Baseline Model", RMSE=rmse_mean_result)
rmse_results

## Movie-Based Model, a Content-based Approach
# Calculate the average per each movie

movie_rmse <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))
movie_rmse

# Predict the ratings for validation dataset

rmse_validation <- validation %>%
  left_join(movie_rmse, by='movieId') %>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)
rmse_validation

rmse_validation_result <- RMSE(validation$rating, rmse_validation)
rmse_validation_result

# Adding the results to the results dataset
rmse_results <- rmse_results %>% add_row(model="Movie-Based Model", RMSE=rmse_validation_result)
rmse_results

#The RMSE on the ```validation``` dataset is **0.9417822**. It is slightly better than the Naive Mean-Baseline Model, but it is also far from the required RMSE (below 0.87) leading to poor performance for the model.

## Movie + User Model, a User-based approach


# Calculate the average by movie
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))


# Calculate the average for every user
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

# Compute the predicted ratings on validation dataset
rmse_movie_plus_user_model <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

rmse_movie_plus_user_model_result <- RMSE(validation$rating, rmse_movie_plus_user_model)

# Adding the results to the results dataset
rmse_results <- rmse_results %>% add_row(model="Movie+User Based Model", RMSE=rmse_movie_plus_user_model_result)

rmse_results

#The movie plus user based model has achieved the
#required resultant RMSE of 0.863 which is less than 0.8649
#The model can be improved by using regularisation technique.

