#CAPSTONE Project



##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")




library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                          title = as.character(title),
                                          genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

library(dplyr)
nrow(edx)
ncol(edx)


head(edx)

sum(edx$rating==3)


edx %>% filter(rating==0) %>% head()


edx %>% distinct(movieId) %>% count()


edx %>% distinct(userId) %>% count()

library(stringr)

str_detect(edx$genres[1],"Comedy")


sum(str_detect(edx$genres,"Drama"))

sum(str_detect(edx$genres,"Comedy"))

sum(str_detect(edx$genres,"Thriller"))

sum(str_detect(edx$genres,"Romance"))

#edx %>% mutate(genlen = length(genres)) %>% which.max(genres)

edx %>% group_by(title) %>% summarize(n=n()) %>% arrange(-n)

edx %>% group_by(rating) %>% summarize(n=n()) %>% arrange(-n)

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


library(tidyr)

#separate_rows()

df <- tibble(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  z = c("1", "2,3,4", "5,6")
)

separate_rows(df, y, z, convert = TRUE)

rm(df)

genre_list <- unique(unlist(strsplit(edx$genres,"|", fixed=TRUE)))

#genre_list <- genre_list[1:19]

edx[1:10] %>% mutate(Comedy = ifelse(str_detect(genres,"Comedy"),TRUE,FALSE),
                     Romance = ifelse(str_detect(genres,"Romance"),TRUE,FALSE))

#edx[1:10] %>% mutate(genre_list[1] := ifelse(str_detect(genres,"Comedy"),TRUE,FALSE))
#for (g in genre_list){
#edx[1:10] %>% mutate_( g := ifelse(str_detect('genres',g),TRUE,FALSE))
#}
#for (g in genre_list){
#  print(g)
#}
#class(genre_list[1])
#library(rlang)

data <- edx %>% mutate(Romance = ifelse(str_detect(genres,"Romance"),TRUE,FALSE),
  Comedy = ifelse(str_detect(genres,"Comedy"),TRUE,FALSE),
  Action = ifelse(str_detect(genres,"Action"),TRUE,FALSE),
  Crime = ifelse(str_detect(genres,"Crime"),TRUE,FALSE),
  Thriller = ifelse(str_detect(genres,"Thriller"),TRUE,FALSE),
  Drama = ifelse(str_detect(genres,"Drama"),TRUE,FALSE),
  Sci_Fi = ifelse(str_detect(genres,"Sci-Fi"),TRUE,FALSE),
  Adventure = ifelse(str_detect(genres,"Adventure"),TRUE,FALSE),
  Children = ifelse(str_detect(genres,"Children"),TRUE,FALSE),
  Fantasy = ifelse(str_detect(genres,"Fantasy"),TRUE,FALSE),
  War = ifelse(str_detect(genres,"War"),TRUE,FALSE),
  Animation = ifelse(str_detect(genres,"Animation"),TRUE,FALSE),
  Musical = ifelse(str_detect(genres,"Musical"),TRUE,FALSE),
  Western = ifelse(str_detect(genres,"Western"),TRUE,FALSE),
  Mystery = ifelse(str_detect(genres,"Mystery"),TRUE,FALSE),
  Film_Noir = ifelse(str_detect(genres,"Film-Noir"),TRUE,FALSE),
  Horror = ifelse(str_detect(genres,"Horror"),TRUE,FALSE),
  Documentary = ifelse(str_detect(genres,"Documentary"),TRUE,FALSE),
  IMAX = ifelse(str_detect(genres,"IMAX"),TRUE,FALSE)
)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#We divide the data into training and test sets

library(caret)
library(dplyr)

test_index <- createDataPartition(y = data$rating, times = 1, p = 0.2, 
                                  list = FALSE)

train_set <- data[-test_index,]
test_set <- data[test_index,]

#We don't want movies or users in the test set that don't appear in the training set

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


#simple stupid model: Predict mean for all movies


mu_hat <- mean(data$rating)
mu_hat


naive_rmse <- RMSE(test_set$rating,mu_hat)

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results
mu <- mu_hat

#Y_{u,i} = \mu + b_i + \varepsilon_{u,i}

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

qplot(movie_avgs$b_i,bins=10,color=I("black"))


user_avgs <- train_set %>% 
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u= mean(rating-b_i-mu))


predicted_ratings <- test_set %>%
  left_join(movie_avgs, by="movieId")%>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i +b_u ) %>%
  pull(pred)


User_And_Movie_effect <- RMSE(predicted_ratings , test_set$rating)

rmse_results <- add_row(rmse_results,method = "Movie Average and User Average", RMSE = User_And_Movie_effect)
rmse_results

nmbr_ratings <- data %>% group_by(movieId) %>% summarise(n=n()) 


#Checking errors to see which are the one we got wrong

test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by="userId") %>%
  left_join(nmbr_ratings, by = "movieId") %>%
  mutate(residual = rating - (mu + b_i + b_u)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  select(title, residual, n) %>%
  knitr::kable()


#Mostly obscure. Need to penalize movies with few ratings
#Movies with few ratings but very good ratings, (1 good rating)

#Enter lambdas

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})


rmses


qplot(lambdas,rmses)

lambda <- lambdas[which.min(rmses)]

lambda
min(rmses)



rmse_results <- rmse_results %>% add_row(method ="Regularized user & Movie effects", RMSE =min(rmses))
rmse_results



library(tidyr)
library(ggplot2)

install.packages("recommenderlab")
library(dplyr)
library(recommenderlab)

sparse_m <- train_set %>% 
  select(userId,movieId,rating)%>%
  spread(movieId,rating)%>%
  as.matrix()%>%
  as("realRatingMatrix")
  
view(head(data))


#confirming behavior of spread. acast could also have ben used
train_set[1:50] %>% 
  select(userId,movieId,rating)%>%
  spread(movieId,rating)

head(sparse_m)
view(train_set[1:50] )


as(sparse_m, "matrix") [,1:10]


UBCF_model <- Recommender(sparse_m,method="UBCF",param=list(normalize = "center"))

data("MovieLense")

MovieLense[101:102]



predict(UBCF_model,,n=10)


#Getting Time since first rating

#Plot changing average rating by time for regularly rating users as motivation

library(lubridate)

#filter out top 10 users
top_users <- data %>% 
  group_by(userId)%>%
  summarise(n=n(), first_rating= min(timestamp))

#%>%
 # top_n(10)

#See how their rating changes over time
data%>%
#  filter(userId %in% top_users$userId) %>%
#  filter(userId==58357) %>%
  left_join(top_users, by="userId") %>%
  mutate(date = as.Date(as.POSIXct(timestamp - first_rating, origin = "1970-01-01") )) %>%
  mutate(month = floor_date(date, unit = "month")) %>%
  group_by(month) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(
    aes(month,avg_rating)#,color=as.factor(userId))
    )+
  geom_line()
#+ scale_x_date(month = "%b/%d")


#Check out all the ratings made by user 58357 over time
data%>% filter(userId==58357) %>% 
  mutate(date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01") ))%>%
  arrange(-rating)%>%
  view()


#check how ratings of particular genres changed over time?

#  scale_x_date(date_labels = "%b/%d")




###



df <- tibble(date, value) %>% 
  group_by(month = floor_date(date, unit = "month")) %>%
  summarize(avg = mean(value))

ggplot(df, aes(x = month, y = avg)) + 
  geom_bar(stat = "identity") + 
  scale_x_date(NULL, date_labels = "%b %y", breaks = "month")  
###


#movie_avgs[1:10] %>% 

data[1:10] %>% 
  mutate(release_year = substr(title,nchar(title)-4,nchar(title)-1))%>%
  group_by(movieId)%>%
  summarise(Time_Since_1st= min(timestamp))
  