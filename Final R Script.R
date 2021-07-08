
# Dependencies

packages_required <-c("dplyr", # Wrangling
                      "ggplot2", #for graphics
                      "stringr", #string operations
                      "tidyr", # Wrangling
                      "caret", #for CreateDataPartition and train()
                      "ggthemes", #for graphics themes
                      "lubridate", #for dealing with dates
                      "tidyverse", # Wrangling
                      "data.table", #for wrangling
                      "Matrix", #for SparseMatrix
                      "recosystem", #for Matrix Factorization model
                      "ggthemes", #for ggplot theme economist
                      "recommenderlab",#for UBCF,IBCF, POPULAR models
                      "knitr" #for RMD options 
) 

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}


using(packages_required)


# Create Initial Dataframes

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
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                           genres = as.character(genres))


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

rm(dl, ratings, movies, temp, movielens, removed,test_index)



# Basic Exploration

paste("The dataset has " , nrow(edx) , "ratings")
paste("There are " , ncol(edx), "columns")

#Checking data validity

paste("There are " , edx %>% filter(rating==0) %>% nrow(), "ratings = 0")
paste("There are " , edx %>% filter(rating>5) %>% nrow(), "ratings > 5")


paste("There are " , edx %>% distinct(movieId) %>% count(), "distinct movies")
paste("There are " ,edx %>% distinct(userId) %>% count(), "distinct users")
summary(edx)
glimpse(edx)


# how many movies have rating X?

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()+
  theme_economist()

# how many movies do users rate ?

edx %>%
  group_by(userId)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  ggplot(aes(n))+
  geom_histogram(bins=200)+
  scale_x_continuous(limits = c(0,750))+
  theme_economist()

# how many ratings do individual movies usually receive?

edx %>%
  group_by(movieId)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  ggplot(aes(n))+
  geom_histogram(bins=200)+
  scale_x_continuous(limits = c(0,750))+
  theme_economist()

# What is the most common rating a movie receives?

mu <- mean(edx$rating)

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

qplot(movie_avgs$b_i,bins=10,color=I("red"))


#How do most users rate?

mu <- mean(edx$rating)

user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

qplot(user_avgs$b_u,bins=10,color=I("red"))


# Which genres get the best rating? What is their spread?

#The following code is decodes the genre data by adding a 0 or an NA based on 
#whether the name of the genre is present in the genre column
n_a=NA
genre_data <-   edx %>% mutate(
  Romance = ifelse(str_detect(genres,"Romance"),1*rating,n_a),
  Comedy = ifelse(str_detect(genres,"Comedy"),1*rating,n_a),
  Action = ifelse(str_detect(genres,"Action"),1*rating,n_a),
  Crime = ifelse(str_detect(genres,"Crime"),1*rating,n_a),
  Thriller = ifelse(str_detect(genres,"Thriller"),1*rating,n_a),
  Drama = ifelse(str_detect(genres,"Drama"),1*rating,n_a),
  Sci_Fi = ifelse(str_detect(genres,"Sci-Fi"),1*rating,n_a),
  Adventure = ifelse(str_detect(genres,"Adventure"),1*rating,n_a),
  Children = ifelse(str_detect(genres,"Children"),1*rating,n_a),
  Fantasy = ifelse(str_detect(genres,"Fantasy"),1*rating,n_a),
  War = ifelse(str_detect(genres,"War"),1*rating,n_a),
  Animation = ifelse(str_detect(genres,"Animation"),1*rating,n_a),
  Musical = ifelse(str_detect(genres,"Musical"),1*rating,n_a),
  Western = ifelse(str_detect(genres,"Western"),1*rating,n_a),
  Mystery = ifelse(str_detect(genres,"Mystery"),1*rating,n_a),
  Film_Noir = ifelse(str_detect(genres,"Film-Noir"),1*rating,n_a),
  Horror = ifelse(str_detect(genres,"Horror"),1*rating,n_a),
  Documentary = ifelse(str_detect(genres,"Documentary"),1*rating,n_a),
  IMAX = ifelse(str_detect(genres,"IMAX"),1*rating,n_a)
)


#In order to make a errorbar plot , we now need a table containing genre and rating
# we first gather all the columns into a single values column called rating and then
# we filter out all the na rows
# later we calculate the 2se for making the error plot
genre_data %>% 
  select(Romance:IMAX) %>%
  gather(., key="Genre",value = "rating") %>%
  filter(is.na(rating)==FALSE) %>%
  group_by(Genre) %>%
  summarise(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n)) %>%
  mutate(Genre = reorder(Genre, avg)) %>%
  ggplot(aes(x = Genre, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Error bar plots by Genre" , caption = "Separated Genres from edx dataset") 



# Does the mean rating change over time?

#Here, we use functions from the lubridate package to extract the week from the timestamp
edx %>% 
  mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(week) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(week, rating)) +
  geom_point() + 
  geom_smooth() + 
  ggtitle("Rating evolution by week")+
  theme_economist()


# Is a rating given at night different than one given in the morning?

edx %>% 
  mutate(hour = hour(as_datetime(timestamp))) %>%
  group_by(hour) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(hour, rating)) +
  geom_point() + 
  geom_smooth() + 
  ggtitle("Rating evolution by hour")+
  theme_economist()


# Add columns for each genre

#We choose 0 or 1 as the output since this will make it easier to make a linear model

data <-  edx %>% mutate(Romance = ifelse(str_detect(genres,"Romance"),1,0),
                        Comedy = ifelse(str_detect(genres,"Comedy"),1,0),
                        Action = ifelse(str_detect(genres,"Action"),1,0),
                        Crime = ifelse(str_detect(genres,"Crime"),1,0),
                        Thriller = ifelse(str_detect(genres,"Thriller"),1,0),
                        Drama = ifelse(str_detect(genres,"Drama"),1,0),
                        Sci_Fi = ifelse(str_detect(genres,"Sci-Fi"),1,0),
                        Adventure = ifelse(str_detect(genres,"Adventure"),1,0),
                        Children = ifelse(str_detect(genres,"Children"),1,0),
                        Fantasy = ifelse(str_detect(genres,"Fantasy"),1,0),
                        War = ifelse(str_detect(genres,"War"),1,0),
                        Animation = ifelse(str_detect(genres,"Animation"),1,0),
                        Musical = ifelse(str_detect(genres,"Musical"),1,0),
                        Western = ifelse(str_detect(genres,"Western"),1,0),
                        Mystery = ifelse(str_detect(genres,"Mystery"),1,0),
                        Film_Noir = ifelse(str_detect(genres,"Film-Noir"),1,0),
                        Horror = ifelse(str_detect(genres,"Horror"),1,0),
                        Documentary = ifelse(str_detect(genres,"Documentary"),1,0),
                        IMAX = ifelse(str_detect(genres,"IMAX"),1,0)
)

glimpse (data)



# Ready for modeling! Create test and train

#We divide the data into training and test sets. 
#Before this, we set a seed to ensure that the work is reproducible
#The index already made in the first code chunk was to separate 
#the validation set so we need to remake the index


#We choose an 80/20 split, the standard in ML

test_index <- createDataPartition(y = data$rating, times = 1, p = 0.2, list = FALSE)

train_set <- data[-test_index,]
test_set <- data[test_index,]

#We don't want movies or users in the test set that don't appear in the training set

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Validation set was already created in the code chunk Initial dataframes



# Function to create sparse matrix. Somehow, recommenderlab models change the prior data to its handy to have a function to reproduce it

#For using recommenderlab models, we need to make a sparse matrix and partition it 
#using the inbuilt partition functions

create_sparse_m <- function(){
  
  sparse_m <- sparseMatrix(
    i = as.numeric(as.factor(edx$userId)),
    j = as.numeric(as.factor(edx$movieId)), 
    x = edx$rating,
    dims = c(length(unique(edx$userId)),
             length(unique(edx$movieId))),
    dimnames = list(paste("u", 1:length(unique(edx$userId)), sep = ""),
                    paste("m", 1:length(unique(edx$movieId)), sep = "")))
  
  sparse_m <- new("realRatingMatrix", data = sparse_m)
  return (sparse_m)
}

# Data Reduction


#90 percentile data selection
sparse_m <- create_sparse_m()
y = 0.9 
min_movies_by_user <- quantile(rowCounts(sparse_m),y)
min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
  


# Function to calculate RMSE

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Basic Model: predict mean

mu <- mean(data$rating)

#We make a a dataframe as long as test_set with mu repeated as R Markdown 
#interprets the difference in length between mu and test_set as an error.

naive_rmse <- RMSE(test_set$rating,rep(mu,nrow(test_set)))
naive_rmse

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)


# User and Movie model, non regularized

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

user_avgs <- train_set %>% 
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u= mean(rating-b_i-mu))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by="movieId")%>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i +b_u ) %>%
  pull(pred)

User_And_Movie_effect <- RMSE(test_set$rating,predicted_ratings)

User_And_Movie_effect 

rmse_results <- add_row(rmse_results,method = "Movie Average and User Average", 
                        RMSE = User_And_Movie_effect)


# Motivation for regularization

#Checking errors to see which are the one we got wrong
nmbr_ratings <- data %>% group_by(movieId) %>% summarise(n=n()) 
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by="userId") %>%
  left_join(nmbr_ratings, by = "movieId") %>%
  mutate(residual = rating - (mu + b_i + b_u)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  select(title, residual, n) %>%
  knitr::kable()


# Regularized user and movie model

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

lambda <- lambdas[which.min(rmses)]
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

gc()
qplot(lambdas,rmses)

rmse_results <- rmse_results %>% add_row(method ="Regularized user & Movie effects", RMSE =min(rmses))
rmse_results



# Regularized User Movie, Hour model

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
  
  b_t <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    mutate(hour = hour(as_datetime(timestamp))) %>%
    group_by(hour) %>%
    summarize(b_t = sum(rating - mu - b_i - b_u)/(n()+l) ) 
  
  predicted_ratings <- 
    test_set %>% 
    mutate(hour = hour(as_datetime(timestamp))) %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "hour") %>%
    mutate(pred = mu + b_i + b_u + b_t) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})


lambda <- lambdas[which.min(rmses)]

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu -b_i)/(n()+lambda))

b_t <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(hour = hour(as_datetime(timestamp))) %>%
  group_by(hour) %>%
  summarize(b_t = sum(rating - mu - b_i - b_u)/(n()+lambda) )

qplot(lambdas,rmses)

gc()#Garbage collection

rmse_results <- rmse_results %>% add_row(method ="Regularized user,movie,time effects",
                                         RMSE =min(rmses))

# Regularized user movie, time model with lm genre

genre_train <- train_set %>% 
  mutate(hour = hour(as_datetime(timestamp))) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "hour") %>%
  mutate(residual = rating - mu- b_i-b_u-b_t ) 

temp <- genre_train %>% select(c(Romance:IMAX,residual)) 
lm_fit <-   lm(residual ~ ., data=temp)

temp2 <- test_set %>% select(c(Romance:IMAX))  

pred <- predict.lm(lm_fit, newdata = temp2)

predicted_ratings <- 
  test_set %>% 
  mutate(hour = hour(as_datetime(timestamp))) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId")%>%
  left_join(b_t, by = "hour") %>%
  mutate(pred = mu + b_i + b_u + b_t + pred) %>%
  pull(pred)


rmse_genre_effect <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- rmse_results %>% add_row(method ="Regularized user,movie,time effects with Genre (lm)",
                                         RMSE =min(rmse_genre_effect))

rm(temp,temp2)

rmse_results



# UBCF Model

y=0.9
sparse_m <- create_sparse_m()
nn=15
set.seed(1991)
min_movies_by_user <- quantile(rowCounts(sparse_m),y)
min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, 
                             colCounts(sparse_m)>=min_ratings_per_movie]
e4 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                       k=1, given=8)
r_UBCF <- Recommender(getData(e4, "train"), "UBCF",parameter=c(nn=nn))
p_UBCF <- predict(r_UBCF, getData(e4, "known"), type="ratings")
rmse_UBCF <- calcPredictionAccuracy(p_UBCF, getData(e4, "unknown"))[1]

rmse_results <- rmse_results %>% add_row(method ="UBCF using reccomenderlab", 
                                         RMSE =rmse_UBCF)

# IBCF Model

y=0.9
sparse_m <- create_sparse_m()
nn=15
set.seed(1991)
min_movies_by_user <- quantile(rowCounts(sparse_m),y)
min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, 
                             colCounts(sparse_m)>=min_ratings_per_movie]
model_ibcf <- Recommender(sparse_m_limited, method="IBCF", param=list(normalize="center"))
#Testing prediction
#pred_pop <- predict(model_ibcf, sparse_m[1:10], type="ratings")

#as(pred_pop, "matrix")[,1:10]
#Finding RMSE
e6 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                       k=1, given=8)
p_IBCF <- predict(model_ibcf, getData(e6, "known"), type="ratings")
rmse_IBCF <- calcPredictionAccuracy(p_IBCF, getData(e6, "unknown"))[1]

rmse_results <- rmse_results %>% add_row(method ="IBCF using reccomenderlab", RMSE =rmse_IBCF)

# Popular model

sparse_m <- create_sparse_m()
#Reduction parameter
y=0.9
min_movies_by_user <- quantile(rowCounts(sparse_m),y)
min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)

sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, 
                             colCounts(sparse_m)>=min_ratings_per_movie]
#Making the model
model_popular <- Recommender(sparse_m_limited, method="POPULAR", param=list(normalize="center"))
#Testing prediction

#Finding RMSE
e5 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                       k=1, given=8)
p_POPULAR <- predict(model_popular, getData(e5, "known"), type="ratings")
rmse_POP <- calcPredictionAccuracy(p_POPULAR, getData(e5, "unknown"))[1]


rmse_results <- rmse_results %>% add_row(method ="POPULAR using reccomenderlab", RMSE =rmse_POP)


# Recosystem Matrix factorization model

#we select only the userID, MovieID and rating in the next three statements
test_GD <- as.matrix (test_set [,1:3])
train_GD <- as.matrix(test_set [,1:3])


set.seed(1)

#We need to build data stream objects as these are accepted inputs for this algorithm
train_GD_2 <- data_memory(train_GD[,1],train_GD[,2],train_GD[,3])
test_GD_2 <- data_memory(test_GD[,1],test_GD[,2],test_GD[,3])


#Next step is to build Recommender object
r = Reco()
# Matrix Factorization :  tuning training set
# lrate is the gradient descend step rate
# dim are the number of latent factors
# nthread is number of threads to use : REDUCE IF YOUR PROCESSOR DOESN'T SUPPORT 6 THREADS

opts = r$tune(train_GD_2, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 6, niter = 10))


r$train(train_GD_2, opts = c(opts$min, nthread = 6, niter = 20))

pred <- r$predict(test_GD_2, out_memory())

rmse_MFGD <- RMSE(pred,test_set$rating)

rmse_results <- rmse_results %>% add_row(method ="Matrix Factorization using recosystem", 
                                         RMSE =rmse_MFGD)

rmse_results


# Checking RMSE on validation set with MF model from recosystem

#Selecting only pertinent columns
valid_GD <-as.matrix(validation [,1:3])
valid_GD_2 <- data_memory(valid_GD[,1],valid_GD[,2],valid_GD[,3])

#selecting to output to variable
pred <- r$predict(valid_GD_2, out_memory())

rmse_MFGD_final <- RMSE(pred,validation$rating)

rmse_results <- rmse_results %>% add_row(method ="Matrix Factorization using recosystem : FINAL Validation RMSE ",
                                         RMSE =rmse_MFGD_final)

options(pillar.sigfig = 10)
rmse_results






#------- END -----


# Testing of reccomenderlab models

#RecommenderLab Full Redo

install.packages("tictoc")

library(recommenderlab)
library(dplyr)
library(Matrix)
library(tictoc)

create_sparse_m <- function(){
  
  sparse_m <- sparseMatrix(
    i = as.numeric(as.factor(edx$userId)),
    j = as.numeric(as.factor(edx$movieId)), 
    x = edx$rating,
    dims = c(length(unique(edx$userId)),
             length(unique(edx$movieId))),
    dimnames = list(paste("u", 1:length(unique(edx$userId)), sep = ""),
                    paste("m", 1:length(unique(edx$movieId)), sep = "")))
  
  sparse_m <- new("realRatingMatrix", data = sparse_m)
  return (sparse_m)
  
}


identical(sparse_m,sparse_m2)

create_sparse_m()


as(sparse_m[1:10,1:10],"matrix")
as(sparse_m2[1:10,1:10],"matrix")

tic("Full r_UBCF_nn_25_given~15/36")
## create 90/10 split (known/unknown) for the first 500 users in Jester5k

given <- min(rowCounts(sparse_m))


e <- evaluationScheme(sparse_m, method="split", train=0.9,
                      k=1, given=4)
e
## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF", parameter= c(nn=15))
## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type="ratings")
p

as(p[1:5,1:5],"matrix")

## compute error metrics averaged per user and then averaged over all
## recommendations
rmse_UBCF <-calcPredictionAccuracy(p, getData(e, "unknown"))
#Above statement executed to give an RMSE of 1.08 on the whole dataset with all standard parameters.
#The predict function took ~8 hours to execute

#head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser=TRUE))

gc()

toc()
#seq(0,1000,by=100)

#sparse_m <- new("realRatingMatrix", data = sparse_m)

#hist(rowCounts(sparse_m)[rowCounts(sparse_m) >= 0 &rowCounts(sparse_m)< 1000], xlim=c(0,1000),breaks = seq(0,1000,by=25), main="Movie Rating Count") 

#median(rowCounts(sparse_m))
#quantile(rowCounts(sparse_m),0.25)

#Making a model with the entire set of users and movies takes 8 hours
#the median user rates 51 movies
#Let us eliminate the bottom x percentiles.

#hist(colCounts(sparse_m)[colCounts(sparse_m)<2000], main="Ratings by movie Count", xlim = c(0,2000)) 
#quantile(colCounts(sparse_m))


x<-0.25

min_movies_by_user <- quantile(rowCounts(sparse_m),x)

min_ratings_per_movie<- quantile(rowCounts(sparse_m),x)

sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]

#tail(recommenderRegistry$get_entries(dataType = "realRatingMatrix"), 1)

tic("75 pc r_UBCF_nn_25_given_4")

#min(rowCounts(sparse_m_limited))
dim(sparse_m_limited) - dim(sparse_m)

## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e2 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                       k=1, given=4)


r_UBCF_nn_25 <- Recommender(getData(e2, "train"), "UBCF")
## create predictions for the test data using known ratings (see given above)
p_UBCF_nn_25 <- predict(r_UBCF_nn_25, getData(e2, "known"), type="ratings")
p_UBCF_nn_25

## compute error metrics averaged per user and then averaged over all
## recommendations
rmse_UBCF_limited_data <- calcPredictionAccuracy(p_UBCF_nn_25, getData(e2, "unknown"))

toc()


test_percentiles <- seq(0.95,0.25,-0.10)

#Checking time taken by percentile exclusion of data

time_and_rmse <- sapply(test_percentiles, function(y){
  
  tic(y)  
  min_movies_by_user <- quantile(rowCounts(sparse_m),y)
  min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
  sparse_m_limited_t <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
  e3 <- evaluationScheme(sparse_m_limited_t , method="split", train=0.9,
                         k=1, given=4)
  r_UBCF_nn_25_t <- Recommender(getData(e3, "train"), "UBCF")
  p_UBCF_nn_25_t <- predict(r_UBCF_nn_25_t, getData(e3, "known"), type="ratings")
  rmse_ <- calcPredictionAccuracy(p_UBCF_nn_25_t, getData(e3, "unknown"))[1]
  
  
  print(rmse_)
  
  return(list(perc = y,time = toc(),rmse = rmse_))
  
})



#Checking response of rmse to change in nn keeping percentile reduction = 0.9
tic.clearlog()
test_nn <- seq(10,50,5)

time_and_rmse <- sapply(test_nn, function(nn){
  
  y=0.9
  tic(nn)  
  min_movies_by_user <- quantile(rowCounts(sparse_m),y)
  min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
  sparse_m_limited_nn <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
  e4 <- evaluationScheme(sparse_m_limited_nn , method="split", train=0.9,
                         k=1, given=4)
  r_UBCF_nn <- Recommender(getData(e4, "train"), "UBCF",parameter=c(nn=nn))
  p_UBCF_nn <- predict(r_UBCF_nn, getData(e4, "known"), type="ratings")
  rmse_ <- calcPredictionAccuracy(p_UBCF_nn, getData(e4, "unknown"))[1]
  
  toc(log=TRUE,quiet = FALSE)
  print(paste(nn," is nn. ", rmse_, " is RMSE" ))
  
  return(list(perc = nn, rmse = rmse_))
  
})

#RMSE minimum at nn=25




#Checking response of rmse to change in given keeping percentile reduction = 0.2

min(rowCounts(sparse_m))
tic.clearlog()
test_given <- seq(3,8,1)

given_test <- sapply(test_given, function(g){
  
  y=0.2
  tic(g)  
  min_movies_by_user <- quantile(rowCounts(sparse_m),y)
  min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
  sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
  e4 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                         k=1, given=g)
  r_UBCF <- Recommender(getData(e4, "train"), "UBCF",parameter=c(nn=25))
  p_UBCF <- predict(r_UBCF, getData(e4, "known"), type="ratings")
  rmse_ <- calcPredictionAccuracy(p_UBCF, getData(e4, "unknown"))[1]
  
  toc(log=TRUE,quiet = FALSE)
  print(paste(g," is given. ", rmse_, " is RMSE" ))
  
  return(list(given = g, rmse = rmse_))
  
})

log_g1 <- tic.log(format = TRUE)

given_test
#increasing given reduced rmse until given was 7. Then reduced.


#Checking response of rmse to change in given with no reduction of data 


min(rowCounts(sparse_m))
tic.clearlog()
test_given <- seq(3,8,1)

given_test_2 <- sapply(test_given, function(g){
  
  y=0.2
  tic(g)  
  min_movies_by_user <- 0
  min_ratings_per_movie<- 0
  sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
  e4 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                         k=1, given=g)
  r_UBCF <- Recommender(getData(e4, "train"), "UBCF",parameter=c(nn=25))
  p_UBCF <- predict(r_UBCF, getData(e4, "known"), type="ratings")
  rmse_ <- calcPredictionAccuracy(p_UBCF, getData(e4, "unknown"))[1]
  
  toc(log=TRUE,quiet = FALSE)
  print(paste(g," is given. ", rmse_, " is RMSE" ))
  
  return(list(given = g, rmse = rmse_))
  
})

log_g2 <- tic.log(format = TRUE)


#One iteration ran in 17.6 K secs and RMSE 2926 (bullshit)


#Given Test 3 : Adding sparse_m creation function and gc()
#Checking response of rmse to change in given keeping percentile reduction = 0.9

min(rowCounts(sparse_m))
tic.clearlog()
test_given <- seq(8,7,-1)

given_test_3 <- sapply(test_given, function(g){
  y=0.9
  tic(g)
  sparse_m <- create_sparse_m()
  
  print(identical(sparse_m,sparse_m2))
  
  min_movies_by_user <- quantile(rowCounts(sparse_m),y)
  min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
  sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
  e4 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                         k=1, given=g)
  r_UBCF <- Recommender(getData(e4, "train"), "UBCF",parameter=c(nn=25))
  p_UBCF <- predict(r_UBCF, getData(e4, "known"), type="ratings")
  rmse_ <- calcPredictionAccuracy(p_UBCF, getData(e4, "unknown"))[1]
  
  toc(log=TRUE,quiet = FALSE)
  print(paste(g," is given. ", rmse_, " is RMSE" ))
  
  gc()
  print(identical(sparse_m,sparse_m2))
  return(list(given = g, rmse = rmse_))
  
})

log_g1 <- tic.log(format = TRUE)

given_test_3

#FINALLY!!! given 7 decreased rmse to 1.009 from 1.029



#Testing with different quantities of data from 0.9 - 0.5 and for given from 8-4 , nn from 15 to 55

test_perc_given_nn <- expand.grid(perc=seq(0.9,0.5,-0.1),given=seq(8,4,-1),nn=seq(15,55,10))
#min(rowCounts(sparse_m))
tic.clearlog()
#toString(test_perc_given_nn[1,],)
#test_perc_given_nn[1,1]
full_test_UBCF <- apply(test_perc_given_nn,1, function(t){
  
  print( paste("Now running model :", t[1], " is perc  ", t[2]," is given. ",  t[3]," is nn. "))
  tic(toString(t))
  y=t[1]
  sparse_m <- create_sparse_m()
  nn=t[3]
  print(identical(sparse_m,sparse_m2))
  set.seed(1991)
  min_movies_by_user <- quantile(rowCounts(sparse_m),y)
  min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
  sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
  e4 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                         k=1, given=t[2])
  r_UBCF <- Recommender(getData(e4, "train"), "UBCF",parameter=c(nn=nn))
  p_UBCF <- predict(r_UBCF, getData(e4, "known"), type="ratings")
  rmse_ <- calcPredictionAccuracy(p_UBCF, getData(e4, "unknown"))[1]
  
  toc(log=TRUE,quiet = FALSE)
  
  print(paste( rmse_, " is RMSE"))
  gc()
  print(identical(sparse_m,sparse_m2))
  return(list(perc = t[1] , given = t[2], nn=t[3] , rmse = rmse_))
  
})

log_full_test <- tic.log(format = TRUE)

full_test_UBCF
log_full_test

df <- data.frame(matrix(unlist(full_test_UBCF), nrow=125, byrow=TRUE),stringsAsFactors=FALSE)

df
which.min(df$X4)
min(df$X4)

#Best parameters are : perc = 0.9. given = 8, nn=15



#somehow, 8 validation set items aren't a part of the test and validation set

# Probably because we didn't semijoin on genre 

#With only 8 it wont make a difference to RMSE



na_index <- c(50641,118361,223470,487503,674188,688404,800929,830872)

8/nrow(validation)

val_set <- validation[-na_index]

pred <- predict.lm(lm_fit, newdata = temp2)

predicted_ratings_validation 

val_set <- val_set %>% 
  mutate(Romance = ifelse(str_detect(genres,"Romance"),1,0),
         Comedy = ifelse(str_detect(genres,"Comedy"),1,0),
         Action = ifelse(str_detect(genres,"Action"),1,0),
         Crime = ifelse(str_detect(genres,"Crime"),1,0),
         Thriller = ifelse(str_detect(genres,"Thriller"),1,0),
         Drama = ifelse(str_detect(genres,"Drama"),1,0),
         Sci_Fi = ifelse(str_detect(genres,"Sci-Fi"),1,0),
         Adventure = ifelse(str_detect(genres,"Adventure"),1,0),
         Children = ifelse(str_detect(genres,"Children"),1,0),
         Fantasy = ifelse(str_detect(genres,"Fantasy"),1,0),
         War = ifelse(str_detect(genres,"War"),1,0),
         Animation = ifelse(str_detect(genres,"Animation"),1,0),
         Musical = ifelse(str_detect(genres,"Musical"),1,0),
         Western = ifelse(str_detect(genres,"Western"),1,0),
         Mystery = ifelse(str_detect(genres,"Mystery"),1,0),
         Film_Noir = ifelse(str_detect(genres,"Film-Noir"),1,0),
         Horror = ifelse(str_detect(genres,"Horror"),1,0),
         Documentary = ifelse(str_detect(genres,"Documentary"),1,0),
         IMAX = ifelse(str_detect(genres,"IMAX"),1,0)) %>%
  mutate(hour = hour(as_datetime(timestamp))) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId")%>%
  left_join(b_t, by = "hour")

genre_pred <- predict.lm(lm_fit, newdata = val_set)


predicted_ratings_Validation<-val_set%>%
  cbind(genre_pred)%>%
  mutate(pred = mu + b_i + b_u + b_t + genre_pred) %>%
  pull(pred)

#na_index <-which(is.na(predicted_ratings_Validation))


RMSE(predicted_ratings_Validation,val_set$rating)
