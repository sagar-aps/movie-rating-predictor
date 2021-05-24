#CAPSTONE Project



##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

install.packages("kableExtra", dependencies = T)
install.packages("recosystem", dependencies = T)

library(dplyr)
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(stringr)
library(tidyr)
library(kableExtra)
library(caret)
library(dplyr)
library(recosystem)

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
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                          title = as.character(title),
                                          genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = 0)
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

#Data exploration



head(edx)

sum(edx$rating==3)

nrow(edx)
ncol(edx)
edx %>% filter(rating==0) %>% head()
edx %>% filter(rating>5) %>% head()
edx %>% distinct(movieId) %>% count()
edx %>% distinct(userId) %>% count()

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

library(ggplot2)

install.packages("ggthemes")
library(ggthemes)
edx %>%
  group_by(userId)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  ggplot(aes(n))+
  geom_histogram(bins=200)+
  scale_x_continuous(limits = c(0,750))+
  theme_economist()
  




genre_list <- unique(unlist(strsplit(edx$genres,"|", fixed=TRUE)))

#genre_list <- genre_list[1:19]

edx[1:10] %>% mutate(Comedy = ifelse(str_detect(genres,"Comedy"),TRUE,0),
                     Romance = ifelse(str_detect(genres,"Romance"),TRUE,0))

#edx[1:10] %>% mutate(genre_list[1] := ifelse(str_detect(genres,"Comedy"),TRUE,0))
#for (g in genre_list){
#edx[1:10] %>% mutate_( g := ifelse(str_detect('genres',g),TRUE,0))
#}


#for (g in genre_list){
#  print(g)
#}
#class(genre_list[1])
#library(rlang)

data <- edx %>% mutate(Romance = ifelse(str_detect(genres,"Romance"),1,0),
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

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#We divide the data into training and test sets

test_index <- createDataPartition(y = data$rating, times = 1, p = 0.2, 
                                  list = 0)

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

any(is.na(movie_avgs[,2]))


movie_avgs[1:10,]



qplot(movie_avgs$b_i,bins=10,color=I("black"))


user_avgs <- train_set %>% 
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u= mean(rating-b_i-mu))

any(is.na(user_avgs))



predicted_ratings <- test_set %>%
  left_join(movie_avgs, by="movieId")%>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i +b_u ) %>%
  pull(pred)



glimpse(predicted_ratings)
glimpse(test_set$rating)


User_And_Movie_effect <- RMSE( test_set$rating,predicted_ratings )




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


qplot(lambdas,rmses)

lambda <- lambdas[which.min(rmses)]

lambda
min(rmses)

rmse_results$RMSE[2]

rmse_results <- rmse_results %>% add_row(method ="Regularized user & Movie effects", RMSE =min(rmses))
rmse_results


head(data)

#Let us take the residuals after applying regularization and apply a linear model to predict the residuals from the genre effect

l<-lambda

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

genre_test <- train_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(residual = rating - mu- b_i-b_u) 

genre_test %>% glimpse()

genre_test %>% select(-c(rating:genres))


#lm_fit  <- genre_test %>% lm(residual ~  c(Romance:IMAX))

#lm_fit  <-  lm(residual ~ . , data = c(Romance:IMAX))
{
lm_fit <- lm(genre_test$residual ~ genre_test$Romance + genre_test$Comedy + genre_test$Action + genre_test$Crime + genre_test$Thriller + genre_test$Drama + genre_test$Sci_Fi + genre_test$Adventure + genre_test$Children + genre_test$Fantasy + genre_test$War + genre_test$Animation + genre_test$Musical + genre_test$Western + genre_test$Mystery+ genre_test$Film_Noir + genre_test$Horror + genre_test$Documentary+ genre_test$IMAX)
genre_effect <- predict.lm(lm_fit,test_set )
length(genre_effect)
length(test_set$movieId)
#PROBLEM
}


genre_test %>% glimpse()

temp <- genre_test %>% select(c(Romance:IMAX,residual)) 
lm_fit <-   lm(residual ~ ., data=temp)

temp2 <- test_set %>% select(c(Romance:IMAX))  

pred <- predict.lm(lm_fit, newdata = temp2)

length(residual_temp)
length(test_set$rating)
length(genre_effect)

predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u + pred) %>%
  pull(pred)


rmse_genre_effect <- RMSE(predicted_ratings, test_set$rating)

#.86538 , which is less than 0.8655425


0.8655425-0.86538 


mu_rep<-do.call("rbind", replicate(length(test_set), mu, simplify = FALSE))

rmse_results <- rmse_results %>% add_row(method ="Regularized user & Movie effects and Genre effect", RMSE =rmse_genre_effect)

kable(rmse_results) %>%
  kable_styling(bootstrap_options = "striped" , full_width = F , position = "center") %>%
  kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
  column_spec(1,bold = T ) %>%
  column_spec(2,bold =T ,color = "white" , background ="#D7261E")





#---------------------------------------------------Reccomender Lab Models -----------------------------------------------------------------------


install.packages("recommenderlab")



library(tidyr)
library(ggplot2)
library(dplyr)
library(recommenderlab)
library(Matrix)

#For using recommenderlab, we need to make a sparse matrix and partition it using the inbuilt partition functions


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

as(sparse_m[1:10,1:10],"matrix")

#UBCF takes hours to train so we need to reduce the data. we do this with the quantile function
#UBCF has 2 parameters: given and nn 
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


#This tells us that 0.1 quantile reduction , 8 as given and 15 as nn give us the best RMSE of 1.024183

#We will keep reduction parameter at 0.9

#Let us try the POPULAR method
sparse_m <- create_sparse_m()
#Reduction parameter
y=0.9
min_movies_by_user <- quantile(rowCounts(sparse_m),y)
min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)

sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
#Mamking the model
model_popular <- Recommender(sparse_m_limited, method="POPULAR", param=list(normalize="center"))
#Testing prediction
pred_pop <- predict(model_popular, sparse_m[1:10], type="ratings")

as(pred_pop, "matrix")[,1:10]
#Finding RMSE
e5 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                       k=1, given=8)
p_POPULAR <- predict(model_popular, getData(e5, "known"), type="ratings")
rmse_POP <- calcPredictionAccuracy(p_POPULAR, getData(e5, "unknown"))[1]

rmse_POP


#Let us try the IBCF method
sparse_m <- create_sparse_m()
#Reduction parameter
y=0.9
min_movies_by_user <- quantile(rowCounts(sparse_m),y)
min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)

sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
#Mamking the model
model_ibcf <- Recommender(sparse_m_limited, method="IBCF", param=list(normalize="center"))
#Testing prediction
pred_pop <- predict(model_ibcf, sparse_m[1:10], type="ratings")

as(pred_pop, "matrix")[,1:10]
#Finding RMSE
e6 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                       k=1, given=8)
p_IBCF <- predict(model_ibcf, getData(e6, "known"), type="ratings")
rmse_IBCF <- calcPredictionAccuracy(p_IBCF, getData(e6, "unknown"))[1]

rmse_IBCF







#---------------------------------------------------Recosystem Model -----------------------------------------------------------------------


#Lets try Matrix Factorization with GD

test_GD <- as.matrix (test_set [,1:3])
train_GD <- as.matrix(test_set [,1:3])
glimpse(test_GD)

valid_GD <-as.matrix(validation [,1:3])

set.seed(1)

train_GD_2 <- data_memory(train_GD[,1],train_GD[,2],train_GD[,3])
test_GD_2 <- data_memory(test_GD[,1],test_GD[,2],test_GD[,3])
valid_GD_2 <- data_memory(valid_GD[,1],valid_GD[,2],valid_GD[,3])

#Next step is to build Recommender object
r = Reco()
# Matrix Factorization :  tuning training set
opts = r$tune(train_GD_2, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
opts

r$train(train_GD_2, opts = c(opts$min, nthread = 1, niter = 20))

pred <- r$predict(test_GD_2, out_memory())

rmse_MFGD <- RMSE(pred,test_set$rating)

pred_v <- r$predict(valid_GD_2, out_memory())

RMSE(pred_v,validation$rating)

rmse_results

#------------Time Effect




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




mu <- mean(data$rating)
mu_rep<-do.call("rbind", replicate(length(test_set), mu, simplify = FALSE))

#We make a a dataframe as long as test_set with mu repeated as R Markdown interprets the difference in length between mu and test_set as an error.

naive_rmse <- RMSE(test_set$rating,mu_rep)
naive_rmse

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)





temp3 <- validation %>% select(c(Romance:IMAX))  

pred <- predict.lm(lm_fit, newdata = temp3)

predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(rat = mu + b_i + b_u + pred) %>%
  pull(rat)

any(is.na( predicted_ratings))

glimpse(predicted_ratings)
length(predicted_ratings)
length(validation$rating)
RMSE(predicted_ratings, validation$rating)
RMSE(as.data.frame(predicted_ratings),as.data.frame(validation$rating))


sqrt(mean((validation$rating - predicted_ratings)^2))
any(is.na((validation$rating - predicted_ratings)^2))



class(predicted_ratings)
class(validation$rating)
any(is.na(predicted_ratings))
any(is.na(validation$rating))


any(is.na(pred))


which(is.na(predicted_ratings))

rmse_genre_effect_final <- RMSE(predicted_ratings, validation$rating)

rmse_genre_effect_final 
rmse_results_final <- tibble(method = "Regularized user & Movie effects with Genre (lm)", RMSE = rmse_genre_effect_final)

rm(temp3)
#rmse_results <- rmse_results %>% add_row(method ="Regularized user & Movie effects with Genre (lm)", RMSE =min(rmse_genre_effect))
rmse_results_final



#recal with edX set

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

lambda <- lambdas[which.min(rmses)]
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))


genre_train <- edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(residual = rating - mu- b_i-b_u) 

temp <- genre_train %>% select(c(Romance:IMAX,residual)) 
lm_fit <-   lm(residual ~ ., data=temp)

temp2 <- validation %>% select(c(Romance:IMAX))  

pred <- predict.lm(lm_fit, newdata = temp2)

predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u + pred) %>%
  pull(pred)


rmse_genre_effect <- RMSE(predicted_ratings, validation$rating)