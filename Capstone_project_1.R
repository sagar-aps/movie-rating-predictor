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

#---------------------------------------------------Reccomender Lab Models -----------------------------------------------------------------------


install.packages("recommenderlab")



library(tidyr)
library(ggplot2)
library(dplyr)
library(recommenderlab)
library(Matrix)

#For using recommenderlab, we need to make a sparse matrix and partition it using the inbuilt partition functions

#Edx dataset transformation:usersId and movieId should be treat as factors for some analysis purposes.
edx.copy <- edx
edx.copy$userId <- as.factor(edx.copy$userId)
edx.copy$movieId <- as.factor(edx.copy$movieId)
#SparseMatrix function is used in order to get an output 0f sparse matrix of class dgcMatrix.
# To use this function, the userId & movieId are converted to numeric vectors.
edx.copy$userId <- as.numeric(edx.copy$userId)
edx.copy$movieId <- as.numeric(edx.copy$movieId)

sparse_m <- sparseMatrix(i = edx.copy$userId,
                         j = edx.copy$movieId ,
                         x = edx.copy$rating,
                         dims = c(length(unique(edx.copy$userId)),
                                  length(unique(edx.copy$movieId))),
                         dimnames = list(paste("u", 1:length(unique(edx.copy$userId)), sep = ""),
                                         paste("m", 1:length(unique(edx.copy$movieId)), sep = "")))



identical(edx,edx.copy)


class(edx$movieId)
 c(length(unique(edx$userId)),
         length(unique(edx$movieId)))

 c(length(unique(edx.copy$userId)),
   length(unique(edx.copy$movieId)))
 
edx_backup <- edx

sparse_m <- sparseMatrix(
              i = as.numeric(as.factor(edx$userId)),
              j = as.numeric(as.factor(edx$movieId)), 
              x = edx$rating,
             dims = c(length(unique(edx$userId)),
                      length(unique(edx$movieId))),
             dimnames = list(paste("u", 1:length(unique(edx$userId)), sep = ""),
                             paste("m", 1:length(unique(edx$movieId)), sep = "")))

sparse_m <- new("realRatingMatrix", data = sparse_m)


#In order to use the evaluation scheme from reccomenderlab, we need to specify the minimum number of ratings that a user makes to qualify for being taken into account for the model

given <- min(rowCounts(sparse_m))


eval <- evaluationScheme(sparse_m, method="split", train=0.9, given=given, k=1)

any(rowCounts(sparse_m)<5)

which(rowCounts(sparse_m)<10)

#UBCF_model <- Recommender(sparse_m,method="UBCF",param=list(normalize = "center"))

UBCF_model <- Recommender(getData(eval, "train"), method = "UBCF",
                     param=list(normalize = "center", method="Cosine", nn=50))

#Next staement predicts for known data? It crashed out after 6+ hours
pred_UBCF <- predict(UBCF_model, getData(eval, "known"), type="ratings")

#This statement has run for 4 hours now and is still running.Stopped with an error.
pred_UBCF <- predict(UBCF_model, getData(eval, "unknown"), type="ratings")


#Trying prediction for one user with few ratings
pred_UBCF <- predict(UBCF_model, getData(eval, "unknown")[9], type="ratings")

#Algorithm predicted 1671 movies?? Didn't predict movies required? User number is with a decimal???
length(as(pred_UBCF, "list")[[1]])

#Next statement yeilds a variabl size of 9 MB
unknown <- getData(eval, "unknown")

unknown[1]

which(rowCounts(unknown)<10)

length(as(unknown, "list")[[1]])

as(unknown, "matrix")[[9]]
as(unknown, "list")[9]

class(pred_UBCF)
class(unknown)

#Was looking for movie id 163 but doesn't exist in prediction
as(pred_UBCF, "list")[[1]][91]

#data frame doesn't work
as(unknown, "data.frame")[[9]]

length(as(pred_UBCF, "list")[[1]])


as(pred_UBCF, "list")

head(as(pred_UBCF, "data.frame"))


#These statements havn't yet run
rmse_ubcf <- calcPredictionAccuracy(pred_UBCF, getData(eval, "unknown")[1:100])[1]
rmse_ubcf

class(getData(eval, "known"))

pred_UBCF_validation <-predict(UBCF_model,)

#Calculation of rmse for UBCF method







#ratings of 30% of users are excluded for testing
model_pop <- Recommender(getData(eval, "train"), "POPULAR")

prediction_pop <- predict(model_pop, getData(eval, "unknown"), type="ratings")

temp2<- getData(eval, "train")
temp<- getData(eval, "known")

as(prediction_pop, "list")[[9]]
prediction_pop

rmse_pop <- calcPredictionAccuracy(prediction_pop, getData(eval, "known"))[1]
rmse_pop


getRatingMatrix(prediction_pop[c(1:5),c(1:4)])

nratings(getData(eval, "known")[1])

hist(getRatings(prediction_pop), breaks="FD")

hist(getRatings(getData(eval, "known")), breaks="FD")


hist(getRatings(sparse_m), breaks="FD")


removeKnownRatings()


nratings(prediction_pop[1])

getRatingMatrix(getData(eval, "known")[c(1:5),c(1:4)])
#Something is wrong. UserId is blank for some rows. 


getRatingMatrix(sparse_m[c(1:5),c(1:4)])

eval <- evaluationScheme(sparse_m, 
                         method="split", 
                         train=0.9,
                         given=given,
                         k=1)

#this takes too long to execute. There must be something wrong that it is doing. It takes no time to create an index to separate data.



eval <- evaluationScheme(sparse_m, 
                         method="split", 
                         train=0.9,
                         given=-7)



getRatingMatrix(sparse_m[c(1:5),c(1:4)])

getRatingMatrix(getData(eval, "known")[c(1:5),c(1:4)])

getRatingMatrix(getData(eval, "unknown")[c(1:5),c(1:4)])


getRatingMatrix(getData(eval, "unknown")[,1])
#model <- Recommender(getData(eval, "known", 
 #                    method = "UBCF", 
  #                   param = list(method = "pearson", nn = 50)))


UBCF_model <- Recommender(getData(eval, "train"), 
                          method = "UBCF",
                          param=list(method="Cosine", nn=50))


prediction <- predict(UBCF_model, sparse_m[9, ], type = "ratings")

as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] 

sparse_m[9,]

getRatingMatrix(prediction)
getRatingMatrix(sparse_m[9])


rowCounts(sparse_m[9])

rowc

gc()



#What if there is no movie number x? How can I pasete names to create a realrating matrix

temp<- edx %>% group_by(movieId) %>%
  summarise(n=n())%>%
  arrange(movieId)

#Found out there is no movie number 91.

colCounts(sparse_m)

#And there is no column 91 either. Pasting names will work.


temp<- edx %>%
  select(userId,movieId,rating)%>%
  spread(movieId,rating)%>%
  as.matrix()

dimnames(temp)


rm(temp)

dim(sparse_m)

dimnames(sparse_m)

view(sparse_m[1:10])

rownames(sparse_m@data)

length(unique(edx$userId))
length(unique(edx$movieId))       
       

length(unique(validation$userId))
length(unique(validation$movieId))    
       
head(sparse_m)
view(train_set[1:50] )


as(sparse_m, "matrix") [1:10,1:10]

head(sparse_m)
head(rowCounts(sparse_m))
dimnames(sparse_m)

str(sparse_m)
predict(UBCF_model,sparse_m,type="ratings",data=sparse_m_test)

as(predict(UBCF_model,sparse_m[1:10],type="ratings"),"matrix")[,1:10]


























###Checking how it is done

MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
class(MovieLense100[101:102])

glimpse(edx)

## predict ratings for new users
rec <- Recommender(train, method = "POPULAR")
#didn't work

rec <- Recommender(MovieLense100[1:50],method="UBCF",param=list(normalize = "center"))
pre <- predict(rec, MovieLense100[101:102], type="ratings")
pre
as(pre, "matrix")[,1:10]


as(MovieLense100[,100:102],"matrix")

rm(list=ls(pattern="^MovieL"))

###














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
  

