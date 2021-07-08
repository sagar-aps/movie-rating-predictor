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

write.csv2(full_test_UBCF, file = "full_test_UBCF")

##Early trials : useless


















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

