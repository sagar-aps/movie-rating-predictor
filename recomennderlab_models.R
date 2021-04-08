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
full_test_UBCF <- sapply(test_perc_given_nn, function(t){
  
  tic(toString(t))
  y=t[1]
  sparse_m <- create_sparse_m()
  
  print(identical(sparse_m,sparse_m2))
  set.seed(1991)
  min_movies_by_user <- quantile(rowCounts(sparse_m),y)
  min_ratings_per_movie<- quantile(rowCounts(sparse_m),y)
  sparse_m_limited <- sparse_m[rowCounts(sparse_m)>=min_movies_by_user, colCounts(sparse_m)>=min_ratings_per_movie]
  e4 <- evaluationScheme(sparse_m_limited , method="split", train=0.9,
                         k=1, given=t[2])
  r_UBCF <- Recommender(getData(e4, "train"), "UBCF",parameter=c(nn=t[3]))
  p_UBCF <- predict(r_UBCF, getData(e4, "known"), type="ratings")
  rmse_ <- calcPredictionAccuracy(p_UBCF, getData(e4, "unknown"))[1]
  
  toc(log=TRUE,quiet = FALSE)
  print(paste(t[1], " is perc  ", t[2]," is given. ",  t[3]," is nn. ", rmse_, " is RMSE"  ))
  
  gc()
  print(identical(sparse_m,sparse_m2))
  return(list(perc = t[1] , given = t[2], nn=t[3] , rmse = rmse_))
  
})

log_full_test <- tic.log(format = TRUE)
full_test_UBCF
