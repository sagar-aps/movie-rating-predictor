library(recommenderlab)
library(dplyr)


### recommender for real-valued ratings
data(Jester5k)
## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(Jester5k[1:500,], method="split", train=0.9,
                      k=1, given=15)
e
## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")
## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type="ratings")
p

class(p)

glimpse(Jester5k)
glimpse(Jester5k@data)

Jester5k[1:10,1:10] %>% as("matrix")


nrow(Jester5k)
nrow(getData(e, "unknown"))
nrow(getData(e, "known"))

ncol(Jester5k)
ncol(getData(e, "unknown"))
ncol(getData(e, "known"))

nratings(Jester5k[2])
nratings(getData(e, "known")[2])
nratings(getData(e, "unknown")[2])
nratings(getData(e, "train")[2])

as(p,"data.frame")

p["u15547",1:5] %>% as("matrix")

getData(e, "known")[1:10,1:10]%>% as("matrix")
getData(e, "unknown")[1:10,1:10]%>% as("matrix")

rownames()

indx <- intersect(rownames(getData(e, "known")),rownames( getData(e, "known")))
indx <- intersect(indx, rownames(p))

indx

p["u7147",1:5] %>% as("matrix")
getData(e, "known")["u7147",1:10]%>% as("matrix")
getData(e, "unknown")["u7147",1:10]%>% as("matrix")

nratings(Jester5k["u7147"])
nratings(getData(e, "known")["u7147"])
nratings(getData(e, "unknown")["u7147"])


## compute error metrics averaged per user and then averaged over all
## recommendations
calcPredictionAccuracy(p, getData(e, "unknown"))
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser=TRUE))

length(getData(e, "unknown")
       

hist(getRatings(p), breaks="FD")

hist(getRatings(Jester5k), breaks="FD")   


p %>% as("data.frame") %>%
  filter(rating>10 | rating< -10)


JesterJokes
best <- which.max(colMeans(Jester5k))
cat(JesterJokes[best])


cutoff<-quantile(colMeans(Jester5k))[4]
bj_idx<-which(colMeans(Jester5k)>cutoff)
cat(JesterJokes[bj_idx],sep = "\n")
