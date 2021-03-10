library(dslabs)
data("heights")

heights
class(heights)
class(heights$sex)
class(heights$height)
nrow(heights)
heights$height[777]
heights$sex[777]
heights[1, 777]
heights[777,1]
max(heights$height)

which.min(heights$height)
mean(heights$height)
median(heights$height)

mean(heights$sex=="Male")
sum(heights$height>78)

sum(heights$height>78 & heights$sex=="Female")


install.packages("caret")
install.packages("purr")
library(caret)
library(dplyr)
library(purr)
#We will try to predict sex based on height

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
#Our functions developed for machine learning,such as those in the caret package, require or recommend that categorical outcomes be coded as factors.


y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)%>% 
  factor(levels = levels(test_set$sex))

mean(y_hat==test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

#Males are slightly taller than females.
#Lets predict male if height is within two sd of male average

y_hat <- ifelse(x>69.3-2*3.61,"Male","Female")%>% factor(levels=levels(test_set$sex))
mean(y_hat==test_set$sex)

cutoff <- seq(61, 70)
accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})


data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


data(mnist)

mnist_27

mnist<-read_mnist()

colnames(mnist)
class(mnist)


#Excersice 1

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


head(dat)
dat %>% filter(type=="inclass") -> temp

 mean(temp$sex == "Female")

 
 
 dat %>% filter(type=="online") -> temp
 
 mean(temp$sex == "Female")
mean(dat$type=="online" & dat$sex=="Female")

dat

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"), prop_male = mean(sex=="Male"))

head(y)

y_hat <- as.factor( ifelse(x=="inclass","Female","Male"))


mean(y_hat==y)

table(y, y_hat)

table(y_hat, y)

help(sensitivity)
??caret::sensitivity 

cm <- confusionMatrix(data = y_hat, reference = y)
cm

mean(dat$sex=="Female")


#Excercise  Practice with Machine Learning, Part 2

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


set.seed(2, sample.kind="Rounding") 


test_index <- createDataPartition(y,times=1,p=0.5, list = FALSE)

test_index
test <- iris[test_index,]
train <- iris[-test_index,]

#Sepal Length
cutoff <- seq(min(train$Sepal.Length),max(train$Sepal.Length),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})
max(accuracy)
# 0.84

#Sepal Width
cutoff <- seq(min(train$Sepal.Width),max(train$Sepal.Width),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})
max(accuracy)
# 0.68


#Petal Length
cutoff <- seq(min(train$Petal.Length),max(train$Petal.Length),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})
max(accuracy)
# 0.92

#Petal Width
cutoff <- seq(min(train$Petal.Width),max(train$Petal.Width),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})
max(accuracy)


y<-cutoff[which.max(accuracy)]




#Petal Length
cutoff <- seq(min(train$Petal.Length),max(train$Petal.Length),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})

x<-cutoff[which.max(accuracy)]

# 0.92 is accuracy in train set

y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") 
mean(y_hat == test$Species)




#Same test on test data

#Sepal Length
cutoff <- seq(min(test$Sepal.Length),max(test$Sepal.Length),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") 
  mean(y_hat == test$Species)
})
max(accuracy)
# 0.68

#Sepal Width
cutoff <- seq(min(test$Sepal.Width),max(test$Sepal.Width),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") 
  mean(y_hat == test$Species)
})
max(accuracy)
# 0.62


#Petal Length
cutoff <- seq(min(test$Petal.Length),max(test$Petal.Length),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") 
  mean(y_hat == test$Species)
})
max(accuracy)
# 0.96

#Petal Width
cutoff <- seq(min(test$Petal.Width),max(test$Petal.Width),by=0.1)

accuracy <- sapply(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") 
  mean(y_hat == test$Species)
})
max(accuracy)
# 0.96


plot(iris,pch=21,bg=iris$Species)


y_hat <- ifelse((test$Petal.Length > x | test$Petal.Width>y), "virginica", "versicolor") 
mean(y_hat == test$Species)

#Bayes Theorem, Again.


# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test)

mean(disease*ifelse(test == 0, 1, 0))

mean(disease[test==0])

mean(disease[test==1])/mean(disease)


#Comprehension Check: Conditional Probabilities Part 2

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)



ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)




Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


Sigma
dat

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps))) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)




# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
prediction_rsmes<- replicate(n, {
  test_idx <- createDataPartition(dat$y, times = 1, p = 0.5, list=F)
  test_s <- dat %>% slice(test_idx)
  train_s <- dat %>% slice(-test_idx)
  fit_y <- lm(y~x, data = train_s)
  
  predictions <- predict(fit_y, test_s)
  sqrt(mean((predictions - test_s$y)^2))
})

mean(prediction_rsmes)
sd(prediction_rsmes)





Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
n<- c(100, 500, 1000, 5000, 10000)
rsme_mean_sd <-  function(n) {
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
    prediction_rsmes <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list=F)
    test_set <- dat %>% slice(test_index)
    train_set <- dat %>% slice(-test_index)
    fit_y_model <- lm(y~x, data = train_set)
    predictions <- predict(fit_y_model, test_set)
    sqrt(mean((predictions - test_set$y)^2))
  })
 data.frame(n=n, mn=mean(prediction_rsmes),sd=sd(prediction_rsmes))
}

set.seed(1,sample.kind = "Rounding") # if using R 3.6 or later
sapply(n,rsme_mean_sd) 


##Q4

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")
prediction_rsmes<- replicate(n, {
  test_idx <- createDataPartition(dat$y, times = 1, p = 0.5, list=F)
  test_s <- dat %>% slice(test_idx)
  train_s <- dat %>% slice(-test_idx)
  fit_y <- lm(y~x, data = train_s)
  
  predictions <- predict(fit_y, test_s)
  sqrt(mean((predictions - test_s$y)^2))
})

mean(prediction_rsmes)
sd(prediction_rsmes)

#Q6


# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)



set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list=F)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)
fit_y_model_x1 <- lm(y~x_1, data = train_set)
fit_y_model_x2 <- lm(y~x_2, data = train_set)
fit_y_model_x12<- lm(y~x_1+x_2, data = train_set)
predictions_1 <- predict(fit_y_model_x1, test_set)
predictions_2 <- predict(fit_y_model_x2, test_set)
predictions_3 <- predict(fit_y_model_x12, test_set)

sqrt(mean((predictions_1 - test_set$y)^2))
sqrt(mean((predictions_2 - test_set$y)^2))
sqrt(mean((predictions_3 - test_set$y)^2))


#Q8

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list=F)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)
fit_y_model_x1 <- lm(y~x_1, data = train_set)
fit_y_model_x2 <- lm(y~x_2, data = train_set)
fit_y_model_x12<- lm(y~x_1+x_2, data = train_set)
predictions_1 <- predict(fit_y_model_x1, test_set)
predictions_2 <- predict(fit_y_model_x2, test_set)
predictions_3 <- predict(fit_y_model_x12, test_set)

sqrt(mean((predictions_1 - test_set$y)^2))
sqrt(mean((predictions_2 - test_set$y)^2))
sqrt(mean((predictions_3 - test_set$y)^2))



#Categorical variable prediction using linear regression

library(dslabs)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding") 



test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()


lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]


lm_fit
p_hat




#Logistic Regression Excercises


set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

#Next section fits a logistic model on the train set and gets accuracy when compared to tthe test set
glm_fit <- dat$train %>% 
  glm(y ~ x, data=., family = "binomial")
p_hat_logist <- predict(glm_fit, newdata = dat$test, type = "response")
y_hat_logist <- ifelse(p_hat_logist > 0.5, 1, 0) %>% factor
confusionMatrix(y_hat_logist, dat$test$y)$overall[["Accuracy"]]

dat$test

dat$train %>% ggplot(aes(x, color = y)) + geom_density()


set.seed(1, sample.kind="Rounding")



mu_1 <- seq(0, 3, len=25)



accuracy_fn <- function(mu_1)
{
  dat<-make_data(mu_1=mu_1)
  glm_fit <- dat$train %>% 
    glm(y ~ x, data=., family = "binomial")
  p_hat_logist <- predict(glm_fit, newdata = dat$test, type = "response")
  y_hat_logist <- ifelse(p_hat_logist > 0.5, 1, 0) %>% factor
  confusionMatrix(y_hat_logist, dat$test$y)$overall[["Accuracy"]]
}

set.seed(1, sample.kind="Rounding")  
mu_1 <- seq(0, 3, len=25)
accuracy <- sapply(mu_1,accuracy_fn,simplify = "array")

plot(accuracy,mu_1)


#Smoothing Excercises


library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

plot(dat$date,dat$deaths)


# My Answer
total_days <- as.numeric(difftime(max(dat$date),min(dat$date),units = "days"))
total_days
class(total_days)

span <- 60/total_days

fit <- loess(deaths ~ day, degree =1, span=span, data=dat)

dat %>% filter(deaths != "NA") %>%
  mutate(smooth = fit$fitted) %>%
  ggplot()+
  geom_point(aes(day,deaths)) +
  geom_line(aes(day,smooth), color="red", lwd = 2)


#Given Solution

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")





# Book answer for next questions.


span <- 60/as.numeric(difftime(max(dat$date),min(dat$date),units = "days"))

fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)

dat  %>%
  mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot()+
  geom_point(aes(date,deaths)) +
  geom_line(aes(date,smooth), color="red", lwd = 2)


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


#Q3 Smoothing

library(dslabs)
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

mnist_27$test%>% mutate(sevnotsev = ifelse(y==7,1,0))%>% ggplot(aes(x_2,sevnotsev))+
geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))


#Section Matrices

#Dataframes aren't enough. Large datasets need use of Matrices


library(tidyverse)
library(dslabs)


if(!exists("mnist")) mnist <- read_mnist()
class(mnist$train$images)


x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

class(mnist$train$images)
dim(x)

my_vector <- 5:17

mat<- matrix(my_vector,5,3)

mat

my_vector <- 1:15
mat<- matrix(my_vector,5,3)
matT<- matrix(my_vector,5,3,byrow = T)
matT
mat
identical(matT,t(mat))
t(mat)
mat

class(y)
y[3] #Evaluates to 4 telling us 3rd digit is 4

grid <- matrix(x[3,], 28, 28)
#Put the third row of x which has 784 columns into a 28x28 matrix

image(1:28,1:28, grid[,28:1]) #We vreverse columns order to get correct image

sums <- rowSums(x)
avg<- rowMeans(x)


data.frame(labels=as.factor(y), row_avg = avg) %>%
  qplot(labels,row_avg,data=.,geom="boxplot")

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)
#apply can do same thing but with any function, at the cost of speed.

#1 and 2 refer to rows and columns respectively.

install.packages("matrixStats")
library(matrixStats)

sds <- colSds(x)

qplot(sds, bins = "30", color = I("black"))

sds %>% as.data.frame() %>% ggplot(aes(sds)) + geom_histogram()

qplot(apply(x, 2, mean))

# What does the X-Axis limit of 120 signify?


image(1:28,1:28,matrix(sds,28,28)[,28:1])




new_x <- x[,colSds(x)>60, drop=F]
#Put in new_x a new matrix made from x where the SD of Cols > 60

#image(1:28,1:28,matrix(sds,28,28)[,28:1])


dim(new_x)


class(new_x)

#Now the matrix new_x has only 314 columns. We discarded 470 columns i.e. predictors


#Time to BINARIZE the data. instead of using darkness value between 0 and 255, we will convert to 0 or 1


qplot(as.vector(x))

#We see the dichotomy : with and without ink.

bin_x <- x

bin_x[bin_x<255/2] <- 0
bin_x[bin_x>255/2] <- 1


qplot(as.vector(bin_x))



#Excercises Matrices

x <- matrix(rnorm(100*10), 100, 10)
x

dim(x)
nrow(x)
ncol(x)

seq(nrow(x))


X <- mnist$train$images

Y<- mnist$train$labels

ind <- which(X>50 & X<205) 


new_X <- X[X>50 & X<205]

length(new_X)/length(X)


#Distance

if(!exists("mnist")) mnist <- read_mnist()
#set.seed(0) # if using R 3.5 or earlier
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)


#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]


x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]




sqrt(crossprod(x_1-x_2))

d<-dist(x)

class(d)
as.matrix(d)[1:5,1:5]

image(as.matrix(d))

image(as.matrix(d)[order(y),order(y)])

view(d)

library(dslabs)
data(tissue_gene_expression)


dim(tissue_gene_expression$x)

table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)


as.matrix(d)[c(1,2,39,40,73,74),c(1,2,39,40,73,74)]


# trial to get labels in the img function: failed
image(
  as.matrix(d)[c(1,2,39,40,73,74),c(1,2,39,40,73,74)],
  xlab=tissue_gene_expression$y[c(1,2,39,40,73,74)]
)

#Q1 KNN

library(dplyr)
library(dslabs)
library(caret)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
se <-  seq(1,101,3)
meas <- sapply(se, function(k){
  knn_fit <- knn3(sex ~ height, data = train_set, k=k)
  y_hat_knn <- predict(knn_fit, newdata = test_set, type = "class")
  F_meas(data=y_hat_knn, reference = test_set$sex)
})
meas
data.frame(k=se, meas=meas)
max(meas)
which.max(meas)





#y <- heights$sex
#x <- heights$height # this doesn't work. knn requires names as below
knn_fit <- knn3(sex ~ height, data = train_set, k=5) # x and y don't work
y_hat_knn <- predict(knn_fit, newdata = test_set, type = "class") # this isn't needed : %>% factor(levels = levels(train_set$sex))

F_meas(data=y_hat_knn, reference = train_set$sex) # Not needed: factor(train_set$sex))
#test_set <- heights[test_index, ]
#train_set <- heights[-test_index, ] works same as slice. tested.


test_set
length(test_set)
identical(test_set,test_set2)



#Q2 KNN

data("tissue_gene_expression")
table(tissue_gene_expression$y)
class(tissue_gene_expression$y)
levels(tissue_gene_expression$y)

# x has numbers, y is a factor with 7 levels. tissue_gene_expression is a list

#tissue_gene_expression %>% data.frame()


#y <- tissue_gene_expression$y
#x<- tissue_gene_expression$x

set.seed(1, sample.kind = "Rounding") 
test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p=0.5, list =FALSE)


#y <- (tissue_gene_expression$y)
#x <- (tissue_gene_expression$x)

#test_set_x <- tissue_gene_expression$x %>% slice(test_index)
#test_set_y <- tissue_gene_expression$y [test_index]
#train_set_x <- x[-test_index]
#train_set_x <- x %>% slice[-test_index]
#train_set_y <- tissue_gene_expression$y [-test_index]

train_set_y <- tissue_gene_expression$y[-test_index]
train_set_x <-  tissue_gene_expression$x[-test_index, ]

test_set_y <- tissue_gene_expression$y[test_index]
test_set_x <-  tissue_gene_expression$x[test_index, ]

se <-  seq(1,11,2)

accuracy <- sapply(se, function(k){
  knn_fit <- knn3(train_set_x ,train_set_y , k=k)
  y_hat_knn <- predict(knn_fit,test_set_x,type="class")
  confusionMatrix(data = y_hat_knn, reference = test_set_y)$overall[1]
})

data.frame(k=se,acc=accuracy)





# KNN Cross Validation

#Testing and validation of an ML algorithm agaisnt subsets of the TRAIN set

library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
# y is a vector of 1000 outcomes

x_subset <- x[ ,sample(p, 100)]
#x_subset is a matrix of 1000 rows and 100 cols. Cols are selected randomly. 
#why are predictors selected randomly?!

fit <- train(x_subset, y, method = "glm")
fit$results

install.packages("BiocManager")
BiocManager::install("genefilter")

library(BiocManager)
library(genefilter)
tt <- colttests(x, y)

#We test here if the effect of columns in x has on y is statistically significant

length(y)
dim(x_subset)

pvals <- tt$p.value

ind <- which(pvals<=0.01)
length(ind)

x_subset2 <- x[ ,ind]
fit <- train(x_subset2, y, method = "glm")
fit$results

fit <- train(x_subset2, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)



#Q7


train_set_y <- tissue_gene_expression$y
train_set_x <-  tissue_gene_expression$x
fit <- train(train_set_x, train_set_y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)


#Bootstrap Excercises.

#Idea is that to estimate median of a large dataset of which we have only a part,
#we can sample with replacement from the small dataset to make another which is as large as the previous
# the CIs we will get for the median will be sharper than if we directly try to apply the CLT(CLT doesn't apply for median. Only applies for mean)

library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

class(indexes)

ind <- which(indexes$Resample01 %in% c(3,4,7)) #%>% nrow()

which(indexes$Resample01 %in% c(3))
sum(indexes$Resample01 %in% c(3))


x=sapply(indexes, function(ind){ sum(ind==3) })

#Count the number of 3s in all the resamples within indexes
sapply(indexes, function(ind){ sum(ind==3) }) %>% sum()


#Q3 estimating EV and Standard Error of dataset y <- rnorm(100, 0, 1)

set.seed(1, sample.kind="Rounding")

length(y)
qnorm(0.75)
quantile(y, 0.75)

sd(y)

B <- 10^5

M <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y,0.75)
})

sd(M)
mean(M)


#Q4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y,10)
quantiles <- sapply(indexes, function(x){quantile(y[x],0.75)})
mean(quantiles)
sd(quantiles)


indexes$Resample01


#Q5
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y,10000)
quantiles <- sapply(indexes, function(x){quantile(y[x],0.75)})
mean(quantiles)
sd(quantiles)


#Generative Models

#When data is not available, we can use a naive approach
#This is a very difficult section to understand.
# LDA is linear discriminant analysis.
#We assume all predictors share the same normal distribution




library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#train_lda <- train(y~., method = "lda", data = cbind(x,y) )

train_lda <- train(x,y, method = "lda" )

train_lda[["results"]][["Accuracy"]]

train_lda$finalModel$means %>% t()%>% as.data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name))+
  geom_point()+
  geom_text() +
  geom_abline()


#Q4

train_qda <- train(x,y, method = "qda" )

train_qda[["results"]][["Accuracy"]]

train_qda$finalModel$means %>% t()%>% as.data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name))+
  geom_point()+
  geom_text() +
  geom_abline()


#Q5


train_lda <- train(x,y, method = "lda" , preProcess = "center")

train_lda[["results"]][["Accuracy"]]

train_lda$finalModel$means %>% t()%>% as.data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name))+
  geom_point()+
  geom_abline()+
  ggrepel::geom_label_repel()


#Q6

library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]


train_lda <- train(x,y, method = "lda" , preProcess = "center")

train_lda[["results"]][["Accuracy"]]



#Descision Trees and Random Forests



data("olive")
head(olive)


table(olive$region,olive$area)




library(dplyr)
library(tidyverse)

# Plot distribution of each predictor stratified by region
olive <- select(olive, -area)
head(olive)
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

#Nice use of ggplot!



# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)



#Q1 Descision Trees and Random Forests


library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat) 

plot(fit, margin = 0.1)
rpart.plot(fit)


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)


library(randomForest)


fit <- randomForest(y~x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
  
  plot(fit)
  
  
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = "red")  
  
    
    
#Questions Caret Package
library(rpart)
modelLookup("rpart")        
x<- tissue_gene_expression$x %>% as.data.frame()
y <- tissue_gene_expression$y %>% as.data.frame() 
y_1 <- tissue_gene_expression$y
colnames(y)<- make.names("y")
colnames(x) <- make.names(colnames(x))
dat <- data.frame(x,y)

#First way of using train
fit <- train(y~.,method="rpart",tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)),data = dat)
ggplot(fit)

#Second way of using train
fit <- train(x,y_1,method="rpart",tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)))  
ggplot(fit)


#third way of train using with
fit<- with (tissue_gene_expression,
      train(x,y,method = "rpart",
            tuneGrid=data.frame(cp=seq(0, 0.1, 0.01))))

ggplot(fit)



#Q2

with(tissue_gene_expression,
     table(y))

set.seed(1991, sample.kind="Rounding")
#Plancenta is 6. Minimum partition size defaults to 20
fit2 <- with (tissue_gene_expression,
      train(x,y,method = "rpart",
            tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)),
            control = rpart.control(minsplit = 0)
            ))
max(fit2[["results"]][["Accuracy"]])

ggplot(fit2)

plot(fit2)
plot(fit2, margin = 0.1)

install.packages("rpart.plot",dependencies = T)
rpart.plot(fit2)
library("rpart.plot")

rpart.plot(fit2$finalModel,cex = 0.7)
plot(fit2$finalModel, margin = 0.1)
text(fit2$finalModel, cex = 0.75)


#Q3 Random Forest

library(repart)
library(dplyr)
library(caret)


set.seed(1991, sample.kind="Rounding")


caret::modelLookup("rf")

fit3 <- with (tissue_gene_expression,
              train(x,y,method = "rf",
                    tuneGrid=data.frame(mtry=seq(50, 500,25)),
                    nodesize = 1
                    #,ntree=5 This line limits trees and speeds up
              ))


plot(fit3)







#caret::modelLookup("rf")

set.seed(1991, sample.kind="Rounding")
data("tissue_gene_expression")
library(caret)
library(dslabs)
library(dplyr)

#My code
set.seed(1991, sample.kind="Rounding")
fit3 <- with (tissue_gene_expression,
              train(x,y,method = "rf",
                    tuneGrid=data.frame(mtry=seq(50, 200,25)),
                    nodesize = 1
                    #,ntree=5 This line limits trees and speeds up
              ))

plot(fit3)
#Gave answer 50 without seed but ok after seeding


#Other guys code
data(tissue_gene_expression)
tiss_dat <- as.data.frame(tissue_gene_expression)
#set.seed(1991)
fit <- train(y~., method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)), nodesize = 1, data = tiss_dat)
fit$bestTune
plot(fit)
#gives answer 100


imp<-varImp(fit)
tree_terms <- as.character(unique(fit2$finalModel$frame$var[!(fit2$finalModel$frame$var == "<leaf>")]))
tree_terms 
imp$importance %>% as.data.frame()%>% order(decreasing = T)

imp


#Titanic Dataset questions



library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
library(dplyr)


head(titanic_train)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


dim(titanic_clean)

set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times=1,p=0.2,F)
titanic_test_set <- titanic_clean[test_index,]
titanic_train_set <- titanic_clean[-test_index,]

nrow(titanic_test_set)
nrow(titanic_train_set)

titanic_train_set %>% select(Survived)%>% table()

titanic_train_set %>% group_by(Survived) %>%summarise(n())

train_set <- titanic_train_set 
test_set <- titanic_test_set

mean(train_set$Survived==1)


#Q2
set.seed(3, sample.kind = "Rounding")

survived_guess_hat <- sample(c(0,1),size=nrow(test_set),replace = T)
mean(survived_guess_hat)

confusionMatrix(as.factor(survived_guess_hat),reference = as.factor(test_set$Survived))


#Q3a
mean(train_set$Survived==1 & train_set$Sex=="female")

with(train_set,
     sum(Survived==1 & Sex=="female")/sum(Sex=="female"))

with(train_set,
     sum(Survived==1 & Sex=="male")/sum(Sex=="male"))

train_set %>% 
  group_by(Sex) %>%
  summarise(Survived = mean(Survived ==1))


#Q3b : rule based prediction

survived_rule_hat <- ifelse(test_set$Sex=="female",1,0)
confusionMatrix(as.factor(survived_rule_hat),reference = as.factor(test_set$Survived))

F_meas(as.factor(survived_rule_hat),as.factor(test_set$Survived))


#Q4a

#WHich clasess were more likely than not to survive

train_set %>% 
  group_by(Pclass) %>%
  summarise(Survived = mean(Survived ==1)) %>%
  filter(Survived>0.5)

#Q4b

class_rule_hat <- ifelse(test_set$Pclass==1,1,0)
confusionMatrix(as.factor(class_rule_hat),reference = as.factor(test_set$Survived))

F_meas(as.factor(class_rule_hat),as.factor(test_set$Survived))


#Q4c

#WHich class sex combinations were more likely than not to survive

train_set %>% 
  group_by(Sex,Pclass) %>%
  summarise(Survived = mean(Survived ==1)) #%>%
 #filter(Survived>0.5)

#Q4d

class_sex_rule_hat <- ifelse(((test_set$Pclass==1 |test_set$Pclass==2) & test_set$Sex =="female"),1,0)
confusionMatrix(as.factor(class_sex_rule_hat),reference = as.factor(test_set$Survived))

F_meas(as.factor(class_sex_rule_hat),as.factor(test_set$Survived))

# better way:
class_sex_rule_hat <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(class_sex_rule_hat == test_set$Survived)

#Q5-6
#0.991 sensitivity sex class 0.551 specificity 0.771 bal acc 0.872 fmeas
#0.855 sensitivity class 0.464 specificity 0.659 bal acc 0.78 fmeas
#0.873 sensitivity sex 0.739 specificity 0.806 balac 0.857

#Q7 : LDA and QDA

set.seed(1, sample.kind = "Rounding")

fit_lda <- train(Survived~Fare, method="lda", data = train_set)

lda_y_hat<- predict(fit_lda,test_set)
mean(lda_y_hat==test_set$Survived)


set.seed(1, sample.kind = "Rounding")
fit_qda <- train(Survived~Fare, method="qda", data = train_set)

qda_y_hat<- predict(fit_qda,test_set)
mean(qda_y_hat==test_set$Survived)


#Q8 : Logistic Regression

#Only Age
set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived~Age, method="glm", data = train_set)

glm_y_hat<- predict(fit_glm,test_set)
mean(glm_y_hat==test_set$Survived)

# Age, Sex, Class, Fare
set.seed(1, sample.kind = "Rounding")
fit_glm2 <- train(Survived~Age+Sex+Pclass+Fare, method="glm", data = train_set)

glm_y_hat2<- predict(fit_glm2,test_set)
mean(glm_y_hat2==test_set$Survived)


# All predictors
set.seed(1, sample.kind = "Rounding")
fit_glm3 <- train(Survived ~ . , method="glm", data = train_set)

glm_y_hat3<- predict(fit_glm3,test_set)
mean(glm_y_hat3==test_set$Survived)


#Q9a : Knn
getModelInfo("knn")


set.seed(6, sample.kind = "Rounding")
fit_knn <- train(Survived ~ . , method="knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)))

knn_y_hat<- predict(fit_knn,test_set)

ggplot(fit_knn)

mean(knn_y_hat==test_set$Survived)
# could also do 

confusionMatrix(knn_y_hat, test_set$Survived)$overall["Accuracy"]

#Q10 : Knn with cross validation :10 fold

set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10)#, p = .9)

#control <- trainControl(method = "cv", number = 10, p = .9)

fit_knn2 <- train(Survived ~ . ,
                 method="knn",
                 data = train_set,
                 tuneGrid = data.frame(k = seq(3, 51, 2)),
                 trControl = control)

ggplot(fit_knn2, highlight = T)


fit_knn2$bestTune
max(fit_knn2$results$Accuracy)

knn_y_hat2<- predict(fit_knn2,test_set)
mean(knn_y_hat2==test_set$Survived)


#Q11 : Classification Tree model 

set.seed(10, sample.kind = "Rounding")

fit_rpart <- train(Survived ~ . ,
                  method="rpart",
                  data = train_set,
                  tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
)

ggplot(fit_rpart, highlight = T)
rpart_y_hat<- predict(fit_rpart,test_set)

fit_rpart$bestTune
confusionMatrix(rpart_y_hat, test_set$Survived)$overall["Accuracy"]

library("rpart.plot")

rpart.plot(fit_rpart$finalModel,cex = 0.7)


plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel, cex = 0.75)

install.packages("rattle")
library(rattle)
fancyRpartPlot(fit_rpart$finalModel)



#Q12 Random Forest

getModelInfo("rf")
modelLookup("rf")

set.seed(14, sample.kind = "Rounding")

fit_rf <- train(Survived ~ . ,
                   method="rf",
                   data = train_set,
                   tuneGrid = data.frame(mtry = seq(1:7)),
                ntree=100
)

ggplot(fit_rf)
rf_y_hat<- predict(fit_rf,test_set)
fit_rf$bestTune
confusionMatrix(rf_y_hat, test_set$Survived)$overall["Accuracy"]


varImp(fit_rf)



#Case Study MNIST

library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123, sample.kind = "Rounding")
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)

x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])


#Preprocessing:
#removing predictors that are not useful, are highly correlated with others, have very few non-unique values, or have close to zero variation.
#standardizing or transforming predictors 

library(matrixStats)
sds <- colSds(x)
qplot(sds,bins = 256, color = I("black"))

#here the bins are 256 because there are 0-255 levels of variation (integer values) for each pixel in the dataset



nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
#col_ind <- x[,-nzv] doesn't work
length(col_index)


#caret requires us to have col names for all our predictors
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)


n <- 1000 #model testing, use fewer rows in initial phases
b <- 2 # fold cross validation
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[ ,col_index], y,  k = 3)


y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]

install.packages("Rborist")
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

modelLookup("Rborist")

library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp

image(matrix(imp, 28, 28))

#Above image shpws theparts of the image used to actually predict the number

## below code not tested
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

## end of non tested code



# Ensembles

#install.packages(models, dependencies=T)
#devtools::install_github("majkamichal/naivebayes")
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
#packages ‘glm’, ‘naive_bayes’, ‘svmLinear’, ‘knn’, ‘gamLoess’, ‘multinom’, ‘qda’, ‘rf’, ‘adaboost’ are not available (for R version 3.6.1)
install.packages("naivebayes")
install.packages("svmLinear",repos="https://cloud.r-project.org/")

library(dplyr)
library(caret)
library(dslabs)
library(tidyverse)
library(svmLinear)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

#options(download.file.method="libcurl")
#train(y ~ ., method = "svmLinear", data = mnist_27$train)


fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


predictions <- sapply(fits, function(fit){
  predict(fit, mnist_27$test)
})

as.factor(mnist_27$test$y)

#predictions[,1]

#predictions is an array, not a list object. Can't use sapply
#accuracies <- sapply(predictions, function(p){
#  confusionMatrix(as.factor(p), as.factor(mnist_27$test$y))$overall["Accuracy"]
#})


#for(i in 1:10){
#  confusionMatrix(as.factor(predictions[,i]), as.factor(mnist_27$test$y))$overall["Accuracy"]
#} -> accuracies 

mean(accuracies)

#above approach is flawed for unknown reasons although it yeilded the correct answer


accuracies <- apply(predictions, 2, function(x){
  confusionMatrix(as.factor(x), as.factor(mnist_27$test$y))$overall["Accuracy"]
})

length(as.factor(mnist_27$test$y))
dim(predictions)

rsums <- rowSums(predictions)

#ens <- for(i in 1:nrow(predictions)){
#  if_else(mean(predictions[i,]==7)>0.5 ,7,2) 
#}

#Above code should work but doesn't

ens <- apply(predictions,1,function(x){
  ifelse(mean(x==7)>0.5,7,2)
} )

ens
cm<- confusionMatrix(as.factor(ens), as.factor(mnist_27$test$y))
cm$overall["Accuracy"]
#cm$byClass[,1:2]

#this doesn't work


accuracies

data.frame(models,accuracies)

MinAccs <- sapply(fits, function(fit){
  min(fit$results$Accuracy)
})

mean(MinAccs)

data.frame(models,MinAccs)

idx <- which(MinAccs>=0.8)

predictions2<- predictions[,idx]

ens2 <- apply(predictions2,1,function(x){
  ifelse(mean(x==7)>0.5,7,2)
} )

ens2
cm<- confusionMatrix(as.factor(ens2), as.factor(mnist_27$test$y))
cm$overall[["Accuracy"]]


#Netflix challenge and Matrix Factorization

library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
library(dplyr)

#Q1
movielens %>% group_by(movieId,year)%>%summarise(Ratings=sqrt(n())) %>% 
  ggplot(aes(as.factor(year),Ratings))+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))
  
#Q2
movielens %>% filter(year<=2018 & year>=1993) %>%
  group_by(movieId) %>%
  summarise(title=title[1],
            n=n(),
            release_yr=first(year),#first(year(as_datetime(timestamp))),
            out_yrs = 2018 - release_yr,
            mean_rating=mean(rating),
            rat_p_yr = n/out_yrs
            ) %>%
  top_n(25,n) -> top93_18

#above code deosn't work. Why?


movielens %>% filter(year<=2018 & year>=1993) %>%
  mutate(yrs_before_2018 = 2018-year) %>%
  group_by(movieId) %>%
  summarise(title=title[1],
            n=n(),
            mean_rating=mean(rating),
            rat_p_yr= n / yrs_before_2018[1])%>%
  top_n(25,n) -> top93_18_2


#Q3
movielens %>% filter(year<=2018 & year>=1993) %>%
  mutate(yrs_before_2018 = 2018-year) %>%
  group_by(movieId) %>%
  summarise(title=title[1],
            n=n(),
            mean_rating=mean(rating),
            rat_p_yr= n / yrs_before_2018[1])%>%
            ggplot(aes(mean_rating,rat_p_yr))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm")


#Q5
movielens <- mutate(movielens, date = as_datetime(timestamp))

#Q6

movielens %>% mutate(week=round_date(date, unit="week"))%>% 
  group_by(week) %>%
  summarise(avg=mean(rating)) %>%
  ggplot(aes(week,avg))+
  geom_point(alpha=0.1)+
  geom_smooth()

#Q8

movielens %>%
  group_by(genres) %>%
  summarise(
            avg=mean(rating),
            n= n(),
            sd=sd(rating),
            se = sd/sqrt(n),
            lb= avg- 1.96*se,
            ub= avg + 1.96*se
            ) %>%
  filter(n>1000) %>%
  ggplot(aes(x=genres,y=avg,ymin=lb,ymax=ub))+
  geom_point()+
  geom_errorbar()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))
  
#Regularization

library(dplyr)

library(caret)
set.seed(755, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]


test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse


rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

# Estimating b_i, the bias for each movie


mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))


predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
RMSE(predicted_ratings, test_set$rating)

#RMSE decreased a bit. Lets try to isolate the user effect also

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")



user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)

#RMSE improved but only slightly. Lets see our mistakes

test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  pull(title)

#All are obscure movies. Lets create a db movieID vs title and check the vest movies

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()


movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

#This shows our best movies are also obscure. We are relying too much on obscure patrons who give great ratings to very few movies.

#we need to pay less attention to movies with low number of ratings
#and users who don't rate much.

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)

train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)
train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)


#In order to regularize, we change our bias term and introduce a coeff lambda

lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 


tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)


train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)

#These make more sense

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  pull(title)


predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)


#We can choose labmda by cross validation (tuning?)

#We can use lambda for both user effect and movie effect


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


qplot(lambdas, rmses)



#Excercises Regularization

set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))


set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))


set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1
schools %>% arrange(desc(score)) %>% slice(1:10)

#Q2
median(schools$size)

schools %>% arrange(desc(score)) %>% slice(1:10) %>% 
  pull(size) %>%
  median()

#Q3

schools %>% arrange(score) %>% slice(1:10) %>% 
  pull(size) %>%
  median()


topQ<-schools %>% arrange(desc(quality)) %>% slice(1:10) 


schools %>% ggplot(aes(score,size)) +
  geom_point(alpha=0.3)+
  geom_point(aes(score,size),
             data = topQ,color="red",
             size=3)

overall <- mean(sapply(scores, mean))

alpha=25

reg_avg = overall + sapply(1:nrow(schools), function(i){
  (sum(scores[[i]]-overall))/(length(scores[[i]])+alpha) 
})


schools<- schools%>% mutate(regularized=overall+reg_avg)

schools %>% ggplot(aes(regularized,size)) +
  geom_point(alpha=0.3)+
  geom_vline(xintercept = overall)+
  gghighlight::gghighlight(rank(quality)<10)


rsme <- function(quality,estimate){
  #sqrt(mean(quality-estimate)^2)
  sqrt(mean((quality - estimate)^2))
}

aplphatry <- seq(10,250)


res<- sapply(aplphatry, function(alpha){
  reg_avg = overall + sapply(1:nrow(schools), function(i){
                            (sum(scores[[i]]-overall))/(length(scores[[i]])+alpha)
  })
  
  rsme(quality = schools$quality, estimate = reg_avg) 
  })

df<-data.frame(aplphatry,res)  
df$aplphatry[which.min(res)]

plot(aplphatry, res)

##Q7

alpha =135

reg_avg_best_alpha = overall + sapply(1:nrow(schools), function(i){
  (sum(scores[[i]]-overall))/(length(scores[[i]])+alpha) 
})

schools%>% mutate(regularized=reg_avg_best_alpha) %>% 
  arrange(desc(regularized)) %>%
  slice(1:10)


###Q8


res<- sapply(aplphatry, function(alpha){
  reg_avg = 0 + sapply(1:nrow(schools), function(i){
    (sum(scores[[i]]-0))/(length(scores[[i]])+alpha)
  })
  
  rsme(quality = schools$quality, estimate = reg_avg) 
})

df<-data.frame(aplphatry,res)  
df$aplphatry[which.min(res)]

## SVD


set.seed(1987, sample.kind="Rounding")
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))




my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)


my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)




s <- svd(y)
names(s)


y_svd <- s$u %*% diag(s$d) %*% t(s$v)


max(abs(y - y_svd))

#ss_y <- colSums(y)
#ss_yv <- colSums((y %*% s$v)^2)

#ss_y <- colSums((y%*%t(y))^2)

#ss_y <- colSums((y%*%t(y))^2)



#ss_y<-colSums(y%*%t(y))



#ss_yv<- colSums((y %*% s$v)^2)
ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)


library(dplyr)

sum(ss_y)
sum(ss_yv)

ss_y %>% plot()
ss_yv %>% plot()





plot(sqrt(ss_yv),s$d)


yv <- y %*% s$v
pca <- prcomp(yv)
pca_sum <- summary(pca)
sum(pca_sum$importance[2,1:3])


identical(t(s$u %*% diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% t(diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% diag(s$d), sweep(s$u, 2, s, FUN = "*"))


plot(s$u[,1]*s$d[1], rowMeans(y))


s$u[,1]*s$d[1]


my_image(s$v)


#Dimension reduction

set.seed(1988, sample.kind = "Rounding")
library(MASS)
n <- 100
Sigma <- matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)
x <- rbind(mvrnorm(n / 2, c(69, 69), Sigma),
           mvrnorm(n / 2, c(55, 55), Sigma))



plot(x[,1],x[,2])

library(ggplot2)
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)


pca<-prcomp(tissue_gene_expression$x)


summary(pca)

data.frame(pca$x[,1:2], Tissue = tissue_gene_expression$y) %>%
  ggplot(aes(PC1,PC2, fill = Tissue)) +
  geom_point(cex=3,pch=21) +
  coord_fixed(ratio=1)


data.frame(PC1=pca$x[,1],rm=rowMeans(tissue_gene_expression$x),Tissue = tissue_gene_expression$y) %>%
  ggplot(aes(PC1,rm, color = Tissue)) +
  geom_point()


dim(pca$x[,1])
pca$x[,1]


rm=rowMeans(tissue_gene_expression$x)
PC1=pca$x[,1]
cor(rm,PC1)


library(ggplot2)

rm(list=ls())

library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

avgs <- rowMeans(tissue_gene_expression$x)

pc<-prcomp(tissue_gene_expression$x)

data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

x_orig <- tissue_gene_expression$x

avgs[1]

x1 <- with(tissue_gene_expression, sweep(x, 1, mean(x)))
#mean(x) isn't rowmeans(x)
x2 <- sweep(x, 1, rowMeans(tissue_gene_expression$x))
#sweep(x) won't work becasue x is in tissue_gene...
x3 <- tissue_gene_expression$x - mean(tissue_gene_expression$x)
#Same as 1
x4 <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))

x_orig[1,1]-avgs[1]#-

  x_orig[1,1]-avgs[1]-x1[1,1]
  x_orig[1,1]-avgs[1]-x2[1,1]
  x_orig[1,1]-avgs[1]-x3[1,1]
  x_orig[1,1]-avgs[1]-x4[1,1]
  

x<- x4

pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


data.frame(pc=pc$x[,7],tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc,color=tissue)) +
  geom_boxplot()
  
data.frame(pc_7 = pc$x[,7], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(y=pc_7, x = tissue)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))


for(i in 1:7){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

pc$x[,1] %>% sd() 
pc$x[,2] %>% sd() 
pc$x[,3] %>% sd() 
pc$x[,4] %>% sd() 

var_explained <- cumsum(pc$sdev^2/sum(pc$sdev^2))
plot(var_explained)
ifelse(var_explained > 0.5, 1, 0)


#Clustering

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))


h <- hclust(d)

plot(h, cex = 0.65, main = "", xlab = "")


library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]


heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)

rm(list=ls())


#Breast Cancer Project

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
data(brca)


dim(brca$x)

mean(brca$y=="M")

which.max(colMeans(brca$x))

which.min(colSds(brca$x))

x <- brca$x
centered_x <- sweep(x, 2, colMeans(x))
scaled_x <- sweep(centered_x,2,colSds(centered_x),FUN = "/")



sd(scaled_x[,1])
median(scaled_x[,1])


d<-dist(scaled_x)

length(d)
length(as.matrix(d))
dim(as.matrix(d))

hclust(d)

y<- brca$y

ind_B <- which(y=="B")
#ind_B_f <- ind_B [-1]

dist_benign <- as.matrix(dist(scaled_x[ind_B,]))
rowMeans(dist_benign[,-1])[1]

hclust(d)
plot(hclust(d))







ind_M <- which(y=="M")
ind_M_f <- c(1,ind_M)

dist_malign <- as.matrix(dist(scaled_x[ind_M_f,]))
colMeans(dist_malign)[1]

rowMeans(dist_malign[,-1])[1]



heatmap(as.matrix(dist(t(scaled_x))), col= RColorBrewer::brewer.pal(11, "Spectral"))

heatmap(as.matrix(dist(t(scaled_x))),labRow = NA, labCol = NA)

h<-hclust(as.matrix(dist(t(scaled_x))))

h<-hclust(as.matrix(dist(scaled_x)))



plot(h, cex = 0.65, main = "", xlab = "")


d_features <- dist(t(scaled_x))
h <-hclust(d_features) 
plot(h)

groups <- cutree(h,k=5)

split(names(groups), groups)



pca<-prcomp(scaled_x)


summary(pca)
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))

plot(var_explained)
var_explained[1]
ifelse(var_explained > 0.9, 1, 0)

data.frame(pca$x[,1:2], cancer = y) %>%
  ggplot(aes(PC1,PC2, fill = y)) +
  geom_point(cex=3,pch=21,alpha=0.7) 

length(y)


data.frame(pca$x[,1:10],  y) %>%
 gather(PC,Val,-y) %>% 
  ggplot(aes(y=Val, x=PC, color=y))+
  geom_boxplot()


boxplot(x=pca$x[,1:10])

set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- scaled_x[test_index,]
test_y <- brca$y[test_index]
train_x <- scaled_x[-test_index,]
train_y <- brca$y[-test_index]

mean(train_y=="B")
mean(test_y=="B")

set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later

k <- kmeans(train_x, centers = 2)

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

predicted_kmeans=as.factor(ifelse(predict_kmeans(test_x,k)==1,"B","M"))

mean(predicted_kmeans==test_y)

length(test_x)
length(train_y)
length(predicted)
length(test_y)

c<-confusionMatrix(predicted,test_y)

c$byClass[["Sensitivity"]]
35/(35+8)

tset <- data.frame(train_x,train_y)

fit_glm <- train(train_y~., method="glm",data = tset)
fit_glm <- train(train_x,train_y, method="glm")


glm_y_hat<- predict(fit_glm,test_x)
mean(glm_y_hat==test_y)


fit_lda <- train(train_x,train_y, method="lda")
fit_qda <- train(train_x,train_y, method="qda")


lda_y_hat<- predict(fit_lda,test_x)
mean(lda_y_hat==test_y)
qda_y_hat<- predict(fit_qda,test_x)
mean(qda_y_hat==test_y)


install.packages("gam")
library(gam)

set.seed(5, sample.kind = "Rounding")
fit_loess <- train(train_x,train_y, method="gamLoess")

loess_y_hat<- predict(fit_loess,test_x)
mean(loess_y_hat==test_y)

getModelInfo("gam")
modelLookup("gam")

gam()


mean(train_y=="B")

mean(test_y=="B")

set.seed(7, sample.kind = "Rounding")
fit_knn <- train(train_x,
                 train_y, 
                 method="knn",
                 tuneGrid = data.frame(k = seq(3,21,2))
                 )

fit_knn$bestTune

knn_y_hat<- predict(fit_knn,test_x)
mean(knn_y_hat==test_y)





set.seed(9, sample.kind = "Rounding")
fit_rf <- train(train_x,
                 train_y, 
                 method="rf",
                tuneGrid=data.frame(mtry=c(3, 5, 7, 9)),
                importance=T #or True for one part of question
)


rf_y_hat<- predict(fit_rf,test_x)
mean(rf_y_hat==test_y)
fit_rf$bestTune
top<-varImp(fit_rf)$importance%>% as.data.frame()%>% select(M)%>% order(decreasing = T)

varImp(fit_rf)$importance[top,]

varImp(fit_rf)$importance[24,]


predictions<- data.frame(predicted_kmeans,glm_y_hat,knn_y_hat,lda_y_hat,loess_y_hat,rf_y_hat,qda_y_hat)



ens <- apply(predictions,1,function(x)
  as.factor(ifelse(mean(x=="B")>0.5,"B","M"))
 )

mean(ens==test_y)



predictions <- cbind(predictions,ens)


final<-apply(predictions,2,function(y){
  confusionMatrix(as.factor(y),as.factor(test_y))$overall["Accuracy"]
})

test_y



final<-for(i in 1:8){
data.frame(i,confusionMatrix(predictions[,i],test_y)$overall["Accuracy"])
}


accuracies <- apply(predictions, 2, function(x){
  confusionMatrix(as.factor(x), as.factor(test_y))$overall["Accuracy"]
})



confusionMatrix(predictions[,1],test_y)$overall["Accuracy"]
