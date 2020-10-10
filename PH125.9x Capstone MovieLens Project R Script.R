######Install all needed packages if not present

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(caret)) install.packages("caret")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")
if(!require(corrplot)) install.packages("corrplot")
if(!require(data.table)) install.packages("data.table")
invisible(gc())
invisible(memory.limit(size = 56000))
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
###Loading all the needed libraries
library(tidyverse)
library(caret)
library(data.table)
library(corrplot)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")
head(movielens)



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




###Data Cleaning

#No NA value present in our data

summary(edx)
any(is.na(edx))

head(edx)%>%knitr::kable("simple")

###1.Explore the variables in the edx data set

str(edx)

###no of distinct users and movies in the data set
n_distinct(edx$movieId)
n_distinct(edx$userId)

###Mean and Median Ratings####
mean(edx$rating)
median(edx$rating)


###2. Add attributes year of release and year in which movie was rated

edx<-edx%>%mutate(year=as.numeric(str_sub(title,-5,-2)))%>%
  mutate(rated_year=year(as_datetime(timestamp)))%>%
  mutate(age=rated_year-year)




######3.Data Visualization and exploration######
#3.1 Visualize correlation between numeric variables of the edx data set

cor.data<-cor(edx[,c("userId","movieId","rating","timestamp","year","rated_year","age")])
cor.data
corrplot(cor.data,method='color')

####3.2 Visualize distribution of the ratings for each of the attributes###

###Distribution of ratings


edx%>%group_by(rating)%>%summarize(n=n())%>%
  ggplot(aes(rating,n))+geom_bar(stat="identity",alpha=.5, color="black",fill="red")+theme_bw()+
  scale_x_continuous(breaks=seq(min(0),max(5),by=0.5))+
  ylab("No of Ratings")+geom_vline(xintercept = 3.512)+
  geom_text(aes(x=3.512,label="mean rating", y=2000000),angle=90)

prop.table(table(as.factor(edx$rating)))                                 

##3.3Visualization of  Movies distribution

###Distribution of Movies##
edx%>%group_by(movieId)%>%
  summarize(n=n())%>%ggplot(aes(movieId,n))+
  geom_point(alpha=.5,size=1,color="blue")

edx%>%group_by(movieId)%>%
  summarize(n=n())%>%ggplot(aes(n))+
  geom_histogram(bins = 30,color="red",fill="blue")+
  scale_x_log10()+xlab("no of ratings")+ylab('no of movies')

#Movies rated only once
edx%>%group_by(movieId,title)%>%
  summarize(n=n())%>%filter(n==1)%>%
  head(10)%>%
  knitr::kable("pipe")

#Top 10 rated movies
edx%>%group_by(movieId,title)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(10)%>%knitr::kable()

#Distribution of average ratings for movies
edx%>%group_by(movieId)%>%
  summarize(mean_ratings=mean(rating))%>%
  ggplot(aes(mean_ratings))+
  geom_histogram(bins =  20, color="black")


####3.4 Visualization of Distribution of users#####

#Distribution of users
edx%>%group_by(userId)%>%
  summarize(n=n())%>%ggplot(aes(n))+
  geom_histogram(bins = 30,color="red",fill="blue")+
  scale_x_log10()+xlab("no of ratings")+ylab("no of users")

#Distribution of mean ratings grouped by users#
edx%>%group_by(userId)%>%
  summarize(mean_ratings=mean(rating))%>%
  ggplot(aes(mean_ratings))+
  geom_histogram(bins=20,color="red")


#####3.5Visualization and analysis of Release year, rating year and Age of Movie at time of rating#####

### Release Year
#Trend for no of ratings for movies released in various years#
edx%>%group_by(year)%>%
  count()%>%
  ggplot(aes(year,n,color="red"))+
  geom_line(show.legend = FALSE)+
  ylab("No of ratings")+xlab("Release year")

edx%>%group_by(year)%>%
  count()%>%
  ggplot(aes(year,n,color="red"))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("No of ratings")+xlab("Release year")

#Movies released in which years were rated the most#
edx%>%group_by(year)%>%summarise(n=n())%>%
  arrange(desc(n))%>%
  head()%>%
  knitr::kable("pipe")

#Movies released in which years were rated least no of times#
edx%>%group_by(year)%>%summarise(n=n())%>%
  arrange(n)%>%
  head()%>%
  knitr::kable("pipe")

#Average Ratings for different release years#
edx%>%group_by(year)%>%
  summarize(mean_rating=mean(rating))%>%
  ggplot(aes(year,mean_rating))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept = 3.51)+geom_text(aes(x=1950,y=3.51,label="mean rating"))



###rated year--Year in which movie is rated####

#Distribution of Rating years and count of ratings#
edx%>%group_by(rated_year)%>%
  count()%>%
  ggplot(aes(rated_year,n))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  xlab("year of rating")+
  ylab("No of Ratings")


#Years in which most movies were rated# 
edx%>%group_by(rated_year)%>%
  summarize(n=n())%>%
  arrange(desc(n))%>%
  head(3)%>%knitr::kable("pipe")

#Average Ratings given in different rating years#
edx%>%group_by(rated_year)%>%
  summarize(mean_rating=mean(rating))%>%knitr::kable()

edx%>%group_by(rated_year)%>%
  summarize(mean_rating=mean(rating))%>%
  ggplot(aes(rated_year,mean_rating))+
  geom_line()+
  geom_hline(yintercept = 3.51)+xlab("Year of Rating")+
  geom_text(aes(x=2005,y=3.51,label="mean rating"))



###Visualizing Movie Age at time of rating###

###Distribution of Total ratings for different Movie ages##
edx%>%group_by(age)%>%
  count()%>%
  ggplot(aes(age,n))+
  geom_bar(stat="identity")+
  xlab("Age of Movie at Rating")+
  ylab("No of ratings")


###Average ratings for different movie ages ####
edx%>%group_by(age)%>%
  summarize(mean_rating=mean(rating))%>%
  ggplot(aes(age,mean_rating))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept = 3.51)+
  geom_text(aes(x=50,y=3.51,label="mean rating"))


###3.6 Visualization of Ratings distribution by Genres

##No of distinct genres
n_distinct(edx$genres)

###Distribution of top 50 most popular movie genres###
edx%>%group_by(genres)%>%
  summarize(n=n())%>%top_n(50,n)%>%
  ggplot(aes(n,genres))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 90))

###Genres combinations which were rated most###
edx%>%group_by(genres)%>%
  summarize(n=n())%>%arrange(desc(n))%>%
  head(10)%>%knitr::kable("simple")

##Top 10 appearing genres

genre = c("Drama", "Comedy", "Thriller",
          "Romance","Action","Adventure",
          "Sci-Fi","Horror","Fantasy","Children")

sapply(genre, function(g) {
  sum(str_detect(edx$genres, g))
})

### Visualization of Average ratings for genres##
edx%>%group_by(genres)%>%
  filter(n()>1000)%>%summarise(mean_rating=mean(rating))%>%
  ggplot(aes(mean_rating))+
  geom_histogram(bins=20,alpha=0.5,color="black",fill="red")+
  theme_bw()
  
###Visualizing mean ratings for top 50 genres that were rated the most###
edx%>%group_by(genres)%>%
  summarize(n=n(),mean_rating=mean(rating))%>%
  top_n(50,n)%>%
  ggplot(aes(mean_rating,genres))+
  geom_bar(stat="identity",color="red",fill="black",show.legend = FALSE)+
  geom_col()+xlab("Rating")+
  geom_vline(xintercept = 3.51)+
  geom_text(aes(x=3.6,y="Comedy",label="mean rating",angle=90))



# 4.Splitting of edx into train and test data for the purpose of building model

# Training set will be 90 % of edx data and test set will be 10% of edx data

set.seed(1, sample.kind = "Rounding")
test_index<-createDataPartition(edx$rating,times=1,p=0.1, list=FALSE)
train_set<-edx[-test_index,]
temp_1<-edx[test_index,]

#Ensure userId and MovieId in the test set are also in train set

test_set<-temp_1%>%semi_join(train_set,by="movieId")%>%semi_join(train_set,by="userId")

# Add rows removed from test set back into train set

removed<-anti_join(temp_1,test_set)
train_set<-rbind(train_set,removed)

rm(removed,temp_1,test_index)


##########5. Building Prediction Model#########
######Define RMSE

RMSE<-function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}

# 5.1 Basic model using average rating
mu<-mean(train_set$rating)

##### naive RMSE

basic_RMSE<-RMSE(mu,test_set$rating)
basic_RMSE

###creating table for RMSEs calculated###
rmse_calculations<-data.frame(method="baseline average model", RMSE=basic_RMSE)
rmse_calculations%>%knitr::kable("pipe")

##5.2  AGE EFFECT ONLY

#Defining age effect or average rating by movie age at time of rating##
age_avgs<-train_set%>%group_by(age)%>%summarize(b_a=mean(rating-mu))

#Visualization of age effect###
age_avgs%>%ggplot(aes(b_a))+geom_histogram(bins=30,alpha=.5,color="red")

#Predicting ratings using only age effect
predicted_ratings<-mu+test_set%>%left_join(age_avgs,by="age")%>%pull(b_a)

#Calculating RMSE due to age effect
age_effect_rmse<-RMSE(predicted_ratings,test_set$rating)

#Updating the RMSE calculation table
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Age effect",RMSE=age_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")

####5.3 Movie effect only

mu<-mean(train_set$rating)

#Defining average rating by movieId
movie_averages<-train_set%>%
  group_by(movieId)%>%summarize(b_i=mean(rating-mu))

#Visualization of Movie effect
movie_averages%>%ggplot(aes(b_i))+
  geom_histogram(bins = 20,alpha=0.5,color="black",fill="red")+
  theme_bw()

#Predicting ratings using movie effect
predicted_ratings<-mu+test_set%>%
  left_join(movie_averages,by="movieId")%>%
  pull(b_i)

#Calculating RMSE due to movie effect
movie_effect_rmse<-RMSE(predicted_ratings,test_set$rating)

#Updating the RMSE results table
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Movie effect",RMSE=movie_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")


#### 5.4 Adding User Effect###
mu<-mean(train_set$rating)

#Visualization of user effect
train_set%>%group_by(userId)%>%
  summarize(b_u=mean(rating))%>%
  ggplot(aes(b_u))+geom_histogram(bins=20,color='black')

#Defining average ratings by user
user_averages<-train_set%>%
  left_join(movie_averages,by="movieId")%>%
  group_by(userId)%>%summarize(b_u=mean(rating-mu-b_i))

#Predicting ratings using movie and user effect
predicted_ratings<-test_set%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  mutate(pred=mu+b_i+b_u)%>%pull(pred)

#Calculating RMSE due to movie and user effect
movie_user_effect_rmse<-RMSE(predicted_ratings,test_set$rating)

#Updating RMSE results table
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Movie+User effect",RMSE=movie_user_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")


####5.5  Adding Year effect

#Visualization of movie release year
train_set%>%group_by(year)%>%
  summarize(b_y=mean(rating))%>%
  ggplot(aes(b_y))+geom_histogram(bins=20,color="black")

#Defining average ratings by release year
year_averages<-train_set%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  group_by(year)%>%summarize(b_y=mean(rating-mu-b_i-b_u))

#Predicting ratings using movie, user and year effect
predicted_ratings<-test_set%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  left_join(year_averages,by="year")%>%
  mutate(pred_year=mu+b_i+b_u+b_y)%>%
  pull(pred_year)

#Calculating RMSE using movie, user and release year effect
movie_user_year_effect_rmse<-RMSE(predicted_ratings,test_set$rating)

#Updating RMSE results table
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Movie+User+Year effect",RMSE=movie_user_year_effect_rmse))
rmse_calculations%>%knitr::kable()

###5.6 Adding genre effect
#Visualizing genres effect
train_set%>%group_by(genres)%>%
  filter(n()>1000)%>%summarise(b_g=mean(rating))%>%
  ggplot(aes(b_g))+geom_histogram(bins=20,alpha=0.5,color="black",fill="red")+theme_bw()

#Defining Average ratings by genres
genres_averages<-train_set%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  left_join(year_averages,by="year")%>%
  group_by(genres)%>%
  summarize(b_g=mean(rating-mu-b_i-b_u-b_y))

#Predicting ratings using movie,user,year and genre effect
predicted_ratings<-test_set%>%left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  left_join(year_averages,by="year")%>%
  left_join(genres_averages,by="genres")%>%
  mutate(pred_genre=mu+b_i+b_u+b_y+b_g)%>%
  pull(pred_genre)

#Calculating RMSE using movie,user,year and genres effect
movie_user_year_genre_effect_rmse<-RMSE(predicted_ratings,test_set$rating) 

#Updating the RMSE results table
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Movie+User+Year+Genre effect",RMSE=movie_user_year_genre_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")


###5.7 Regularization Model######

## why do we go for regularization model

movie_titles<-train_set%>%
  select(movieId,title)%>%
  distinct()

##Best movies rated by how many users
train_set%>%count(movieId)%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(movie_titles,by="movieId")%>%
  arrange(desc(b_i))%>%
  slice(1:10)%>%
  pull(n)

###Worst movies rated by how many users
train_set%>%count(movieId)%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(movie_titles,by="movieId")%>%
  arrange(b_i)%>%
  slice(1:10)%>%
  pull(n)
  


#####5.8 k- Fold Cross Validation to find optimum lambda using k=5
### Split the data into k folds

set.seed(1,sample.kind = "Rounding")
lambdas<-seq(0,10,.25)

#Split train_set into 5 parts
folds<-createFolds(train_set$rating,k=5,returnTrain = T)

###A matrix is defined to store the results of cross validation##
rmses<- matrix(nrow=5, ncol=length(lambdas))

lambdas

ks<- seq(1,5,1)
ks

for(k in ks){
  train_set_cv<-train_set[folds[[k]],]
  temp<-train_set[-folds[[k]],]
  
  
#make sure userId and movieId in the test set are also in train set
  test_set_cv<-temp%>%
    semi_join(train_set_cv,by="movieId")%>%
    semi_join(train_set_cv,by="userId")

#Add rows removed from test set to train set
  
  removed<-anti_join(temp,test_set_cv)
  train_set_cv<-rbind(train_set_cv,removed)
  
  mu<-mean(train_set_cv$rating)
  
  rmses[k,]<-sapply(lambdas,function(l){
    
    b_i<-train_set_cv%>%
      group_by(movieId)%>%
      summarize(b_i=sum(rating-mu)/(n()+l))
    
    b_u<-train_set_cv%>%
     left_join(b_i,by="movieId")%>%
      group_by(userId)%>%
      summarize(b_u=sum(rating-mu-b_i)/(n()+l))
    
    b_y<-train_set_cv%>%
      left_join(b_i,by="movieId")%>%
      left_join(b_u,by="userId")%>%
      group_by(year)%>%
      summarize(b_y=sum(rating-mu-b_i-b_u)/(n()+l))
    
    b_g<-train_set_cv%>%
      left_join(b_i, by="movieId")%>%
      left_join(b_u, by="userId")%>%
      left_join(b_y, by="year")%>%
      group_by(genres)%>%
      summarize(b_g=sum(rating-mu-b_i-b_u-b_y)/(n()+l))
    
    predicted_ratings<-test_set_cv%>%
      left_join(b_i,by="movieId")%>%
      left_join(b_u,by="userId")%>%
      left_join(b_y,by="year")%>%
      left_join(b_g,by="genres")%>%
      mutate(y_hat=mu+b_i+b_u+b_y+b_g)%>%
      pull(y_hat)
    
    return(RMSE(predicted_ratings,test_set_cv$rating))
      
  })
    
}
  
rmses_cv<-colMeans(rmses)
rmses_cv


#Plot of RMSES  value for range of lambda values 
qplot(lambdas,rmses_cv)
lambda_opt<-lambdas[which.min(rmses_cv)]
lambda_opt
min(rmses_cv)

#####6. Evaluation with test set using optimal lambda#####
mu<-mean(train_set$rating)
mu
movie_reg_averages<-train_set%>%
  group_by(movieId)%>%
  summarize(b_i=sum(rating-mu)/(n()+lambda_opt))

user_reg_averages<-train_set%>%
  left_join(movie_reg_averages,by="movieId")%>%
  group_by(userId)%>%
  summarize(b_u=sum(rating-b_i-mu)/(n()+lambda_opt))

year_reg_averages<-train_set%>%
  left_join(movie_reg_averages,by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  group_by(year)%>%
  summarize(b_y=sum(rating-mu-b_i-b_u)/(n()+lambda_opt))


genre_reg_averages<-train_set%>%
  left_join(movie_reg_averages,by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  left_join(year_reg_averages,by="year")%>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu-b_i-b_u-b_y)/(n()+lambda_opt))

predicted_ratings<-test_set%>%
  left_join(movie_reg_averages, by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  left_join(year_reg_averages,by="year")%>%
  left_join(genre_reg_averages,by="genres")%>%
  mutate(pred=mu+b_i+b_u+b_y+b_g)%>%
  pull(pred)

Reg_cv_rmse<-RMSE(predicted_ratings,test_set$rating)
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Regularisation with 5-fold CV-Movie+User+Year+Genre",RMSE=Reg_cv_rmse))
rmse_calculations%>%knitr::kable("pipe")
Reg_cv_rmse

###7. Regularization on Validation using lambda_min
###  Adding year, Rating year and Age columns to Validation set
validation<-validation%>%mutate(year=as.numeric(str_sub(title,-5,-2)))
validation<-validation%>%mutate(rated_year=year(as_datetime(timestamp)))
validation<-validation%>%mutate(age=rated_year-year)


mu_edx<-mean(edx$rating)

movie_reg_averages<-edx%>%
  group_by(movieId)%>%
  summarize(b_i=sum(rating-mu_edx)/(n()+lambda_opt))

user_reg_averages<-edx%>%
  left_join(movie_reg_averages,by="movieId")%>%
  group_by(userId)%>%
  summarize(b_u=sum(rating-mu_edx-b_i)/(n()+lambda_opt))

year_reg_averages<-edx%>%
  left_join(movie_reg_averages,by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  group_by(year)%>%
  summarize(b_y=sum(rating-mu_edx-b_i-b_u)/(n()+lambda_opt))

 
genre_reg_averages<-edx%>%
  left_join(movie_reg_averages,by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  left_join(year_reg_averages,by="year")%>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu_edx-b_i-b_u-b_y)/(n()+lambda_opt))

#Predicting ratings for validation set
predicted_ratings_val<-validation%>%
  left_join(movie_reg_averages, by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  left_join(year_reg_averages,by="year")%>%
  left_join(genre_reg_averages,by="genres")%>%
  mutate(pred_validation=mu+b_i+b_u+b_y+b_g)%>%
  pull(pred_validation)


#calculation of RMSE using validation set
RMSE_valid<-RMSE(predicted_ratings_val,validation$rating)

#Final RMSE with validation set

print(RMSE_valid)

#Updating RMSE results table
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Final Regularization on Validation with optimal Lambda",RMSE=RMSE_valid))
rmse_calculations%>%knitr::kable("pipe", caption = "RMSE Results Table")
