#####PART 2
#Q1- EDA: Getting a feel
library(tidyverse)
movie = read.csv("movie.csv", fileEncoding = "UTF-8")

view(movie)
dim(movie)
names(movie)
summary(movie)

##EDA: I've picked 6 predictors, now lets visualize them:

#Gross:
ggplot(movie, aes(log(gross), imdb_score))+
  geom_point()+
  geom_smooth()

cor(movie$gross,movie$imdb_score,use="complete.obs")

#Budget:
ggplot(movie, aes(log(budget), imdb_score))+
  geom_point()+
  geom_smooth()

cor(movie$budget,movie$imdb_score,use="complete.obs")

##Number of critics for reviews

ggplot(movie, aes(num_critic_for_reviews, imdb_score))+
  geom_point()+
  geom_smooth()

cor(movie$num_critic_for_reviews,movie$imdb_score,use="complete.obs")

##Director_facebook_likes


ggplot(movie, aes(log(director_facebook_likes), imdb_score))+
  geom_point()+
  geom_smooth()

cor(movie$director_facebook_likes,movie$imdb_score,use="complete.obs")



#Genre:
library(tm)
genres<- (sapply(movie$genres,gsub,pattern="\\|",replacement=" "))
genre.sparse.mat <- DocumentTermMatrix(Corpus(VectorSource(genres)))
genre.sparse.mat<- as.matrix(genre.sparse.mat)


mat.score<- genre.sparse.mat*movie$imdb_score
apply(ifelse(mat.score==0, NA, mat.score), 2, median, na.rm=TRUE)

genre_score<- data.frame(genre=colnames(mat.score),
                         avg_imdb_score=apply(ifelse(mat.score==0, NA, mat.score), 2, median, na.rm=TRUE))

ggplot(genre_score, aes(x=genre, y=avg_imdb_score))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

##Chi-squared test
tab.genre<- table(genre_score$genre, genre_score$avg_imdb_score)
chisq.test(tab.genre)


#Number of voted users
ggplot(movie, aes(num_voted_users, imdb_score))+
  geom_point()+
  geom_smooth()

cor(movie$num_voted_users,movie$imdb_score,use="complete.obs")

#Facebook likes
ggplot(movie, aes(cast_total_facebook_likes, imdb_score))+
  geom_point()+
  geom_smooth()

cor(movie$cast_total_facebook_likes,movie$imdb_score,use="complete.obs")


#Movie title
library(tm)
movie_title<- (sapply(movie$movie_title,gsub,pattern="\\|",replacement=" "))
movie_title.sparse.mat <- DocumentTermMatrix(Corpus(VectorSource(movie_title)))
movie_title.sparse.mat<- as.matrix(movie_title.sparse.mat)


movie_mat.score<- movie_title.sparse.mat*movie$imdb_score
apply(ifelse(movie_mat.score==0, NA, movie_mat.score), 2, median, na.rm=TRUE)

title_score<- data.frame(title=colnames(movie_mat.score),
                         avg_imdb_score=apply(ifelse(movie_mat.score==0, NA, movie_mat.score), 2, median, na.rm=TRUE))

ggplot(title_score, aes(x=title, y=avg_imdb_score))+
  geom_bar(stat="identity")

ggplot(title_score, aes(x= avg_imdb_score, fill=title))+
  geom_bar(position = "fill")

#Chi-squared test
tab.title<- table(title_score$title, title_score$avg_imdb_score)
chisq.test(tab.title)


##3

training<- movie[1:400,]
testing<- movie[-c(1:400),]
lm.fit1<- lm(imdb_score~gross + budget + movie_facebook_likes,
             cast_total_facebook_likes, data = training)

pred.test<- predict(lm.fit1, newdata = testing)
MSPE<- mean((testing$imdb_score-pred.test)^2)
