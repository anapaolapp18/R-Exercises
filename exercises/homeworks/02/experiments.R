library(tidyverse)
movie <- read.csv("movie.csv")

#1.
#Histogram of gross
hist(movie$gross)

movie1 <- mutate(movie, log_gross = log(gross))
dim(movie1)
#Histogram of log of gross
hist(movie1$log_gross)

plot(movie$num_voted_users, movie1$log_gross)
plot(movie$num_voted_users, movie1$gross)

ggplot(movie) +
  geom_point(aes(num_voted_users, log(gross)))

ggplot(movie, aes(num_voted_users, log(gross)))+
  geom_point(shape =5, color = "red")+
  geom_smooth()

m1 <- lm(num_voted_users ~ log(gross), movie1)
with(movie1, plot(num_voted_users, log_gross))
abline(m1)



summary(m1)
install.packages("tm")
library(tm)
genres<- (sapply(movie$genres,gsub,pattern="\\|",replacement=" "))
genre.sparse.mat <- DocumentTermMatrix(Corpus(VectorSource(genres)))
genre.sparse.mat<- as.matrix(genre.sparse.mat)
genres

mat.score<- genre.sparse.mat*movie$imdb_score
apply(ifelse(mat.score==0, NA, mat.score), 2, median, na.rm=TRUE)

