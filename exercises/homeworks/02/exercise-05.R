# install.packages("tm")
# install.packages("tidyverse")

library(tidyverse)
library(tm)

movie = read_csv("exercises/homeworks/02/movie.csv")

genres = (sapply(movie$genres, gsub,pattern = "\\|", replacement=" "))
genre.sparse.mat = DocumentTermMatrix(Corpus(VectorSource(genres)))
genre.sparse.mat = as.matrix(genre.sparse.mat)
view(genre.sparse.mat)


freqs = apply(genre.sparse.mat, 2, sum)

data = data.frame(
  genres = names(freqs),
  freqs = freqs
)
view(freqs)
mat.score = genre.sparse.mat*movie$imdb_score
data$mean_scores = apply(ifelse(mat.score==0, NA, mat.score), 2, median, na.rm=TRUE)

view(mean_scores)
view(data)

ggplot(data = data, aes(x = freqs, y = mean_scores)) + geom_point()

##6

mat.budget = genre.sparse.mat*movie$budget
data$budget = apply(ifelse(mat.budget==0, NA, mat.budget), 2, median, na.rm=TRUE)
view(data)
view(genres)
view(data)

  ggplot(data = data, aes(x = log(budget), y = mean_scores, color = genres)) + geom_point()
view(data)

cor(movie1$imdb_score,movie1$budget, use = "complete.obs")
names(movie1)
view(movie1$num_voted_users)
