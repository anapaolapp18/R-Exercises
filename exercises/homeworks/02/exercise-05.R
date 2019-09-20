# install.packages("tm")
# install.packages("tidyverse")

library(tidyverse)
library(tm)

movie = read_csv("exercises/homeworks/02/movie.csv")

genres = (sapply(movie$genres, gsub,pattern = "\\|", replacement=" "))
genre.sparse.mat = DocumentTermMatrix(Corpus(VectorSource(genres)))
genre.sparse.mat = as.matrix(genre.sparse.mat)

# TBD
