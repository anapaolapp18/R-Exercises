# install.packages("tm")
# install.packages("tidyverse")

library(tidyverse)
library(tm)

movie = read_csv("exercises/homeworks/02/movie.csv")

genres = (sapply(movie$genres, gsub,pattern = "\\|", replacement=" "))
genre.sparse.mat = DocumentTermMatrix(Corpus(VectorSource(genres)))
genre.sparse.mat = as.matrix(genre.sparse.mat)

freqs = apply(genre.sparse.mat, 2, sum)

data = data.frame(
  genres = names(freqs),
  freqs = freqs
)

ggplot(data, aes(x = genres, y = freqs)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
