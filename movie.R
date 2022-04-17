library(recommenderlab)
library(reshape2)
library(data.table)
library(ggplot2)
movie_data <- read.csv("movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data)
str(rating_data)
data.table(movie_data)
data.table(rating_data)
summary(movie_data)
summary(rating_data)
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], "[|]", type.convert = TRUE), stringsAsFactors = FALSE)
colnames(movie_genre2) <-c(1:10)
list_genre <- c("Action","Adventure","Animation","Children",
                 "Comedy","Crime","Documentary","Drama","Fantasy",
                 "Film-Noir","Horror","Musical","Mystery","Romance",
                 "Sci-Fi","Thriller","war","western")
genre_matl <- matrix(0,34209,18)
genre_matl[1,] <- list_genre
colnames(genre_matl) <- list_genre
for(index in 1:nrow(movie_genre2)){
  for(col in 1:ncol(movie_genre2)){
    gen_col = which(genre_matl[1,] == movie_genre2[index,col])
    genre_matl[index+1,gen_col] <-1
  }
}
genre_mat2 <- as.data.frame(genre_matl[-1,], stringsAsFactors=FALSE)
for(col in 1:ncol(genre_mat2)){
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}
str(genre_mat2)
#head(movie_data)
SearchMovie <- cbind(movie_data[,1:2],genre_mat2[])
head(SearchMovie)
ratingMatrix <- reshape2::dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1])

ratingMatrix<-as(ratingMatrix, "realRatingMatrix")

recommendation_model<-recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")

recommendation_model$IBCF_realRatingMatrix$parameters

movie_similarity<-similarity(ratingMatrix[ ,1:4],method="cosine",which="items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity),main="Movie similarity")

rating_values<-as.vector(ratingMatrix@data)
unique(rating_values)

Table_rating<-table(rating_values)
Table_rating


movie_views<-colCounts(ratingMatrix)
table_views<-data.frame(movie=names(movie_views),views=movie_views)
table_views<-table_views[order(table_views$views,decreasing=TRUE), ]
table_views$title<-NA
for (index in 1:34205){
  table_views[index,3]<-as.character(subset(movie_data,movie_data$movieId==table_views[index,1])$title)
}
table_views[1:6,]

ggplot(table_views[1:6, ], aes(x=title,y=views)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Total views of the Top Films")

image(ratingMatrix[1:30,1:30], axes=FALSE, main="30 x 30 heatmap")

movie_ratings<-ratingMatrix[rowCounts(ratingMatrix)>50,colCounts(ratingMatrix)>50]
movie_ratings

minimum_movies<-quantile(rowCounts(movie_ratings),0.98)
minimum_users<-quantile(colCounts(movie_ratings),0.98)
image(movie_ratings[rowCounts(movie_ratings)>minimum_movies,colCounts(movie_ratings)>minimum_users],main="Heatmap of top users and movies")

average_ratings<-rowMeans(movie_ratings)
qplot(average_ratings, fill=I("black"),col=I("blue")) +
  ggtitle("Distribution of the average rating per user")

normalized_ratings<-normalize(movie_ratings)
sum(rowMeans(normalized_ratings)>0.00001)

image(normalized_ratings[rowCounts(normalized_ratings)>minimum_movies,colCounts(normalized_ratings)>minimum_users],main="Normalized ratings of Top Users")

binary_minimum_movies<-quantile(rowCounts(movie_ratings),0.90)
binary_minimum_users<-quantile(colCounts(movie_ratings),0.90)

movies_watched<-binarize(movie_ratings,minRating=1)

good_rated_films<-binarize(movie_ratings,minRating=3.5)
image(good_rated_films[rowCounts(movie_ratings)>binary_minimum_movies,colCounts(movie_ratings)>binary_minimum_users],main="Heatmap of top users and movies")

sampled_data<-sample(x=c(TRUE,FALSE),size=nrow(movie_ratings),replace=TRUE,prob=c(0.8, 0.2))
training_data<-movie_ratings[sampled_data, ]
testing_data<-movie_ratings[!sampled_data, ]
