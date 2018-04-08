#MovieLens Analysis
rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(lubridate)
library(Amelia)
library(stringr)
library(plotly)

#Function for reading in multiple seperators

# readMulti <- function(dataset, sep){
#   ds <- readLines(dataset)
#   ds1 <- gsub(sep, "\t", ds)
#   ds2 <- read.csv(text = ds1, sep = "\t")
#   return(ds2)
# }

readMulti <- function(dataset, sep){
  ds <- readLines(dataset)
  ds1 <- gsub(sep, "\t", ds)
  ds2 <- paste0(ds1, collapse = "\n")
  ds3 <- fread(ds2, sep = "\t")
  return (ds3)
}

movies <- readMulti("D:\\Projects\\Movielens Analysis\\Dataset\\ml-1m\\ml-1m\\movies.dat", sep = "::")
ratings <- readMulti("D:\\Projects\\Movielens Analysis\\Dataset\\ml-1m\\ml-1m\\ratings.dat", sep = "::")
users <- readMulti("D:\\Projects\\Movielens Analysis\\Dataset\\ml-1m\\ml-1m\\users.dat", sep = "::")

colnames(movies) <- c("MovieID", "Title", "Genres")
colnames(ratings) <- c("UserID", "MovieID", "Rating", "Timestamp")
colnames(users) <- c("UserID", "Gender", "Age", "Occupation", "Zip-code")

#Data Cleaning

#Rating
str(ratings)

ratings$Timestamp <- as_datetime(ratings$Timestamp)
summary(ratings)

any(is.na(ratings)) #No missing values.

glimpse(ratings)
str(ratings)

ratings.plot <- ggplot(ratings, aes(x = Rating)) + geom_bar(aes(fill = ..count..))
ratings.plot <- ratings.plot + scale_fill_gradient(low = 'red', high = 'green') + theme_bw()
ratings.plot

#Movies
glimpse(movies)
head(movies)

#Extracting debut year as a new column
r <- gregexpr("\\(\\d+\\)", movies$Title)
debut.year <- unlist(regmatches(movies$Title, r))
debut.year <- gsub("\\(|\\)", "", debut.year)
debut.year <- as.Date(debut.year, "%Y")
debut.year <- year(debut.year)
movies$Year <- debut.year
str(movies)

#Remove the debut years from original title column
movies$Title <- gsub("\\(\\d+\\)", "", movies$Title)
movies$Title <- str_trim(as.character(movies$Title), side = "both")
any(is.na(movies))


#USERS
str(users)
users$Occupation <- as.factor(users$Occupation)


occupation.encode <- function(x){
  switch(as.character(x), "0"="other or notspecified"
         ,"1"="academic/educator"
         ,"2"="artist"
         ,"3"="clerical/admin"
         ,"4"="college/gradstudent"
         ,"5"="customerservice"
         ,"6"="doctor/healthcare"
         ,"7"="executive/managerial"
         ,"8"="farmer"
         ,"9"="homemaker"
         ,"10"="K-12student"
         ,"11"="lawyer"
         ,"12"="programmer"
         ,"13"="retired"
         ,"14"="sales/marketing"
         ,"15"="scientist"
         ,"16"="self-employed"
         ,"17"="technician/engineer"
         ,"18"="tradesman/craftsman"
         ,"19"="unemployed"
         ,"20"="writer")
}

users$Occupation <-  sapply(users$Occupation, occupation.encode)

#Data Exploration

#How many movies were produced per year?

movies.per.year <- movies %>% group_by(Year) %>% summarise(count = n()) %>% arrange(desc(Year))
movies.per.year


movies.perYear.plot <- ggplot(movies.per.year, aes(x = Year, y = count)) + geom_col(aes(fill = count))
movies.perYear.plot <- movies.perYear.plot + scale_fill_gradient(low = 'green', high = 'blue') + theme_dark()
movies.perYear.plot

movies.perYear.plot <- ggplot(movies.per.year, aes(x = Year, y = count)) + geom_line(col = 'blue', linetype = 'solid', size = 1) + theme_bw()
movies.perYear.plot

#From the plot above we can see that the number of movies produced is increasing year after year and so is the deman
#The sharp decline in the year 2000 is due to the fact that the data in the dataset is only till year 2000.

#What were the most popular movies genres year by year?

genresByYear <- movies %>% separate_rows(Genres, sep = "\\|") %>% select(MovieID, Year, Genres) %>% group_by(Year, Genres) %>% summarise(count = n()) %>% arrange(desc(Year))

ggplot(genresByYear, aes(x = Year, y = count)) + geom_col(aes(fill = Genres), position = 'dodge')
genresByYear %>% filter(Genres %in% c('Action', 'Drama', 'Horror')) %>% ggplot(aes(x = Year, y = count)) + geom_line(aes(col = Genres), size = 1) + facet_grid(Genres~.)

#Discretization of Year to make it readable

year.discretize <- function(x){
  if(x >= 1920 && x <= 1940){
    return('1920-1940')
  }
  else if(x >= 1941 && x <= 1960){
    return('1941-1960')
  }
  else if(x >= 1961 && x <= 1980){
    return('1961-1980')
  }
  else{
    return('1981-2000')
  }
}

genresByYear$grouped.year <- sapply(genresByYear$Year, year.discretize)

ggplot(genresByYear, aes(x = grouped.year, y = count)) + geom_col(aes(fill = Genres), position = 'dodge')

#Choosing only a few main genres for readability

genresByYear %>% filter(Genres %in% c('Action', 'Drama', 'Comedy', 'Romance')) %>% ggplot(aes(x = grouped.year, y = count)) + geom_col(aes(fill = Genres), position = 'dodge')

#Line graph for looking at trend

genresByYear %>% filter(Genres %in% c('Action', 'Drama', 'Comedy', 'Romance', 'War')) %>% ggplot(aes(x = Year, y = count)) + geom_line(aes(col = Genres), size = 1)

#Average ratings by Genre*

#Best movies of every decade based on users ratings

#All time best movies

movies.ratings <- inner_join(ratings, movies, by = 'MovieID')
str(movies.ratings)

any(is.na(movies.ratings))

head(movies.ratings)

movies.ratings <- rename(movies.ratings, ratings.date = Timestamp)

best.ratings <- movies.ratings %>% select(Rating, Title, UserID) %>% group_by(Title) %>% summarise(avg_rating = mean(Rating), number_of_ratings = n()) %>% arrange(desc(avg_rating))
print(best.ratings, n = 100)

ggplot(best.ratings[1:10,], aes(x = Title, y = avg_rating)) + geom_col(fill = "blue") + coord_flip()

#Looking at the best movies with more than 100 ratings since alot of movies have 1 rating of 5 stars as seen in the above plot.
best.ratings.filtered <- best.ratings %>% filter(number_of_ratings > 100) %>% arrange(desc(avg_rating))
best.ratings.filtered
#Plot made interative and x labels removed cuz it was too overcrowded.
# ratings.plot <- ggplot(best.ratings.filtered[1:10,], aes(x = Title, y = avg_rating)) + geom_col(aes(fill = number_of_ratings)) + ggtitle("Average Ratings by Movies") + theme(axis.title.x = element_blank(),
#                                                                                                                                                                               axis.text.x = element_blank(),
#                                                                                                                                                                               axis.ticks.x = element_blank())
str(best.ratings.filtered)
best.ratings.filtered$Title<- factor(best.ratings.filtered$Title, levels = rev(unique(as.character(best.ratings.filtered$Title))))
head(best.ratings.filtered)
ratings.plot <- ggplot(best.ratings.filtered[1:10,], aes(x = Title, y = avg_rating)) + geom_col(aes(fill = number_of_ratings)) + ggtitle("Average Ratings by Movies") + coord_flip()
ratings.plot
ggplotly(ratings.plot)

#Best years for a genre based on rating

genres.ratings.by.year <- movies.ratings %>% select(Year, Genres, Rating) %>% separate_rows(Genres, sep = "\\|") %>% group_by(Year, Genres) %>% summarise(avg_rating = mean(Rating))

genres.ratings.by.year %>% filter(Genres %in% c('Action', 'Drama', 'Sci-Fi', 'Documentary')) %>% ggplot(aes(x = Year, y = avg_rating)) + geom_line(aes(col = Genres)) + facet_grid(.~Genres) + geom_smooth()

#Plot above is biased because it takes average rating instead of weighted ratings based on the number of reviews.

#We take only the genres that have atleast a 5000 ratings.


genres.ratings.by.year.filtered <- movies.ratings %>% select(UserID, Year, Genres, Rating) %>% separate_rows(Genres, sep = "\\|") %>% group_by(Year, Genres) %>% summarise(avg_rating = mean(Rating), number_of_ratings = n()) %>% filter(number_of_ratings >= 5000)

genres.ratings.by.year.filtered %>% filter(Genres %in% c('Action', 'Drama', 'Sci-Fi', 'Documentary')) %>% ggplot(aes(x = Year, y = avg_rating)) + geom_line(aes(col = Genres)) + facet_grid(.~Genres) + geom_smooth()


#Distribution of Users Age

ggplot(users, aes(x = Age)) + geom_histogram(binwidth = 1)


# Encoding Age

Age.encoding <- function(x){
  switch(as.character(x), "1"="Under 18"
         ,"18"="18-24"
         ,"25"="25-34"
         ,"35"="35-44"
         ,"45"="45-49"
         ,"50"="50-55"
         ,"56"="56+")
}

users$Age.Grouped <- sapply(users$Age, Age.encoding)


ggplot(users, aes(x = Age.Grouped)) + geom_bar(aes(fill = ..count..))


#Distribution of users Occupation



occupation.sorted <- as.data.frame(sort(table(users$Occupation), decreasing = TRUE))
colnames(occupation.sorted) <- c('Occupation', 'Frequency')
occupation.sorted$Occupation <- factor(occupation.sorted$Occupation, levels = rev(unique(as.character(occupation.sorted$Occupation))))


ggplot(occupation.sorted, aes(x = Occupation, y = Frequency)) + geom_col(aes(fill = Frequency)) + ggtitle('Distribution of Users Occupations') + coord_flip()

#In the above plot, it makes sense that graduate/college students are the highest number of raters and farmers are the lowest
#as farmers may hardly watch any movies or be interested in them.


#Which are the most popular genres by gender?
users.movies.ratings <- inner_join(users, movies.ratings, by = 'UserID')

head(users.movies.ratings)
str(users.movies.ratings)

install.packages("modeest")

library(modeest)

gender.popular.genres <- users.movies.ratings %>% select(Gender, Genres) %>% group_by(Gender) %>% do(mode.genres.func1(.))
colnames(gender.popular.genres) <- c('Gender', 'Genres')
ggplot(gender.popular.genres, aes(x = Gender, y = Genres)) + geom_col(fill = 'blue') + ylim(c('Drama', 'Comedy'))


which.max(table(users.movies.ratings$Genres))

users.movies.ratings$Genres[which.max(table(users.movies.ratings$Genres))]

mode.genres.func <- function(x){
  ux <- unique(x$Genres)
  as.data.frame(ux[which.max(table(match(x$Genres,ux)))])
}

mode.genres.func1 <- function(x){
  as.data.frame(names(sort(-table(x$Genres)))[1])
}


class(mode.func(users.movies.ratings))

names(sort(-table(users.movies.ratings[users.movies.ratings$Gender == 'M','Genres'])))[1]

x <- table(users.movies.ratings$Genres)
names(x)[x == max(x)]

#Which are the most popular genres by occupation?

mode.occupation.func <- function(x){
  as.data.frame(names(sort(-table(x$Genres)))[1])
}

occupation.popular.genres <- users.movies.ratings %>% select(Occupation, Genres) %>% group_by(Occupation) %>% do(mode.genres.func1(.))
print(occupation.popular.genres, n = 100)
