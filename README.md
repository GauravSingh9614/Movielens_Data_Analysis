Movielens Data Analysis
================
Gaurav Singh
31 March 2018

This is a data analysis project of movielens data. It includes data cleaning and exploration of the Users, Ratings and Movies dataset. In addition to that, I have answered some analytical questions using the dataset which would help us gain some interesting insights into the field of cinematography.

The files contained are 1,000,209 anonymous ratings of approximately 3,900 movies made by 6,040 MovieLens users who joined MovieLens in 2000.

Please have a look at the dataset information document for a better understanding of the dataset and how the data was collected.

Set working directory
---------------------

``` r
library("knitr")
opts_knit$set(root.dir = "D:/Projects/Movielens Analysis/")
```

Loading required packages
-------------------------

``` r
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(lubridate)
library(Amelia)
library(stringr)
library(plotly)
```

Loading the datasets
--------------------

The datasets that we have are seperated by "::". Since R only has the ability to read datasets whose delimiter is a single character, I created a function to convert the seperater to a tab character before reading it in.

``` r
readMulti <- function(dataset, sep){
  ds <- readLines(dataset)
  ds1 <- gsub(sep, "\t", ds)
  ds2 <- paste0(ds1, collapse = "\n")
  ds3 <- fread(ds2, sep = "\t")
  return (ds3)
}
```

The readMulti function takes in the dataset and its seperator as parameters and converts the seperater to a tab character. The paste function is being used to convert the rows of the dataset into a character vector of length 1 since the fread function only takes a single length character vector.

``` r
movies <- readMulti("Dataset\\movies.dat", sep = "::")
ratings <- readMulti("Dataset\\ratings.dat", sep = "::")
users <- readMulti("Dataset\\users.dat", sep = "::")

colnames(movies) <- c("MovieID", "Title", "Genres")
colnames(ratings) <- c("UserID", "MovieID", "Rating", "Timestamp")
colnames(users) <- c("UserID", "Gender", "Age", "Occupation", "Zip-code")
```

The datasets have now been loaded and the column names have been set appropriately.

Data Cleaning
-------------

### Ratings

``` r
str(ratings)
```

    ## Classes 'data.table' and 'data.frame':   1000209 obs. of  4 variables:
    ##  $ UserID   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ MovieID  : int  1193 661 914 3408 2355 1197 1287 2804 594 919 ...
    ##  $ Rating   : int  5 3 3 4 5 3 5 5 4 4 ...
    ##  $ Timestamp: int  978300760 978302109 978301968 978300275 978824291 978302268 978302039 978300719 978302268 978301368 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

The ratings dataset contains 1 million observations and 4 columns. Only the Timestamp column needed to be dealt with. I converted it to a datetime format.

``` r
ratings$Timestamp <- as_datetime(ratings$Timestamp)
str(ratings)
```

    ## Classes 'data.table' and 'data.frame':   1000209 obs. of  4 variables:
    ##  $ UserID   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ MovieID  : int  1193 661 914 3408 2355 1197 1287 2804 594 919 ...
    ##  $ Rating   : int  5 3 3 4 5 3 5 5 4 4 ...
    ##  $ Timestamp: POSIXct, format: "2000-12-31 22:12:40" "2000-12-31 22:35:09" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
any(is.na(ratings))
```

    ## [1] FALSE

The ratings dataset does not contain any missing values.

### Movies

``` r
str(movies)
```

    ## Classes 'data.table' and 'data.frame':   3883 obs. of  3 variables:
    ##  $ MovieID: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Title  : chr  "Toy Story (1995)" "Jumanji (1995)" "Grumpier Old Men (1995)" "Waiting to Exhale (1995)" ...
    ##  $ Genres : chr  "Animation|Children's|Comedy" "Adventure|Children's|Fantasy" "Comedy|Romance" "Comedy|Drama" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

The movies dataset contains 3883 observations and 3 variables.

``` r
head(movies)
```

    ##    MovieID                              Title                       Genres
    ## 1:       1                   Toy Story (1995)  Animation|Children's|Comedy
    ## 2:       2                     Jumanji (1995) Adventure|Children's|Fantasy
    ## 3:       3            Grumpier Old Men (1995)               Comedy|Romance
    ## 4:       4           Waiting to Exhale (1995)                 Comedy|Drama
    ## 5:       5 Father of the Bride Part II (1995)                       Comedy
    ## 6:       6                        Heat (1995)        Action|Crime|Thriller

The titles have the debut year listed in brackets. I extracted this information into a new column called Year as this could be an important variable to gather insights.

``` r
r <- gregexpr("\\(\\d+\\)", movies$Title)
debut.year <- unlist(regmatches(movies$Title, r))
debut.year <- gsub("\\(|\\)", "", debut.year)
debut.year <- as.Date(debut.year, "%Y")
debut.year <- year(debut.year)
movies$Year <- debut.year

head(movies)
```

    ##    MovieID                              Title                       Genres
    ## 1:       1                   Toy Story (1995)  Animation|Children's|Comedy
    ## 2:       2                     Jumanji (1995) Adventure|Children's|Fantasy
    ## 3:       3            Grumpier Old Men (1995)               Comedy|Romance
    ## 4:       4           Waiting to Exhale (1995)                 Comedy|Drama
    ## 5:       5 Father of the Bride Part II (1995)                       Comedy
    ## 6:       6                        Heat (1995)        Action|Crime|Thriller
    ##    Year
    ## 1: 1995
    ## 2: 1995
    ## 3: 1995
    ## 4: 1995
    ## 5: 1995
    ## 6: 1995

Since, the debut year information has already been extracted into a new column, I removed it from the title column.

``` r
movies$Title <- gsub("\\(\\d+\\)", "", movies$Title)
movies$Title <- str_trim(as.character(movies$Title), side = "both")

head(movies)
```

    ##    MovieID                       Title                       Genres Year
    ## 1:       1                   Toy Story  Animation|Children's|Comedy 1995
    ## 2:       2                     Jumanji Adventure|Children's|Fantasy 1995
    ## 3:       3            Grumpier Old Men               Comedy|Romance 1995
    ## 4:       4           Waiting to Exhale                 Comedy|Drama 1995
    ## 5:       5 Father of the Bride Part II                       Comedy 1995
    ## 6:       6                        Heat        Action|Crime|Thriller 1995

``` r
any(is.na(movies))
```

    ## [1] FALSE

The movies dataset does not contain any missing values.

### Users

``` r
str(users)
```

    ## Classes 'data.table' and 'data.frame':   6040 obs. of  5 variables:
    ##  $ UserID    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Gender    : chr  "F" "M" "M" "M" ...
    ##  $ Age       : int  1 56 25 45 25 50 35 25 25 35 ...
    ##  $ Occupation: int  10 16 15 7 20 9 1 12 17 1 ...
    ##  $ Zip-code  : chr  "48067" "70072" "55117" "02460" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

The users dataset contains 6040 observations and 5 variables. The Occupation and Age has been encoded as mentioned in the readme document.

I de-encoded these variables so that the labels would be accurate and make more sense when creating plots in the Data Exploration section.

``` r
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

Age.encoding <- function(x){
  switch(as.character(x), "1"="Under 18"
         ,"18"="18-24"
         ,"25"="25-34"
         ,"35"="35-44"
         ,"45"="45-49"
         ,"50"="50-55"
         ,"56"="56+")
}

users$Occupation <-  sapply(users$Occupation, occupation.encode)
users$Age.Grouped <- sapply(users$Age, Age.encoding)

str(users)
```

    ## Classes 'data.table' and 'data.frame':   6040 obs. of  6 variables:
    ##  $ UserID     : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Gender     : chr  "F" "M" "M" "M" ...
    ##  $ Age        : int  1 56 25 45 25 50 35 25 25 35 ...
    ##  $ Occupation : chr  "K-12student" "self-employed" "scientist" "executive/managerial" ...
    ##  $ Zip-code   : chr  "48067" "70072" "55117" "02460" ...
    ##  $ Age.Grouped: chr  "Under 18" "56+" "25-34" "45-49" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
any(is.na(users))
```

    ## [1] FALSE

The users dataset does not contain any missing values.

Data Exploration
----------------

### How many movies were produced per year?

``` r
movies.per.year <- movies %>% group_by(Year) %>% summarise(count = n()) %>% arrange(desc(Year))

head(movies.per.year)
```

    ## # A tibble: 6 x 2
    ##    Year count
    ##   <dbl> <int>
    ## 1 2000.   156
    ## 2 1999.   283
    ## 3 1998.   337
    ## 4 1997.   315
    ## 5 1996.   345
    ## 6 1995.   342

To get a better understanding of the trend, I created a line graph.

``` r
movies.perYear.plot <- ggplot(movies.per.year, aes(x = Year, y = count)) + geom_line(col = 'blue', linetype = 'solid', size = 1) + theme_bw() + ggtitle('Number of Movies by Year') + ylab('Number of Movies')

print(movies.perYear.plot)
```

![](README_files/figure-markdown_github/unnamed-chunk-17-1.png)

We can see that the demand for movies has been increasing year after year. The sharp drop in the year 2000 is due to the fact that the dataset does not contain information for the entire year 2000.

### What were the most popular movie genres year by year?

``` r
genresByYear <- movies %>% separate_rows(Genres, sep = "\\|") %>% select(MovieID, Year, Genres) %>% group_by(Year, Genres) %>% summarise(count = n()) %>% arrange(desc(Year))

head(genresByYear)
```

    ## # A tibble: 6 x 3
    ## # Groups:   Year [1]
    ##    Year Genres     count
    ##   <dbl> <chr>      <int>
    ## 1 2000. Action        19
    ## 2 2000. Adventure      6
    ## 3 2000. Animation      8
    ## 4 2000. Children's     9
    ## 5 2000. Comedy        69
    ## 6 2000. Crime          8

I created a barplot to get a better picture of the popularity.

``` r
ggplot(genresByYear, aes(x = Year, y = count)) + geom_col(aes(fill = Genres), position = 'dodge') + theme_bw() + ylab('Number of Movies') + ggtitle('Popularity per year by Genre')
```

![](README_files/figure-markdown_github/unnamed-chunk-19-1.png)

Since the plot is too cluttered, I grouped the year column.

``` r
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

ggplot(genresByYear, aes(x = grouped.year, y = count)) + geom_col(aes(fill = Genres), position = 'dodge') + ylab('Number of Movies') + xlab('Year') + ggtitle('Popularity per year by Genre') + theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-20-1.png)

For better readbility, I filtered the genres.

``` r
genresByYear %>% filter(Genres %in% c('Action', 'Drama', 'Comedy', 'Romance')) %>% ggplot(aes(x = grouped.year, y = count)) + geom_col(aes(fill = Genres), position = 'dodge') + ylab('Number of Movies') + xlab('Year') + ggtitle('Popularity per year by Genre') + theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-21-1.png)

This barplot is pretty self explanatory. We can see that the popularity of all the genres have been improving year after year. We can see that the maximum increase in the popularity has been for the drama genre. To analyse the trend even more clearly, I created a line graph.

``` r
genresByYear %>% filter(Genres %in% c('Action', 'Drama', 'Comedy', 'Romance')) %>% ggplot(aes(x = Year, y = count)) + geom_line(aes(col = Genres), size = 1) + ylab('Number of Movies') + xlab('Year') + ggtitle('Popularity per year by Genre') + theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)

It can be clearly seen that the demand for these genres exponentially increased in the 1990s.

### Which are the best movies of all time based on ratings?

The movies and ratings dataset are joined based on the MovieID.

``` r
movies.ratings <- inner_join(ratings, movies, by = 'MovieID')

str(movies.ratings)
```

    ## 'data.frame':    1000209 obs. of  7 variables:
    ##  $ UserID   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ MovieID  : int  1193 661 914 3408 2355 1197 1287 2804 594 919 ...
    ##  $ Rating   : int  5 3 3 4 5 3 5 5 4 4 ...
    ##  $ Timestamp: POSIXct, format: "2000-12-31 22:12:40" "2000-12-31 22:35:09" ...
    ##  $ Title    : chr  "One Flew Over the Cuckoo's Nest" "James and the Giant Peach" "My Fair Lady" "Erin Brockovich" ...
    ##  $ Genres   : chr  "Drama" "Animation|Children's|Musical" "Musical|Romance" "Drama" ...
    ##  $ Year     : num  1975 1996 1964 2000 1998 ...

For beter clarity, I renamed the Timestamp column to ratings.date as this attribute refers to when the movies were rated.

``` r
movies.ratings <- rename(movies.ratings, ratings.date = Timestamp)

head(movies.ratings)
```

    ##   UserID MovieID Rating        ratings.date
    ## 1      1    1193      5 2000-12-31 22:12:40
    ## 2      1     661      3 2000-12-31 22:35:09
    ## 3      1     914      3 2000-12-31 22:32:48
    ## 4      1    3408      4 2000-12-31 22:04:35
    ## 5      1    2355      5 2001-01-06 23:38:11
    ## 6      1    1197      3 2000-12-31 22:37:48
    ##                             Title                          Genres Year
    ## 1 One Flew Over the Cuckoo's Nest                           Drama 1975
    ## 2       James and the Giant Peach    Animation|Children's|Musical 1996
    ## 3                    My Fair Lady                 Musical|Romance 1964
    ## 4                 Erin Brockovich                           Drama 2000
    ## 5                   Bug's Life, A     Animation|Children's|Comedy 1998
    ## 6             Princess Bride, The Action|Adventure|Comedy|Romance 1987

Now to retrieve the average ratings for the movies in descending order.

``` r
best.ratings <- movies.ratings %>% select(Rating, Title, UserID) %>% group_by(Title) %>% summarise(avg_rating = mean(Rating), number_of_ratings = n()) %>% arrange(desc(avg_rating))

head(best.ratings)
```

    ## # A tibble: 6 x 3
    ##   Title                       avg_rating number_of_ratings
    ##   <chr>                            <dbl>             <int>
    ## 1 Baby, The                           5.                 1
    ## 2 Bittersweet Motel                   5.                 1
    ## 3 Follow the Bitch                    5.                 1
    ## 4 Gate of Heavenly Peace, The         5.                 3
    ## 5 Lured                               5.                 1
    ## 6 One Little Indian                   5.                 1

I then proceeded to visualize the above data.

``` r
ggplot(best.ratings[1:10,], aes(x = Title, y = avg_rating)) + geom_col(fill = "blue") + coord_flip() + ylab('Movie Title') + xlab('Average Rating') + ggtitle('Best movies by ratings') + theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-26-1.png)

The movies shown above all have an average rating of 5 stars. This information is biased as these movies have very few ratings (&lt;5). To get a more accurate plot, I filtered it to only include movies which have atleast 100 ratings.

``` r
best.ratings.filtered <- best.ratings %>% filter(number_of_ratings > 100) %>% arrange(desc(avg_rating))

best.ratings.filtered$Title<- factor(best.ratings.filtered$Title, levels = rev(unique(as.character(best.ratings.filtered$Title))))

ggplot(best.ratings.filtered[1:10,], aes(x = Title, y = avg_rating)) + geom_col(aes(fill = number_of_ratings)) + ggtitle("Top 10 movies by ratings") + coord_flip() + ylab('Average Rating') + theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-27-1.png)

The above plot displays the top 10 movies by ratings in an ordered fashion. This can be confirmed by looking at the top 10 rows of best.ratings.filtered.

``` r
head(best.ratings.filtered, 10)
```

    ## # A tibble: 10 x 3
    ##    Title                                       avg_rating number_of_ratin~
    ##    <fct>                                            <dbl>            <int>
    ##  1 Seven Samurai (The Magnificent Seven) (Shi~       4.56              628
    ##  2 Shawshank Redemption, The                         4.55             2227
    ##  3 Godfather, The                                    4.52             2223
    ##  4 Close Shave, A                                    4.52              657
    ##  5 Usual Suspects, The                               4.52             1783
    ##  6 Schindler's List                                  4.51             2304
    ##  7 Wrong Trousers, The                               4.51              882
    ##  8 Sunset Blvd. (a.k.a. Sunset Boulevard)            4.49              470
    ##  9 Raiders of the Lost Ark                           4.48             2514
    ## 10 Rear Window                                       4.48             1050

### Distribution of users age

``` r
ggplot(users, aes(x = Age.Grouped)) + geom_bar(aes(fill = ..count..)) + ggtitle('Distribution of users ages') + theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-29-1.png)

It can be seen that most of the users in the dataset are between 25-34 years old. I then wanted to have a look at the distribution of occupations for these users.

### Distribution of users occupations

``` r
occupation.sorted <- as.data.frame(sort(table(users$Occupation), decreasing = TRUE))
colnames(occupation.sorted) <- c('Occupation', 'Frequency')
occupation.sorted$Occupation <- factor(occupation.sorted$Occupation, levels = rev(unique(as.character(occupation.sorted$Occupation))))


ggplot(occupation.sorted, aes(x = Occupation, y = Frequency)) + geom_col(aes(fill = Frequency)) + ggtitle('Distribution of Users Occupations') + coord_flip() + theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-30-1.png)

College/gradstudent make the majority of the users while farmers make the minority. This makes sense as farmers may hardly watch any movies due to inaccessibility or be interested in them.

### Which is the most popular Genre by Gender?

I first joined the movies, ratings and users datasets by the UserID.

``` r
users.movies.ratings <- inner_join(users, movies.ratings, by = 'UserID')

str(users.movies.ratings)
```

    ## 'data.frame':    1000209 obs. of  12 variables:
    ##  $ UserID      : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Gender      : chr  "F" "F" "F" "F" ...
    ##  $ Age         : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Occupation  : chr  "K-12student" "K-12student" "K-12student" "K-12student" ...
    ##  $ Zip-code    : chr  "48067" "48067" "48067" "48067" ...
    ##  $ Age.Grouped : chr  "Under 18" "Under 18" "Under 18" "Under 18" ...
    ##  $ MovieID     : int  1193 661 914 3408 2355 1197 1287 2804 594 919 ...
    ##  $ Rating      : int  5 3 3 4 5 3 5 5 4 4 ...
    ##  $ ratings.date: POSIXct, format: "2000-12-31 22:12:40" "2000-12-31 22:35:09" ...
    ##  $ Title       : chr  "One Flew Over the Cuckoo's Nest" "James and the Giant Peach" "My Fair Lady" "Erin Brockovich" ...
    ##  $ Genres      : chr  "Drama" "Animation|Children's|Musical" "Musical|Romance" "Drama" ...
    ##  $ Year        : num  1975 1996 1964 2000 1998 ...

As expected we have 1 million rows and 12 variables.

Proceeding further, I created my own function(mode.genres.func) which I then used to summarise the grouped data by finding the mode.

``` r
mode.genres.func <- function(x){
  as.data.frame(names(sort(-table(x$Genres)))[1])
}

gender.popular.genres <- users.movies.ratings %>% select(Gender, Genres) %>% group_by(Gender) %>% do(mode.genres.func(.))
colnames(gender.popular.genres) <- c('Gender', 'Genres')

print(gender.popular.genres)
```

    ## # A tibble: 2 x 2
    ## # Groups:   Gender [2]
    ##   Gender Genres
    ##   <chr>  <chr> 
    ## 1 F      Drama 
    ## 2 M      Comedy

We can see that *Drama* is the most popular genre for females while *Comedy* is the most popular genre for males.

Conclusion
----------

This data analysis project helped us gain some interesting insights into the history of cinematography. The insights gained can be used by movie makers to help decide the type of movies to produce in the future.
