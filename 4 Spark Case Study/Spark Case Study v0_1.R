# For Spark 2.2.0 (You can also do this thru .bash_profile) ####
Sys.setenv(SPARK_HOME = '/usr/lib/spark-2.2.0-bin-hadoop2.6')
Sys.setenv(JAVA_HOME = '/usr/java/jdk1.8.0_144')

# Sys.setenv('SPARKR_SUBMIT_ARGS'='"--conf" "spark.debug.maxToStringFields=100"')
library(sparklyr)
library(ggplot2)
library(plotly)
library(SparkR)
rm(list = ls())

# initiating the spark session
sc = sparkR.session(master = 'local')

# # Create a Spark DataFrame and examine structure
movies_df <-
  SparkR::read.df(
    "hdfs://192.168.56.101:8020/user/cloudera/amazon_review/Movies_and_TV_5.json",
    header = T,
    "json"
  )
books_df <-
  SparkR::read.df(
    "hdfs://192.168.56.101:8020/user/cloudera/amazon_review/Kindle_Store_5.json",
    header = T,
    "json"
  )
cds_df <-
  SparkR::read.df(
    "hdfs://192.168.56.101:8020/user/cloudera/amazon_review/CDs_and_Vinyl_5.json",
    header = T,
    "json"
  )

# Note that there are character type, numeric type and array type
str(books_df)
str(movies_df)
str(cds_df)

# examine the size
reviewCountBooks <- dim(books_df) # 982619 * 9
reviewCountMovies <- dim(movies_df) # 1697533 * 9
reviewCountCDs <- dim(cds_df) # 1097592 * 9

# look at the first few rows
head(books_df)
head(movies_df)
head(cds_df)
## All datasets are properly imported and contains reviews information

# subset only the columns Reviews
columns(books_df)
columns(movies_df)
columns(cds_df)
#### All 3 dataframes have same columns

######################## Kindle dataset EDA ##################################
# removing missing values: Use filter ####
books_df = filter(books_df, isNotNull(books_df$helpful))
books_df = filter(books_df, isNotNull(books_df$overall))
books_df = filter(books_df, isNotNull(books_df$reviewText))


# checking the average rating given by users who give maximum reviews ########
reviewsByUser <- books_df %>% groupBy(books_df$reviewerID) %>%
  summarize(
    countOfReview = count(books_df$unixReviewTime),
    avgRating = mean(books_df$overall)
  )
dim(reviewsByUser) # Total of 68223 users

reviewsByUser %>% arrange(desc(reviewsByUser$countOfReview)) %>% head(10)
#### 9 out of top 10 Users who have given most ratings have average rating of
#### more than 4. Only 1 user among these top 10 reviewers has average rating
#### of 2.49

histStatsUser <-
  histogram(reviewsByUser, reviewsByUser$countOfReview, nbins = 100)
ggplotly(
  ggplot(histStatsUser, aes(
    x = centroids, y = counts)) +
    geom_line() +
    xlab("Count of Reviews") + ylab("Frequency")
)
#### There are very high number of users who have given reviews less than 10

# checking the count of reviews given for each ratings #######################
bookreviewsByRating <- books_df %>% groupBy(books_df$overall) %>%
  summarize(countOfReview = count(books_df$unixReviewTime)) %>% collect()

bookreviewsByRating ##%>% arrange(desc(reviewsByRating$countOfReview)) %>% head()

ggplot(bookreviewsByRating,aes(x= overall, y = countOfReview)) + geom_line() +
  xlab("Overall Rating") + ylab("Count of Reviews")
#### Count of reviews gets increased as the rating of the products gets
#### increased

## Handling Review Text #######
books_df$rev_length <- length(books_df$reviewText)
histStatsBookRevLength <-
  histogram(books_df, books_df$rev_length, nbins = 25)
histStatsBookRevLength %>% head()

ggplotly(
  ggplot(histStatsBookRevLength, aes(
    x = centroids, y = counts)) +
    geom_line() +
    xlab("Review Lengths") + ylab("Frequency")
)

#### Maximum reviews (80%) have length within 460 characters
#### there are around 9 lakh (93%) reviews within 1380 characters

## Handling helpful array ####################################################
# confirming that there are no missing values
# nrow(where(books_df, isNull(books_df$helpful))) # There are no rows where helpful column is Null

books_df_helpful <- books_df %>%
  select(books_df$helpful) %>%
  collect()

# books_df_helpful[[1]] %>% as.data.frame() %>% View()
# 
# head(select(books_df, books_df$helpful))
helpful <-
  unlist(books_df_helpful,  recursive = FALSE) %>% as.data.frame()
helpful %>% View()


######################## Movies dataset EDA ##################################
# removing missing values: Use filter ####
movies_df = filter(movies_df, isNotNull(movies_df$helpful))
movies_df = filter(movies_df, isNotNull(movies_df$overall))
movies_df = filter(movies_df, isNotNull(movies_df$reviewText))


# checking the average rating given by users who give maximum reviews ########
moviesReviewsByUser <- movies_df %>% groupBy(movies_df$reviewerID) %>%
  summarize(
    countOfReview = count(movies_df$unixReviewTime),
    avgRating = mean(movies_df$overall)
  )
dim(moviesReviewsByUser) # Total of 123960 users

moviesReviewsByUser %>% arrange(desc(moviesReviewsByUser$countOfReview)) %>% head(10)

ggplot(moviesReviewsByUser,aes(x= overall, y = countOfReview)) + geom_line() +
  xlab("Overall Rating") + ylab("Count of Reviews")

#### Among top 10 Users most have have average rating of 3 and 4.
#### Only 1 user among these top 10 reviewers has average rating of 2.86

histStatsUser <-
  histogram(reviewsByUser, reviewsByUser$countOfReview, nbins = 5)
ggplotly(
  ggplot(histStatsUser, aes(
    x = centroids, y = counts)) +
    geom_line() +
    xlab("Count of Reviews") + ylab("Frequency")
)

# checking the count of reviews given for each ratings #######################
reviewsByRating <- movies_df %>% groupBy(movies_df$overall) %>%
  summarize(countOfReview = count(movies_df$unixReviewTime))

reviewsByRating %>% arrange(desc(reviewsByRating$countOfReview)) %>% head()
#### Count of reviews are decreased as the rating of the products gets
#### decreased

## Handling Review Text #######
movies_df$rev_length <- length(movies_df$reviewText)
histStatsMovieRevLength <-
  histogram(movies_df, movies_df$rev_length, nbins = 25)
histStatsMovieRevLength %>% head()
#### Maximum reviews (77%) have length within 655 characters
#### there are around 9 lakh (91.96%) reviews within 1965 characters

ggplotly(
  ggplot(
    histStatsMovieRevLength,
    aes(x = centroids, y = counts)) +
    geom_line() +
    xlab("Review Lengths") + ylab("Frequency")
)


## Handling Time #############################################################
moviesWithTimestamp <-
  withColumn(movies_df, "ts", cast(movies_df$unixReviewTime, "timestamp"))

moviesWithTimestamp %>% select(moviesWithTimestamp$unixReviewTime,
                               moviesWithTimestamp$ts,
                               movies_df$reviewTime) %>% head(10)

year(moviesWithTimestamp$ts) %>% head()


## Handling Timestamp column in dataset #####################################
sparkR:from_unixtime(books_df$)



## Handling helpful array ####################################################
# confirming that there are no missing values
nrow(where(movies_df, isNull(movies_df$helpful))) # There are no rows where helpful column is Null

movies_df_helpful <- movies_df %>%
  select(movies_df$helpful) %>%
  collect()

movies_df_helpful[[1]] %>% as.data.frame() %>% View()

head(select(movies_df, movies_df$helpful))
helpful <-
  unlist(movies_df_helpful,  recursive = TRUE) %>% as.data.frame()
helpful %>% as.data.frame %>% View()






######################## CDs and Vinyl dataset EDA ##################################
# removing missing values: Use filter ####
cds_df = filter(cds_df, isNotNull(cds_df$helpful))
cds_df = filter(cds_df, isNotNull(cds_df$overall))
cds_df = filter(cds_df, isNotNull(cds_df$reviewText))


# checking the average rating given by users who give maximum reviews ########
cdsReviewsByUser <- cds_df %>% groupBy(cds_df$reviewerID) %>%
  summarize(
    countOfReview = count(cds_df$unixReviewTime),
    avgRating = mean(cds_df$overall)
  )
dim(cdsReviewsByUser) # Total of 75258  users

cdsReviewsByUser %>% arrange(desc(cdsReviewsByUser$countOfReview)) %>% head(10)
ggplot(cdsReviewsByUser,aes(x= overall, y = countOfReview)) + geom_line() +
  xlab("Overall Rating") + ylab("Count of Reviews")

#### Among top 10 Users most have have average rating of 3 and 4.

histStatsUser <-
  histogram(reviewsByUser, reviewsByUser$countOfReview, nbins = 5)
ggplotly(
  ggplot(histStatsUser, aes(
    x = centroids, y = counts, col = counts
  )) +
    geom_bar(stat = "identity") +
    xlab("Count of Reviews") + ylab("Frequency")
)

# checking the count of reviews given for each ratings #######################
reviewsByRating <- cds_df %>% groupBy(cds_df$overall) %>%
  summarize(countOfReview = count(cds_df$unixReviewTime))

reviewsByRating %>% arrange(desc(reviewsByRating$countOfReview)) %>% head()
#### Count of reviews are decreased as the rating of the products gets
#### decreased

## Handling Review Text #######
cds_df$rev_length <- length(cds_df$reviewText)
histStatsCDRevLength <-
  histogram(cds_df, cds_df$rev_length, nbins = 25)
histStatsCDRevLength %>% head()
#### Maximum reviews (75.5%) have length within 654 characters
#### there are around 9 lakh (92.89%) reviews within 1962 characters

ggplotly(
  ggplot(histStatsCDRevLength, aes(
    x = centroids, y = counts, col = counts
  )) +
    geom_bar(stat = "identity") +
    xlab("Review Lengths") + ylab("Frequency")
)


## Handling Time #############################################################
booksWithTimestamp <-
  withColumn(cds_df, "ts", cast(cds_df$unixReviewTime, "timestamp"))

booksWithTimestamp %>% select(booksWithTimestamp$unixReviewTime,
                              booksWithTimestamp$ts,
                              cds_df$reviewTime) %>% head(10)

year(booksWithTimestamp$ts) %>% head()

## Handling helpful array ####################################################
# confirming that there are no missing values
nrow(where(cds_df, isNull(cds_df$helpful))) # There are no rows where helpful column is Null

cds_df_helpful <- cds_df %>%
  select(cds_df$helpful) %>%
  collect()

cds_df_helpful[[1]] %>% as.data.frame() %>% View()

head(select(cds_df, cds_df$helpful))
helpful <-
  unlist(cds_df_helpful,  recursive = TRUE) %>% as.data.frame()
helpful %>% as.data.frame %>% View()



####1. Which product category has a larger market size? ###############
# Taking number of reviews as a proxy for larger market size
# Checking number of reviews in each of three categories
reviewCountBooks[1]  #  982619
reviewCountMovies[1] # 1697533
reviewCountCDs[1]    # 1097592

#Since reviewCountMovies[1] is largest, so using it for finding ratio between the number of reviews
reviewCountBooks[1] / reviewCountMovies[1]   # .5788512
reviewCountMovies[1] / reviewCountMovies[1]  # 1
reviewCountCDs[1] / reviewCountMovies[1]     # .6465807

#### Therefore Movies has the largest market size.

####2. Which product category is likely to be purchased heavily? ###############
books_df_helpful <-
  select(books_df, books_df$helpful) %>% collect()

books_df_helpful %>% as.data.frame() %>% head(50)


####3. Which product category is likely to make the customers happy after the purchase? ###############
# Using overall ratin 5 and long reviews as proxy for checking the count of
# customers which are made happy by the product segment
avgRevLengthBooks <-
  books_df %>% summarize(avgLength = mean(books_df$rev_length)) %>% collect()
avgRevLengthBooks # 603.94
booksAggregatedRating_Len <- books_df %>%
  filter(books_df$overall == 5 &
           books_df$rev_length > as.integer(avgRevLengthBooks)) %>%
  summarize(numberofRev = count(books_df$overall)) %>% collect()
booksAggregatedRating_Len
# 154,738 reviews which are longer than average length and have 5 rating

avgRevLengthMovies <-
  movies_df %>% summarize(avgLength = mean(movies_df$rev_length)) %>% collect()
avgRevLengthMovies # 922.91
moviesAggregatedRating_Len <- movies_df %>%
  filter(movies_df$overall == 5 &
           movies_df$rev_length > as.integer(avgRevLengthMovies)) %>%
  summarize(numberofRev = count(movies_df$overall)) %>% collect()
moviesAggregatedRating_Len
# 230,481 reviews which are longer than average length and have 5 rating

avgRevLengthCDs <-
  cds_df %>% summarize(avgLength = mean(cds_df$rev_length)) %>% collect()
avgRevLengthCDs  # 992.59
cdsAggregatedRating_Len <- cds_df %>%
  filter(cds_df$overall == 5 &
           cds_df$rev_length > as.integer(avgRevLengthCDs)) %>%
  summarize(numberofRev = count(cds_df$overall)) %>% collect()
cdsAggregatedRating_Len
# 209,963 reviews which are longer than average length and have 5 rating

#### Movies have maximum count of customers who have given reviews longer than
#### average review and gave 5 as an overall rating.
