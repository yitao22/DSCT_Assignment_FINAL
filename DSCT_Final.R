#REGRESSION AND MODEL EVALUATION ASSIGNMENT #4
#DONE BY: ZHENG YI TAO, VINCENT TAN MIN SHENG, ZHENG MIN, YEAP JUIN AING

library(Amelia)
library(ggplot2)
library(dplyr)
library(pastecs)
library(data.table)
library(splitstackshape)
library(psych)
library(MASS)
library(perturb)
library(car) 
library(bigmemory)
library(biganalytics)
library(ggfortify)

#Read data
#Set working directory
setwd("D:\\DSCT\\DSCT-Regression-Final-master")

#Load csv file
movies <- read.csv('movie_metadata.csv',header = TRUE, sep=",", na.strings = c("", " ", "NA"), stringsAsFactors = FALSE)

# reorder the columns
movies <- movies[, c("director_facebook_likes", 
                     "actor_1_facebook_likes", 
                     "actor_2_facebook_likes", 
                     "actor_3_facebook_likes",
                     "cast_total_facebook_likes",
                     "movie_facebook_likes",
                     "imdb_score",
                     "num_voted_users",
                     "num_user_for_reviews",
                     "num_critic_for_reviews",
                     "title_year",
                     "duration",
                     "movie_title",
                     "country", 
                     "genres", 
                     "color", 
                     "aspect_ratio", 
                     "content_rating", 
                     "language",
                     "facenumber_in_poster",
                     "budget", 
                     "gross")]

#******************************************Question 1********************************************

#Visual of missing records
missmap(movies, main = "Missing values vs observed")

#Looking at gross summary
summary(movies)
#stat.desc(movies$gross) 

#Highest and Lowest Grossing Movie
movies %>%
  dplyr::select(title_year, gross, imdb_score, country, movie_facebook_likes) %>%
  filter(movies$gross == 760505847 | movies$gross == 162)

#Gross over years plot
ggplot(movies, aes(title_year, gross)) +
  geom_bar(stat = "identity") +
  labs(title = "Gross Earning of Movies over the years",x="Years",y="Gross Earnings")

#Gross over years plot for each country
ggplot(movies, aes(title_year, gross)) +
  geom_bar(stat = "identity") +
  labs(title = "Gross Earning of Movies over the years",x="Years",y="Gross Earnings")+
  facet_wrap(~country)


#******************************************Question 2********************************************

#Removing duplicated movies
movies <- movies[!duplicated(movies$movie_title),]

#Remove data noise from movie title and trim leading white spaces
movies$movie_title <- gsub("^\\s+|Â\\s+$", "", movies$movie_title)

###Content Ratings###
#Guide: https://contribute.imdb.com/updates/guide/certificates

#Removing TV shows
movies <- movies %>% filter(!grepl("TV-", content_rating))

#Combine similar ratings, left with G, PG, NC, R and Unrated
movies$content_rating[movies$content_rating %in% "GP"] <- "G"
movies$content_rating[movies$content_rating %in% c("PG-13", "Approved", "Passed")] <- "PG"
movies$content_rating[movies$content_rating %in% "X"] <- "R"
movies$content_rating[movies$content_rating %in% c("NC-17", "M")] <- "NC"
movies$content_rating[movies$content_rating %in% "Not Rated"] <- "Unrated"

###Countries###
#Change noisy data to the right country
movies$country[movies$country %in% c("Official site","New Line")] <- "USA"

#Limit to only country that is USA
movies <- movies %>% filter(country == "USA")

#Convert categorical variables into factors
categoricalVar <- c("country", "color", "content_rating", "language")
movies[,categoricalVar] <- lapply(movies[,categoricalVar], factor)

###Missing Values###

#Check for each variable how many have missing records
sapply(movies, function(x) sum(is.na(x)))

#Replay all NA colors to "Color" since it has already so many (3000+ color vs 140+ BW)
movies["color"][is.na(movies["color"])] <- "Color"

#Changing all language to English, because all these films are from USA, 
#and some of the movies with other languages still have English
movies$language <- "English"

#MICE Operation
#library(mice)
#moviesMice <- mice(movies, m=5, maxit = 1, meth = 'rf', seed = 500)
#moviesNoMiss1 <- complete(moviesMice,1)
#moviesNoMiss5 <- complete(moviesMice,5)

#Remove all rows that contains NA
movies<-na.omit(movies)

#Removing outlier in aspect_ratio
movies <- movies %>% filter(aspect_ratio != 16.00)

#Removing outlier in cast_total_facebook_likes
movies <- movies %>% filter(cast_total_facebook_likes != 656730)

# filter and reorder the columns to only contain columns beneficial to model
movies <- movies[, c("director_facebook_likes", 
                     "actor_1_facebook_likes",
                     "actor_2_facebook_likes", 
                     "actor_3_facebook_likes",
                     "cast_total_facebook_likes",
                     "title_year",
                     "duration",
                     "genres",
                     "color",
                     "aspect_ratio", 
                     "content_rating",
                     "facenumber_in_poster",
                     "budget",
                     "gross")]

#Normalizations
movies$gross <- sapply(movies$gross, function(x) { return(log10(x+1)) })
movies$budget <- sapply(movies$budget, function(x) { return(log10(x+1)) })
movies$cast_total_facebook_likes <- sapply(movies$cast_total_facebook_likes, function(x) { return(log10(x+1)) })
movies$actor_3_facebook_likes <- sapply(movies$actor_3_facebook_likes, function(x) { return(log10(x+1)) })
movies$actor_2_facebook_likes <- sapply(movies$actor_2_facebook_likes, function(x) { return(log10(x+1)) })
movies$actor_1_facebook_likes <- sapply(movies$actor_1_facebook_likes, function(x) { return(log10(x+1)) })
movies$director_facebook_likes <- sapply(movies$director_facebook_likes, function(x) { return(log10(x+1)) })


#******************************************Question 3********************************************
###Genre###
#Removing genre "Short"
movies <- movies %>% filter(!grepl("Short", genres))

#Split all 26 genres into dichotomous variables
moviesGenreDict <- cSplit_e(data = movies, split.col = "genres", sep = "|", type = "character",
                    drop = TRUE, fill = 0)

#Combining and renaming the genres: Reducing all genres to 9 only
moviesGenreDict$Action_Genres <- ifelse(rowMeans(moviesGenreDict[c("genres_Action",
                                                                   "genres_Adventure",
                                                                   "genres_War",
                                                                   "genres_Western")]) > 0, 1, 0)

moviesGenreDict$Drama_Genres <- ifelse(rowMeans(moviesGenreDict[c("genres_Musical",
                                                                   "genres_Drama",
                                                                   "genres_Film-Noir",
                                                                   "genres_Music")]) > 0, 1, 0)

moviesGenreDict$Fantasy_Genres <- ifelse(rowMeans(moviesGenreDict[c("genres_Sci-Fi",
                                                                  "genres_Fantasy")]) > 0, 1, 0)

moviesGenreDict$Thriller_Genres <- ifelse(rowMeans(moviesGenreDict[c("genres_Thriller",
                                                                    "genres_Mystery")]) > 0, 1, 0)

moviesGenreDict$Educational_Genres <- ifelse(rowMeans(moviesGenreDict[c("genres_Biography",
                                                                        "genres_Documentary",
                                                                        "genres_History")]) > 0, 1, 0)


colnames(moviesGenreDict)[20] <- "Animation_Genres"
colnames(moviesGenreDict)[22] <- "Comedy_Genres"
colnames(moviesGenreDict)[23] <- "Crime_Genres"
colnames(moviesGenreDict)[26] <- "Family_Genres"
colnames(moviesGenreDict)[29] <- "Horror_Genres"
colnames(moviesGenreDict)[33] <- "Romance_Genres"
colnames(moviesGenreDict)[35] <- "Sport_Genres"


#Removing the rest of the duplicate genres
moviesGenreDict <- moviesGenreDict[,-grep("genres_", colnames(moviesGenreDict))]
movies <- moviesGenreDict

#******************************************Question 4********************************************
##Need to dichotomous values for aspect_ratio, content_rating

pairs.panels(movies[c("profit", "gross", "aspect_ratio", "title_year", "content_rating")])

plot(movies$aspect_ratio)
#Split all 17 aspect_ratio into dichotomous variables
moviesARDict <- cSplit_e(data = movies, split.col = "aspect_ratio", sep = "|", type = "character",
                            drop = TRUE, fill = 0)

movies <- moviesARDict


#Split all 5 content_rating into dichotomous variables
moviesCRDict <- cSplit_e(data = movies, split.col = "content_rating", sep = "|", type = "character",
                         drop = TRUE, fill = 0)

movies <- moviesCRDict

#Split all 5 content_rating into dichotomous variables
moviesCDict <- cSplit_e(data = movies, split.col = "color", sep = "|", type = "character",
                         drop = TRUE, fill = 0)

movies <- moviesCDict

train_id <- sample(seq_len(nrow(movies)),size = (nrow(movies) * .8))

train <- movies[train_id,]
test <- movies[-train_id,]

model <- glm(gross ~.,family=gaussian(link = "identity"),data=train) 
# review the model
summary(model)

# coefficeint and confidence interver of duration
coef(model)
confint(model)
# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
anova(model, test="Chisq")

#******************************************Question 8********************************************
step <- stepAIC(model, direction="both")
step$anova # display results
summary(step)

print(colldiag(step),fuzz=.3)
model2 <- glm(gross ~ 
                director_facebook_likes +
                cast_total_facebook_likes+ 
                duration+ 
                facenumber_in_poster+
                budget+ 
                Comedy_Genres +
                Sport_Genres+
                Drama_Genres+ 
                Thriller_Genres +
                Educational_Genres+
                aspect_ratio_1.18+
                aspect_ratio_1.33+
                aspect_ratio_1.5+
                aspect_ratio_1.78+
                aspect_ratio_1.85+
                aspect_ratio_2+
                aspect_ratio_2.39+
                aspect_ratio_2.76+
                aspect_ratio_2.24+ 
                content_rating_PG+ 
                content_rating_G, 
              family=gaussian(link = "identity"), data=train)
summary(model2)

vif(model2) # will cause error due to perfect collinearity
alias(model2)

model3 <- glm(gross ~ 
                director_facebook_likes +
                cast_total_facebook_likes+ 
                duration+ 
                facenumber_in_poster+
                budget+ 
                Comedy_Genres +
                Sport_Genres+
                Drama_Genres+ 
                Thriller_Genres +
                Educational_Genres+ 
                content_rating_PG+ 
                content_rating_G, 
              family=gaussian(link = "identity"), data=train)
summary(model3)

vif(model3)

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
anova(model3, test="Chisq")

results <- predict(model3,newdata=test[-10],type='response')

predict(model3,newdata = c(), type = 'response')

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Example data
actual <- test[10]
predicted <- results

# Calculate error
error <- exp(actual)  - exp(predicted)

# Example of invocation of functions
rmse(error$gross)
mae(error$gross)