#dataset from https://www.kaggle.com/CooperUnion/anime-recommendations-database
#I save all my images as 1280 x 720, so the text lines up at that sized image.
#I don't know how to do this yet with relative coordinates to make things line up automatically

library(needs)

needs(stringr, plyr, ggplot2, grid, caTools)
library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(caTools)

anime <- read.csv("anime.csv")  
rating <- read.csv("rating.csv") 

##########################################################
# 01 # PREPROCESSING DATA - "Fix" current data 
##########################################################
#no "blank" values for genre and type
anime$genre = as.character(anime$genre)
anime$genre[anime$genre  == ""] <- "No Genre"
anime$type = as.character(anime$type)
anime$type[anime$type == ""] <- "No Type"
anime$genre = as.factor(anime$genre)
anime$type = as.factor(anime$type)

anime$anime_id = factor(anime$anime_id)
anime$episodes = as.integer(anime$episodes)
rating$user_id = factor(rating$user_id)
rating$anime_id = factor(rating$anime_id)

# Rating: change -1 to NA for watched but not rated in rating column
rating[rating$rating == -1, "rating"] <- NA 

##########################################################
# Delve into the Genre column
# Will make an expanded anime dataframe wherein instead of 1 genre item with many genres, 
# there will be a duplicate row for each genre beyond the first
# Also, add a column to anime dataset for number of genres
##########################################################

totalAnimeID = vector('numeric')
totalGenres = vector('character')
totalRatings = vector('numeric')
totalTypes = vector('character')
totalEpisodes = vector('numeric')
totalMembers = vector('numeric')
totalnumCat = vector('numeric')
anime$numCat = NA_integer_

for (x in 1:length(anime$genre)) {
  anime$numCat[x] <- length((strsplit(as.character(anime[x,"genre"]), ", ")[[1]]))
  totalAnimeID <- append(totalAnimeID, rep(anime[x,'anime_id'],anime$numCat[x]))
  totalGenres <-  append(totalGenres, (strsplit(as.character(anime[x,"genre"]), ", ")[[1]]))
  totalRatings <- append(totalRatings, rep(anime[x,'rating'],anime$numCat[x]))
  totalTypes <- append(totalTypes, rep(as.character(anime[x,'type']),anime$numCat[x]))
  totalEpisodes <- append(totalEpisodes, rep(anime[x,'episodes'],anime$numCat[x]))
  totalMembers <- append(totalMembers, rep(anime[x,'members'],anime$numCat[x]))
  totalnumCat <- append(totalnumCat, rep(anime[x,'numCat'],anime$numCat[x]))
}

animeExpanded <- data.frame(totalAnimeID, totalGenres,totalRatings, 
                            totalTypes, totalEpisodes, totalMembers, totalnumCat)



##########################################################
# 02 #Look at some data distributions of anime dataset
##########################################################
# Default graphics settings
th <-   theme(axis.title.x = element_text(color="DarkGreen", size=30),
              axis.title.y = element_text(color="Red", size=30),
              
              axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              
              legend.title = element_text(size=20),
              legend.text = element_text(size=20),
              
              legend.position = c(1,1),  
              legend.justification = c(1,1),
              
              plot.title = element_text(color="DarkBlue",
                                        size=40,
                                        family = "Courier")) 
plot = ggplot(data = anime)


# A # Genre Distributions using AnimeExpanded.  This was the motivation behind animeExpanded
GenreDist2 <- count(animeExpanded$totalGenres)
GenreDist2$x <- factor(GenreDist2$x, levels = GenreDist2$x[order(-GenreDist2$freq)]) #ordering genres by occurence

plot_A <- ggplot(data = GenreDist2) + 
  geom_col(aes(x = x, y=freq)) +
  ggtitle("Anime Genre Distribution") +
  th +
  theme(axis.text.x = element_text(size=20, angle = 90, hjust = 1, vjust = 0.5)) + 
  xlab("Genres") +
  ylab("Number of Animes")
plot_A

# B # Ratings Distributions using anime
plot_B = plot + 
  geom_histogram(binwidth = 0.25, aes(x=rating, fill=..count..), 
                               na.rm = TRUE, color = "black") +  
  ggtitle("Anime Ratings Distribution") + 
  th + 
  scale_x_continuous(breaks = c(4,5,6,7,8)) +
  scale_fill_gradient("Count", low = "green", high = "red", guide = FALSE) +
  xlab("Rating") +
  ylab("Number of Animes") +
  geom_vline(xintercept = median(anime$rating, na.rm = TRUE), color = "black", linetype = "longdash") +
  annotate("text", label = "Median = 6.57", color = "black", size = 8, x= 6.45, y = 250, angle = 90)
plot_B

# C # Genre Distributions using anime
plot_C = plot + geom_bar(aes(x=type), na.rm = TRUE, color = "black") +  
  ggtitle("Anime Type Distribution") + 
  th + 
  xlab("Type of Anime") +
  ylab("Number of Animes") +
  theme(legend.position = c(0.1,1),  
        legend.justification = c(1,1))
plot_C


# D # Gives a plot of the distribution of episode counts.  
#Movies (episodes = 1 are kept in dataset)
#To generate labels for the X-axis
xmin <- 0
xmax <- 200
numbins <- 10
width = xmax/numbins
d <- vector('character')
e <- vector('numeric')
for (i in 0:(numbins-1)) {
  f <- xmin + i*width + 1
  g <- xmin + width*(i+1)
  d <- append(d, paste(as.character(f),"-",as.character(g)))
  e <- append(e,f)
}                
#finally, make the plot
plot_D = plot + geom_histogram(binwidth  = width, aes(x=episodes), na.rm = TRUE, color = "black") +  
  ggtitle("Anime Episode Count Distribution") + 
  th +
  scale_x_continuous(breaks = e, labels = d) +
  theme(axis.text.x = element_text(size=20, angle = 90, hjust = 1, vjust = 0.5)) + 
  xlab("Number of Episodes") +
  ylab("Number of Animes") 
plot_D

# E # Gives a plot of the distribution of members.  Split the plot because of the sheer scale
plot_E1 = plot +
  geom_histogram( binwidth = 100, aes(x=members), na.rm = TRUE, color = "black") +
  ggtitle("Anime Member Count Distribution") +
  th +
  xlim(1,5000) +
  coord_cartesian(ylim=c(0, 500)) + 
  annotate("text", label = "count = ~2100", color = "green", size = 6, x= 100, y = 300, angle = 90) +
  annotate("text", label = "count = ~800", color = "green", size = 6, x= 200, y = 200, angle = 90) +
  annotate("text", label = "Far left side of the distribution: \n 5-5000 members", 
           color = "blue", size = 12, x= 3000, y = 400) +
  theme(axis.title.y = element_text(color="Red", size=20),
        axis.text.y = element_text(size=15, angle = 45))
plot_E1

plot_E2 = plot +
  geom_histogram( binwidth = 25000, aes(x=members), na.rm = TRUE, color = "black") +
  th +
  xlim(114000,1014000) +
  coord_cartesian(ylim=c(0, 75)) +
  annotate("text", label = "Far right side of the distribution: \n 11400-1014000 members", 
           color = "blue", size = 12, x= 750000, y = 60) +
  theme(axis.title.y = element_text(color="Red", size=20),
        axis.text.y = element_text(size=15, angle = 45))
plot_E2

grid.newpage()
grid.draw(rbind(ggplotGrob(plot_E1),ggplotGrob(plot_E2),size = "last"))


# F # Number of categories Distribution
plot_F = plot +
  geom_histogram(bins = 13, aes(x=numCat, fill =..count..), na.rm = TRUE, color = "black") +
  scale_x_continuous(breaks = c(1:13)) +
  theme(axis.text.x = element_text(size=20, angle = 90, hjust = 1, vjust = 0.5)) + 
  coord_cartesian(xlim=c(0, 14)) +
  th +
  ggtitle("Distribution of the number of Genres \n assigned to an anime") +
  xlab("Number of Genres Assigned") +
  ylab("Number of Animes")
plot_F


##########################################################
# 03 #Look at some correlations and finer points of the anime dataset graphically
##########################################################

# Corr 1 # Ratings distribution by Anime Genre (the other point of AnimeExpanded)
medVal = ddply(.data = animeExpanded, .variables = .(totalGenres), .fun = function(d) {data.frame(m=mean(d$totalRatings, na.rm = TRUE))})

pl <- ggplot(data = animeExpanded)
plot_Corr1 <- pl + th +
  geom_density(aes(x=totalRatings, fill = totalGenres), 
               na.rm = TRUE, color = "black",
               show.legend = FALSE) + 
  scale_x_continuous(breaks = c(2,4,6,8)) +
  facet_wrap(~totalGenres, ncol =11) +
  geom_vline(data=medVal, aes(xintercept=m), linetype = "longdash") +
  geom_label(data = medVal, 
             aes(fill = factor(m), 
                 label = paste("mean=",as.character(format(round(m,2), nsmall=2)))), 
             size = 4, x= 6, y = 0.7, show.legend = F) +
  xlab("Ratings") +
  ylab("Density") +
  ggtitle("Distribution Ratings per Genre \nPlot fill by Genre, Label fill by Average Rating")
plot_Corr1

# Corr2 # Ratings distribution by Anime Type
animetmp <- anime
animetmp <- animetmp[!animetmp$type == "No Type",] #Ignore Animes without types
medVal2 = ddply(.data = animetmp, .variables = .(type), .fun = function(d) {data.frame(m=mean(d$rating, na.rm = TRUE))})

pl <- ggplot(data = animetmp)
plot_Corr2 <- pl + th +
  geom_density(aes(x=rating, fill = type), 
               na.rm = TRUE, color = "black",
               show.legend = FALSE) + 
  scale_x_continuous(breaks = c(2,4,6,8)) +
  facet_wrap(~type, ncol =3) +
  geom_vline(data=medVal2, aes(xintercept=m), linetype = "longdash") +
  geom_label(data = medVal2, 
             aes(fill = factor(m), 
                 label = paste("mean=",as.character(format(round(m,2), nsmall=2)))), 
             size = 4, x= 3, y = 0.5, show.legend = F) +
  xlab("Ratings") +
  ylab("Density") +
  ggtitle("Distribution of Ratings by Type \nPlot fill is by type, Label fill is by Average Rating")
plot_Corr2

# Corr3 # ratings episodes type members
pl2 <- ggplot(data = anime)
plot_Corr3 <- pl2 + th +
  geom_jitter(aes(y=episodes, x = rating, color = type, size = members), 
              na.rm = TRUE, alpha = 0.33,
              show.legend = T) +
  ylab("Episodes Count") +
  xlab("Ratings") +
  ggtitle("Ratings Distribution by Episode Count") +
  theme(              legend.title = element_text(size=10),
                      legend.text = element_text(size=10)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 9, linetype = "longdash") +
  annotate("text", label = "Rating = 9.0", color = "black", size = 8, x= 8.85, y = 50, angle = 90) +
  geom_vline(xintercept = 7.5, linetype = "longdash") +
  guides(color=guide_legend(title="Type")) +
  guides(size=guide_legend(title="Members"))
plot_Corr3


# Corr4 # ratings episodes numberGenres members
plot_Corr4 <- pl2 + th +
  geom_jitter(aes(y=episodes, x = rating, color = numCat, size = members), 
              na.rm = TRUE, alpha = 0.5,
              show.legend = T) +
  ylab("Episodes Count") +
  xlab("Ratings") +
  ggtitle("Ratings Distribution by Episode Count") +
  theme(              legend.title = element_text(size=10),
                      legend.text = element_text(size=10)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 9, linetype = "longdash") +
  annotate("text", label = "Rating = 9.0", color = "black", size = 8, x= 8.85, y = 50, angle = 90) +
  geom_vline(xintercept = 7.5, linetype = "longdash") +
  scale_color_gradient(low = "green", high = "red") +
  guides(color=guide_legend(title="Genres")) +
  guides(size=guide_legend(title="Members"))
plot_Corr4

##########################################################
# 05 #Fit multiple linear regression models to numerically examine correlations of the anime dataset
##########################################################
#Will use anime dataset, its pretty cool with animeExpanded but too many genres
###USING ANIME
###DONT FORGET TO REMOVE THINGS WITH EPISODE OF 1 (movies) 
anEx2 <- anime[!anime$episodes == 1, ]
anEx2$anime_id <- NULL #remove not needed columns for model
anEx2$genre <- NULL
anEx2$name <- NULL
anEx2$genreCat <- NULL
anEx2$genreNum <- NULL
anEx2 <- anEx2[complete.cases(anEx2),]

set.seed(123)
split = sample.split (anEx2$rating, SplitRatio = 0.8)   
training_set = subset(anEx2, split == TRUE)
testing_set = subset(anEx2, split == FALSE)

regressor = lm(formula = rating~., 
               data = training_set) 
summary(regressor)

#predict the test set results
y_pred = predict(object = regressor, newdata = testing_set)
ts2 <- testing_set
ts2$predicted <- y_pred
ts3 <- ts2[ts2$rating >= 6,]
lm(ts2$predicted ~ ts2$rating) #b = 4.636, m = 0.305
lm(ts3$predicted ~ ts3$rating) #b = 4.048, m = 0.3872

#plot actual and predicted ratings
plo = ggplot(data = ts2)
plot_R02 = plo + geom_point(aes(x = rating, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "longdash") +
  geom_abline(intercept = 4.636, slope = 0.305, color = "red", lwd = 2) +
  coord_cartesian(xlim=c(3,10), ylim=c(3,10)) +
  geom_abline(intercept = 4.048, slope = 0.3872, color = "blue", lwd = 2) +
  th +
  ggtitle("Predicted vs. Actual Ratings using a linear regression model") +
  annotate("text", label = "Black line is perfect fit, red is fit to all predicted data \n blue line is fit to predicted data only for rating greater than 6\n",
           size = 6, x= 6.45, y = 3, color = "blue") +
  xlab("Actual Rating") +
  ylab("Predicted Rating")
plot_R02



#Build model with entire dataset
regressor = lm(formula = rating ~ .,
               data = anEx2)
summary(regressor)
#SUMMARY OF MODEL:
#model never predicts anything less than 5 and thusly sucks at predicting 
#actual ratings less than 6 - see graph
#number of members and number of genres affect ratings (p<0.05)
#But the effects of members and number of genres only accounts for 
#about a quarter of the variation in rattings observed (R squared ~0.25)
#(can see above by rerunning the model with only numcat and members)





##########################################################
# 05 #The Rating dataset - Derived dataset insights
##########################################################

# D1 # Number of Anime each person Watched.
#I went with frequency instead of density for the next two plots 
#to also give an idea of how popular the animes were
countAnime = count(rating$anime_id) #gets animeID and freq (# times anime watched)
countUsers = count(rating$user_id) #gets userID and freq(# anime they watched)
aveWatch_per_person = mean(countUsers$freq) #106.28! = ave num of anime users watch
medWatch_per_person = median(countUsers$freq) #57L

plot_Rat01 <- ggplot(data = countUsers) + 
  th +
  geom_histogram(binwidth = 50, aes(x=freq), color = "black") + 
  xlim(c(0,1000)) +
  xlab("Number Anime Watched per Person") +
  ylab("Count") +
  ggtitle("Number of Anime Watched Per Person") +
  geom_vline(aes(xintercept=aveWatch_per_person), linetype = "longdash", color = "red") +
  geom_vline(aes(xintercept=medWatch_per_person), linetype = "longdash", color = "green") +
  annotate("text", label = "Median = 57", color = "green", size = 8, x= 35, y = 15000, angle = 90) +
  annotate("text", label = "Mean = 106.3", color = "red", size = 8, x= 120, y = 15000, angle = 90) 
plot_Rat01


#D2 - ave and median number of ratings given by a user rates
countRated = count(rating[!is.na(rating$rating),"user_id"]) #gets user ID and count of not NAs
aveRate_per_person = mean(countRated$freq) #91.05 = ave num of anime users rate
medRate_per_person = median(countRated$freq) #45

plot_Rat02 <- ggplot(data = countRated) + 
  geom_histogram(binwidth = 50, aes(x=freq), color = "black") + 
  xlim(c(0,1000)) +
  coord_cartesian(ylim = c(0,20000)) +
  th +
  xlab("Number Anime Rated per Person") +
  ylab("Count") +
  ggtitle("Number of Anime Rated Per Person") +
  geom_vline(aes(xintercept=aveRate_per_person), linetype = "longdash", color = "red") +
  geom_vline(aes(xintercept=medRate_per_person), linetype = "longdash", color = "green") +
  annotate("text", label = "Median = 45", color = "green", size = 8, x= 25, y = 12000, angle = 90) +
  annotate("text", label = "Mean = 91.05", color = "red", size = 8, x= 110, y = 12000, angle = 90) 
plot_Rat02

#D3 - percentage of users that rate -> ave watch/ave rate
ave_rate_percentage_per_person = 100 * aveRate_per_person/aveWatch_per_person #85.66% -> how likely a person is to rate an anime they watch
ave_rate_percent_per_person_median = 100 * medRate_per_person/medWatch_per_person #78.94 -> how likely a person is to rate an anime they watch

totalNA <- nrow(rating[is.na(rating$rating),]) #1476496 = num of NA
totalWatch <- nrow(rating) #7813737 = total watches
Percent_Rated <- 100* (1-(totalNA/totalWatch)) #81% = likelyhood of a watched anime being rated

#D4 - num users who never rate 
#remember, countUsers = number of times someone watched
countUseNA = count(rating[is.na(rating$rating),"user_id"]) #gets userID and #NAs for ratings
countUseNA$numNA = countUsers[countUseNA$x,"freq"] #check the user watched and add to user NA count
CountNevRat = count(countUseNA$freq == countUseNA$numNA) #3915 TRUE, so 3915 people watch but never rate


#countUsers who always rate -> people with no NA in their profile
#people with NA in their profile = countUseNA -> 37128 observations or people
countUseAL <- nrow(countUsers) - nrow(countUseNA) # 36387 users always rate an anime they watch
countOfUsers <- nrow(countUsers) #73515 total users in the database

print(paste("The average percent of time a person rates an anime they watch is: ", format(round(ave_rate_percentage_per_person,2), nsmall=2)))
print(paste("The average percent of time a person rates an anime they watch is: ", format(round(ave_rate_percent_per_person_median,2), nsmall=2)))
print(paste("The average percent of time a watched anime gets rated is: ", format(round(Percent_Rated,2), nsmall=2)))

print(paste("Summary: ", countOfUsers , " Users; ", countUseAL, " Users always rate if they watch",
            "3915 never rate \n",
            "The rest rate some of what they watch"
             ))

#SUMMARY:73515 Users, 36387 always rate, 3915 never rate, rest rate some of what they watched


##########################################################
# 06 #The Rating dataset - Individual Anime
##########################################################
#the rating distribution received by each anime

anime[grepl("Inuyasha", anime$name, ignore.case = T), ]
anNam <- "InuYasha"
anNum <- as.integer(anime[anime$name == anNam, "anime_id"])
duh <- count(rating[rating$anime_id == anNum, "rating"])
plot_myAnime <- ggplot(data = duh) + 
  geom_col(aes(x = x, y = freq), na.rm = T) +
  th +
  ggtitle(paste(anNam," Rating Distribution")) +
  xlab("Rating") +
  ylab("Count") +
  annotate("text", 
           label = paste("Average Rating: ", anime[anime$name == anNam, "rating"]),
           color = "green",
           x = 2.5, y = max(duh$freq), size = 8) +
  coord_cartesian(ylim = c(0,1.2 * max(duh$freq)))
plot_myAnime

