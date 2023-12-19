# Question 1 - benchmark data.table's grouping on real data

library(data.table)
library(microbenchmark)
library(tidyverse)
library(Matrix)
library(sparsesvd)

setwd("D:/Spring_2022_D/BIOS735/hw/hw4")

# data read and check
dt <- fread("../Scorecard_2009-2016.csv")
dt2 <- dt
class(dt)
str(dt)
summary(dt)

# data type modification
dt2$TUITFTE <- as.numeric(dt2$TUITFTE)
dt2$SAT_AVG <- as.numeric(dt2$SAT_AVG)
dt2$CONTROL <- as.factor(dt2$CONTROL)
levels(dt2$CONTROL) <- c("pub", "pnp", "pfp")
str(dt2$CONTROL)

#        | C | Value              |     |
#        |---|--------------------|-----|
#        | 1 | Public             | pub |
#        | 2 | Private nonprofit  | pnp |
#        | 3 | Private for-profit | pfp |

# The number of schools by CONTROL
dt2[, .N, by = CONTROL]

# The numbet of schools by CONTROL that have non-NA values for TUITFTE and SAT_AVG
dt2[TUITFTE != "NA" & SAT_AVG != "NA", .N, by = CONTROL]
# or dt2[!is.na(TUITFTE) & !is.na(SAT_AVG), .N, by = CONTROL]

# Check data.table function
# x <- c(1, NA, 3)
# x2 <- as.data.table(x)
# str(x2)
# # data.table: can't operate mean with na.rm = T
# x2[x != "NA"]
# mean(x2, na.rm = TRUE)
# mean(x, na.rm = TRUE)

#
# My NA-removed mean and sd function
mean_NA_removed <- function(x){
        return(mean(x[!is.na(x)]))
}
sd_NA_removed <- function(x){
        return(sd(x[!is.na(x)]))
}

#
# Using data.table
dt2[,
    .(TUITFTE_mean=mean_NA_removed(TUITFTE), SAT_AVG_mean=mean_NA_removed(SAT_AVG),
      TUITFTE_sd=sd_NA_removed(TUITFTE), SAT_AVG_sd=sd_NA_removed(SAT_AVG)), 
    by = CONTROL]

# Using aggregate
merge(aggregate(dt2[, c("TUITFTE", "SAT_AVG")], dt2[, "CONTROL"], mean_NA_removed),
      aggregate(dt2[, c("TUITFTE", "SAT_AVG")], dt2[, "CONTROL"], sd_NA_removed), 
      by = "CONTROL", suffixes = c("_mean", "_sd"), sort = FALSE)

#
# Compare two methods
microbenchmark(dt2[,
                   .(TUITFTE_mean=mean_NA_removed(TUITFTE), SAT_AVG_mean=mean_NA_removed(SAT_AVG),
                     TUITFTE_sd=sd_NA_removed(TUITFTE), SAT_AVG_sd=sd_NA_removed(SAT_AVG)), 
                   by = CONTROL],
               merge(aggregate(dt2[, c("TUITFTE", "SAT_AVG")], dt2[, "CONTROL"], mean_NA_removed),
                     aggregate(dt2[, c("TUITFTE", "SAT_AVG")], dt2[, "CONTROL"], sd_NA_removed), 
                     by = "CONTROL", suffixes = c("_mean", "_sd"), sort = FALSE),
               times = 10)



#
#
#
# Question 2- doing more with "by" in data.table

# Making `scores.sub`
scores.sub <- na.omit(dt2, cols=c("TUITFTE", "SAT_AVG"))

# Making a plot
ggplot(scores.sub, aes(TUITFTE, SAT_AVG)) +
        geom_point(aes(color= CONTROL), alpha = 0.7, position = "jitter") +
        ggtitle("Scatter Plot of SAT_AVG over TUITFTE") +
        coord_cartesian(xlim=c(0,40000), ylim=c(500,1600)) +
        labs(x= "Tuition per FTE", y="Average SAT")
        
# Colouring
# scale_colour_brewer(palette = "Dark2")
# scale_colour_manual(values = c(pub="red", pnp="blue", pfp="black"))
        
scores.sub[, .N, keyby = .(CONTROL, 
                        TUITFTE_over20000 = TUITFTE > 20000, 
                        SAT_AVG_over1200 = SAT_AVG > 1200)]

#
#
#
# Question 3 - subsets of data 
scores.sub[, .(CONTROL, TUITFTE, SAT_AVG)][order(-SAT_AVG)][, head(.SD, 1), keyby = CONTROL]

# Double-checking
# scores.sub[CONTROL == "pub", .(CONTROL, TUITFTE, SAT_AVG)][which.max(SAT_AVG),]
# scores.sub[CONTROL == "pnp", .(CONTROL, TUITFTE, SAT_AVG)][which.max(SAT_AVG),]
# scores.sub[CONTROL == "pfp", .(CONTROL, TUITFTE, SAT_AVG)][which.max(SAT_AVG),]

#
#
#
# Question 4 - MovieLens sparse dataset
links <- read_csv("../ml-latest-small/links.csv")
movies <- read_csv("../ml-latest-small/movies.csv")
ratings <- read_csv("../ml-latest-small/ratings.csv")
tags <- read_csv("../ml-latest-small/tags.csv")

# How many Comedy genre movies are there?
sum(grepl("Comedy", movies$genres))

# Making sparse matrix of movies by user.
# make sure that every user in a data set.
# user_Id <- data.frame(userId = 1:max(ratings$userId))
# ratings2 <- left_join(user_Id, ratings, by = "userId")
# 
# movie_dt <- left_join(movies, ratings2, by = "movieId") %>%
#         select(-genres, -timestamp)

# Making sparse matrix of movies by user.
mtrx <- sparseMatrix(ratings$movieId, ratings$userId, x=1)
mtrx_ratings <- mtrx[rowSums(mtrx) != 0,]
mtrx_ratings

# Summary
summary <- summary(mtrx_ratings) %>% 
        group_by(j) %>% 
        summarise(count = n())
ggplot(summary, aes(y=count)) +
                geom_boxplot(width=1) +
                scale_x_continuous(limits = c(-1, 1)) +
                scale_y_continuous(breaks=seq(0, 3000, 500)) +
                ggtitle("Boxplot of the number of ratings by users") +
                theme(axis.text.x=element_blank())
summary(summary$count)


# Making key variable to match the spare matrix number to movieId 
keys <- data.frame(movieId = unique(ratings$movieId), key = 1:9724)
keys_byRow <- left_join(keys, movies, by="movieId")

# Computing SVD
svd <- sparsesvd(mtrx_ratings, rank=3)
args(sparsesvd::sparsesvd)
dim(svd$u)


# plot
par(mfrow=c(1,1))
plot(svd$u[,1], svd$u[,2])
plot(svd$u[,1], svd$u[,3])
plot(svd$u[,2], svd$u[,3])

# U vector: relationship wt movies and some latent varaiable (genre?)
svd_df <- data.frame(svd$u) %>%
        mutate(key = 1:nrow(svd$u)) %>%
        rename("u1"="X1", "u2"="X2", "u3"="X3")

# Top 6 moives w.r.t the sum of u1 and u3
df_movies_key_u <- left_join(keys_byRow, svd_df, by="key")
df_movies_key_u %>% mutate(sum_u1_u3 = u1 + u3) %>%
                        arrange(desc(sum_u1_u3)) %>%
                        head(n=6)

# Extremes of u2
# Largest
df_movies_key_u %>% arrange(desc(u2)) %>%
                        head(n=5)
# Smallest
df_movies_key_u %>% arrange(u2) %>%
                        head(n=5)

# comments
The movies with the highest values in both u1 and u3 
have the same characterstics of two latent factors. 
It is difficult for me to infer some property to be shared by those movies when considering its name and genre.
Similarly, one can group movies by a latent factor represented by u2, but it is hard to tell the differences between two groups.






