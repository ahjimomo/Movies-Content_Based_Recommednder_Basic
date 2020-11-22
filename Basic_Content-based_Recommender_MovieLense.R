# Building basic Content-based Recommender System
# Use of correlation-similarity to get the measurements
################

# Load Library
library(recommenderlab)

# Load dataset - Movie Lens
data("MovieLense") #1,664 items
view(MovieLenseMeta)

# Remove URL column
data = MovieLenseMeta

data$url = NULL

# Set row names(indexes) to be title of movie
rownames(data)

rownames(data) = data$title
rownames(data)

data$title = NULL

###########################
# Single movie preference
# Recommender system to recommend top 10 for a user that likes Toy Story 1995
## cor() compares by column level, thus current table/matrix format does not work

## transpose cols to row & row to cols
dataT = t(data)
nrow(dataT) #20 rows & 1,664 cols

## Create the Correlation Matrix
cor_mat = cor(dataT)
nrow(cor_mat) # 1,664 rows
ncol(cor_mat) # 1,664 cols

## Find the top 10 movies in relation to preference
preference = "Toy Story (1995)"

## Getting correlation value against preference
cor_with_preference = cor_mat[rownames(cor_mat) == preference,]
print(cor_with_preference)

## get all correlation value against preference
cor_preference_with_others = cor_with_preference[names(cor_with_preference) != preference]
length(cor_preference_with_others) #1,663 rows

## Sort correlation values in descending/decreasing order & get top 10
items = sort(cor_preference_with_others,
             decreasing = T)[1:10]

print(items)
names(items) # Printing title only

###########################
# Working on Multiple preferences
preferences = c("Star Wars (1977)", "Stargate (1994)", "Men in Black (1997)")
preferences

## Create a vector of correlation on preferences
preference_vector = character() # Creates empty vector
for (i in 1: length(preferences)) {
  preference_vector = rbind(preference_vector, data[rownames(data) == preferences[i],])
}
View(preference_vector)

## Combine the 3 vectors using averages for user
combine_preference_vector = colMeans(preference_vector)
print(combine_preference_vector)

### Alternative - manually
combine_preference_vector1 = colSums(preference_vector) / length(preferences)
combine_preference_vector == combine_preference_vector1 # Checking

## Adding the user preference to dataset & transpose
data1 = t(rbind(combine_preference_vector, data))

## Generate the correlation values against all other movies
cor_mat1 = cor(data1)

nrow(cor_mat1) #1,665 rows & 1,665 cols

## Getting the coorelation values of the user
cor_with_preferences = cor_mat1[,1]
cor_with_preferences
length(cor_with_preferences) #1,665 items

## Remove the user preference entry & 3 preference movies
cor_with_preferences = cor_with_preferences[2:length(cor_with_preferences)]
length(cor_with_preferences) #1,664 items

for(i in 1:length(preferences)) {
  cor_with_preferences = cor_with_preferences[names(cor_with_preferences) != preferences[i]]
}

length(cor_with_preferences) #1,661 items, 3 additional removed

## Sort in descending & list top 10 recommended items to users
items1 = sort(cor_with_preferences, decreasing = T)[1:10]

names(items1)
