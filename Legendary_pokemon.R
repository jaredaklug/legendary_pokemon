library(tidyverse)
library(sjlabelled)

# Read in dataset
df <- read.csv("pokemon.csv")
df <- remove_all_labels(df)
# Get rid of first 20 columns (abilities and type advantage) b/c unecessary feature
df <- df[20:41]

# Remove irrelevant features: classification, japanese_name, name, percentage_male, type1, type2, generation, pokedex_number
df$classfication <- NULL
df$japanese_name <- NULL
df$name<- NULL
df$percentage_male <- NULL
df$type1 <- NULL
df$type2 <- NULL
df$pokedex_number <- NULL
df$generation <- NULL
df$base_egg_steps <-NULL

# Row 774 has non numeric stats ... remove it
df <- df[-c(774),]

# Capture_rate is a factor -> convert to numeric
df$capture_rate <- as.numeric(df$capture_rate)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(12345)
split <- sample.split(df$is_legendary, SplitRatio = 0.80)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

# Feature scaling
training_set[-13] = scale(training_set[-13])
test_set[-13] = scale(test_set[-13])

# Fitting classifier to the training set
library(rpart)
classifier = rpart(formula = is_legendary ~ .,
                   data = training_set)

# Make predictions on the test set
y_pred = predict(classifier, newdata = test_set[-13])

y_pred = ifelse(y_pred > 0.5, 1, 0)

# Make confusion matrix
cm = table(test_set[, 13], y_pred)
cm

# Visualize Decision Tree
plot(classifier)
text(classifier)
