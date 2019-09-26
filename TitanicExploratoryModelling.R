# Now we are gonna start the Exploratory Modelling in R ....

# We will be using random forest algorithm in this modelling ....

# Start using the randomForest Library which is already installed ....
library(randomForest)

# This below code will grab two rows "Pclass" and "title" from combined data till 891
 # rows ....
random_forest_train1 <- combined_data[1:891,c("Pclass","title")]

random_forest_label <- as.factor(train$Survived)

# By setting seed we can achieve same result even though algorithm is random, else it
 # will produce different result each time which we do not want ....
set.seed(1234)

random_forest_1 <- randomForest(x = random_forest_train1, y = random_forest_label, importance = TRUE, ntree = 1000)

random_forest_1

# Parameter which will be most right in graph is more important ....
varImpPlot(random_forest_1)

# Lets have another attribute to the model ....
random_forest_train2 <- combined_data[1:891,c("Pclass","title","SibSp")]

# label is gonna be same ....
set.seed(1234)

random_forest_2 <- randomForest(x = random_forest_train2, y = random_forest_label, importance = TRUE, ntree = 1000)

random_forest_2

varImpPlot(random_forest_2)


# Adding more features to test the accuracy ....
random_forest_train3 <- combined_data[1:891,c("Pclass","title","SibSp","family_size")]

set.seed(1234)

random_forest_3 <- randomForest(x = random_forest_train3, y = random_forest_label, importance = TRUE, ntree = 1000)

random_forest_3
# By including "family_size" accuracy has increased, only 18.74% error or say inaccuracy ....
# check out the confusion matrix also ....

# checking the importance of feature by plotting graph
varImpPlot(random_forest_3)

# It came out that "family_size" is more important feture for infering the accuracy of
 # the model ....