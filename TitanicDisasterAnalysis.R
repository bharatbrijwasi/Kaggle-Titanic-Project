# We are gonna do some analysis on Titanic Disaster using R........................
# Remember, R is a case sensitive language, take care of that......................
# We need to be in the file where our data files are or give the complete path...

# train here is not the train(vehicle), it is the training data...
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

print(ncol(train))
print(ncol(test))

# This below code will just change the order of the columns of train data frame.......
manipulated_train <- subset(train,select = c(2,1,3,4,5,6,7,8,9,10,11,12))
print(manipulated_train)

# Now, we want to combine both the data of test and train. But, we know that number
 # of columns in "test" is one less than "train". Column "survival" is missing in
  # "test". So, we need to add the column "survival" in "test" data frame.
manipulated_test <- data.frame(Survived = rep("None", nrow(test)),test[,])
# test[,] represent the whole data frame. So, we are combining newly created
 # list of "survived" with whole "test" data frame and make that combination a 
  # data frame......................................................................

print(class(manipulated_train))
  
# Now, we will combine the "manipulated_train" and "manipulated_test" data frames using rbind...
combined_data <- rbind(manipulated_train,manipulated_test)

# Knowing the structure of the data frame...........................................
str(combined_data)

# Knowing the class of combined_data$Survived...
class(combined_data$Survived)
# They are characters
  
# Now, we will change the "Survived" and "Pclass" column into factor
 # because fot data analysis factors are good ....................
combined_data$Survived <- as.factor(combined_data$Survived)
combined_data$Pclass <- as.factor(combined_data$Pclass)

# Now, the "Survived" and "Pclass" columns have become factors..........................
str(combined_data)

# Taking a look at gross survival rates
table(combined_data$Survived) 
# table() method will give the all the results and differnet types of possibilities... 

# Taking a look at gross classes
table(combined_data$Pclass)

# We have a very good package installed in R i.e. "ggplot2" . So, we will be using this
 # package into our environment.........................................................
library("ggplot2")

# Converting Pclass of train data into the factor...
train$Pclass <- as.factor(train$Pclass)

# "geom_histogram" was there in place of "stat_count" which was giving some error
 # Now, it is working well, may be error was due to version of R..................

# Drawing the graph of the data..................................................
# aes stands for aesthetics.... fill is optional... See the documentation
 # for aes by ?aes in console....
# We can use as.factor as well in place of factor below.... 
# That "Survived" is the "Survived" column of the "train" data frame ....
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  stat_count(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Meh-Survived")
# This above line is saying that we are giving the fill the label of 
 # Survived....

# "geom_bar" can also be used in place of geom_histogram and it will create same 
 # result and will not give any kinda error......................................
# x = Pclass refers to different classes and will be drawn on x axis...
# fill = factor(Survived) refers to the survival data which will be drawn
 # onto the y axis....
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# This is using combined data.... 
ggplot(combined_data, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


# head will give first 6 rows of the data asked....
# Converting the names from "train" to characters
head(as.character(train$Name))

# How many unique names are there in "test" and "train" data frames................
# So, here we will be working with combine data set that is "combined_data"
length(unique(as.character(combined_data$Name)))

# We, got two duplicate names as we know the number of rows in "combined_data" are
 # 1309 while unique names are 1307, which means that there are two duplicate names.

# See, the documentation of "which" or for more convenience search internet........
# It works as which(LETTERS == "R")  , LETTERS is a vector of character type
 # Now, this which() function will return the index where "R" exists................
dup_names <- as.character(combined_data[which(duplicated(as.character(combined_data$Name))),"Name"])
# duplicated() function will give True or False for every "Name" based on whether 
 # they have duplicate value or not ....

# "which" will generate numbers i.e. row numbers. So, below code will fetch us 4 full 
 # rows as we are not giving 2nd parameter inside []  .
# This below code will give 4 rows as one name is duplicate to other and another
 # name is duplicate to some other. So, we will be getting 2 rows of same name
  # and another 2 rows with same name. So, overall we will have 4 rows....
combined_data[which(combined_data$Name %in% dup_names),]    
                          
                          
# Loading the "stringr" library
library(stringr)

# Knowing, which data have "Miss" in it...............................................
# str_detect is a function which will detect some pattern and it is the 
 # function of "stringr" library.....
misses <- combined_data[which(str_detect(combined_data$Name,"Miss.")),]

# "misses" is a data frame which we have received using above expression..............
class(misses)

# Printing the misses data
print(misses)  

# We wanna see the first five rows of the "misses" data frame..........................
misses[1:5,]


# Knowing, which data have "Mrs." in it...............................................
mrses <- combined_data[which(str_detect(combined_data$Name,"Mrs.")),]

# We wanna see the first five rows of the "mrses" data frame..........................
mrses[1:5,]


# Check out for males now.............................................................
males <- combined_data[which(train$Sex == "male"),]
males[1:5,]

# Here, we are defining a function and naming it as "extract_title"...................
extract_title <- function(name)
{
  # Converting name into character
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) > 0)
  {
    return("Miss.")
  }else if(length(grep("Master.",name)) > 0)
  {
    return("Master.")
  }else if(length(grep("Mrs.",name)) > 0)
  {
    return("Mrs.")
  }else if(length(grep("Mr.",name)) > 0)
  {
    return("Mr.")
  }else
  {
    return("Other")
  }
}

titles <- NULL
for(i in 1:nrow(combined_data)){
  # In R, we can access the data frame element both by index values and also by their
   # column names
  # In combined_data data frame we have got a column named "Name"....................
 
  # Here, we will keep adding all the "titles" into the title and it will become a vector
   # which will have all the titles from all the data...................................
  # "c" is actually a function for concatenate.....
   titles <- c(titles, extract_title(combined_data[i,"Name"]))
}
 
# Creating a new column in combined_data called as title................................
combined_data$title <- as.factor(titles)

# table shows us the frequencies of different data in an column...................
# We can't run it on data frame...................................................
table(combined_data$Sex)

# Now, we have got 13 variables for the data frame, "combined_data"......................
# facet_wrap will categorize titles on the basis of class.... 
ggplot(combined_data[1:891,], aes(x = title, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") + 
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# In case of geom_histogram( binwidth = 0.5) we need to give "binwidth", in case of 
 # geom_bar() we don't need to give anything. And, finally in case of 
  # stat_count(width = 0.5) we have to give "width". Well, In our case
   # we are using stat_count(width = 0.5) which is actually solves every
    # problem............................................................

# plotting for "Sex" column
# This below code means we are plotting for who survied on the basis of "Sex" in
 # different classes.............................................................
ggplot(combined_data[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") + 
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# This below code will just plot for who survived on the basis of "Sex" without
 # considering classes...........................................................
ggplot(combined_data[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count(width = 0.5) +
  ggtitle("Pclass") + 
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# This will plot the histogram on the basis of "Age" for who survived and also 
 # categorize those on the basis of "Sex" and "Pclass"
ggplot(combined_data[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  ggtitle("Pclass") +
  xlab("Age") +
  ylab("Total Count")

# We may be having many data entry where "Age" entry is not defined, so we have to find
 # a way to fill up those places using some technique. One of the technique to fill
  # missing values is "linear regression"................................................

# So, let's do some summarization of the "Age" in "combined_data" for 891 rows....
# summary() function will give lots of information about ,min, max, median
summary(combined_data[1:891,"Age"])

# Summary will give us min, median, mean, max and NA values inside the "Age"......

# Let's see the situation for remaining the rows.....................................
summary(combined_data[891:1309,"Age"])

# It is giving 86 NA's, it means that huge amount of the "Age" data is missing from 
 # our sample data....................................................................


# Now, lets search for the boys from the data........................................
# This below code will bring all the rows where title of individual is "Master.".....
boys <- combined_data[which(combined_data$title == "Master."),]

# Printing all the boys rows.........................................................
print(boys)
class(boys)

# Number of rows in "boys" data frame................................................
nrow(boys)  # 61 rows means 61 are boys in whole "combined_data" data frame..........

# Summary for "Age" column in boys data frame........................................
# With summary it is clear that title as "Master" means those boys are kids as max age
 # is 14.500 and min age is 0.330    .................................................
summary(boys$Age)

# 8 rows of data frame "boys" have no "Name".........................................


misses <- combined_data[which(combined_data$title == "Miss."),]
print(misses)
summary(misses$Age)

# There are a lots of "None" misses data frame........................................
misses$Survived

# This below line will take all the rows where "Survived" is not "None"...............
misses[misses$Survived != "None",]

# Plotting the histogram for surival from misses data frame where "Survived" is not 
 # equal to "None". Plotting will be done on the basis of Pclass.
ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for Miss. by Pclass") +
  xlab("Age") +
  ylab("Total Count")
  

misses_alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses_alone$Age)
length(which(misses_alone$Age <= 14.5))
# Means there are 4 teenager girls ....

summary(combined_data$SibSp)

length(unique(combined_data$SibSp))

# Unique values can be checked by factor as well

# Lets convert the column "sibsp" into factor........................................
combined_data$SibSp <- as.factor(combined_data$SibSp)


ggplot(combined_data[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Sibsp") + 
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")  
  
# Converting Parch column into the factor
# We have to convert Parch column into factor bcoz we are plotting Parch on X-axis
 # So, that is why converting Parch column must be converted to factor ....
combined_data$Parch <- as.factor(combined_data$Parch)


ggplot(combined_data[1:891,], aes(x = Parch, fill = Survived)) + 
  stat_count(width = 0.5) +
  facet_wrap(~Pclass + title) +
  xlab("Parch") +
  ylab("Total Count") +
  ggtitle("Parent-Child Survival") +
  ylim(0,300) +
  labs(fill = "Survived")

temp_Sibsp <- c(train$SibSp, test$SibSp)
temp_Parch <- c(train$Parch, test$Parch)

# This way we can find the family size which is number of siblings + number of parent
 # children + himself/herself ....
combined_data$family_size <- as.factor(temp_Parch + temp_Sibsp + 1)


# Plotting Survival for family size....
ggplot(combined_data[1:891,], aes(x = family_size, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass + title) +
  xlab("Family Size") +
  ylab("Total Count") +
  ggtitle("Survival By Family Size") +
  ylim(0,300) +
  labs(fill = "Survived")


