# Firstly loading the heart attack dataset

heart_record <- read.csv("heart.csv", header = TRUE)
heart_record

#Identifying the datatypes of the heart_record
str(heart_record)

is.na(heart_record)
head(heart_record)
tail(heart_record)
ncol(heart_record)
nrow(heart_record)
colnames(heart_record)

summary(heart_record)
#finding the count of na values

sum(is.na(heart_record))
# no na in the dataset

# By observing the dataset, there are categorical and numerical values, those 
# are further displayed with the help of unique funcion

# Representation of columns of heart_record and ploting them
# we need to import libraries ggplot2
install.packages("ggplot2")
library(ggplot2)

# 1. Effect of age on heart attack to know the range of ages 

# will display the range/ unique records
unique(heart_record$age)

males <- subset(heart_record, sex == 1)
females <- subset(heart_data, sex == 0)

# finding maximum and minimum range of ages
max_age <- max(heart_data$age)
min_age <- min(heart_data$age)
max_age
min_age

#we use a boxplot for the age variable
plotting_age <- table(heart_record$age)
boxplot(heart_data$age)

# 2.Effect of cholestrol on heart attack
# to know the inique range of cholestrol
unique(heart_record$chol)
cholesterol1 <- (heart_record$chol)

# finding maximum and minimum range of cholestrol
max_cholestrol <- max(cholesterol1)
max_cholestrol
min_cholestrol <- max(cholesterol1)
min_cholestrol

#we use a boxplot for the cholestrol variable
plotting_cholestrol <- table(heart_record$chol)
boxplot(plotting_cholestrol)

#3 Effect of Resting electrocardiogram results on heart attack
# to know the range of Resting electrocardiogram 
unique(heart_record$restecg)
# preparing the subset of Resting electrocardiogram 
restecg1 <- subset(heart_record, restecg == "0")
restecg1
restecg2 <- subset(heart_record, restecg == "1")
restecg2
restecg3 <- subset(heart_record, restecg == "2")
restecg3

# finding maximum and minimum range of Resting electrocardiogram results
max_restecg <- max(heart_record$restecg)
max_restecg 
min_restecg <- min(heart_record$restecg)
min_restecg

#we use a boxplot for the restecg variable
restecgplot <- table(heart_record$restecg)
boxplot(restecgplot)

# 4.Finding out the maximum and minimum age of in the records
max_age <- max(heart_record$age)
max_age
min_age <- min(heart_record$age)
min_age
#summary if ages
summary(heart_record$age)

#plotting the graph for better visualizations
hist(heart_record$age, main = "Histogram for age", xlab = "Age")
#we use a boxplot for the age variable
boxplot(heart_record$age)

# Similarly, plotting the rest of the attributes of datasets can be achieved

# we need to further process the dataset to perform Exploratory Data Analysis
# (as per users for carrying further analysis)

# Step 1:Renaming few column name required for further analysis.
# Renaming cp to chestPain
names(heart_record)[names(heart_record)=="cp"] <- "chestPain"

# Renaming cbol to cholesterol
names(heart_record)[names(heart_record)=="chol"] <- "cholesterol"

# Renaming fbs to fasting_blood_sugar
names(heart_record)[names(heart_record)=="fbs"] <- "fasting_blood_sugar"

# Renaming restecg to resting_electrocardio
names(heart_record)[names(heart_record)=="restecg"] <- "resting_electrocardio"

# Renaming caa to major_vessels
names(heart_record)[names(heart_record)=="caa"] <- "major_vessels"

# we can also remove the columns from the datasets on which we do not 
# carry analysis by using 
       # Analysis <- subset
       # (heart_record, select =  -c(column names tobe removed seperated by commas))

# Verifying the names of the column name changed or not?
names(heart_record)

# Checking the summary of the dataset
summary(heart_record)

# Hypothesis Building

# Q1.Which chest pains have the greatest effect on the chances of a heart attack? 
# H0: Chest Pain and Heart attack has relation (heart attach is either 0 or 1 and is represeneted by variable output).
# H1: Chest Pain and Heart attack does not have relation. 
# Chest Pain has categorical variable with 4 different values and Heart attack variable has 2 variable.
# Value 0: typical angina/ Value 1: atypical angina  
# Value 2: non-anginal pain /Value 3: asymptomatic 
# sky blue =  More prone for a heart attack \
# orange = More prone for a heart attack \

ggplot(heart_record, aes(x = age, fill = factor(output))) + 
geom_bar(width = 0.8) + 
facet_wrap(~chestPain) + 
ggtitle("type of chest pain") + 
xlab("chestPain") +
ylab("output") +
labs("Prone to have heart attack")

# by observing the plots it can be stated that Chest Pain 
# and Heart attack has relation with each other 
# 4 types of output shows clear evidance of it. Hence H0/ Null hypothesis is supported and H1 is not.

# Q2. Does maximum heart rate or cholesterol have more effect for an upcoming heart attack?  

# H0:cholesterol does not have any effect on maximum heart rate
# H1:cholesterol has effect on maximum heart rate.

# Create two subsets, one where all the patients have reached a max heart 
# rate greater than the average max heart rate, and another where all the patients 
# have reached a max heart rate less than the average max heart rate.

higher_heartrate_thalachh <- 
       heart_record[which(heart_record$thalachh > mean(heart_record$thalachh)),]
lower_heartrate_thalachh <- 
      heart_record[which(heart_record$thalachh < mean(heart_record$thalachh)), ]

# Similarly create subsets for higher and lower cholestrol
higher_heartrate_Cholesterol <- 
      heart_record[which(heart_record$cholesterol > mean(heart_record$cholesterol)),]
lower_heartrate_Cholesterol <- 
      heart_record[which(heart_record$cholesterol < mean(heart_record$cholesterol)), ]

# ploting for high heart rate

qqnorm(heart_record$thalachh, main="QQ plot for Maximum Heart Rate", 
       pch=19, ylab = "thalachh")
qqline(heart_record$thalachh)

# ploting for cholestrol

qqnorm(heart_record$cholesterol, main="QQ plot for Cholesterol", 
       pch=19, ylab = "Cholesterol")
qqline(heart_record$cholesterol)

# by observing the plots it can be stated that the maximum heart rate has more 
# effect on heart attack as compared to cholestrol.
# Hence Null hypothesis is favoured and the we reject the other hypothesis

# Q3. Does the number of major vessels a patient has have any effect 
# on the chances of a heart attack? 
# H0 = major vessels has lower chance of heart attack
# H1 = major vessels has higher chance of heart attack

table(heart_record[which(heart_record$major_vessels == 1), ]$output) 
# one major vessel
table(heart_record[which(heart_record$major_vessels == 2), ]$output) 
# two major vessels
table(heart_record[which(heart_record$major_vessels == 3), ]$output)
# three major vessels
table(heart_record[which(heart_record$major_vessels == 4), ]$output)
# four major vessels

ggplot(heart_record, aes(x = major_vessels, fill = factor(output))) + 
  geom_bar(width = 0.8) + 
  facet_wrap(~major_vessels) + 
  ggtitle("Type of chest pain") + 
  xlab("major_vessels") +
  ylab("heart_record") +
  labs("Prone to have heart attack")

# by observing the plots it can be stated that the major vessels has less 
# effect on heart attack.
# Hence Null hypothesis is favoured and the we reject the other hypothesis

# Q4 Does age have any relation to cholesterol levels in a patient?
# H0: age does not have relationship with cholesterol
# H1: age have relationship with cholesterol

#ploting the graph of age v/s cholesterol using ggplot
ggplot(heart_record, aes(age, cholesterol)) + geom_col()
ggplot(heart_record, aes(age, cholesterol)) + geom_col() + ggtitle("Relationship between age and cholestrol levels in a patient")

# from the plot it cann be noticed that higher the age, more thecholesterol.
# Hence, Null hypothesis H0 is not favoured and H1 is favoured.


# Q5 Is age responsible for a heart attack?
# H0:Age is not responsible for heart attack 
# H1:Age is responsible for heart attack 
more_likely <- subset(heart_record, output == 1)
more_likely
less_likely <- subset(heart_record, output == 0)
less_likely
mean(less_likely$age, na.rm = TRUE)
mean(more_likely$age, na.rm = TRUE)
# more_likely has more records omimited and the less likely ages returns 52.49697
# So the null hypothesis is true and we favour the null hypothesis and rejet the other




