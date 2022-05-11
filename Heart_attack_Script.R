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
#install.packages("ggplot2")
library(ggplot2)

# 1. Effect of age on heart attack to know the range of ages 

# will display the range/ unique records
unique(heart_record$age)

males <- subset(heart_record, sex == 1)
females <- subset(heart_record, sex == 0)
males
females

# finding maximum and minimum range of ages
max_age <- max(heart_data$age)
min_age <- min(heart_data$age)
max_age
min_age

#we use a boxplot for the age variable
plotting_age <- table(heart_record$age)
boxplot(heart_record$age)
plotting_age

# 2.Effect of cholestrol on heart attack
# to know the inique range of cholestrol
unique(heart_record$chol)
cholesterol1 <- (heart_record$chol)
cholesterol1

# finding maximum and minimum range of cholestrol
max_cholestrol <- max(cholesterol1)
max_cholestrol
min_cholestrol <- min(cholesterol1)
min_cholestrol

#we use a boxplot for the cholestrol variable
plotting_cholestrol <- table(heart_record$chol)
boxplot(plotting_cholestrol)

#3 Effect of Resting electrocardiogram results on heart attack
# To know the range of Resting electrocardiogram 
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

# Similarly, plotting the rest of the attributes of datasets can be achieved

# we need to further process the dataset to perform Exploratory Data Analysis
# (as per users for carrying further analysis)

# Step 1:Renaming few column name required for further analysis.
# Renaming cp to chestPain
names(heart_record)[names(heart_record)=="cp"] <- "chestPain"

# Renaming chol to cholesterol
names(heart_record)[names(heart_record)=="chol"] <- "cholesterol"

# Renaming fbs to fasting_blood_sugar
names(heart_record)[names(heart_record)=="fbs"] <- "fasting_blood_sugar"

# Renaming restecg to resting_electrocardio
names(heart_record)[names(heart_record)=="restecg"] <- "resting_electrocardio"

# Renaming caa to major_vessels
names(heart_record)[names(heart_record)=="caa"] <- "major_vessels"

# Renaming caa to major_vessels
names(heart_record)[names(heart_record)=="trtbps"] <- "resting_blood_pressure"


# we can also remove the columns from the datasets on which we do not 
# carry analysis by using 
       # Analysis <- subset
       # (heart_record, select =  -c(column names to be removed separated by commas))

# Verifying the names of the column name changed or not?
names(heart_record)

# Checking the summary of the dataset
summary(heart_record)


#********************
# Using the default pairs() option first
# to examine correlations between variables
pairs(heart_record, labels = colnames(heart_record), main = "Heart attack dataset correlation plot")



# Hypothesis Building

# Q1. cholesterol have more effect for chances of heart attack?  

# H0:cholesterol does not have any effect heart attack
# H1:cholesterol has effect on heart attack.


heart_record$new_output <- factor(heart_record$output, labels = c("less", "more"))
attach(heart_record)
plot(new_output, cholesterol, pch = 19, col = "lightblue")

library("lattice")
attach(heart_record)
histogram(~cholesterol | new_output, 
          data = heart_record, 
          main = "Distribution of cholesterol", 
          xlab = "cholesterol", 
          ylab = "output")


normality_test <- shapiro.test(heart_record$cholesterol)
normality_test$p.value

wilcox.test(cholesterol ~ new_output)
#Done

# Q2. Does the old peak has relation with heart attack?

# H0 = old peak has lower chance of heart attack
# H1 = old peak has higher chance of heart attack

library("lattice")
attach(heart_record)
histogram(~oldpeak | new_output, 
          data = heart_record, 
          main = "Distribution of oldpeak", 
          xlab = "new_output", 
          ylab = "output")


normality_test <- shapiro.test(heart_record$oldpeak)
normality_test$p.value

wilcox.test(oldpeak ~ new_output)
# end of 2nd hypothesis
# Done


# Q3 Does has maximum heart rate (thallachh) has an impact on 
# Resting Blood pressure (restecg)

heart_record$new_restecg <- factor(heart_record$restecg, labels = c("normal", "abnormal", "hypertrophy"))
attach(heart_record)
plot(new_restecg, thalachh, pch = 19, col = "lightblue")

library("lattice")
attach(heart_record)
histogram(~thalachh | new_restecg, 
          data = heart_record, 
          main = "Distribution of ", 
          xlab = "cholesterol", 
          ylab = "output")


normality_test <- shapiro.test(heart_record$thalachh)
normality_test$p.value

kruskal.test(thalachh, new_restecg)

null hypo is rejected
Favour H1

#------------------------------------------------
# Q4 Does resting blood pressure (trtbps) has any impact on fasting blood sugar (fbs)  

heart_record$new_fbs <- factor(heart_record$fbs, labels = c("false", "true"))
attach(heart_record)
plot(new_fbs, resting_blood_pressure, pch = 19, col = "lightblue")

library("lattice")
attach(heart_record)
histogram(~resting_blood_pressure | new_fbs, 
          data = heart_record, 
          main = "Distribution of cholesterol", 
          xlab = "resting_blood_pressure", 
          ylab = "fbs")

normality_test <- shapiro.test(heart_record$resting_blood_pressure)
normality_test$p.value


wilcox.test(resting_blood_pressure ~ new_fbs)
# H0 
# H1 

#==========================================================================

#Done
#Q5 - Does the type of Chest Pain have a effect on the chances of having a heart attack?

#H0 : Chest Pain and Heart attack are not related to each other
#H1 : Chest Pain and Heart attack are related to each other 


Hypo5 <-table(heart_record$chestPain, heart_record$output)
Hypo5
chisq.test(Hypo5)

chisq.test(table(heart_record$chestPain, heart_record$output), correct = FALSE)

#we get a p value of less than the value of significance(0.05) 
#X-squared = 23.914
#df = 1
#p value < 2.2e-16
#From the results of the test we understand that the p value is less than that 
#of the significant value 0.05, 
#We reject the null hypothesis and accept the alternate hypothesis

#OUTPUT - The type of Chest pain is related to the target variable(chance of having heart attack)
