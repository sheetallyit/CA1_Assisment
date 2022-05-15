# Firstly storing the file in the working directory,
# followed by reading into a data frame.

heart_record <- read.csv("heart.csv", header = TRUE)
heart_record

# To know the structure of the heart.csv file
str(heart_record)

# To know the data type of the heart.csv file
class(heart_record)

# The next step is to deal with the missing data records.
# In heart.csv file, we do not have any missing record in the file,
# so this step is eliminated still for finding the count of NA values

sum(is.na(heart_record))
# no NA values in the file

# To check whether 
is.na(heart_record)

# To check the head 6 records of the file.
head(heart_record)

# To check the tail 6 records of the file.
tail(heart_record)

# To check the numbers of column present in the file.
ncol(heart_record)

# To check the numbers of row present in the file.
nrow(heart_record)

# To know the column names present in the file.
colnames(heart_record)

# To know the summary of the file.
summary(heart_record)

# To examine the correlation between the variables of file
pairs(heart_record, labels = colnames(heart_record), main = "Heart attack plot")

# to know the positive correlation between age and sex       
pairs.panels(heart_record,
             smooth = TRUE,      # If TRUE will return draws less smooths
             scale = FALSE,      # If TRUE will scales the correlation text font
             density = TRUE,     # If TRUE will adds density plots and histograms
             ellipses = TRUE,    # If TRUE will draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, report  ts correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE will adds significance level with stars
             ci = TRUE)   


# By observing the file, there are categorical and numerical values, 
# those are further displayed with the help of unique function
# Secondly, we will be performing some uni-variate analysis.

# 1. Finding of sex variables in the dataset
# To know the unique range of sex

unique(heart_record$sex)

# Finding the males and females in the file and ages,
# considering male = 1 & female = 0
males <- subset(heart_record, sex == 1)
females <- subset(heart_record, sex == 0)
# Listing the males and females in the file.
males
females

install.packages("ggplot2")
library(ggplot2)
library(viridis)
# Visualization of Sex  using histogram.
hist(heart_record$sex,
     main = "Histogram for sex",
     xlab = "Sex",
     ylab = "Numbers of humans")

# Visualization of Sex using box plot
boxplot(heart_record$sex,
        main = "Box plot of sex",
        xlab = "Sex Rate",
        ylab = "Sex Rate value")
# for viewing the density of sex in the file 
plot(density(heart_record$sex),
     main = "Density plot for Sex",
     xlab = "Sex Rate" )
# Viewing it in pink
polygon(density(heart_record$sex), col = "pink")

# To know the summary of sex varaible present in dataset
summary(heart_record$sex)
#------------------------------------------------------------

# 2. To know maximum and minimum ages present in the file

# will display the range/unique records
unique(heart_record$age)

# Finding maximum and minimum range of ages
max_age <- max(heart_record$age)
min_age <- min(heart_record$age)
# Listing the maximum and minimum ages
max_age
min_age
# Visualization of Sex and Age using histogram.
hist(heart_record$age,
     main = "Histogram of age",
     ylab = "Number of humans",
     xlab = "Age")

# Visualization of age using box plot
boxplot(heart_record$age,
        main = "Box plot for Age",
        xlab = "Age Rate",
        ylab = "Age Rate value")
# for viewing the density of age in the file 
plot(density(heart_record$age),
     main = "Density plot for age",
     xlab = "Age Rate" )
# Viewing it in blue
polygon(density(heart_record$age), col = "blue")

# To know the summary of age varaible present in dataset
summary(heart_record$age)

#---------------------------------------------------------------------
# 3. To know maximum and minimum cholesterol in the file

# to know the unique range of cholesterol
cholesterol1 <- (heart_record$chol)
# Displaying unique range of cholesterol
cholesterol1

# Finding maximum and minimum range of cholesterol
max_cholestrol <- max(cholesterol1)
min_cholestrol <- min(cholesterol1)

# Displaying maximum and minimum range of cholesterol
max_cholestrol
min_cholestrol

# Visualization of cholesterol using histogram.
hist(heart_record$chol,
     main = "Histogram for Cholesterol",
     ylab = "Numbers of Humans",
     xlab = "Cholesterol")

# Visualization of age using box plot
boxplot(heart_record$chol,
        main = "Box plot for Cholesterol",
        xlab = "Cholesterol",
        ylab = "Number of Human")
# for viewing the density of age in the file 
plot(density(heart_record$chol),
     main = "Density plot for Cholesterol",
     xlab = "Cholesterol Rate" )
# Viewing it in green
polygon(density(heart_record$chol), col = "green")

# To know the summary of cholesterol varaible present in dataset
summary(heart_record$chol)

#---------------------------------------------------------------------

#4 Effect of Resting electrocardiogram results on heart attack
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

# We use a boxplot for the restecg variable and
# to know the numbers of restecg 0, 1 n 2 respectively
restecgplot <- table(heart_record$restecg)
restecgplot
hist(heart_record$restecg, main = "Resting Electrocardiogram", 
     xlab = "restecg", ylab = "Number of Humans")

# Visualization of Resting electrocardiogram  using box plot
boxplot(heart_record$restecg,
        main = "Box plot for restecg",
        xlab = "restecg",
        ylab = "restecg value")
# for viewing the density of Resting electrocardiogram  in the file 
plot(density(heart_record$restecg),
     main = "Density plot for Resting electrocardiogram ",
     xlab = "restecg Rate" )
# Viewing it in red
polygon(density(heart_record$restecg), col = "red")

# To know the summary of cholesterol varaible present in dataset
summary(heart_record$restecg)

#--------------------------------------------------------

# Similarly, plotting the rest of the attributes of data sets can also be done
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

# Verifying the names of the column names changed or not?
names(heart_record)

# Checking the summary of the dataset after changing the column names.
summary(heart_record)

#-----------------------------------------------------------------
# Using the default pairs() option first
# to examine correlations between variables
str(heart_record)
pairs(heart_record, labels = colnames(heart_record), main = "Heart attack plot"
)

# Representation of columns of heart_record and plotting them
# we need to install and import libraries ggplot2 which is done earlier.
#------------------------------------------------------------------
# Hypothesis Building

# Q1.Does cholesterol have more effect for chances of heart attack?  

# H0:cholesterol does not have any effect heart attack(Null Hypothesis).
# H1:cholesterol has effect on heart attack(Alternate Hypothesis).
# Dependent variable = cholesterol 
# Independent variable = heart attack (output)

# Forming new column named new_output in order to perform analysis
heart_record$new_output <- factor(heart_record$output, labels = c("less", "more"))

# Verifying whether the column gender has reflect in heart_record or not
View(heart_record)

# Box Plotting for better visualization 
plot(new_output, resting_blood_pressure , pch = 19, col = "lightblue")
# Importing lattice library and attaching the dataset for performing the further functions
attach(heart_record)
library("lattice")

# Visualization using histogram
histogram(~cholesterol | new_output, 
          data = heart_record, 
          main = "Distribution of cholesterol", 
          xlab = "cholesterol", 
          ylab = "output")
# detaching the file after visualisation
detach(heart_record)

# Conducting formal test of normality.
normality_test <- shapiro.test(heart_record$cholesterol)
normality_test$p.value
# The p-value indicates whether the distribution is normal or not.
# P-value less than 0.05 is not normally distributed
# P-value more than 0.05 is normally distributed
# here the p-value is 1.46e-06 which is clearly lower than 0.05 
# hence distribution is not normal.

# Examining the dependent continuous variable (cholesterol)
# with an independent categorical variable (new_output) with the help of
# Mann-Whitney test, also know as "wilcox test" in R and also refering refering histogram
with(heart_record, tapply(cholesterol, new_output, shapiro.test))

wilcox.test(cholesterol~new_output)
# from Mann-Whitney test we get p-value = 0.04 and is less than 0.05
# the Null hypothesis is rejected and the alternative hypothesis is true/excepted.

#-------------------------------------------------------------
# Q2. Does the old peak have relation with heart attack?
# H0: old peak has lower chance of heart attack (Null Hypothesis).
# H1: old peak has higher chance of heart attack (Alternate Hypothesis).
# Dependent variable = oldpeak 
# Independent variable = heart attack (output)

# Forming new column named new_output in order to perform analysis
heart_record$new <- factor(heart_record$output, 
                           labels = c("less", "more"))

# Verifying whether the column gender has reflect in heart_record or not
View(heart_record)
plot(new, output , pch = 19, col = "lightblue")

# Plotting for better visualization
plot(new, oldpeak , pch = 19, col = "lightblue")
# by observing the plot, it can be seen that there are few outliers.

# Importing lattice library and attaching the dataset 
# for performing the further analysis

library("lattice")
attach(heart_record)
# Visualization using histogram
histogram(~oldpeak | new_output, 
          data = heart_record, 
          main = "Distribution of oldpeak", 
          xlab = "oldpeak", 
          ylab = "output")
# detaching the file after visualisation
detach(heart_record)

# Conducting formal test of normality.
normality_test <- shapiro.test(heart_record$oldpeak)
normality_test$p.value
# The p-value indicates whether the distribution is normal or not.
# P-value less than 0.05 is not normally distributed
# P-value more than 0.05 is normally distributed
# here the p-value is 8.18e-17 which is clearly lower than 0.05 
#-----------------------------------------------
# hence distribution is not normal.
#-----------------------------------------------

# Examining the dependent continuous variable (oldpeak)
# with an independent categorical variable (new_output) with the help of
# Mann-Whitney test, also know as "wilcox test" in R and also refering refering histogram
with(heart_record, tapply(oldpeak, new_output, shapiro.test))

wilcox.test(oldpeak ~ new_output)
# from Mann-Whitney test we get p-value = 2e-13 and is less than 0.05
# the Null hypothesis is rejected and the alternative hypothesis is true/excepted.
# Hence it can be said that old peak has higher chance of heart attack 
#-------------------------------------------------------
# Q3 Does maximum heart rate (thallachh) has an impact on sex?

# H0: maximum heart rate (thallachh) does not
#     have an impact on sex (Null Hypothesis).
# H1: maximum heart rate (thallachh) have an 
#     impact on sex (Alternate Hypothesis).
# Dependent variable = sex 
# Independent variable = thallachh

# Forming new column named gender in order to perform analysis
heart_record$gender <- factor(heart_record$sex, 
                              labels = c("Female", "Male"))
# Verifying whether the column gender has reflect in heart_record or not
View(heart_record) 

# Plotting for better visualization
plot(gender, thalachh, pch = 19, col = "lightblue")

# Importing lattice library and attaching the dataset for performing the further functions
library("lattice")
attach(heart_record)

# Visualization using histogram
histogram(~thalachh | gender, 
          data = heart_record, 
          main = "Distribution of thalachh ", 
          xlab = "thalachh", 
          ylab = "Density of thallach")

# detaching the file after visualisation
detach(heart_record)

# Conducting formal test of normality.
normality_test <- shapiro.test(heart_record$thalachh)
normality_test$p.value
# The p-value indicates whether the distribution is normal or not.
# P-value less than 0.05 is not normally distributed
# P-value more than 0.05 is normally distributed
# here the p-value is 6.620819e-05  which is clearly lower than 0.05, 
# so its not normally distributed

# Examining the dependent continuous variable (thalachh)
# with an independent categorical variable (gender) with the help of
# kruskal.test in R.
kruskal.test(thalachh, gender)
# from kruskal.test we get p-value = 0.4884 and is more than 0.05
# the Null hypothesis is excepted and the alternative hypothesis is rejected
#------------------------------------------------
# Q4 Does resting blood pressure (trtbps) have any impact on fasting blood sugar (fbs)?
# H0: resting blood pressure (trtbps) does not
#     have an impact on fasting blood sugar (fbs) (Null Hypothesis).
# H1: resting blood pressure (trtbps) does 
#     have an impact on fasting blood sugar (fbs) (Alternate Hypothesis).
# Dependent variable = fbs 
# Independent variable = trtbps
library("lattice")
attach(heart_record)
# Forming new column named gender in order to perform analysis
heart_record$new_fbs <- factor(heart_record$fasting_blood_sugar, 
                               labels = c("less", "more"))

# Verifying whether the column gender has reflect in heart_record or not
View(heart_record)

# Plotting for better visualization
plot(new_fbs, resting_blood_pressure, pch = 19, col = "lightblue")

# Importing lattice library and attaching the dataset for performing the further functions
library("lattice")
attach(heart_record)

# Visualization using histogram
histogram(~resting_blood_pressure | new_fbs, 
          data = heart_record, 
          main = "Distribution of trtbps", 
          xlab = "resting_blood_pressure", 
          ylab = "fbs")

# detaching the file after visualisation
detach(heart_record)

# Conducting formal test of normality.
normality_test <- shapiro.test(heart_record$resting_blood_pressure)
normality_test$p.value
# The p-value indicates whether the distribution is normal or not.
# P-value less than 0.05 is not normally distributed
# P-value more than 0.05 is normally distributed
# here the p-value is 1.458097e-06  which is clearly lower than 0.05, 
# so its not normally distributed

# Examining the dependent continuous variable (resting_blood_pressure)
# with an independent categorical variable (new_fbs) with the help of
# wilcox. test in R.
with(heart_record, tapply(resting_blood_pressure, new_fbs, shapiro.test))

wilcox.test(resting_blood_pressure ~ new_fbs)
# W = 4376, p-value = 0.008


#we reject the Null hypothesis H0 and accept the Alternate hypothesis H1.
# Hence it can be said that resting blood pressure (trtbps) does
# have an impact on fasting blood sugar (fbs)

#==========================================================================

#Q5 Does chest Pain have a relationship with heart attack?

#H0 : Chest Pain and Heart attack are not related to each other(Null Hypothesis)
#H1 : Chest Pain and Heart attack are related to each other (Alternate Hypothesis)
# Here both the variables are dependent variables

# Creating new variable record for knowing relationship between chestpain and Heart attack
record <-table(heart_record$chestPain, heart_record$output)
record
chisq.test(record)
# Pearson's Chi-squared Test for Data 

chisq.test(table(heart_record$chestPain, heart_record$output), correct = FALSE)
# we get a p value of less than the value of significance(0.05) 
# X-squared = 82, df = 1, p value < 2.2e-16
# Here the p value is less than 0.05 so we reject the null hypothesis
# and accept the alternate hypothesis
# Chest Pain and Heart attack are related to each other
