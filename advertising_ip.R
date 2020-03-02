# Setting work directory and loading the dataset.
#
setwd("C:/Users/MAGUTU MWITA/Desktop/IP_Week_11_Jasline_advertising")
advertising =  read.csv("advertising.csv.csv", header = TRUE)
# CHECKING THE DATA.
#
head(advertising)
tail(advertising)
# Some variables do not have the righ data type.
# Ad.Topic.Line, City and Country should be character string.
# Male and Clicked.on.Ad should be factors, since they are variables,
# with a limited number of different value.
# Timestamp should be a date_time.
# Checking whether each column has an appropriate datatype.
#
str(advertising)
#
# Convert the variables into the correct data types.
#  into string
#
a = c('Ad.Topic.Line', 'City', 'Country')
for (i in a) {
  advertising[,i] = as.character(advertising[,i])
}
# Converting variables into factors.
#
b = c('Male', 'Clicked.on.Ad')
for (i in b) {
  advertising[,i] = as.factor(advertising[,i])
}
# Convering timestamp to appropriate datatype.
# The strptime command is used to take a string and convert it into a time data type.
#
advertising$Timestamp = strptime(advertising$Timestamp,"%Y-%m-%d %H:%M:%S")
# Confirming that all the variables have correct datatype.
#
str(advertising)


# TIDYING THE DATASET.

# Checking for missng values.
#
colSums(is.na(advertising))
# Data is clean and has no missing values.
#
# Checking for duplicated values.
#
dup_advertising = advertising[duplicated(advertising), ]
# There are no duplicated variables.
# The dataframe dup_advertising has 0 observations of 10 variables.
#
# Checking column names.
# The column 'Male' is better represented as gender where 1 is male and 0 female.
#
names(advertising)[names(advertising) == "Male"] <- "Gender"
# Checking for outliers.
# There are 3 numeric variables.(Age, Area.Income, Daily.Internet.Usage)
# Plotting boxplots to check for outliers.
# Boxplot for age column.
#
bxplt_Age = boxplot(advertising$Age,
                    main = "Boxplot for Age variable",
                    xlab = "Age",
                    col = "green",
                    border = "pink",
                    horizontal = TRUE,
                    notch = TRUE
)
# There are no outliers in the Area.Income.
#
# Boxplot for age column.
#
bxplt_Area.Income = boxplot(advertising$Area.Income,
                            main = "Boxplot for Area.Income variable",
                            xlab = "Area Income",
                            col = "yellow",
                            border = "orange",
                            horizontal = TRUE,
                            notch = TRUE
)
# There are some outliers in the area.income variable
#
bxplt_Daily.Internet.Usage = boxplot(advertising$Daily.Internet.Usage,
                                     main = "Boxplot for Daily.Internet.Usage variable",
                                     xlab = "Daily Internet Usage",
                                     col = "green",
                                     border = "blue",
                                     horizontal = TRUE,
                                     notch = TRUE
)
# There are no outliers in the Daily.Internet.Usage variable
#
# Handling the outliers in the area income variable.
# we store the outliers in a variable outliers.
#
outliers = bxplt_Area.Income$out
# This vector is to be excluded from our dataset
# The which() function tells us the rows in which the outliers exist,
# These rows will be removed from our data set.
# The dataset advertising will be stored in a new variable so as not to destroy dataset
#
adv_new = advertising
adv_new = adv_new[-which(adv_new$Area.Income %in% outliers),]
# Checking if the new data frame has outliers.
#
bxplt_Income = boxplot(adv_new$Area.Income)
# The outliers have been removed.
#.

# EXPLORATORY DATA ANALYSIS.
# Univariate Analysis.

#Histogram for Daily time spent on site distribution
#
x = hist(adv_new$Daily.Time.Spent.on.Site,
         main = "Daily time Spent on Site",
         xlab = "Daily time Spent on Site",
         col = "pink",
)

summary(adv_new$Daily.Time.Spent.on.Site)
#Histogram for Daily time spent on website is skewed to the left.
# this shows that generaly people spend alot of time on the website.
# from the summary of the variable they spend 65 minutes on the site.
#Histogram for Age distribution.
#
y = hist(adv_new$Age,
         main = "Age distribution",
         xlab = "Age",
         col = "gold",
)
summary(adv_new$Age)
# Age has a distribution very close to the normal distribution.
# Most people visiting the site are in the (25 - 35) age bracket.
# The average age of the audience is 36.
z = hist(adv_new$Area.Income,
         main = "Area Income distribution",
         xlab = "Area",
         col = "purple",
)
summary(adv_new$Area.Income)
# The graph is skewed to the left.
# From the graph most people have high incomes.
# On average the audience has an income of 55313.
w = hist(adv_new$Daily.Internet.Usage,
         main = "Daily internet usage distribution",
         xlab = "Internet usage",
         col = "red",
)
summary(adv_new$Daily.Internet.Usage)
# INTERPRETATION.
#
# Gender distribution
#
gender_frequency = table(advertising$Gender)
gender_frequency
# Barplot for the frequency.
#
bar_gen = barplot(gender_frequency,
                  main = 'gender_frequency',
                  xlab = 'gender',
                  ylab = 'count',
                  col = 'yellow')
# The females are slightly more than the males.
# This data cannot be considered imbalanced in terms of gender.
# clicked on ad distribution
#
coad_frequency = table(advertising$Clicked.on.Ad)
coad_frequency
# Barplot for clicked on ad variable.
#
bar_coad = barplot(coad_frequency,
                   main = 'clicked on ad_frequency',
                   xlab = 'gender',
                   ylab = 'count',
                   col = 'orange')
# The clicked on ad variable is the dependent variable.
# The values 0 and 1 in the variable are even.
# This is a perrfectly balanced dataset.
# It is fit to be trained on a machine learning model.
# BIVARIATE ANALYSIS.
#
# Analysis to see who clicked more on the ads.
#
j = ggplot(data = advertising, aes(x = Clicked.on.Ad, fill = Gender))+
  geom_bar(width = 0.5)
j
# Slightly more males clicked on the ads compared to the women.
#
# Area income vs clicked on Ad.
#
k = ggplot(data = advertising, aes(x = Area.Income, fill = Clicked.on.Ad))+
  geom_histogram(bins = 25)
k
# people in the Lower income areas clicked on the Ads more.
#
#Subsetting the numerical variables to check correlations.
#
num_cols = subset(advertising, select = c("Daily.Time.Spent.on.Site", "Age", "Area.Income", "Daily.Internet.Usage"))
num_cols
cor(num_cols)
# Plotting a heatmap for the correlations.
heatmap(cor(num_cols))
# pairplots to check correlation of variables.
#
pairs(num_cols)



#Conclusion and Recommendations
#1)People with high income should be targeted as theyre the ones with a higher frequency of clicking on the ad
#2)People with ages between 25- 50 would also be a great target market because they are easily accessible constantly
#3)People with high income should be targeted as theyre the ones with a higher frequency of clicking on the ad



