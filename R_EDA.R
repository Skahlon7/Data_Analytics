#Set wd and import libraries
getwd()
setwd("/Users/sandeepk/Desktop/test-DS/DS311-Technologies-in-Data-Analytic/Week_9_Exploratory_Data_Analysis/EDA_Python/data")
library(forecast)
library(zoo)
library(ggplot2)
library(readxl)
library(growthcurver)
library(readxl)

df = read.csv("ames.csv")

#Inspect Content, Shape, and Info
str(df)
nrow(df)
ncol(df)

#Distribution Sale Price
hist(df$SalePrice,
     main = 'Distribution of Sales Prices',
     ylab = 'Number of Houses',
     xlab = 'Sale Price',
     xlim = c(0,700000),
     col="darkmagenta",
     )
abline(v=mean(df$SalePrice),col="black",lwd=2)

#Descriptive Statistics Function
descriptive <- function(data) {
  paste('Mean:', mean(data),
  paste(),
  paste("Median:", median(data)),
  paste(),
  paste("STDV:", sd(data)))
  
}
descriptive(df$SalePrice)

#Looks like a log normal distribution. Most houses in this sample are
#clustered around the median value of $163,000, but the higher-end
#homes are pulling the mean up to over $180,000

#Rooms Above Grade Analysis
hist(df$TotRmsAbvGrd,
     main = "Distribution of Total Rooms Above Grade",
     ylab = "Number of Houses",
     xlab = "Total Rooms (Does Not Include Bathrooms)",
     col = "blue"
)
axis(1,at=seq(1,14,by=2))
abline(v=mean(df$TotRmsAbvGrd),col="black",lwd=2)

#Descriptive 
descriptive(df$TotRmsAbvGrd)

#The number of rooms in houses is approximately normally distributed, 
#with a mean and median around 6 rooms. There are some houses with
#twice as many rooms as the average, but overall the distribution is
#less skewed than the sale price distribution

#Overall Condition Analysis
hist(df$OverallCond,
     main = "Distribution of Overall Condition of Houses on a 1-10 Scale",
     xlab = "Condition of House",
     ylab = "Number of Houses",
     col = "blue")
axis(1,at=seq(1,9,by=1))
abline(v=mean(df$OverallCond),col="black",lwd=2)

#Descriptive
descriptive(df$OverallCond)

#Most homes have a condition of 5. It seems like we should
#treat this as a categorical rather than numeric variable,
#since the difference between conditions is so abrupt

#Explore Differences Between Subsets
#Cut dataset by condition to plot
below_avg_df <- df[df$OverallCond < 5, ]
avg_df <- df[df$OverallCond == 5,]
above_avg_df <- df[df$OverallCond > 5,]

for(i in 1:nrow(df)){
  if(df$OverallCond[i] < 5){
    df$Condition[i] <- "Below Average"
  }else if(df$OverallCond[i] == 5){
    df$Condition[i] <- "Average"
  }else if(df$OverallCond[i] > 5){
    df$Condition[i] <- "Above Average"
  }
}

#Visualize Sale Price Differences Between Subsets
ggplot(df, aes(x=SalePrice, fill=Condition)) +
  geom_histogram( bins=50, color='#e9ecef', alpha=0.45, position='identity') +
  ggtitle("Distributions of Sale Price Grouped by Condition") +
  xlab("Sale Price") +
  ylab("Number of Houses")
#value <- c(below_avg_df$SalePrice,avg_df$SalePrice,above_avg_df$SalePrice)

#Explore Correlations with Sales Price
num_cols <- unlist(lapply(df, is.numeric))         # Identify numeric columns
num_df<- df[ , num_cols]                        # Subset numeric columns of data
    
matrix <- cor(num_df)
matrix <- matrix[, ncol(matrix), drop=FALSE]  

#Transfrom to DataFrame
matrix_df <- as.data.frame(as.table(matrix))
matrix_df <- na.omit(matrix_df)

#Minimum Correlation and index
min_matrix <- min(matrix_df$Freq)
index <- match(c(min_matrix), matrix_df$Freq)

#Maximum Correlation and index
matrix_df<-subset(matrix_df, Freq!=1)
max_matrix <- max(matrix_df$Freq)
index1 <- match(c(max_matrix), matrix_df$Freq)

#Paste Correlation Message
paste('Most Positively Correlated Column:', matrix_df[index1,1])
paste('Maximum Correlation Value:', max_matrix)
paste('Most Negatively Correlated Column:', matrix_df[index,1])
paste('Minimum Correlation Value:', min_matrix)

#Box Plot distribution of column with most positive correlation
ggplot(df, aes(as.factor(OverallQual), SalePrice, fill=as.factor(OverallQual))) +  #Convert from int-->Factor
  geom_boxplot() +
  ggtitle("Overall Quality VS Sales Price") +
  xlab("Overall Quality") +
  ylab("Sale Price")

#Scatter Plot distribution of column with most positive correlation
ggplot(df, aes(OverallQual, SalePrice)) + geom_point() +
  geom_smooth(method = "lm") +                             # Trendline
  ggtitle("Overall Quality VS Sales Price") +
  xlab("Overall Quality") +
  ylab("Sale Price")

#Box Plot distribution of column with most negative correlation
ggplot(df, aes(as.factor(KitchenAbvGr), SalePrice, fill=as.factor(KitchenAbvGr))) +  #Convert from int-->Factor
  geom_boxplot() +
  ggtitle("Number of Kitchens VS Sales Price") +
  xlab("Number of Kitchens") +
  ylab("Sale Price")

#Scatter Plot distribution of column with most negative correlation
ggplot(df, aes(KitchenAbvGr, SalePrice)) + geom_point() +
  geom_smooth(method = "lm") +                             # Trendline
  ggtitle("Number of Kitchens VS Sales Price") +
  xlab("Number of Kitchens") +
  ylab("Sale Price")

#Engineer and Explore a New Feature
# Make a new column, Age
df$Age <- with(df, YrSold - YearBuilt)

# Plot Age vs. SalePrice
ggplot(df, aes(Age, SalePrice)) +
  geom_point(color="dark green", alpha = 0.5) +
  ggtitle("Home Age vs. Sale Price") +
  xlab("Age of Home at Time of Sale") +
  ylab("Sale Price") 

