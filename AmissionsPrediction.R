# Loading needed libraries 
library("data.table")
library(e1071)
library(corrplot)

setwd("Path of the admissions file from Kaggle")
data <- read.csv("Admission_Predict_Ver1.1.csv")
str(data)
summary(data)

# Subsetting the data to remove the serial no. column
df <- data[2:9]
df
summary(df)

# Conversion of Research to a categorical variable
df$Research <- as.factor(df$Research)
str(df)

#### Plotting boxplots of all the variables to identify the outliers####

par(mfrow=c(1, 2))  # divide graph area in 2 columns
#boxplot(df$GRE.Score, main="GRE", sub=paste("Outlier rows: ", boxplot.stats(df$GRE.Score)$out))
#boxplot(df$TOEFL.Score, main="TOEFL", sub=paste("Outlier rows: ", boxplot.stats(df$TOEFL.Score)$out))
#boxplot(df$University.Rating, main="UnivRanking", sub=paste("Outlier rows: ", boxplot.stats(df$University.Rating)$out))
#boxplot(df$SOP, main="SOP", sub=paste("Outlier rows: ", boxplot.stats(df$SOP)$out))
boxplot(df$LOR, main="LOR", sub=paste("Outlier rows: ", boxplot.stats(df$LOR)$out))
#boxplot(df$CGPA, main="GPA", sub=paste("Outlier rows: ", boxplot.stats(df$CGPA)$out))
boxplot(df$Chance.of.Admit, main="chance of admit", sub=paste("Outlier rows: ", boxplot.stats(df$Chance.of.Admit)$out))
####end####


## Removing the outliers from the dataset 
df = df[!(df$LOR ==1 | df$Chance.of.Admit==0.34),]

## Removing the categorical variable to find correlations among the variables 
df$Research <- NULL
head(df)
df.cor = cor(df, method = c("spearman"))
corrplot(df.cor)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = df.cor, col = palette, symm = TRUE)

# Running regrssion on total data
LinearModel <- lm(Chance.of.Admit~ GRE.Score + TOEFL.Score + LOR + CGPA,data = df )
print(LinearModel)
summary(LinearModel)

# Splitting the data to estimate the performance of the model
index <- sample(1:nrow(df), 0.8*nrow(df))
traindata <- df[index,]
testdata <- df[-index,]

# Evaluating model performance using correlation
Model1 <- lm(Chance.of.Admit~ GRE.Score + TOEFL.Score + LOR + CGPA ,data = traindata)
summary(Model1)

Pred <- predict(Model1, testdata)
actuals_preds <- data.frame(cbind(actuals=testdata$Chance.of.Admit, predicteds= Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) # 91.17%  
