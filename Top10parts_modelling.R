#################### Aggregation of the top 10 parts list per category #######################
##############################################################################################

rm(list=ls())
library(data.table)
library(readxl)
library(zoo)
library(dplyr)
library(magrittr)
library(openxlsx)

# Setting the working directory to perform analysis 
setwd("C:/Users/A315186/Downloads/Competitive Intelligence Study 2019")
df <- read_xlsx('Copy of Top 10 parts in each catagory.xlsx')
df
colnames(df)

# Filling the missing NA values with the previous values 
df$Group <- na.locf(df$Group)
df$`common part number` <- na.locf(df$`common part number`)
df <- setNames(df, c("Group","Commmon_PartNo","PartNo","Desc","Sum_of_2018_Sales"))
df$PartNo <- na.locf(df$PartNo)

df$Sum_of_2018_Sales <- replace(df$Sum_of_2018_Sales,is.na(df$Sum_of_2018_Sales),0)

df$dummy <- ifelse(grepl('Total',df$Group)==FALSE,0,1)

df2 <- df[df$dummy==0,c(1:5),]

df3 <- df2 %>%
  group_by(Commmon_PartNo) %>%
  summarise(Sum_of_2018_Sales = sum(Sum_of_2018_Sales))
# head(df3)

# df4 <- summarise(group_by(df2,Commmon_PartNo),Sum_of_2018_Sales = sum(Sum_of_2018_Sales))
df4 <- df2 %>%
  group_by(Commmon_PartNo) %>%
  summarise(Sum_of_2018_Sales=sum(Sum_of_2018_Sales))

write.xlsx(df2,'aggregated sales with group.xlsx', colNames = TRUE)

