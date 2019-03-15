# Austin Water Quality Case Study

# Loading needed libraries into the environment 
library(tidyverse)
library(stringr)
library(lubridate)

# Reading the dataset
water <- read_csv('http://594442.youcanlearnit.net/austinwater.csv')

# dataset view
glimpse(water)


# Getting rid of a lot of columns that we don't need
#  building a new tibble with just siteName, siteType, parameter, result and unit

water <- tibble('siteName'=water$SITE_NAME,
                'siteType'=water$SITE_TYPE,
                'sampleTime'=water$SAMPLE_DATE,
                'parameterType'=water$PARAM_TYPE,
                'parameter'=water$PARAMETER,
                'result'=water$RESULT,
                'unit'=water$UNIT)

glimpse(water)

# looking at all unique parameter names

unique(water$parameter)

# searching for names that contain PH?

unique(water[which(str_detect(water$parameter,'PH')),]$parameter)

# Looking at parameter types

unique(water$parameterType)

# Filtering the dataset with conditions 

filtered_water <- subset(water,(parameterType=='Alkalinity/Hardness/pH') |
                                  parameterType=='Conventionals')

unique(filtered_water$parameter)

# Subsetting the dataset with 2 parameters

filtered_water <- subset(filtered_water, ((parameter=='PH') |
                                            (parameter=='WATER TEMPERATURE')))

glimpse(filtered_water)

# Summary of data
summary(filtered_water)

# converting some of these to factors
filtered_water$siteType <- as.factor(filtered_water$siteType)
filtered_water$parameterType <- as.factor(filtered_water$parameterType)
filtered_water$parameter <- as.factor(filtered_water$parameter)
filtered_water$unit <- as.factor(filtered_water$unit)

summary(filtered_water)

# And sampleTime should be a date/time object
filtered_water$sampleTime <- mdy_hms(filtered_water$sampleTime)

summary(filtered_water)

# Why are some of these measurements in feet?
subset(filtered_water,unit=='Feet')

# Looks like that is supposed to be Farenheit
convert <- which(filtered_water$unit=='Feet')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'

summary(filtered_water)

# What about the MG/L?
subset(filtered_water,unit=='MG/L')
subset(filtered_water,unit=='MG/L' & parameter=='PH')

convert <- which(filtered_water$unit=='MG/L' & filtered_water$parameter=='PH')
filtered_water$unit[convert] <- 'Standard units'

subset(filtered_water,unit=='MG/L')
subset(filtered_water,unit=='MG/L' & filtered_water$result>70)
convert <- which(filtered_water$unit=='MG/L' & filtered_water$result>70)
filtered_water$unit[convert] <- 'Deg. Fahrenheit'

subset(filtered_water,unit=='MG/L')
convert <- which(filtered_water$unit=='MG/L')
filtered_water$unit[convert] <- 'Deg. Celsius'

summary(filtered_water)

# looking quick and dirty look at all of our results
ggplot(filtered_water,mapping=aes(x=sampleTime, y=result)) +
  geom_point()

# There's clearly one large outlier
subset(filtered_water,result>1000000)

# Going to remove it and removing the NA result
remove <- which(filtered_water$result>1000000 | is.na(filtered_water$result))
filtered_water <- filtered_water[-remove,]

summary(filtered_water)

# Still some very high values, repeating the process
subset(filtered_water,result>1000)

remove <- which(filtered_water$result>1000 | is.na(filtered_water$result))
filtered_water <- filtered_water[-remove,]

summary(filtered_water)

# Looking at the data with boxplots

ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()

# Those Celsius values over 60 should probably  be Fahrenheit
# Because 60 degrees Celsius is 140 degrees Fahrenheit!

convert <- which(filtered_water$result>60 & filtered_water$unit=='Deg. Celsius')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'

# Let's look at the boxplots again

ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()

# Let's find our Fahrenheit values in the dataset
fahrenheit <- which(filtered_water$unit=='Deg. Fahrenheit')

# And convert them to Celsius
filtered_water$result[fahrenheit] <- (filtered_water$result[fahrenheit] - 32) * (5/9)

# Analysis after conversion 
ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()

# Fixing up the unit values
filtered_water$unit[fahrenheit] <- 'Deg. Celsius'

# And checking the plots again
ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()

# Final summary of the data
summary(filtered_water)

# Getting rid of some empty factor levels
filtered_water$unit <- droplevels(filtered_water$unit)
summary(filtered_water)

# Start the spread by looking at the data
summary(filtered_water)

# Getting rid of parameterType and unit
filtered_water <- filtered_water[,-c(4,7)]

filtered_water

# Try a spread
filtered_water_wide <- spread(filtered_water,parameter,result)

# Let's look at a few duplicates
filtered_water[c(49274, 49342,49219,49284),]

# Finding all the duplicate records in the dataset
dupe_check<-filtered_water[,-5]

# Finding which records are duplicates
dupes <- which(duplicated(dupe_check))

# removing those duplicates
filtered_water <- filtered_water[-dupes,]

# retry the spread
filtered_water_wide <- spread(filtered_water,parameter,result)
filtered_water_wide

# cleaning up those column names
colnames(filtered_water_wide)[4] <- 'pH'
colnames(filtered_water_wide)[4] <- 'temperature'

