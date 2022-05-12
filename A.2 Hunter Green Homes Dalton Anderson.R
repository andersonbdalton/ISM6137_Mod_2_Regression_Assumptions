#Dalton Anderson

rm(list=ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(corrplot)
library(stargazer)
library(PerformanceAnalytics)
#preprocessing

#Load in dataset
#Import data
df_master <- read_excel("HuntersGreendfales.xlsx")
colnames(df_master)=tolower(make.names(colnames(df_master)))
#drop excluded variables
df =select (df_master,-c(address,status,pendingdate,datesold,subdivn,pendingdate))
#check for na
colSums(is.na(df))
#need to make a default value for garages and spa
#df <- df %>% replace(df$spa =="NA", FALSE) # replace with NA
#df[is.na(df)] <- '0'
#df[df$spa == 'NA'] <- 'FALSE'
#df1 <- df %>%
#  mutate(spa = replace(spa, 0, "FALSE"))

df <- df %>% mutate(spa = ifelse(is.na(spa), 'FALSE', spa),
                          garages = ifelse(is.na(garages), 0, garages))
#check work
unique(df$spa)
unique(df$garages)
#nulls removed

#fix roof data
df$roof[df$roof=='Slate, Tile']<-'Tile'
df$roof[df$roof=='Shake, Shingle']<-'Shingle'
df$roof[df$roof=='Built-Up']<-'Other'
df$roof[df$roof=='Shingle, Tile']<-'Tile'
df$roof[df$roof=='Concrete, Tile']<-'Tile'
#check work
unique(df$roof)


#turn variables into a factors for model
df$roof  <- factor(df$roof)
df$roof<- relevel(df$roof, "Shingle")

df$pool  <- factor(df$pool)
df$pool<- relevel(df$pool, "None")

df$spa  <- factor(df$spa)
df$spa<- relevel(df$spa, "FALSE")

df$splsale  <- factor(df$splsale)
df$splsale<- relevel(df$splsale, "None")

df$state=as.factor(df$beds)
df$state=as.factor(df$bathsfull)
df$state=as.factor(df$bathshalf)
df$state=as.factor(df$bathstotal)
df$state=as.factor(df$garages)

str(df)

#2.) Run three reasonable models for each DV. 
#You may have to do some feature engineering before running the model. 
#Present each model and summarize their output in a compact manner using stargazer. 
#(2 points for feature engineering + 1 point for modeling)

#sample set created
tempdf = df
set.seed(59657076)
sample <- tempdf
dfSam = sample_n(sample, 350)

#Descriptive Statistics - Data Visualization
hist(df$pricesold, breaks=20, prob=T, main="Histogram of Sale Price of df") 
den <- density(df$pricesold)                    
lines(den, col="red")

#some concerning outliers
#caution when considering to remove observations from the data
#I am not sure there seems to be a vaild reason why the data points are outliers
boxplot(df)

tempdf <- cbind(df$preicesold, df$sqft, df$adom, df$lppersqft, df$pricesold
                , df$sppersqft)
cor(tempdf)
corrplot(cor(tempdf), method="circle")      

pairs(df)  
chart.Correlation(tempdt)

#base model 
m1 <- lm(pricesold~sqft+lppersqft+roof+sppersqft+lotsqft+yrblt, data = df)
summary(m1)

#model with bathroom and bedroom
m2 <- lm(pricesold~sqft+lppersqft+roof+sppersqft+lotsqft+yrblt+bathstotal+beds, data = df)
summary(m2)

#model with all of the extras
m3 <- lm(pricesold~sqft+lppersqft+roof+sppersqft+lotsqft+yrblt+bathstotal+beds+garages+spa, data = df)
summary(m3)

stargazer(m1, m2, m3, type="text", single.row=TRUE)

#LINE

# linearity of the data
plot(m3, 1)

#I am okay with this

# normality of residuals
hist(m3$residuals)
plot(simple_model, 2)


#homoscedasticity assumption
plot(m3, 3)


#Cook's distance
plot(m3, 4)
#values over .5 but was explained

#Residuals vs Leverage
plot(m3, 5)
plot(m3, 6)

#failed homoscedasticity assumption
#fix with log
df$pricesoldlog <- log(df$pricesold)

# linearity of the data
plot(m3, 1)

#I am okay with this

# normality of residuals
hist(m3$residuals)
plot(simple_model, 2)


#homoscedasticity assumption
plot(m3, 3)


#Cook's distance
plot(m3, 4)
#values over .5 but was explained

#Residuals vs Leverage
plot(m3, 5)
plot(m3, 6)

#log model
m3 <- lm(pricesold~sqft+lppersqft+roof+sppersqft+lotsqft+yrblt+bathstotal+beds+garages+spa+pricesoldlog, data = df)
summary(m3)

summary(m3)

stargazer(m1, m2, m3, type="text", single.row=TRUE)

summary(m3)

#base model 
mm1 <- lm(adom_agentdaysonmarket~sqft+lppersqft+roof+sppersqft+lotsqft+yrblt, data = df)
summary(mm1)

#model with bathroom and bedroom
mm2 <- lm(adom_agentdaysonmarket~sqft+lppersqft+roof+sppersqft+lotsqft+yrblt+bathstotal+beds, data = df)
summary(mm2)

#model with all of the extras
mm3 <- lm(adom_agentdaysonmarket~sqft+lppersqft+roof+sppersqft+lotsqft+yrblt+bathstotal+beds+garages+spa, data = df)
summary(mm3)

stargazer(mm1, mm2, mm3, type="text", single.row=TRUE)

# linearity of the data
plot(mm3, 1)

#I am okay with this

# normality of residuals
hist(mm3$residuals)
plot(simple_model, 2)


#homoscedasticity assumption
plot(mm3, 3)


#Cook's distance
plot(mm3, 4)
#values over .5 but was explained

#Residuals vs Leverage
plot(mm3, 5)
plot(mm3, 6)

#failed homoscedasticity assumption
#fix with log
df$adom_agentdaysonmarketlog <- log(df$adom_agentdaysonmarket)

#log model
mm3 <- lm(pricesold~sqft+lppersqft+roof+sppersqft+lotsqft+yrblt+bathstotal+beds+garages+spa+adom_agentdaysonmarketlog, data = df)
summary(mm3)

summary(mm3)

stargazer(mm1, mm2, mm3, type="text", single.row=TRUE)

summary(mm3)




