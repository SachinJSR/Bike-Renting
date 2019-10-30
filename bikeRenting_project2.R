rm(list = ls())
# ------ Reading the data file -----------
#install.packages('dplyr')
data = read.csv('e:/Datascience_Data/day.csv')

##-- EDA --- Exploratory Data Analysis

head(data)

data$instant = NULL  # this is as good as index

str(data)  # checking the structure of the data

table(data$season)

# Seprating the continous and categorical Variables

categorical_vars = colnames(data)[2:8]  

continuous_vars = colnames(data)[9:15]


#-- Converting categorical variables into Factors
for (i in categorical_vars){
  data[,i] = as.factor(data[,i])
}

# lets check some visualisations EDA

#install.packages('ggplot2')  installing ggplot2 
library(ggplot2)  ## calling and loading the library

## checking the count W.R.T year
ggplot(data = data, aes(x = yr,y = cnt)) + geom_bar(stat = 'identity') # over the year bike hiring increased

## Now bike hiring w.r.t Season
#---------------------------------------------------------------------------------
ggplot(data=data, aes(x=season, y= cnt)) + geom_bar(stat = 'identity',color ='blue')

library(dplyr)

data %>% group_by(season) %>% summarise(sum(cnt)) ## using dplyr

aggregate(data$cnt, by=list(data$season),FUN=sum) ## normal Code of R
##--------------------------------------------------------------------------------

print(ggplot(data = data,aes(x = season,y = cnt, color = temp)) + geom_point())
#-- Above graph shows that when temprature is moderate bike hiring is more.
##--------------------------------------------------------------------------------

ggplot(data = data,aes(x = workingday,fill = workingday)) + geom_bar()

ggplot(data = data, aes(x = season, y = cnt, fill = workingday)) + geom_col(position='dodge')

data %>% filter(season==1) %>% group_by(workingday) %>% summarise(sum(cnt))

###----------------------------------------------
install.packages("gridExtra")
library(gridExtra)
#--------------------Outlier Analysis -----------
for (i in 1:length(continuous_vars))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (continuous_vars[i]), x = "cnt"), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=continuous_vars[i],x="cnt")+
           ggtitle(paste("Box plot of ",continuous_vars[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,ncol=1)

#------ Replacing the outlliers with NA
#-- and imputing using KNN

for(i in continuous_vars) {
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i]) $out]
  
  print(length(val))
  data[,i][data[,i] %in% val] = NA
}

data = knnImputation(data = data, k =3) # imputation

sum(is.na(data))  
##-------------------------------------------------------
###----------Feature Selection----
#install.packages("corrgram")
#install.packages('corrplot')
library(corrgram)
library(corrplot)
# -- for continous variables
corrgram(data,lower.panel = panel.pie,main='Correlation Plot')
corrplot(cor(data[continuous_vars]),method = 'color',diag = TRUE)


data = data %>% select(-c(atemp, casual, registered, dteday))
# dteday -- found not useful
# atemp -- highly corelated to temp
# casual,registered = casual + Registered = cnt 

# for categorical variables anova test

for (i in categorical_vars)
{
  print('-------Table starts here------------')
  print(i)
  mod =(lm(cnt ~ data[,i], data = data))
  print(summary(mod))
  print('-------Table ends here------------')
}

data = data %>% select(-c(holiday,workingday)) # after comparing the p value

##------------------------------------------------------------------------------

str(data) # -----now checking the structure of data again after changes.

#------------------------------------------------------------------------
# updating the the continous_vars and Categorical_vars

continuous_vars = colnames(data[sapply(data,is.numeric)])

categorical_vars = colnames(data[sapply(data,function(x){is.factor(x)})])
#-------------------------------------------------------------------------
##--Feature Scaling 

hist(data$temp)
hist(data$hum)
hist(data$windspeed)

# data seems distributed 
#---------------------------------------------------------------------------

#--------------------MOdeling 
#-Sampling data
#install.packages('caTools')
library(caTools)
set.seed(101)
sample = sample.split(data$cnt, SplitRatio = 0.7)
train = subset(data, sample== TRUE)
test = subset(data, sample ==FALSE)
#---------------------------------------------------------------------------
#-----____________RandomForest____-----------------------
#install.packages('randomForest')
library(randomForest)
#--preparing model with training data
rfModel = randomForest(cnt ~., data = train, importance = TRUE, ntree = 100)
plot(rfModel)
rfPredict = predict(rfModel,test[,names(test) !='cnt'])

plot(test$cnt, rfPredict, xlab = 'Actual values', ylab = 'Predicted values', main = 'RF model')

#install.packages("caret")
library(caret)
postResample(rfPredict, test$cnt)

library(DMwR)
regr.eval(test$cnt,rfPredict,stats = c('mae','mse','rmse','mape'))

###----- Linear Regression
#install.packages("dummies")
library(dummies)

lrModel = lm(cnt ~., data=train) # Model preparation with training data
summary(lrModel)

lrPredict = predict(lrModel,test[,names(test) != 'cnt'])
plot(test$cnt,lrPredict, xlab = 'ActualValues', ylab = 'Pred Values', main = 'LR Model')

regr.eval(test$cnt,lrPredict,stats = c('mae','mse','rmse','mape'))
postResample(lrPredict, test$cnt)





