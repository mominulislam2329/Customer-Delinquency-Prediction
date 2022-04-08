
##Title: "Mid Term Project STAT 551"##



#########################LOADING LIBRARIES#################################################
library(ggplot2)
library(rpart.plot)
library(rpart)
library(earth)
library(maptree)
library(gains)
library(dplyr)
library(tidyverse)
library(corrplot)
library(faraway)
library(naniar)
library(earth)
library(Rprofet)
library(caret)
library(readr)
library(viridis)
library(gridExtra)
library(kableExtra)
library(patchwork)
library(ggeasy)
library(GGally)
library(ggsci)
library(ROCR)
library(tidyverse)
library(DataExplorer)

#########################LOADING DATA#################################################
data = read.csv("/Users/pramishathapaliya/Downloads/prosper.csv")

# data <- data %>% 
#         mutate(MonthsSinceDQ = na_if(MonthsSinceDQ , 999))  ## Converting 999 as missing values
data[data==""] <- NA    ## Converting blank spaces as missing values
data[data=="n/a"] <- NA  ## Converting character "n/a" as missing values
str(data)
summary(data)

## Visualization of  missing data
gg_miss_var(data, show_pct = T)+ 
  theme(panel.background = element_rect(fill = 'aliceblue'))+
  theme(panel.grid.major = element_line(colour = "grey80"))+
  theme(text=element_text(size=11,
                          family="Times"))

log.data <- as.data.frame(data%>%
                            dplyr::select(PrincipalBalance, AmountBorrowed, CurrentDelinquencies, Income, Bad))

plot_missing(log.data)


for(i in 1:ncol(log.data)){
  log.data[is.na(log.data[,i]), i] <- median(log.data[,i], na.rm = TRUE)
}
plot_missing(log.data)

## Selecting variables
newdata <- data %>% 
  dplyr::select(3:5,9,10,11:13,15:25,29:30)
str(newdata)

### Variable transformation
newdata$BorrowerState <- as.factor(newdata$BorrowerState)
newdata$IsBorrowerHomeowner <- as.factor(newdata$IsBorrowerHomeowner)
newdata$EmploymentStatus <- as.factor(newdata$EmploymentStatus)
newdata$Income <- as.factor(newdata$Income)

## Binning state variable

summary(newdata$BorrowerState)

levels(newdata$BorrowerState)[levels(newdata$BorrowerState) %in%  c("AK", "AZ", "CA", "CO", "HI", "MT", "NM", "NV", "OR", "UT", "WA", "WY")] <- "West"

levels(newdata$BorrowerState)[levels(newdata$BorrowerState) %in%  c("AL", "AR", "DC", "DE", "FL", "GA", "KY", "LA", "MD",  "MS", "NC", "OK", "SC", "TX", "VA", "WV")] <- "South"

levels(newdata$BorrowerState)[levels(newdata$BorrowerState) %in%  c("CT", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")] <- "Northeast"

levels(newdata$BorrowerState)[levels(newdata$BorrowerState) %in%  c("IL", "KS", "MI", "MN", "MO", "OH", "SD", "WI")] <- "Midwest"

summary(newdata$BorrowerState)   #Summary Data For State
summary(newdata$IsBorrowerHomeowner)      #Summary data for Homeowner

##Numeric Variable Analysis
library(psych)
## Selecting numeric variables
num.data <- newdata %>% 
  dplyr::select(2, 4:15,17,19,20,21)
str(num.data)


## Summary statistics of the continuous predictive variables of total sample and by Bad 


#data.continuous <- newdata %>% dplyr::select(1:2, 5:14, 16, 18, 20)
num.data$Bad <- as.factor(num.data$Bad)
by(num.data, num.data$Bad, summary)
describeBy(num.data, num.data$Bad, digits = 2, mat = F)

## Total sample
describeBy(num.data, digits = 2, mat = F)

## Summary statistics of the categorical predictive variables of total sample and by Bad 


data.categorical <- newdata %>% dplyr::select(1,3,16,17,18,19,20,21)

data.categorical$Bad <- as.factor(data.categorical$Bad)
summary(data.categorical)

table(data.categorical$BorrowerState, data.categorical$Bad, useNA = "ifany")
table(data.categorical$IsBorrowerHomeowner, data.categorical$Bad, useNA = "ifany")
table(data.categorical$EmploymentStatus, data.categorical$Bad, useNA = "ifany")
table(data.categorical$Income, data.categorical$Bad, useNA = "ifany")

## Distribution of continuous variable

library(gridExtra)
library(ggplot2)

hist1 <- ggplot(num.data, aes(x=DebtToIncomeRatio)) + 
  geom_histogram(colour="black", fill="lightskyblue2")+
  geom_density() + 
  theme(panel.background = element_rect(fill = 'aliceblue'))+
  theme(text=element_text(size=11, 
                          family="Times"))

hist2 <- ggplot(num.data, aes(x=AmountBorrowed)) + 
  geom_histogram(colour="black", fill="lightskyblue2")+
  geom_density() + 
  theme(panel.background = element_rect(fill = 'aliceblue'))+
  theme(text=element_text(size=11, 
                          family="Times"))
grid.arrange(hist1, hist2,  ncol=2)
##################Need to add more numeric Variables############

## Exploring borrower characteristics

## by Geographical region

cTab.1 <- table(data.categorical$BorrowerState, data.categorical$Bad, useNA = "ifany")
margin.1 <- addmargins(cTab.1, 2, FUN=sum)
margin.1 <- as.data.frame(margin.1)
colnames(margin.1) <- c("BorrowerState", "Category", "Frequency")
levels(margin.1$Category)[levels(margin.1$Category) %in%  c("sum")] <- "Total Sample"

ggplot(margin.1, aes(x=BorrowerState, y=Frequency, fill=factor(Category)))+ 
  geom_bar(stat="identity",position="dodge", colour="black")+
  geom_text(aes(label = Frequency), position=position_dodge(width=0.9), vjust=-0.5, family="Times", size=3) +
  theme(panel.background = element_rect(fill = 'aliceblue'))+
  theme(text=element_text(size=11, 
                          family="Times"))+ 
  scale_fill_discrete("Factor")+
  xlab("Geographical region") +
  theme(axis.title.x = element_text(vjust=0.1))+ scale_fill_manual(values = c("lightskyblue2", "#E7B800", "#4E84C4"))

## by homeownership

cTab.4 <- table(data.categorical$IsBorrowerHomeowner, data.categorical$Bad, useNA = "ifany")
margin.4 <- addmargins(cTab.4, 2, FUN=sum)
margin.4 <- as.data.frame(margin.4)
colnames(margin.4) <- c("Home", "Category", "Frequency")
levels(margin.4$Category)[levels(margin.4$Category) %in%  c("sum")] <- "Total Sample"

ggplot(margin.4, aes(x=Home, y=Frequency, fill=factor(Category)))+ 
  geom_bar(stat="identity",position="dodge", colour="black")+
  geom_text(aes(label = Frequency), position=position_dodge(width=0.9), vjust=-0.5, family="Times", size=3) +
  theme(panel.background = element_rect(fill = 'aliceblue'))+
  theme(text=element_text(size=11, 
                          family="Times"))+ 
  scale_fill_discrete("Factor")+
  xlab("Home Ownership Type") +
  theme(axis.title.x = element_text(vjust=0.1))+ scale_fill_manual(values = c("lightskyblue2", "#E7B800", "#4E84C4"))

## by Employment Status

cTab.5 <- table(data.categorical$EmploymentStatus, data.categorical$Bad, useNA = "ifany")
margin.5 <- addmargins(cTab.5, 2, FUN=sum)
margin.5 <- as.data.frame(margin.5)
colnames(margin.5) <- c("Home", "Category", "Frequency")
levels(margin.5$Category)[levels(margin.5$Category) %in%  c("sum")] <- "Total Sample"

ggplot(margin.5, aes(x=Home, y=Frequency, fill=factor(Category)))+ 
  geom_bar(stat="identity",position="dodge", colour="black")+
  geom_text(aes(label = Frequency), position=position_dodge(width=0.9), vjust=-0.5, family="Times", size=3) +
  theme(panel.background = element_rect(fill = 'aliceblue'))+
  theme(text=element_text(size=11, 
                          family="Times"))+ 
  scale_fill_discrete("Factor")+
  xlab("Employment Status") +
  theme(axis.title.x = element_text(vjust=0.1))+ scale_fill_manual(values = c("lightskyblue2", "#E7B800", "#4E84C4"))

### Correlation plot of variables

# cor.dtat <- data.continuous %>% select(2:8, 11:12, 15, 14)
# str(cor.dtat)

library(Hmisc)
dat = newdata
dat$DebtToIncomeRatio = impute(dat$DebtToIncomeRatio, 
                               median)
summary(dat$DebtToIncomeRatio)
which(is.na(dat$DebtToIncomeRatio))

dat$CurrentDelinquencies = impute(dat$CurrentDelinquencies, 
                                  median)
summary(dat$CurrentDelinquencies)
which(is.na(dat$CurrentDelinquencies))


dat$DelinquenciesLast7Years = impute(dat$DelinquenciesLast7Years, 
                                     median)
summary(dat$DelinquenciesLast7Years)
which(is.na(dat$DelinquenciesLast7Years))

dat$PublicRecordsLast10Years = impute(dat$PublicRecordsLast10Years, 
                                      median)
summary(dat$PublicRecordsLast10Years)
which(is.na(dat$PublicRecordsLast10Years))


dat$TotalCreditLines = impute(dat$TotalCreditLines, 
                              median)
summary(dat$TotalCreditLines)
which(is.na(dat$TotalCreditLines))

dat$InquiriesLast6Months = impute(dat$InquiriesLast6Months, 
                                  median)
summary(dat$InquiriesLast6Months)
which(is.na(dat$InquiriesLast6Months))


dat$AmountDelinquent = impute(dat$AmountDelinquent, 
                              median)
summary(dat$AmountDelinquent)
which(is.na(dat$AmountDelinquent))


dat$PublicRecordsLast12Months = impute(dat$PublicRecordsLast12Months, 
                                       median)
summary(dat$PublicRecordsLast12Months)
which(is.na(dat$PublicRecordsLast12Months))


dat$CurrentCreditLines = impute(dat$CurrentCreditLines, 
                                median)
summary(dat$CurrentCreditLines)
which(is.na(dat$CurrentCreditLines))

dat$OpenCreditLines = impute(dat$OpenCreditLines, 
                             median)
summary(dat$OpenCreditLines)
which(is.na(dat$OpenCreditLines))

dat$RevolvingCreditBalance = impute(dat$RevolvingCreditBalance, 
                                    median)
summary(dat$RevolvingCreditBalance)
which(is.na(dat$RevolvingCreditBalance))

dat$BankcardUtilization = impute(dat$BankcardUtilization, 
                                 median)
summary(dat$BankcardUtilization)
which(is.na(dat$BankcardUtilization))

dat$LengthStatusMonths = impute(dat$LengthStatusMonths, 
                                median)
summary(dat$LengthStatusMonths)
which(is.na(dat$LengthStatusMonths))

dat$LengthStatusMonths = impute(dat$LengthStatusMonths, 
                                median)
summary(dat$LengthStatusMonths)
which(is.na(dat$LengthStatusMonths))


dat$PrincipalBalance = impute(dat$PrincipalBalance, 
                              median)
summary(dat$PrincipalBalance)
which(is.na(dat$PrincipalBalance))


dat$EmploymentStatus = impute(dat$EmploymentStatus, 
                              median)
summary(dat$EmploymentStatus)
which(is.na(dat$EmploymentStatus))


head(dat,20)
table(is.na(dat))

#Binning

```{r}
train.bins<- log.data%>%
  dplyr::mutate(PrincipalBalance_Bins=cut(PrincipalBalance, breaks=4, right = F),
                AmountBorrowed_Bins=cut(AmountBorrowed, breaks=5, right = F),
                CurrentDelinquencies_Bins=cut(CurrentDelinquencies, breaks=3, right = F),
                Income_Bins=cut(Income, breaks=6, right = F),
  )%>%
  dplyr::select(Bad, PrincipalBalance_Bins, AmountBorrowed_Bins, CurrentDelinquencies_Bins, Income_Bins)

WOEplotter(dat=train.bins,var = 'PrincipalBalance_Bins',target = 'Bad')
WOEplotter(dat=train.bins,var = 'AmountBorrowed_Bins',target = 'Bad')
WOEplotter(dat=train.bins,var = 'CurrentDelinquencies_Bins',target = 'Bad')
WOEplotter(dat=train.bins,var = 'Income_Bins',target = 'Bad')

custRetention <- as.data.frame(train.bins)

#Spliting data to train and test
set.seed(222)
index <- sample(1:nrow(train.bins), ceiling(0.6*nrow(train.bins)))
custRet_train <- train.bins[index,]
custRet_validate <- train.bins[-index,]

#Mars Model
mars_model <- earth(Bad ~ ., 
                    data = custRetention, glm = list(family="binomial"), degree = 1)
summary(mars_model)

kableExtra::kable(caret::varImp(mars_model),caption = "Table.1: Earth variable importance") %>%
  kableExtra::kable_classic(html_font="Times New Roman")#Predictors selected by the MARS model

kable(round(cbind(rss=mars_model$rss, 
                  rsq=mars_model$rsq, 
                  gcv=mars_model$gcv, 
                  grsq=mars_model$grsq),3), 
      caption="Mars Model Summary")%>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 12)

mars_prediction_train <- predict(mars_model, type = "response", newdata = custRet_train)
mars_prediction_validate <- predict(mars_model, type = "response", newdata = custRet_validate)

#on training dataset 
my_predictions_marsT <- prediction(mars_prediction_train, custRet_train$Bad, label.ordering = NULL)
roc_perfT <- performance(my_predictions_marsT, measure = "tpr", x.measure = "fpr")

auc_perf_marsT <- performance(my_predictions_marsT, measure = "auc")
auc_train <- as.numeric(auc_perf_marsT@y.values)


my_predictions_mars <- prediction(mars_prediction_validate, custRet_validate$Bad, label.ordering = NULL)
roc_perf <- performance(my_predictions_mars, measure = "tpr", x.measure = "fpr")

#
auc_perf_mars <- performance(my_predictions_mars, measure = "auc")
auc_valid <- as.numeric(auc_perf_mars@y.values)

#ROC Data
roc_data <- as.data.frame(cbind(
  trainx=roc_perfT@x.values[[1]],
  trainy=roc_perfT@y.values[[1]],
  validx=roc_perf@x.values[[1]],
  validy=roc_perf@y.values[[1]]
))
#roc curve 
mars_roc <- ggplot(roc_data) + 
  geom_line(size=1, col="firebrick",aes(x=trainx, y=trainy)) + 
  geom_line(size=1,col= "blue", aes(x=validx, y=validy)) + 
  theme_minimal() + 
  geom_abline(intercept = 0, linetype = "dashed", size=1) + 
  annotate(geom = "text", y=1, x=0.15, size=6, color="brown", family="Cochin", label = glue::glue("Training AUC = {round(auc_train,2)}")) +
  annotate(geom = "text", y=0.92, x=0.15, size=6, color="brown", family="Cochin", label = glue::glue("Validation AUC = {round(auc_valid,2)}")) + xlab("False Positive Rate") + ylab("True Positive Rate") + labs(title=("ROC for MARS Model (Traning vs Validation)")) + 
  theme(plot.title = element_text(hjust = 0.5))
mars_roc

## MARS prediction
mars.pred=predict(mars_model,type='response', newdata=custRet_validate)
## Gains table and plot of MARS model

library(gains)
par(family="Times")
mars.gains <- gains(custRet_validate$Bad, mars.pred)
mars.gains
plot(mars.gains)


## Cumulative gains plot
par(family="Times")
plot(c(0,mars.gains$depth), c(0,mars.gains$cume.pct.of.total*100), 
     pch = 16, col = 'blue', xlab = '% Borrowers', 
     ylab = '% Positive Response', main = 'Cumulative Gains Chart')
lines(c(0,mars.gains$depth), c(0,mars.gains$cume.pct.of.total*100),col = 'blue')


par(family="Times")
## Lift plot
plot(mars.gains$depth, mars.gains$cume.lift/100, 
     ylim = c(0,4), xlim = c(10,100), pch = 16, col = 'blue',
     xlab = '% Borrowers', ylab = 'Lift', main = 'Lift Chart')
lines(mars.gains$depth, mars.gains$cume.lift/100, col = 'blue')

log_model <- glm(Bad ~ ., data = custRet_train, family = "binomial")

log_summary <- summary(log_model)
log_summary


logit.pred=predict(log_model,newdata = custRet_validate, type = "response")

length(logit.pred)


### Gains plot of logistic regression

library(gains)
par(family="Times")
logit.gains <- gains(custRet_validate$Bad, logit.pred)
logit.gains
plot(logit.gains)


## Cumulative gains plot

par(family="Times")
plot(c(0,logit.gains$depth), c(0,logit.gains$cume.pct.of.total*100), 
     pch = 16, col = 'blue', xlab = '% Borrowers', 
     ylab = '% Positive Response', main = 'Cumulative Gains Chart')
lines(c(0,logit.gains$depth), c(0,logit.gains$cume.pct.of.total*100),col = 'blue')

#----predict from logistic model------


#predict new values on training and validation dataset 
log_prediction_train <- predict(log_model, type = "response", newdata = custRet_train)
log_prediction_validate <- predict(log_model, type = "response", newdata = custRet_validate)

#on training dataset 
my_predictions_logT <- prediction(log_prediction_train, custRet_train$Bad, label.ordering = NULL)
roc_perf_logT <- performance(my_predictions_logT, measure = "tpr", x.measure = "fpr")


auc_perf_logT <- performance(my_predictions_logT, measure = "auc")
log_auc_train <- as.numeric(auc_perf_logT@y.values)

my_predictions_log <- prediction(log_prediction_validate, custRet_validate$Bad, label.ordering = NULL)
roc_perf_log <- performance(my_predictions_log, measure = "tpr", x.measure = "fpr")


auc_perf_log <- performance(my_predictions_log, measure = "auc")
log_auc_valid <- as.numeric(auc_perf_log@y.values)


#ROC Data
roc_data_log <- as.data.frame(cbind(
  trainx=roc_perf_logT@x.values[[1]],
  trainy=roc_perf_logT@y.values[[1]],
  validx=roc_perf_log@x.values[[1]],
  validy=roc_perf_log@y.values[[1]]
))
#roc curve 
logit_roc <- ggplot(roc_data_log) + 
  geom_line(size=1, col="firebrick",aes(x=trainx, y=trainy)) + 
  geom_line(size=1,col= "blue", aes(x=validx, y=validy)) + 
  theme_minimal() + 
  geom_abline(intercept = 0, linetype = "dashed", size=1) + 
  annotate(geom = "text", y=1, x=0.15, size=6, color="brown", family="Cochin", label = glue::glue("Training AUC = {round(log_auc_train,2)}")) +
  annotate(geom = "text", y=0.92, x=0.15, size=6, color="brown", family="Cochin", label = glue::glue("Validation AUC = {round(log_auc_valid,2)}")) + 
  xlab("False Positive Rate") + 
  ylab("True Positive Rate") + 
  labs(title=("ROC for Logistic Model (Traning vs Validation)")) + 
  theme(plot.title = element_text(hjust = 0.5))


logit_roc

#----KS charts----

test <- as.data.frame(cbind(roc_perf_log@x.values[[1]], roc_perf_log@y.values[[1]]))
Percentile <- NULL
Difference <- NULL 
for (i in 1:nrow(test)){
  test[i, 3] = i/nrow(test)
  test[i, 4]= abs(test[i,2]-test[i,1])
}
colnames(test) <- c("FPR", "TPR", "Percentile", "Difference")

#Row with the maximum difference
max_diff <- test[test$Difference==max(test$Difference),]

#Maximum Difference
#max_diff$Difference

logit_ks <- ggplot(test) + 
  geom_line(aes(x=Percentile, y=TPR), col="firebrick", size=1) +
  geom_line(aes(x=Percentile, y=FPR), col="blue", size=1) +
  geom_abline(intercept = 0, linetype="dashed") +
  geom_vline(xintercept = max_diff$Difference, linetype="dashed") +
  labs(title = "KS Chart for Logit Predictions", y="TPR/FPR") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text", y=0.92, x=max_diff$Difference, size=6, color="brown", family="Cochin", label = glue::glue("D = {round(max_diff$Difference,2)}"))


test <- as.data.frame(cbind(roc_perf@x.values[[1]], roc_perf@y.values[[1]]))
Percentile <- NULL
Difference <- NULL 
for (i in 1:nrow(test)){
  test[i, 3] = i/nrow(test)
  test[i, 4]= abs(test[i,2]-test[i,1])
}
colnames(test) <- c("FPR", "TPR", "Percentile", "Difference")


max_diff <- test[test$Difference==max(test$Difference),]

#Maximum Difference
#max_diff$Difference

mars_ks <- ggplot(test) + 
  geom_line(aes(x=Percentile, y=TPR), col="firebrick", size=1) +
  geom_line(aes(x=Percentile, y=FPR), col="blue", size=1) +
  geom_abline(intercept = 0, linetype="dashed") +
  geom_vline(xintercept = max_diff$Difference, linetype="dashed") +
  labs(title = "KS Chart for MARS Predictions", y="TPR/FPR") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text", y=0.92, x=max_diff$Difference, size=6, color="brown", family="Cochin", label = glue::glue("D = {round(max_diff$Difference,2)}"))


((mars_ks + logit_ks))

