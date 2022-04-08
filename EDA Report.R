#########################LOADING LIBRARIES#################################################
library(ggplot2)
library(rpart.plot)
library(rpart)
library(earth)
library(caret)
library(maptree)
library(gains)
library(dplyr)
library(ROCR)
library(tidyverse)
library(corrplot)
library(faraway)


############################LOADING DATA##################################################
init.dat <- read.csv("/Users/md.mominul.islam/OneDrive - South Dakota State University - SDSU/STAT 551 Predictive Analytics/Mid Term Project/Spring 2022 Project/prosper.csv")
#deleting all missing values
init.dat <- na.omit(init.dat)
#selecting variables important in nature
new.dat<- init.dat[,c(4,5,9,11,12,13,16,21,22,23,24,25,27,29,30)]
head(new.dat)
new.dat$Income<- as.factor(new.dat$Income)
########################Summary statistics of all numeric variables#############
num.var <- c("DebtToIncomeRatio","AmountBorrowed",
             "CurrentDelinquencies","DelinquenciesLast7Years",
             "PublicRecordsLast10Years", "InquiriesLast6Months" ,
             "RevolvingCreditBalance","BankcardUtilization",
             "LengthStatusMonths","PrincipalBalance")
a.data <- new.dat[,num.var]
summary(a.data)

########Summary Statistics based on employment status###################
new.dat %>%
  group_by(EmploymentStatus) %>%
  summarise(across(where(is.numeric), mean))

########Summary Statistics based on Homeownership###################
new.dat %>%
  group_by(IsBorrowerHomeowner) %>%
  summarise(across(where(is.numeric), mean))

####################Plotting Histogram for all Numeric Variables############
plotHist <- function(columns,bin,colours){
  par(mfrow = c(3,4))#Histogram plots to visualize the distribution of the numeric variables in the data set.
  for (i in columns) {
    hist(a.data[,i], main = paste("Histogram of ", names(numeric_vars)[i]),
         nclass = bin, las = 1, col = colours, 
         xlab = paste(names(numeric_vars)[i]))
  }
}
plotHist(c(1:10), bin = 60, "turquoise2")

#############################Box-plot of Numeric variables#####################
new.dat %>%
  select(DebtToIncomeRatio,AmountBorrowed,CurrentDelinquencies,DelinquenciesLast7Years,
         BankcardUtilization,LengthStatusMonths,PrincipalBalance,
         Bad) %>% 
  mutate(Bad = recode_factor(Bad,
                             `0` = "Good", 
                             `1` = "Bad" )) %>%
  gather(key   = "key", 
         value = "value",-Bad) %>%
  
  #Distribution of the Bad variable (0=Good,1=Bad) among the numeric variables
  
  ggplot(aes(y = value)) +
  geom_boxplot(aes(fill = Bad),
               alpha  = .6,
               fatten = .7) + 
  labs(x = "",
       y = "",
       title = "Boxplots of Numeric Variables") +
  scale_fill_manual(
    values = c("turquoise2","blue"),
    name   = "Variable\nBad",
    labels = c("Good", "Bad")) +
  theme_bw() +
  facet_wrap(~ key, 
             scales = "free", 
             ncol   = 4)


#########################UNIVARIATE DATA ANALYSIS##################

### Debt Income Ratio distribution
summary(new.dat$DebtToIncomeRatio) # simple summary stat for debt to income ratio
##same plot with binning at 100
ggplot(data=new.dat,aes(DebtToIncomeRatio)) +
  geom_histogram(bins= 100,color=I('black'),fill=I('#F79420')) + 
  xlim(0,1.5)

################################## Correlation matrix##################################
library(corrplot)
library(RColorBrewer)
M <-cor(a.data)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
############Pairs Plot########################
library(GGally)
ggpairs(a.data)

###########################Categorical Data Analysis##########################
## IsBorrowerHomeOwner Column
unique(new.dat$IsBorrowerHomeowner)
tab.home<- table(new.dat$IsBorrowerHomeowner)
prop.table(tab.home)*100 # proportion for a single variable table
tab.home
##Enrolled column
tab.employ <- table(new.dat$EmploymentStatus)
tab.employ
prop.table(tab.employ)*100 # proportion for a single variable table
#######Cross table by two variables##################
xtab1 <- xtabs(~ IsBorrowerHomeowner +EmploymentStatus, new.dat)
xtab1

xtab2 <- xtabs(~ EmploymentStatus+Income, new.dat)
xtab2

xtab3 <- xtabs(~ IsBorrowerHomeowner+ LpStatus, new.dat)
xtab3


#######Cross table by two variables##################
xtab1 <- xtabs(~ IsBorrowerHomeowner +EmploymentStatus, new.dat)
xtab1

xtab2 <- xtabs(~ EmploymentStatus+Income, new.dat)
xtab2

xtab3 <- xtabs(~ IsBorrowerHomeowner+ LpStatus, new.dat)
xtab3

#######################Bad/Good Accounts In Prosper Data###################
bad<-new.dat[,-c(2,6,7,9)] %>% 
  filter(Bad==1)

good<-new.dat[,-c(2,6,7,9)] %>%
  filter(Bad==0)


counts<-function(x,y,z){
  n=nrow(x)
  B_acc=nrow(y)
  G_acc=nrow(good)
  P_B_acc=(B_acc/n)*100
  P_G_acc=((G_acc)/n)*100
  return(list(num_bad_account=B_acc,num_good_account=G_acc,
              percent_bad_account=P_B_acc,percent_good_account=P_G_acc))
}
counts(new.dat,bad,good)

######################variable selection for bad and good#############
bad %>% #Computing the summary statistics of AmountBorrowed,Income,PrincipalBalance,CurrentDelinquencies
  select(AmountBorrowed,Income,PrincipalBalance,RevolvingCreditBalance,EmploymentStatus) %>%
  summary()

good %>% #Computing the summary statistics of AmountBorrowed,Income,PrincipalBalance,CurrentDelinquencies
  select(AmountBorrowed,Income,PrincipalBalance,RevolvingCreditBalance,EmploymentStatus) %>%
  summary()


#################Barplot for Bad Customers########################
#Bar plot on Income
ggplot(bad, aes(x =Income)) + 
  geom_bar(color = "white", fill = "#A4AA83", binwidth = .5) + 
  theme_minimal() + 
  labs(title = "Barplot for Income on Bad Loan Customers", x = "Income", 
       y = "Frequency")

#Bar plot on Employment Status
ggplot(bad, aes(x =EmploymentStatus)) + 
  geom_bar(color = "white", fill = "#A4AA83", binwidth = 0.5) + 
  theme_minimal() + 
  labs(title = "Barplot for Employment Status on Bad Loan Customers", x = "Income", 
       y = "Frequency")

#################Barplot for good Customers########################
#Bar plot on Income
ggplot(good, aes(x =Income)) + 
  geom_bar(color = "white", fill = "#A4AA83", binwidth = .5) + 
  theme_minimal() + 
  labs(title = "Barplot for Income on Bad Loan Customers", x = "Income", 
       y = "Frequency")

#Bar plot on Employment Status
ggplot(good, aes(x =EmploymentStatus)) + 
  geom_bar(color = "white", fill = "#A4AA83", binwidth = 0.5) + 
  theme_minimal() + 
  labs(title = "Barplot for Employment Status on Bad Loan Customers", x = "Income", 
       y = "Frequency")


#Histogram of amount borrowed by bad customers
ggplot(bad, aes(x =AmountBorrowed)) + 
  geom_histogram(color = "white", fill = "#A4AA83") + 
  theme_minimal() + 
  labs(title = "Histogram for Amount Borrowed by Bad Loan Customers", x = "Amount of Money Borrowed", 
       y = "Frequency") + xlim(0,35000)

#Histogram of amount borrowed by good customers
ggplot(good, aes(x =AmountBorrowed)) + 
  geom_histogram(color = "white", fill = "#A4AA83") + 
  theme_minimal() + 
  labs(title = "Histogram for Amount Borrowed by Good Loan Customers", x = "Amount of Money Borrowed", 
       y = "Frequency") + xlim(0,35000)

#Histogram of amount borrowed > 5000 by bad customers
A<-bad %>% 
  filter(AmountBorrowed >=3000) %>%
  ggplot(aes(AmountBorrowed)) + geom_histogram(color="white",fill="#A4AA83") + 
  theme_minimal() + labs(title = "AmountBorrowed >= 3000 on Bad Account",
                         x="AmountBorrowed >= 3000",y="Frequency") + xlim(3000,25000)

B<-good %>% 
  filter(AmountBorrowed >= 3000) %>%
  ggplot(aes(AmountBorrowed)) + geom_histogram(color="white",fill="#A4AA83") + 
  theme_minimal() + labs(title = "AmountBorrowed >= 3000 on Good Account",
                         x="AmountBorrowed >= 3000",y="Frequency") + xlim(3000,25000)
gridExtra::grid.arrange(A,B,ncol=2)