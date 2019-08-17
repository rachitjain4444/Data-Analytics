#Objective:
#Factors affecting %Votes polled in UP state

#Setting working directory
setwd("/Users/rachitjain/Desktop/BACP/Projects/Project5-Predective Modeling")

#Converting myneta website data into csv
#library(XML)
#url="http://myneta.info/uttarpradesh2017/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary"
#table=readHTMLTable(url,which = 3)
#write.csv(table,file = "/Users/rachitjain/Desktop/BACP/Projects/Project5-Predective Modeling/MyNeta.csv")

#Check both csv files ad make the coloumn name same manually
#Reading data from My neta website and converting all values to lowercase
data1=read.csv("MyNeta.csv",header = T)
data1=lapply(data1,tolower)
head(data1)

#Reading downloaded data from Election commision of India website and converting all values to lowercase
data=read.csv("Detailed Results.csv",header = T)
data=lapply(data,tolower)
tail(data)

#Joining both the data on candidate name using outer join
library(plyr)
final=merge(x=data, y=data1, by="Candidate.Name")
summary(final)
head(final)
final <- subset(final, select = -c(Party.Name.y,Constituency.Name.y))

#Data Exploratary analysis
str(final)
dim(final)
nrow(final)
summary(final)
names(final)
#For describe use Hmisc
library(Hmisc)
describe(final)

library(DataExplorer)
plot_missing(final)
plot_correlation(final)

##Checking for missing values
sum(is.na(final))
#Checking coloumnwise blank values
as.matrix(sapply(final, function(x) sum(is.na(x))))

##Party Name distribution
prop.table(table(final$Candidate.Category))

library(plotrix)
pie3D(prop.table((table(final$Candidate.Category))), 
      main='Candidates Categories',
      labels=c("Others", "GEN", "SC", "ST"))

#Varibale type conversion
final$Constituency.No.=as.integer(final$Constituency.No.)
final$Candidate.Age=as.integer(final$Candidate.Age)
final$VALID.VOTES.POLLED.in.General=as.integer(final$VALID.VOTES.POLLED.in.General)
final$VALID.VOTES.POLLED.in.Postal=as.integer(final$VALID.VOTES.POLLED.in.Postal)
final$Total.Valid.Votes=as.integer(final$Total.Valid.Votes)
final$Total.Electors=as.integer(final$Total.Electors)
final$Total.valid.votes.polled..NOTA=as.integer(final$Total.valid.votes.polled..NOTA)
final$X..votes.polled=as.numeric(levels(final$X..votes.polled))[final$X..votes.polled]

#Converting Total.Assets and Liabilities to numeric
library(stringr)
final$Total.Assets=gsub("~.*","",final$Total.Assets)
final$Total.Assets=str_remove_all(final$Total.Assets, "rs")
final$Total.Assets = gsub("[^[:alnum:][:blank:]?&/\\-]", "", final$Total.Assets)
final$Total.Assets=gsub(",","",final$Total.Assets)
final$Total.Assets=as.numeric(final$Total.Assets)

final$Liabilities=str_remove_all(final$Liabilities, "[rs<ca>]")
final$Liabilities=gsub("~.*","",final$Liabilities)
final$Liabilities=gsub(",","",final$Liabilities)
final$Liabilities = gsub("[^[:alnum:][:blank:]?&/\\-]", "", final$Liabilities)
final$Liabilities=as.numeric(final$Liabilities)


##Checking individual variable

#Constituency.No.
summary(final$Constituency.No.)

#Constituency.Name
summary(final$Constituency.Name)

#Candidate.Name
#This is important

#Candidate.Sex
summary(final$Candidate.Sex)
final$Candidate.Sex.f=ifelse(final$Candidate.Sex=="f",1,0)
final$Candidate.Sex.m=ifelse(final$Candidate.Sex=="m",1,0)
final$Candidate.Sex.o=ifelse(final$Candidate.Sex=="o",1,0)


#Candidate.Age
summary(final$Candidate.Age)
ggplot(data=final,aes(x=final$Candidate.Age)) + 
  geom_bar(alpha=0.5,fill="red",color="black") +
  ggtitle("Age Distribution")
library(e1071)
skewness(final$Candidate.Age)

#Candidate.Category
summary(final$Candidate.Category)
final$Candidate.Category.gen=ifelse(final$Candidate.Category=="gen",1,0)
final$Candidate.Category.sc=ifelse(final$Candidate.Category=="sc",1,0)
final$Candidate.Category.st=ifelse(final$Candidate.Category=="st",1,0)

#final$Party.Name
summary(final$Party.Name)

#final$VALID.VOTES.POLLED.in.General
summary(final$VALID.VOTES.POLLED.in.General)
boxplot(final$VALID.VOTES.POLLED.in.General)
hist(final$VALID.VOTES.POLLED.in.General)
skewness(final$VALID.VOTES.POLLED.in.General)

#final$VALID.VOTES.POLLED.in.Postal
summary(final$VALID.VOTES.POLLED.in.Postal)
boxplot(final$VALID.VOTES.POLLED.in.Postal)
hist(final$VALID.VOTES.POLLED.in.Postal)
skewness(final$VALID.VOTES.POLLED.in.Postal)

#final$Total.Valid.Votes
summary(final$Total.Valid.Votes)
boxplot(final$Total.Valid.Votes)
hist(final$Total.Valid.Votes)
skewness(final$Total.Valid.Votes)

#final$Total.Electors
summary(final$Total.Electors)
boxplot(final$Total.Electors)
hist(final$Total.Electors)
skewness(final$Total.Electors)

#final$Total.valid.votes.polled..NOTA
summary(final$Total.valid.votes.polled..NOTA)
boxplot(final$Total.valid.votes.polled..NOTA)
hist(final$Total.valid.votes.polled..NOTA)
skewness(final$Total.valid.votes.polled..NOTA)

#final$X..votes.polled
summary(final$X..votes.polled)
boxplot(final$X..votes.polled)
hist(final$X..votes.polled)
skewness(final$X..votes.polled)

#final$Criminal.Case
final$Criminal.Case=as.numeric(final$Criminal.Case)
summary(final$Criminal.Case)
boxplot(final$Criminal.Case)
hist(final$Criminal.Case)
skewness(final$Criminal.Case)

#final$Education
summary(final$Education)
#Converting graduate professional as graduate
final$Education[final$Education == "graduate professional"] = "graduate"
final$Education.10=ifelse(final$Education=="10th pass",1,0)
final$Education.12=ifelse(final$Education=="12th pass",1,0)
final$Education.5=ifelse(final$Education=="5th pass",1,0)
final$Education.8=ifelse(final$Education=="8th pass",1,0)
final$Education.doctorate=ifelse(final$Education=="doctorate",1,0)
final$Education.graduate=ifelse(final$Education=="graduate",1,0)
final$Education.illiterate=ifelse(final$Education=="illiterate",1,0)
final$Education.literate=ifelse(final$Education=="literate",1,0)
final$Education.notgiven=ifelse(final$Education=="not given",1,0)
final$Education.others=ifelse(final$Education=="others",1,0)
final$Education.postgraduate=ifelse(final$Education=="post graduate",1,0)

View(final)

#final$Total.Assets
summary(final$Total.Assets)

#It contains 5 NA's, we will remove NA using kNN.
library(VIM)
data1=kNN(final,variable = "Total.Assets",k=5)
summary(data1$Total.Assets)
#All NA's are removed

#removing extra coloumns that are added by kNN in data1 and saving it back to data
final=data1
View(final)
final=subset(final,select = Candidate.Name:Education.postgraduate)

boxplot(final$Total.Assets)
hist(final$Total.Assets)
skewness(final$Total.Assets)

#final$Liabilities
summary(final$Liabilities)
boxplot(final$Liabilities)
hist(final$Liabilities)
skewness(final$Liabilities)

#Feature Selection
#set.seed(123)
#ibrary(Boruta)
#boruta.test = Boruta(final$X..votes.polled~. ,data=final, doTrace = 2)
#as per boruta Education and sex is not important. We will not remove this

#Normalize the data
normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
#normalizing all numeric variables
final$Candidate.Age.Normalize=normalize(final$Candidate.Age)
final$VALID.VOTES.POLLED.in.General.Normalize=normalize(final$VALID.VOTES.POLLED.in.General)
final$VALID.VOTES.POLLED.in.Postal.Normalize=normalize(final$VALID.VOTES.POLLED.in.Postal)
final$Total.Valid.Votes.Normalize=normalize(final$Total.Valid.Votes)
final$Total.Electors.Normalize=normalize(final$Total.Electors)
final$Total.valid.votes.polled..NOTA.Normalize=normalize(final$Total.valid.votes.polled..NOTA)
final$X..votes.polled.Normalize=normalize(final$X..votes.polled)
final$Criminal.Case.Normalize=normalize(final$Criminal.Case)
final$Total.Assets.Normalize=normalize(final$Total.Assets)
final$Liabilities.Normalize=normalize(final$Liabilities)

#Applying Logistic regression on the variables.
final.logit=final[,c(2,18:44)]

model1=glm(final.logit$X..votes.polled.Normalize~., final.logit,family = binomial)
summary(model1)
## Here we can see that Constituency Number,Age, Votes olled in General, Votes Polled in Postal and Total Valid votes are significant.
##For a person to win, all these factors matters.