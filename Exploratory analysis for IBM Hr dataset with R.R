d = read.csv('~/R programming/WA_Fn-UseC_-HR-Employee-Attrition (2).csv')

library(tidyverse)
library(ggplot2)
library(caret)
library(SuperLearner)
library(gridExtra)
library(magrittr) 
library(dplyr)    

#char  to factor
d[,c(2,3,5,7,8,12,16,18,23)]=lapply(d[,c(2,3,5,7,8,12,16,18,23)],as.factor)
str(d)
#cleaning the data
# Check for duplicates
sum(duplicated(d))
#Data Preprocessing
#Missing Value Imputations
sum(is.na(d))

# Remove duplicates
d <- d[!duplicated(d), ]


d <- d%>%mutate(Education = as.factor(if_else(Education == 1,"Below College", if_else(Education == 2, "College", if_else(Education == 3, "Bachelor", if_else(Education == 4, "Master","Doctor")))))
         ,EnvironmentSatisfaction = as.factor(if_else(EnvironmentSatisfaction == 1,"Low",if_else(EnvironmentSatisfaction == 2, "Medium", if_else(EnvironmentSatisfaction == 3, "High", "Very High"))))
         ,JobInvolvement = as.factor(if_else(JobInvolvement == 1,"Low",if_else(JobInvolvement == 2, "Medium",if_else(JobInvolvement == 3, "High", "Very High"))))
         ,JobSatisfaction = as.factor(if_else(JobSatisfaction == 1, "Low",if_else(JobSatisfaction == 2, "Medium",if_else(JobSatisfaction == 3, "High","Very High"))))
         ,PerformanceRating = as.factor(if_else(PerformanceRating == 1, "Low",if_else(PerformanceRating == 2, "Good", if_else(PerformanceRating == 3, "Excellent", "Outstanding"))))
         ,RelationshipSatisfaction = as.factor(if_else(RelationshipSatisfaction == 1, "Low",if_else(RelationshipSatisfaction == 2, "Medium", if_else(RelationshipSatisfaction == 3, "High", "Very High"))))
         ,WorkLifeBalance = as.factor(if_else(WorkLifeBalance == 1, "Bad",if_else(WorkLifeBalance == 2, "Good", if_else(WorkLifeBalance == 3, "Better", "Best"))))
         ,JobLevel = as.factor(JobLevel)
  ) %>%
  select(-EmployeeCount, -EmployeeNumber, -Over18, -StandardHours, -StockOptionLevel, -JobLevel)
#eda-
#d Summary
summary(d)
#Attrition count

d %>% 
  select(Attrition) %>%
  group_by(Attrition) %>% 
  summarize(N = n()) %>% 
  mutate(percent = round(prop.table(N), 2))

#Employee Personal Demographics - Numerical Variables
p1 <- ggplot(d) + geom_histogram(aes(Age), binwidth = 5, fill = "blue",col = "black")
p2 <- ggplot(d) + geom_histogram(aes(DistanceFromHome), binwidth = 5, fill = "blue",col = "black")
p3 <- ggplot(d) + geom_histogram(aes(NumCompaniesWorked), binwidth = 2, fill = "blue",col = "black")
p4=ggplot(d)+geom_histogram(aes(MonthlyIncome),binwidth=60,fill='blue')
grid.arrange(p1, p2, p3,p4, ncol = 2, nrow = 2)


###Employee Work Demographics - Numerical Variables
w1 <- ggplot(d) + geom_histogram(aes(MonthlyIncome), binwidth = 1000, fill = "blue",col = "black")
w2 <- ggplot(d) + geom_histogram(aes(PercentSalaryHike), binwidth = 1, fill = "blue",col = "black")
w3 <- ggplot(d) + geom_histogram(aes(YearsAtCompany), binwidth = 2, fill = "blue",col = "black")
w4 <- ggplot(d) + geom_histogram(aes(YearsInCurrentRole), binwidth = 2, fill = "blue",col = "black")
w5 <- ggplot(d) + geom_histogram(aes(YearsSinceLastPromotion), binwidth = 2, fill = "blue",col = "black")
w6 <- ggplot(d) + geom_histogram(aes(YearsWithCurrManager), binwidth = 2, fill = "blue",col = "black")

grid.arrange(w1, w2, w3, w4, w5, w6, nrow = 2, ncol = 3)
#Employee Personal Demographics - Categorical Variables
pd1<- d %>%
  group_by(Gender) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(Gender), y = counts)) + geom_bar(stat = 'identity',fill='coral1') + ggtitle("Gender") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 900))
pd1
pd2<- d %>%
  group_by(Education) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(Education), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Education") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 650))
pd3 <- d %>%
  group_by(EducationField) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(EducationField), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Education Field") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 650))
pd4 <- d %>%
  group_by(MaritalStatus) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(MaritalStatus), y = counts)) + geom_bar(stat = 'identity', fill = "coral1")+ ggtitle("Marital Status") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 750))
pd5 <- d %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(RelationshipSatisfaction), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Relationship Satisfaction") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())+ scale_y_continuous(limits = c(0, 500))
pd6 <- d %>%
  group_by(WorkLifeBalance) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(WorkLifeBalance), y = counts)) + geom_bar(stat = 'identity', fill = "coral1")+ ggtitle("Work Life Balance") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 950))

grid.arrange(pd1, pd2, pd3, pd4, pd5, pd6, nrow = 2, ncol = 3)
#Employee Work Demographics - Categorical Variables
wd1 <- d %>%
  group_by(BusinessTravel) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(BusinessTravel), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Business Travel") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size =10,angle = 45, hjust = 1),axis.title.x=element_blank())+ scale_y_continuous(limits = c(0, 1100))

wd2 <- d %>%
  group_by(EnvironmentSatisfaction) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(EnvironmentSatisfaction), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Environment Satisfaction") + geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =10,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 500))

wd3 <- d %>%
  group_by(JobInvolvement) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(JobInvolvement), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Job Involvement") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size =10,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 900))

wd4 <- d %>%
  group_by(JobSatisfaction) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(JobSatisfaction), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Job Satisfaction") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 500))

wd5 <- d %>%
  group_by(OverTime) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(OverTime), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Over Time") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 1100))

wd6 <- d %>%
  group_by(PerformanceRating) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(PerformanceRating), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Performance Rating") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 1300))

grid.arrange(wd1,wd2,wd3,wd4,wd5,wd6,nrow = 2)

#Bivariate EDA
#Employee Personal Demographics - Numerical Variables
pl1 <- d %>%
  ggplot(aes(x = Age, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Age") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

pl2 <- d %>%
  ggplot(aes(x = DistanceFromHome, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Distance From Home")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

pl3 <- d %>%
  ggplot(aes(x = NumCompaniesWorked, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Number of Companies")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

pl4 <- d %>%
  ggplot(aes(x = TotalWorkingYears, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Total Working Years")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

grid.arrange(pl1, pl2, pl3, pl4, nrow = 2, ncol = 2)

####Employee Billing Rate Demographics - Numerical Variables
bl1 <- d %>%
  ggplot(aes(x = HourlyRate, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Hourly Rate") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


bl2 <- d %>%
  ggplot(aes(x = DailyRate, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Daily Rate") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


bl3 <- d %>%
  ggplot(aes(x = MonthlyRate, fill = Attrition)) + geom_density(alpha = 0.5)+ ggtitle("Monthly Rate") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


grid.arrange(bl1, bl2, bl3)

#Employee Work Demographics - Numerical Variables
wl1 <- d %>%
  ggplot(aes(x = MonthlyIncome, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Monthly Income") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


wl2 <- d %>%
  ggplot(aes(x = PercentSalaryHike, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Percentage Salary Hike") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


wl3 <- d %>%
  ggplot(aes(x = YearsAtCompany, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years At Company") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


wl4 <- d %>%
  ggplot(aes(x = YearsInCurrentRole, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years in Current Role") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


wl5 <- d %>%
  ggplot(aes(x = YearsSinceLastPromotion, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years Since Last Promotion") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


wl6 <- d %>%
  ggplot(aes(x = YearsWithCurrManager, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years With Current Manager") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


grid.arrange(wl1, wl2, wl3, wl4, wl5, wl6 , nrow = 3, ncol = 2)

#Employee Personal Demographics - Categorical Variables
pc1 <- d %>%
  group_by(Gender) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = Gender, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Gender") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 20))


pc2 <- d %>%
  group_by(Education) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = Education, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Education") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 20))

pc3 <- d %>%
  group_by(EducationField) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = EducationField, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Education Field") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

pc4 <- d %>%
  group_by(MaritalStatus) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = MaritalStatus, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Marital Status") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

pc5 <- d %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = as.factor(RelationshipSatisfaction), y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Relationship Satisfaction") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

pc6 <- d %>%
  group_by(WorkLifeBalance) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = as.factor(WorkLifeBalance), y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Work Life Balance") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 35))

grid.arrange(pc1, pc2, pc3, pc4, pc5, pc6, nrow = 2, ncol = 3)
t.test(d$MonthlyIncome~d$Attrition)
summary(d$MonthlyIncome[d$Attrition == "Yes"])
summary(d$MonthlyIncome[d$Attrition == "No"])

# Boxplot of MonthlyIncome by Attrition
ggplot(d,aes(y=MonthlyIncome,x=Age,fill=Attrition))+geom_point(aes(color=Attrition))+scale_color_manual(values=c('coral1','blue'))
###people who want to leave in every department
ggplot(d, aes(y = MonthlyIncome, x = d$Department)) +geom_boxplot(aes(color = d$Attrition))
#monthly income vs attrition without gender
ggplot(d,aes(x =d$MonthlyIncome, y=d$Attrition,color=Attrition)) +geom_boxplot(fill=gender)
#monthly income vs attrition vs gender 
ggplot(d, aes(x=Attrition, y=MonthlyIncome, color=Gender, fill=Gender)) + geom_boxplot() +  scale_color_manual(values=c("Female"="black", "Male"="black"))+ggtitle("Monthly income vs Attrition vs Gender")

