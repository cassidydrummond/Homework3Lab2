#Cassidy Drummond
#September 15, 2022
#Homework 3, Lab 2
#Group Members: Valery and Zack

#We chose to compare sex, vaccination status, and whether or not they have had Covid-19. We found that 88.3% of females were vaccinated and 9.89% had been told by a doctor they had Covid-19. We found that 87.95% of males were vaccinated and 7.49% had been told by a doctor they had Covid-19. I am not confident as we do not know what factors make up the sample. For instance, did all participants have equal access to testing sites?

#Looking at the crosstabs and marginal probabilities, we found that our dataset was not statistically significant as the p-value is much greater than .05 (males = .35 and females = .33). Our crosstab is not mutually exclusive as some individuals who were vaccinated had not had covid and vice versa, despite their sex. Our crosstab is exhaustive as all possible outcomes are represented. 

#There are many factors that could impact the difference in outcome as the data’s collection of information was limited, and there are many compounding factors. We found there was an increase in vaccination status among higher educated individuals, and that higher educated females were vaccinated at a higher rate than males. However, when looking at education less than high school, the percent of females vaccinated was 10% less than their male counterparts. 

#Some additional evidence or context I would love to look out would be: political status, frequency of voting, religion, location (especially in a dense area), most viewed news sources, people they knew who had Covid-19, etc.. This information could determine the impact misinformation had on vaccination rates; political parties can be closely correlated with vaccination rates, etc.. Regarding my confidence, it is limited because I think a person’s decision on whether or not they get vaccinated is influenced by an extraordinary amount of factors. There are social, political, equitable, cultural, religious, historical, and economic influences, and I do not think we can quantify or test for the intersectionality of these influences. 



load("Household_Pulse_data")
install.packages("gmodels")
install.packages("descr")
library(descr)
install.packages("margins")
restrict2 <- (Household_Pulse_data$EGENID_BIRTH == "male") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$HADCOVID == "yes doctor told had covid") | (Household_Pulse_data$HADCOVID == "no did not")
restrict1 <- (Household_Pulse_data$EGENID_BIRTH == "female") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$HADCOVID == "yes doctor told had covid") | (Household_Pulse_data$HADCOVID == "no did not")
data_newf <- subset(Household_Pulse_data,restrict1)
data_newm <- subset(Household_Pulse_data, restrict2)
summary(data_newf$RECVDVACC)
summary(data_newm$RECVDVACC)
summary(data_newf$HADCOVID)
RECVDVACC = sample(c("yes got vaxx","no did not get vaxx","NA"), 68962, replace = TRUE)
EGENID_BIRTH = sample(c("female", "male"), 68962, replace = TRUE)
x = data.frame(RECVDVACC,EGENID_BIRTH)
CrossTable(RECVDVACC,EGENID_BIRTH)
HADCOVID = sample(c("yes doctor told had covid","no did not","NA"), 68962, replace = TRUE)
EGENID_BIRTH = sample(c("female", "male"), 68962, replace = TRUE)
x = data.frame(HADCOVID,EGENID_BIRTH)
CrossTable(HADCOVID,EGENID_BIRTH)
sd(summary(data_newm$RECVDVACC))
t.test(summary(data_newm$RECVDVACC),var.equal = TRUE)

sd(summary(data_newf$RECVDVACC))
t.test(summary(data_newf$RECVDVACC),var.equal = TRUE)

sd(summary(data_newf$HADCOVID))
t.test(summary(data_newf$HADCOVID),var.equal = TRUE)

sd(summary(data_newm$HADCOVID))
t.test(summary(data_newm$HADCOVID),var.equal = TRUE)
prop.table(summary(data_newf$RECVDVACC), margin = NULL)
prop.table(summary(data_newm$RECVDVACC), margin = NULL)
prop.table(summary(data_newf$HADCOVID), margin = NULL)
prop.table(summary(data_newm$HADCOVID), margin = NULL)

restrict2 <- (Household_Pulse_data$EGENID_BIRTH == "male") & (Household_Pulse_data$EEDUC == "less than hs") 
restrict1 <- (Household_Pulse_data$EGENID_BIRTH == "female") & (Household_Pulse_data$EEDUC == "less than hs") 
data_newf <- subset(Household_Pulse_data,restrict1)
data_newm <- subset(Household_Pulse_data, restrict2)
summary(data_newf$RECVDVACC)
summary(data_newf$HADCOVID)
summary(data_newm$RECVDVACC)
summary(data_newm$HADCOVID)

restrict2 <- (Household_Pulse_data$EGENID_BIRTH == "male") & (Household_Pulse_data$EEDUC == "adv deg") 
restrict1 <- (Household_Pulse_data$EGENID_BIRTH == "female") & (Household_Pulse_data$EEDUC == "adv deg") 
data_newf <- subset(Household_Pulse_data,restrict1)
data_newm <- subset(Household_Pulse_data, restrict2)
summary(data_newf$RECVDVACC)
summary(data_newf$HADCOVID)
summary(data_newm$RECVDVACC)
summary(data_newm$HADCOVID)

summary(data_newm$HADCOVID)