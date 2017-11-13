library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
setwd("~/Desktop/Programming/Sandwich/Subjects")

#Create function to add Subject column to data frame
read_table_filename <- function(filename){
  sandwich <- read.table(filename, fill = TRUE)
  sandwich$Subject <- filename #EDIT
  sandwich
}

#Import all text files from Sandwich
filenames <- list.files(path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)

#Create data frame combining all subject data 
sandwich.list <- ldply(filenames, read_table_filename)

#Add column names
colnames(sandwich.list) <- c("TrialNum", "CondName", "Resp_1", "RT", "CorrResp", "StimName",  "Resp_2", "CondType", "TrigNum", "Jitter1", "Jitter2", "Jitter3", "Subject")

#Main Questions: 
# 1) Is there a relationship between reaction time and correct response?
# 2) Is there a relationship between reaction time and type of condition?
# 3) Is there a relationship between number of correct responses and type of condition?

# How strong is the relationship between reaction time and correct response?
sandwich.list <- transform(sandwich.list, Response = ifelse(CorrResp %in% c("HICorr", "LOCorr"), "Correct", "Incorrect"))
sandwich.list<-sandwich.list[!(sandwich.list$RT<0), ]

Sandwichttest <- t.test(RT ~ Response, data = sandwich.list)
Sandwichttest

boxplot(RT~Response,data=sandwich.list, main="Reaction Time and Response", 
        xlab="Response", ylab="Reaction Time")

#data:  RT by Response
#t = -19.25, df = 3699.2, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -142.9129 -116.4923
#sample estimates:
#  mean in group Correct mean in group Incorrect 
#609.3071                739.0097 

#There is a significant p value, indicating a significant difference between response and RT
#We can infer that students have a faster reaction time with outcome is correct versus incorrect

#Is there a relationship between reaction time and type of condition?
SandwichANOVACond <- aov(RT ~ CondName, data = sandwich.list)
summary(SandwichANOVACond)

boxplot(RT~CondName,data=sandwich.list, main="Reaction Time and Condition", 
        xlab="Condition", ylab="Reaction Time")

#Is there a relationship between number of correct responses and type of condition?
CorrectCond <- data_frame(sandwich.list$CondName, sandwich.list$Response)
colnames(CorrectCond) <- c("Condition", "Response")
CorrectCond <- transform(CorrectCond, Response = ifelse(Response == "Correct", 1, 0))
CorrectCond <-CorrectCond[!(CorrectCond$Response==0), ]

CorrectNumber <- table(CorrectCond$Condition)
barplot(CorrectNumber, main="Condition and Correct Responses", 
        xlab="Condition")

#At first look, there seems to be no significant relationship between factors



