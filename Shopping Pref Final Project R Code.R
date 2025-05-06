##################################################
#IST 687 Group A 
#Final Project
#Customer Shopping Preferences


#####libraries needed#####
library(readxl)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(R.utils)
library(ggplot2)
library(ggmap)
library(jsonlite)
library(RCurl)
library(dplyr)

#####Importing Customer Preference Dataset excel file and put into dataframe named ST (Shopping Trends)
Final_Project_Data1 <- read_excel("~/Final_Project_Data1.xlsx")
View(Final_Project_Data1)

#Create dataframe ST
ST <- data_frame(Final_Project_Data1)


#####Exploring the data at high level#####
str(ST)
summary(ST)
View(ST)
###################

######Data Cleaning#####
#Check dataframe for NAs
sum(is.na.data.frame(ST))
#No NAs were found

#Check for spaces
colnames(ST) #view column names
#replace space with underscore
colnames(ST) <- gsub(" ","_",colnames(ST))
head(ST,1) #confirm column names updated

#Column Rename to get rid of characters
ST <- ST %>%  rename("Purchase_Amount" = "Purchase_Amount_(USD)")
###################


######Customer Demographics Analysis#####
Age <- ST$Age
Gender <- ST$Gender
Age_Gender <-data.frame(Age, Gender)
range(Age_Gender$Age)
mean(Age_Gender$Age)
#Customer Gender Analysis
Female <- sum(Age_Gender$Gender == "Female")
Male <- sum(Age_Gender$Gender == "Male")
Female
Male
nrow(Age_Gender)
Gender <- c("Female","Male")
Totals <- c(41.9, 58.1)
Genderdf <- data_frame(Gender,Totals)

GenderAnalysis <- ggplot(Genderdf, aes(x="",y=Gender, fill=Gender)) + geom_bar(width=1,stat="identity") + coord_polar("y", start=0) + theme_void() + 
                  theme(plot.title=element_text(size=18),legend.text=element_text(size=12),legend.title=element_text(size=12)) + 
                  scale_fill_manual(values = c("darkgoldenrod2","seagreen")) + ggtitle("Customer by Gender (%)") +  
                  geom_text(aes(label = Totals), size= 5, position = position_stack(vjust = 0.5))
GenderAnalysis

#Customer Age Analysis
#Customer Age by Gender Histogram
HistAG <- ggplot(Age_Gender,aes(x=Age, fill=Gender)) + geom_histogram(position="stack", binwidth = 3, color="black", alpha=.9) + 
  labs(title = "Customer Age by Gender Histogram", x = "Age", y = "Frequency") + scale_fill_manual(values = c("darkgoldenrod2","seagreen")) +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),plot.title=element_text(size=18),legend.text=element_text(size=14),legend.title=element_text(size=14)) 
HistAG

#Sampling Distribution for Customer Age and Descriptive Statistics
Age_Distribution <- replicate(1000,mean(sample(Age_Gender$Age,size=5,replace=TRUE)))

mean(Age_Distribution)
summary(Age_Distribution)
quantile(Age_Distribution, probs =c(0.25, 0.50, 0.75))
sd(Age_Distribution)

#Sampling Distribution for Customer Age
Age_Distribution_Hist <-  hist(replicate(1000,mean(sample(Age_Gender$Age,size=5,replace=TRUE))), main="Customer Age Normal Distribution", xlab="Average Age")
Age_Distribution_Hist

ST18_24 <- ST %>% filter(between (Age, 18, 24)) %>% arrange(Age, Gender, Category, Item_Purchased)
ST25_34 <- ST %>% filter(between (Age, 25, 34))  %>% arrange(Age, Gender, Category, Item_Purchased)
ST35_44 <- ST %>% filter(between (Age, 35, 44))  %>% arrange(Age, Gender, Category, Item_Purchased)
ST45_54 <- ST %>% filter(between (Age, 45, 54))  %>% arrange(Age, Gender, Category, Item_Purchased)
ST55_64 <- ST %>% filter(between (Age, 55, 64))  %>% arrange(Age, Gender, Category, Item_Purchased)
ST65_older <- ST %>% filter(between (Age, 65, 100))  %>% arrange(Age, Gender, Category, Item_Purchased)

#Age Vector Math for report Writeup
Age1 <- (nrow(ST18_24)/nrow(ST))*100
Age2 <- (nrow(ST25_34)/nrow(ST))*100
Age3 <- (nrow(ST35_44)/nrow(ST))*100
Age4 <- (nrow(ST45_54)/nrow(ST))*100
Age5 <- (nrow(ST55_64)/nrow(ST))*100
Age6 <- (nrow(ST65_older)/nrow(ST))*100

Agepercent <- c(Age1, Age2, Age3, Age4, Age5, Age6)
Agepercent <- format(Agepercent,digits = 2, format="f")
Agegroup <- c(1824, 2534, 3544, 4554,5564,65)
Agepie <- data_frame(Agegroup, Agepercent)

AgeAnalysis <- ggplot(Agepie, aes(x="",y=Agepercent, fill=Agepercent)) + geom_bar(width=1,stat="identity") + coord_polar("y", start=0) + theme_void() + 
  theme(plot.title=element_text(size=18),legend.text=element_text(size=12),legend.title=element_text(size=12)) + ggtitle("Customer by Age (%)") +  
  geom_text(aes(label = Agepercent), size= 6, position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("darkgoldenrod2","seagreen","aquamarine3","cadetblue3", "steelblue","royalblue4"), name = "Age Group", labels = c("65+","18-24","55-64","25-34","35-44","45-54"))
AgeAnalysis

#Map of Total Number of Purchases by State
dfPurchaseLocation <- ST %>% group_by(Location) %>% count(Location)
view(dfPurchaseLocation)

dfPurchaseLocation[which.min(dfPurchaseLocation$n),]
dfPurchaseLocation[which.max(dfPurchaseLocation$n),]

USA <- data.frame(map_data("state"))
view(USA)
dfPurchaseLocation$Location <- tolower(dfPurchaseLocation$Location)
view(dfPurchaseLocation)
merged_df2 <- merge (USA, dfPurchaseLocation, all.x = TRUE, by.x = 'region', by.y = 'Location')
view(merged_df2)
merged_df2 <- merged_df2[order( merged_df2[,4], merged_df2[,5] ),]
colnames(merged_df2)[7] = "Location_Count"

ggplot() + geom_polygon(data=merged_df2,aes(x=long, y=lat, group = group, fill=Location_Count)) + 
  ggtitle("US Total Purchases per State") + coord_map()  + 
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),axis.title=element_text(size=20),plot.title=element_text(size=18)) +
  labs(fill = "Total Purchases")

###################

######Customer Behavior Analysis#####

#Season of Purchase Breakdown
ST_Sea <- ggplot(ST) + aes(x=Season) + geom_bar(color="black", fill = "seagreen") + 
          ggtitle("Season of Customer Purchase") + xlab("Season") + ylab("Frequency") + 
          theme(axis.text.x=element_text(angle=0, vjust=0.6, size=18),axis.text.y=element_text(size=18),axis.title=element_text(size=20),plot.title=element_text(size=18))
ST_Sea

#Item Category Breakdown
ST_cat <- ggplot(ST) + aes(x=Category) + geom_bar(color="black", fill = "seagreen") +  
          ggtitle("Product Category") + xlab("Product Category") + ylab("Frequency") + 
          theme(axis.text.x = element_text(size=18, angle=0, vjust=0.6),axis.text.y=element_text(size=18),axis.title=element_text(size=18),plot.title=element_text(size=18))
ST_cat

#Payment Method Breakdown
ST_pay <- ggplot(ST) + aes(x=Payment_Method) + geom_bar(color="black", fill = "seagreen") + 
          ggtitle("Most Common Payment Method") + xlab("Payment Method") + ylab("Frequency") + 
          theme(axis.text.x=element_text(angle=0, vjust=0.5, size=18),axis.text.y=element_text(size=18),axis.title=element_text(size=20),plot.title=element_text(size=18)) 
ST_pay

#Frequency of Purchase breakdown
ST_FP <- ggplot(ST) + aes(x=Frequency_of_Purchases) + geom_bar(color="black", fill = "seagreen") + 
         ggtitle("Purchase Frequency") + xlab("Purchase Frequency") + ylab("Frequency") +
         theme(axis.text.x = element_text(angle=65, vjust=0.6, size=18),axis.text.y=element_text(size=18),axis.title=element_text(size=20),plot.title=element_text(size=18))
ST_FP

#Map of $ Spent by State
dfPurchase <- ST %>% group_by(Location) %>% summarise(Purchase_Total = sum(Purchase_Amount))
view(dfPurchase)

dfPurchase[which.min(dfPurchase$Purchase_Total),]
dfPurchase[which.max(dfPurchase$Purchase_Total),]

USA <- data.frame(map_data("state"))
view(USA)
dfPurchase$Location <- tolower(dfPurchase$Location)
view(dfPurchase)
merged_df <- merge (USA, dfPurchase, all.x = TRUE, by.x = 'region', by.y = 'Location')
view(merged_df)
merged_df <- merged_df[order( merged_df[,4], merged_df[,5] ),]

ggplot() + geom_polygon(data=merged_df,aes(x=long, y=lat, group= group, fill=Purchase_Total)) + 
  ggtitle("US Total $ Spent per State") + coord_map() + 
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),axis.title=element_text(size=20),plot.title=element_text(size=18)) +
  labs(fill = "Total $ Spent")

###################

#####Focused Customer Behavior Analysis######
#Focus analysis on Age group 35-55
#Item Purchase Frequency and $ Spent per Item
ST35_54 <- ST %>% filter(between (Age, 35, 54))
ST35_541 <- ST35_54 %>% group_by(Item_Purchased) %>% summarise(sum=sum(Purchase_Amount))
ST35_542 <- ST35_54 %>% group_by(Item_Purchased) %>% count(Item_Purchased)
ST35_543 <- merge(ST35_541, ST35_542, by = "Item_Purchased")

df_IP <- ggplot(ST35_543) + aes(x=Item_Purchased, y=n, color=sum, size=sum) + geom_point() + theme(axis.text.x = element_text(angle=65, vjust=0.6, size=18), axis.text.y=element_text(size=18),axis.title=element_text(size=18), plot.title=element_text(size=20), legend.text=element_text(size=14), legend.title=element_text(size=14)) + 
         ggtitle("Frequency of Purchase and $ Spent per Item") + xlab("Item Purchased") + ylab("Frequency of Item Purchase") +
         scale_color_continuous(name = "$ Spent") + scale_size(name = "$ Spent", range = c(3 , 23))
df_IP




