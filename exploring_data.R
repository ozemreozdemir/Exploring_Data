## OZKAN EMRE OZDEMIR
## R Homework 1
## 04/07/16
## Class: PCE Data Science Methods Class
##--------------------------------------------
## Clear objects from Memory :
rm(list=ls())
##Clear Console:
cat("\014")
##Set working directory- :
setwd('~/DataAnalysis/1_Intro_Lecture')
##Load Libraries :
library(dplyr)
library(plyr)
library(data.table)
library(ggplot2)
##Load jittered Data :
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
##Look at the Data and check for duplicates :
head(headcount)
summary(headcount)
anyDuplicated(headcount[c("DateFormat", "Hour","GameCode")])
###------------Part One --------------------------------------------------------
##First, let's look at the aggregation of head counts by day of the week only :
headcount = as.data.table(headcount)
aggHeadcountDay=aggregate(x = headcount$HeadCount, by =list(DayofWeek = headcount$DayOfWeek), FUN = sum)
aggHeadcountDay=rename(aggHeadcountDay,c("x" = "TotalHeadcounts"))
aggHeadcountDay
##Let's find out what are the most and least busy days :
aggHeadcountDay[which.max(aggHeadcountDay$TotalHeadcounts),]
aggHeadcountDay[which.min(aggHeadcountDay$TotalHeadcounts),]
##Let's find out the %  difference of the head counts compared to the busiest day :
aggHeadcountDay$MaxHeadCountPercentageComparison=round(100*(max(aggHeadcountDay$TotalHeadcounts)-aggHeadcountDay$TotalHeadcounts)/max(aggHeadcountDay$TotalHeadcounts))
aggHeadcountDay
##The total number of head counts on Sundays is around 42 % higher than the total number of people on Wednesdays :
###------------Part Two --------------------------------------------------------
##First, let's calculate the average occupied table per day for each game :
AveOccTable= aggregate(TablesOcc ~ DayOfWeek + GameCode , data = headcount, FUN = mean)
##In Figure 1 (below), results show that game type S6 has the most occupied tables every day :
ggplot(AveOccTable, aes(DayOfWeek,TablesOcc, fill= GameCode)) +
  geom_bar(stat="identity", position = "dodge") +
  xlab("Monday           Tuesday           Wednesay         Thursday          Friday             Saturday         Sunday") +
  ylab("Average Number of Occupied Table") +
  ggtitle("Figure 1 : Average Number of Occupied Tables Per Game")
###Coding improvement
### Since I will be using the same plotting format couple more times let's write a simple function to avoid coppy and paste
plot_results <-function(x,y,z,ylabel,tittle) {
        ggplot(x, aes(y,z, fill= GameCode)) +
                geom_bar(stat="identity", position = "dodge") +
                xlab("Monday           Tuesday           Wednesay         Thursday          Friday             Saturday         Sunday") +
                ylab(ylabel) +
                ggtitle(tittle)
}
### Let's replot Figure 1 using the above plot_results funtion
plot_results(AveOccTable,AveOccTable$DayOfWeek,AveOccTable$TablesOcc,"Average Number of Occupied Table", "Figure 1 : Average Number of Occupied Tables Per Game")
##End
##Second, let's look at the average head count for per day for each game :
AveHeadCount= aggregate(HeadCount ~ DayOfWeek + GameCode , data = headcount, FUN = mean)
##In Figure 2 (below), the trend looks similar for the type S6 game compare to the average number of occupied tables plotted previously. :
plot_results(AveHeadCount,AveHeadCount$DayOfWeek,AveHeadCount$HeadCount,"Average Number of Players", "Figure 2 :Average Number of Players per Game")
##Finally, in order to have a fare comparision for the games popularity let's look at the number of player density per each occupied table for each game :
AveHeadCount$HeadCountperTableOcc=(AveHeadCount$HeadCount/AveOccTable$TablesOcc)
AveHeadCount$HeadCountperTableOcc[is.nan(AveHeadCount$HeadCountperTableOcc)]<-0
#Figure 3 (below) show a significant difference compared to Figure 1 and Figure 2 : 
plot_results(AveHeadCount,AveHeadCount$DayOfWeek,AveHeadCount$HeadCountperTableOcc,"Average Number of Players/Table", "Figure 3 :Average Number of Players per Table")
###------------Summary --------------------------------------------------------
##The total maximum and minimum head counts were obtained on Sundays and on Wednesdays, respectively. The difference between number of head counts of the most and the least busy days are calculated as 42 %.
##When we look at the given data set some games were offered only on certain days of the year which makes it an unfair to compare their popularity based on their total head counts only. In addition it is also not clear to guess the maximum occupancy of a table based on the head counts only. However, we can compare the average number of people per average occupied table ratio for each game over days of week and come up with an idea which game attracts customers the most when it is offered.
##First the average number of occupied tables per day is calculated for each game as shown in Figure 1 below. Game S6 is found out to have the most occupied tables.
##Then the average number of head count per day is calculated for each game and as shown in Figure 2 below. Similarly, Game S6 is found out to have the most head counts.
##Finally, the density of each occupied table is calculated based on average number of head counts player over average number of occupied table per day for each game. As shown in Figure 3 below, game CR has the most head count per occupied table. In addition, when game BA was offered, its head count per occupied table is relatively higher than other games that are offered more often than BA.
##PS: I am fairly new to R, hopefully my coding skills will improve throughout this class.
##End