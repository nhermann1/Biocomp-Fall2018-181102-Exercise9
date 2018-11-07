setwd("~/Documents/Biocomp/Biocomp-Fall2018-181102-Exercise9/")
rm(list=ls())

library(ggplot2)
library(dplyr)

#Question 1
Fish_Sizes<-read.csv("Fish_Sizes.csv") #Data taken from research of the length and weight of 48 fish at a given time
#Plotting
ggplot(Fish_Sizes, aes(x=Fish_Length, y=Fish_Mass))+ #plotting the two vectors of data against each other
  geom_point()+ #make a scatter plot
  geom_smooth(method=lm, se=FALSE)+ #Add a trendline (linear by method=lm) without 95% confidence interval (by se=FALSE)
  theme_classic() #remove background to look like base R

#Question 2
Data<-read.table("data.txt",header=TRUE,sep = ",") #inputting data
#Bar Graph
DataMean<-Data%>% #Need a new data frame for the means to be found in
  group_by(region)%>% #Means need to be for each of the different regions
  summarise(MEAN=mean(observations)) #Need to generate the mean values to plot them in a bar graph
ggplot(DataMean,aes(x=region,y=MEAN,fill=region))+ #using fill to add color coding to the graph
  geom_col()+ #col uses the default stat_identity rather than bar using stat_count--either can be told to use the other though
  theme(legend.position = "none") #Don't need to colors labeled since they are on the x-axis
#Scatter Plot
ggplot(Data,aes(x=region,y=observations,color=region))+ #again color coding by region
  geom_jitter()+ #plots every point, but it pushes overlapping points away so they're all seen
  theme(legend.position = "none") #Again, already labeled on x-axis

#What's the story between these graphs?
#Bar plot shows the exact mean very clearly and how those compare, which is that they are almost identical by region
#Jitter plot shows the distribution of those points which have very different patterns by the regions, namely:
#East is "normally" distributed
#North is very narrowly distributed around mean
#South is bimodally distributed with some high and some low points
#West is uniformly distributed within the entire range of observations
#This distribution information is missing from the simple bar plot--can be seen with error bars to some extent

#Additional Stuff
#Bar Graph with Error bars--potential way to put these stories together
DataErrorMeans<-Data%>%
  group_by(region)%>%
  summarise(MEAN=mean(observations), #Same as above ^^^
            SD=sd(observations, na.rm=TRUE), #Calculate the standard deviation of the data without an zeroes
            sem=sd(observations, na.rm=TRUE)/sqrt(length(observations)))%>% #Calculate the standard error too. Relative to the number of observations, confidence in mean
  mutate(lower=MEAN-SD, upper=MEAN+SD) #Using sem instead of SD here reflects that so many observations are made for the confidence that this is the mean without distribution info
ggplot(DataErrorMeans, aes(region, MEAN, fill=region))+
  geom_col()+ #Same as above ^^^
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.3, position=position_dodge(0.9))+ #Show the wide spread of values, but are resulting from many observations so standard error is much lower
  theme(legend.position = "none")

#Adding the error bars, of SD, can show that not all the regions have comparable distributions despite similar means
#Does not show the nature of the distribution like jitter was able to
#Similarly, sem shows that the sheer number of observations makes the distributions not important for the confidence in the estimated means









