##Imported Libraries

library(stringr)
library(ggplot2)
library(Rmisc)


## Statistical analysis in RStudio


## Importing Data

data <- read.csv("Seal averages 2007-2010.csv", header = T)

## Converting Data to data frame
data <- data.frame(data)

# Checking first 6 six rows of data
head(data)


# Checking  structure of data 
str(data)





##Converting data types 

###Removing string "Summer-" from Column Summer for Better understanding 
data$Summer <- str_remove_all(data$Summer, "[summer-]")

### Converting Summer Column data type to Factor
data$Summer <- as.factor(data$Summer)

### Converting Year.Month Column data type to Factor
data$Year.Month <- as.factor(data$Year.Month)

### Converting Site Column data type to Factor

data$Site <- as.factor(data$Site)

### Converting Species Column data type to Factor
data$Species <- as.factor(data$Species)

# Checking  structure of data 
str(data)



## As Mention in the Section 2 Checking for Normalizes 

qqnorm(data$average.count)


## shapiro Test 

shapiro.test(data$average.count)

## So After running Shaprio test we can find the our data is not data are not significantly different from normal distribution.As Our p-value is 2.2
## We will run Kruskal-Wallis Test

kruskal.test(data$average.count, data$Summer) 

#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the groups.

## Running pairwise wilox Test 

pairwise.wilcox.test(data$average.count, data$Summer)

## Now avoiding false-positive values for the data 
pairwise.wilcox.test(data$average.count, data$Summer,
                     p.adjust.method = "BH")


##Checking the Avg Count value difference between 2010 and 2007

plot(data$Summer, data$average.count)


## Exploring diffrence between months in 2007 using kruskal test

kruskal.test(data$average.count[data$Summer == "2007"], data$Year.Month[data$Summer == "2007"]) 

## Exploring diffrence between months in 2007 using pairwise wilox Test 

pairwise.wilcox.test(data$average.count[data$Summer == "2007"],data$Year.Month[data$Summer == "2007"],
                     p.adjust.method = "BH")

## Comparing species abundance

kruskal.test(data$average.count, data$Species)


## Plotting Seal avg count vs Species


plot(data$average.count ~ data$Species)


## Calculating for all years

kruskal.test(data$average.count, data$Species) 


## Now avoiding false-positive values for the data 
pairwise.wilcox.test(data$average.count, data$Species,
                     p.adjust.method = "BH")

 
## Calculating for Year 2007


kruskal.test(data$average.count[data$Summer == "2007"], data$Species[data$Summer == "2007"]) 

## Exploring diffrence between months in 2007 using pairwise wilox Test 

pairwise.wilcox.test(data$average.count[data$Summer == "2007"],data$Species[data$Summer == "2007"],
                     p.adjust.method = "BH")

## Calculating for Year 2008


kruskal.test(data$average.count[data$Summer == "2008"], data$Species[data$Summer == "2008"]) 

## Exploring diffrence between months in 2007 using pairwise wilox Test 

pairwise.wilcox.test(data$average.count[data$Summer == "2008"],data$Species[data$Summer == "2008"],
                     p.adjust.method = "BH")


## Calculating for Year 2009


kruskal.test(data$average.count[data$Summer == "2009"], data$Species[data$Summer == "2009"]) 

## Exploring difference between months in 2007 using pairwise wilox Test 

pairwise.wilcox.test(data$average.count[data$Summer == "2009"],data$Species[data$Summer == "2009"],
                     p.adjust.method = "BH")


## Calculating for Year 2010


kruskal.test(data$average.count[data$Summer == "2010"], data$Species[data$Summer == "2010"]) 

## Exploring difference between months in 2010 using pairwise wilox Test 

pairwise.wilcox.test(data$average.count[data$Summer == "2010"],data$Species[data$Summer == "2010"],
                     p.adjust.method = "BH")


##In 2009 population was different


##AVG count VS Month

ggplot(data,aes(fill=Species, x=Year.Month, y=average.count)) +
  geom_bar(position = "dodge",stat = "identity", width = 0.9) +
  scale_fill_manual(values = c("Yellow","pink")) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))


## Error Bars
dataError <- summarySE(data,measurevar = "average.count",groupvars = c("Year.Month","Species"))


## Error plot
ggplot(dataError,aes(fill=Species, x=Year.Month, y=average.count)) +
  geom_bar(position = "dodge",stat = "identity", width = 0.9) +
  scale_fill_manual(values = c("Yellow","pink")) +
  geom_errorbar(aes(ymin=average.count-se, ymax=average.count+se),width = 0.2, position = position_dodge(0.9)) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))


## Geom_Line 

ggplot(dataError,aes(fill=Species, x=Year.Month, y=average.count,group=Species)) +
  geom_line(aes(color=Species))+
  geom_point(aes(color=Species))+
  scale_fill_manual(values = c("Yellow","pink")) +
  geom_errorbar(aes(ymin=average.count-se, ymax=average.count+se),width = 0.2, position = position_dodge(0.9)) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))+
  facet_grid(Species ~ .,scales = "free")


##AVG Species VS Month

ggplot(data,aes(fill=Species, x=Species, y=average.count)) +
  geom_bar(position = "dodge",stat = "identity", width = 0.9) +
  scale_fill_manual(values = c("Yellow","pink")) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))


##AVG Summer VS Month

ggplot(data,aes(fill=Species, x=Summer, y=average.count)) +
  geom_bar(position = "dodge",stat = "identity", width = 0.9) +
  scale_fill_manual(values = c("Yellow","pink")) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))




##AVG Summer VS Month

ggplot(data,aes(fill=Species, x=Site, y=average.count)) +
  geom_bar(position = "dodge",stat = "identity", width = 0.9) +
  scale_fill_manual(values = c("Yellow","pink")) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))


## facet Summer 
ggplot(data,aes(fill=Species, x=Summer, y=average.count,group=Species)) +
  geom_point(aes(color=Species))+
  scale_fill_manual(values = c("Yellow","pink")) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))+
  facet_grid(Species ~ Summer,scales = "free")


## facet Site

## facet Summer 
ggplot(data,aes(fill=Species, x=Site, y=average.count,group=Species)) +
  geom_bar(aes(fill=Species),position = "dodge",stat = "identity", width = 0.9)+
  scale_fill_manual(values = c("light blue","Orange")) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))+
  facet_grid(Species ~ Summer,scales = "free")



ggplot(data,aes(fill=Site, x=Year.Month, y=average.count,group=Site)) +
  geom_bar(aes(fill=Site),position = "dodge",stat = "identity", width = 0.9)+
  scale_fill_manual(values = c("light blue","Orange")) +
  theme(panel.background = element_blank(),axis.text.x = element_text(angle = 90))+
  facet_grid(Species ~ Site,scales = "free")
