
# Set Dir, load packages, merging, view --------------------------------------------------


#Import data 
setwd("/Users/calebhaynes/Documents/GitHub/Lab_Candidate_Assignment/data2")

library(dplyr)
library(readr)
library(ggplot2)
library(dplyr)

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)
}

full_data = multmerge("/Users/calebhaynes/Documents/GitHub/Lab_Candidate_Assignment/data2")


View(full_data)

#pdata is only Particpant responses
pdata <- subset(full_data, Computer.Response == 0)
attach(pdata)

# Part 1 ------------------------------------------------------------------
#Average response times (for participants choosing for themselves and choosing for their partners; no computer responses) for Choice 1 across all set size (2, 3, 6, 12) conditions.

#type conversion
ch1RTnum <- as.numeric(Choice.1.RT)
condition <- factor(Item.Option)

#get means across 4 conditions
x1 = mean(ch1RTnum[Item.Option == 2], na.rm = T)
x2 = mean(ch1RTnum[Item.Option == 3], na.rm = T)
x3 = mean(ch1RTnum[Item.Option == 6], na.rm = T)
x4 = mean(ch1RTnum[Item.Option == 12], na.rm = T)

xmeans <-c(x1, x2, x3, x4)

#plotting 

ds <- plyr::ddply(pdata, "condition", plyr::summarise, mean = mean(ch1RTnum), sd = sd(ch1RTnum))
ds$mean <- xmeans

g1 <- ggplot(data = pdata, aes(condition, ch1RTnum)) + geom_point(data = pdata, aes(condition, ch1RTnum)) + geom_point(data = ds, aes(y = mean), colour = 'red', size = 3) + geom_errorbar(
  data = ds,
  aes(condition, mean, ymin = mean - sd, ymax = mean + sd),
  colour = 'red',
  width = 0.4
) +
  labs(title = "Average response times for Choice 1 across object conditions",
      subtitle = "(no computer responses)",
      caption = "Data from Neuroeconomics Lab and Social Developmental Neuroscience Lab",
      tag = "Figure 1",
      x = "Set Size",
      y = "Response Time")
g1 + theme_minimal() 


# Part 2 ------------------------------------------------------------------
#Average response times (for participants choosing for themselves and choosing for their partners; no computer responses) for Choice 2 across all monetary (0.5, 1, 1.25, 1.5, 1.75) conditions.

#type conversion
ch2RTnum <- as.numeric(Choice.2.RT)
condition2 <- factor(Money.Option)

#get means across 6 conditions
x5 = mean(ch2RTnum[Money.Option == 0.5], na.rm = T)
x6 = mean(ch2RTnum[Money.Option == 0.75], na.rm = T)
x7 = mean(ch2RTnum[Money.Option == 1], na.rm = T)
x8 = mean(ch2RTnum[Money.Option == 1.25], na.rm = T)
x9 = mean(ch2RTnum[Money.Option == 1.5], na.rm = T)
x10 = mean(ch2RTnum[Money.Option == 1.75], na.rm = T)

xmeans2 <-c(x5, x6, x7, x8, x9, x10)


#plotting 

ds2 <- plyr::ddply(pdata, "condition2", plyr::summarise, mean = mean(ch2RTnum), sd = sd(ch2RTnum))
ds2

ds2$mean <- xmeans2


g4 <- ggplot(data = pdata, aes(condition2, ch2RTnum)) + geom_point(data = pdata, aes(condition2, ch2RTnum)) + geom_point(data = ds2, aes(y = mean), colour = 'red', size = 3) + geom_errorbar(
  data = ds2,
  aes(condition2, mean, ymin = mean - sd, ymax = mean + sd),
  colour = 'red',
  width = 0.4
) +
  labs(title = "Average response times for Choice 2 across monetary conditions",
       subtitle = "(no computer responses)",
       caption = "Data from Neuroeconomics Lab and Social Developmental Neuroscience Lab",
       tag = "Figure 2",
       x = "Money Choice",
       y = "Response Time")

g4 + theme_minimal() 


# Part 3 ------------------------------------------------------------------
#Average snack preference ratings between Task A, Task B (participants choosing for themselves), and Task B (participants choosing for their partners))

as.factor(



