library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)
library(readxl)
library(ggcorrplot)
library(corrr)
library(gridExtra)
library(psych)
library(kableExtra)
library(base)
library(MASS)
library(corrplot) 
library(ggpubr)


# 1. Blood Types
# Create a vector of the values
observed <- c(12,8,24,6)
# Create a vector of the probabilities
p <-c(0.2,0.28, 0.36,0.16)
# Set significance level
alpha <- 0.10
# Run the test
result <- chisq.test(x=observed, p = p)
# Check the results
result$statistic # Chi-square test value
result$p.value   # Chi-square p-value
result$parameter  # degrees of freedom

# Compare p-value to alpha and make decision
ifelse(result$p.value > alpha, "Fail to reject  the null hypothesis", "Reject the null hypothesis")

# 2. On-time Performance
# Create a vector of the values
observed <- c(125,10,25,40)
# Create a vector of the probabilities
p <-c(0.708,0.082, 0.09,0.12)
# Set significance level
alpha <- 0.05
# Run the test
result <- chisq.test(x=observed, p = p)
# Check the results
result$statistic # Chi-square test value
result$p.value   # Chi-square p-value
result$parameter  # degrees of freedom

# Compare p-value to alpha and make decision
ifelse(result$p.value > alpha, "Fail to reject  the null hypothesis", "Reject the null hypothesis")

# 3. Ethnicity and Movie Admissions
# Set significance level
alpha <- 0.05
# Create one vector for each row
r1 <- c(724, 335, 174, 107)
r2 <- c(370, 292, 152, 140)
# State the number of rows for the matrix
rows = 2
# Create a matrix from the rows
mtrx = matrix(c(r1,r2), nrow = rows, byrow = TRUE)

# Name the rows and column matrix
rownames(mtrx) = c("2013", "2014")
colnames(mtrx) = c("Caucasian", "Hispanic", "African American", "Other")
# View the matrix
mtrx
# Run the test 
result <- chisq.test(mtrx)
# Check the results
result$statistic # Chi-square test value
result$p.value   # Chi-square p-value
result$parameter  # degrees of freedom

# Compare p-value to alpha and make decision
ifelse(result$p.value > alpha, "Fail to reject  the null hypothesis", "Reject the null hypothesis")


# 4. Women in Military
# Set significance level
alpha <- 0.05
# Create one vector for each row
r1 <- c(10791, 62491)
r2 <- c(7816, 42750)
r3 <- c(932, 9525)
r4 <- c(11819, 54344)
# State the number of rows for the matrix
rows = 4
# Create a matrix from the rows
mtrx = matrix(c(r1,r2,r3,r4), nrow = rows, byrow = TRUE)

# Name the rows and column matrix
rownames(mtrx) = c("Army", "Navy", "Marine Corps", "Air Force")
colnames(mtrx) = c("Officers", "Enlisted")
# View the matrix
mtrx
# Run the test 
result <- chisq.test(mtrx)
# Check the results
result$statistic # Chi-square test value
result$p.value   # Chi-square p-value
result$parameter  # degrees of freedom

# Compare p-value to alpha and make decision
ifelse(result$p.value > alpha, "Fail to reject  the null hypothesis", "Reject the null hypothesis")

# 5. Sodium Content of Foods

# Set significance level
alpha <- 0.05
# Create a data frame for Condiments
condiments <- data.frame('sodium' = c(270, 130, 230, 180, 80, 70, 200), 'food' =rep('condiments',7), stringsAsFactors = FALSE)
# Create a data frame for Cereals
cereals <- data.frame('sodium' = c(260, 220, 290, 290, 200, 320, 140), 'food' =rep('cereals',7), stringsAsFactors = FALSE)
# Create a data frame for Desserts
desserts <- data.frame('sodium' = c(100, 180, 250, 250, 300, 360, 300, 160), 'food' =rep('desserts',8), stringsAsFactors = FALSE)
# Combine the data frames into one
sodium <-rbind(condiments, cereals, desserts)
sodium$food <- as.factor(sodium$food)
# Run the ANOVA test
anova <- aov(sodium ~ food, data = sodium)
# View the model summary
summary(anova)
Summary <-summary(anova)
# Extract the F value
F_value <- Summary[[1]][[1,'F value']]
F_value
# Extract the p.value
p.value <- Summary[[1]][[1,'Pr(>F)']]
p.value
# Compare p-value to alpha and make decision
ifelse(p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")


# 6. Sales for Leading Companies

# Set significance level
alpha <- 0.01
# Create a data frame for Cereal
cereal <- data.frame('sales' = c(578, 320, 264, 249, 237), 'leading companies' =rep('cereal',5), stringsAsFactors = FALSE)
# Create a data frame for Chocolate Candy
chocolate <- data.frame('sales' = c(311, 106, 109, 125, 173), 'leading companies' =rep('chocolate',5), stringsAsFactors = FALSE)
# Create a data frame for Coffee
coffee <- data.frame('sales' = c(261, 185, 302, 689), 'leading companies' =rep('coffee',4), stringsAsFactors = FALSE)
# Combine the data frames into one
sales <-rbind(cereal, chocolate, coffee)
sales$leading.companies <- as.factor(sales$leading.companies)
# Run the ANOVA test
anova <- aov(sales ~ leading.companies, data = sales)
# View the model summary
summary(anova)
a.summary <-summary(anova)
# Extract the F value
F_value <- a.summary[[1]][[1,'F value']]
F_value
# Extract the p.value
p.value <- a.summary[[1]][[1,'Pr(>F)']]
p.value
# Compare p-value to alpha and make decision
ifelse(p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")


# 7. Per-Pupils Expenditures

# Set significance level
alpha <- 0.05
# Create a data frame for Eastern Third
eastern <- data.frame('expenditures' = c(4946, 5953, 6202, 7243, 6113), 'sections' =rep('eastern',5), stringsAsFactors = FALSE)
# Create a data frame for Middle Third
middle <- data.frame('expenditures' = c(6149, 7451, 6000, 6479), 'sections' =rep('middle',4), stringsAsFactors = FALSE)
# Create a data frame Western Third
western <- data.frame('expenditures' = c(5282, 8605, 6528, 6911), 'sections' =rep('western',4), stringsAsFactors = FALSE)
# Combine the data frames into one
expenditures <-rbind(eastern, middle, western)
expenditures$sections <- as.factor(expenditures$sections)
# Run the ANOVA test
anova <- aov(expenditures ~ sections, data = expenditures)
# View the model summary
summary(anova)
b.summary <-summary(anova)
# Extract the F value
F_value <- b.summary[[1]][[1,'F value']]
F_value
# Extract the p.value
p.value <- b.summary[[1]][[1,'Pr(>F)']]
p.value
# Compare p-value to alpha and make decision
ifelse(p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# 8. Increasing Growth Plant
# We create a data frame in Excel using the values of the growth measure and import into R

growth_data <- read.csv("C:\\Users\\selah\\Desktop\\Bimpyus\\Plant Growth.csv") #load the dataset

growth_data$Plant.Food <- factor(growth_data$Plant.Food, 
                            levels = c(1, 2),
                            labels = c("Plant Food A", "Plant Food B"))
growth_data$Block <- factor(growth_data$Block, 
                          levels = c(1, 2, 3, 4),
                          labels = c("B1", "B2", "B3", "B4"))
growth_data$Grow.light <- factor(growth_data$Grow.light, 
                               levels = c(1, 2),
                               labels = c("Grow Light 1", "Grow Light 2"))
str(growth_data)

alpha <- 0.05
aovv <- aov(Growth.Measure ~ Grow.light * Plant.Food, data = growth_data)
aovv <- aov(Growth.Measure ~ Grow.light + Plant.Food + Grow.light:Plant.Food, data = growth_data)
summary(aovv)
smry <- summary(aovv)

# Extract the F value for Grow light
F_value1 <- smry[[1]][[1,'F value']]
F_value1
# Extract the p.value for Grow light
p.value1 <- smry[[1]][[1,'Pr(>F)']]
p.value1
# Compare p-value to alpha and make decision
ifelse(p.value1 > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# Extract the F value for Plant Food
F_value2 <- smry[[1]][[2,'F value']]
F_value2
# Extract the p.value for Plant Food
p.value2 <- smry[[1]][[2,'Pr(>F)']]
p.value2
# Compare p-value to alpha and make decision
ifelse(p.value2 > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# Extract the F value for the Interaction
F_value3 <- smry[[1]][[3,'F value']]
F_value3
# Extract the p.value for Plant Food
p.value3 <- smry[[1]][[3,'Pr(>F)']]
p.value3
# Compare p-value to alpha and make decision
ifelse(p.value3 > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")




# Exploratory Data Analysis

baseball <- read.csv("C:\\Users\\selah\\Desktop\\Bimpyus\\baseball.csv") #load the dataset
names(baseball) # check the column names
str(baseball) #data types
dim(baseball) # check for the dimension of the dataset
cat_vars <- names(baseball)[which(sapply(baseball, is.character))] # identify variables that are categorical
cat_vars
numeric_vars <- names(baseball)[which(sapply(baseball, is.numeric))] # identify variables that are numerical
numeric_vars
colSums(sapply(baseball, is.na)) # Check for missing values

mysample_num <- select_if(baseball, is.numeric) # Filter the data to select Numeric variables
df <- subset(mysample_num, select = c(Year, RS, RA, W, OBP, SLG, BA,G, OOBP, OSLG)) #Selet the Relevant variables
str(df)
colSums(sapply(df, is.na)) # Check for missing values
describe(df) # Descriptive statistics
correlate(df)
cor.plot(df)

baseball_dt<-baseball[,c("RS","RA","W","OBP","SLG","BA","Playoffs","G")]

##data visualization

a=ggplot(data = baseball,aes(x=League,y=W))+
  geom_bar(stat = 'identity',aes(fill=League))+
  theme_minimal()
b=ggplot(data = baseball,aes(x=League,y=RS))+
  geom_bar(stat = 'identity',aes(fill=League))+
  theme_minimal()
c=ggplot(data = baseball,aes(x=League,y=RA))+
  geom_bar(stat = 'identity',aes(fill=League))+
  theme_minimal()
d=ggplot(data = baseball,aes(x=League,y=G))+
  geom_bar(stat = 'identity',aes(fill=League))+
  theme_minimal()
comb_plot<-ggarrange(a,b,c,d,nrow = 2,ncol = 2)
comb_plot

#box plot of the data
e=ggplot(data = baseball,aes(x=League,y=W))+
  geom_boxplot(outlier.color = "black",outlier.shape = 16,outlier.size = 2,notch = F)+
  theme_minimal()
f=ggplot(data = baseball,aes(x=League,y=RS))+
  geom_boxplot(outlier.color = "black",outlier.shape = 16,outlier.size = 2,notch = F)+
  theme_minimal()
g=ggplot(data = baseball,aes(x=League,y=RA))+
  geom_boxplot(outlier.color = "black",outlier.shape = 16,outlier.size = 2,notch = F)+
  theme_minimal()
h=ggplot(data = baseball,aes(x=League,y=G))+
  geom_boxplot(outlier.color = "black",outlier.shape = 16,outlier.size = 2,notch = F)+
  theme_minimal()
comb_plot1<-ggarrange(e,f,g,h,nrow = 2,ncol = 2)
comb_plot1

# Scatter Plot
ggplot(df,aes(x=RS,y=OBP))+geom_point()+
  xlab("Runs Allowed")+ylab("On-base Percentage")+scale_color_brewer(palette="Dark2")+theme_bw()

ggplot(df,aes(x=OBP,y=SLG))+geom_point()+
  xlab("Runs Allowed")+ylab("Slugging Percentage")+scale_color_brewer(palette="Set1")+theme_bw()


ggplot(df,aes(x=BA,y=SLG))+geom_point()+
  xlab("Battling Average")+ylab("Slugging Percentage")+scale_color_brewer(palette="BrBG")+theme_bw()

# Histogram and Density Plot

hist(df$W, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Wins",
     main = "Wins")
lines(density(df$W), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")


hist(df$RA, # histogram
     col="Blue", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Runs Allowed",
     main = "Runs Aloowed")
lines(density(df$RA), # density plot
      lwd = 2, # thickness of line
      col = "Red")

hist(df$RS, # histogram
     col="Purple", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Runs Scored",
     main = "Runs Scored")
lines(density(df$RS), # density plot
      lwd = 2, # thickness of line
      col = "Red")



# Extract decade from year
Decade <- df$Year - (df$Year %% 10)

# Create a wins table by summing the wins by decade
Wins <- df %>%
  group_by(Decade) %>%
  summarize(wins = sum(W)) %>%
  as.tibble()

chisq <- chisq.test(Wins)
chisq
chisq$observed

crop_data <- read.csv("C:\\Users\\selah\\Desktop\\Bimpyus\\crop_data.csv") #load the dataset

crop_data$density <- factor(crop_data$density, 
                       levels = c(1, 2),
                       labels = c("D1", "D2"))
crop_data$block <- factor(crop_data$block, 
                            levels = c(1, 2, 3, 4),
                            labels = c("B1", "B2", "B3", "B4"))
crop_data$fertilizer <- factor(crop_data$fertilizer, 
                          levels = c(1, 2, 3),
                          labels = c("F1", "F2", "F3"))
str(crop_data)
head(crop_data)

res.aov <- aov(yield ~ fertilizer * density, data = crop_data)
res.aov <- aov(yield ~ fertilizer + density + fertilizer:density, data = crop_data)
summary(res.aov)

