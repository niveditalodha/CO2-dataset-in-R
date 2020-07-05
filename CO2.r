#EDA on built-in datasets in R

#view all the built-in datasets in R
data()

#viewing an inbuilt dataset named CO2
CO2

# ? is used to get infomation about any entity in R
# here we use ?(CO2) to get information about the CO2
?CO2

#structure of the datasets
str(CO2)

#class of dataset
class(CO2)

#loading the CO2 dataset into a variable

carbon<-CO2

print(carbon)

#performing EDA on DATASET
#EDA stand for exploratory data analysis in which we analyze the datasset to summerize their main characteristics

#EDA on CO2 dataset
#nrow() function is used to print the number of rows in dataset
nrow(carbon)

#ncol() function is used to print the number of columns in the dataset
ncol(carbon)

#print dimensions
dim(carbon)

#print column names
names(carbon)

#see all the attributes
attributes(carbon)

#check for null values in dataset
is.null(carbon)

#duplicated function is used to dicarbonlay duplicate values
#it prints 'false' for non duplicate values and 'true' for duplicate values
duplicated(carbon)

#print summary
summary(carbon)

#covariance
cov(carbon[,4:5])

#correlation
cor(carbon[,4:5])

#distribution of concentrate value
table(carbon$conc)

#scatterplot of conc and uptake rate
pairs(~carbon$conc+carbon$uptake)

#From the scatterplot, there is no linear relationship between conc and uptake but there may be a clearer relationship once the data is separated into sub-categories
plot(carbon$conc,
     carbon$uptake,
     xlim = c(0, 1300),
     xlab = "conc",
     ylab = "uptake",
     main = "Scatterplot by treatment",
     col = ifelse(carbon$Treatment=="nonchilled",
                  "red",
                  "blue"),pch = 16) 
legend("topright",
       c("nonchilled",
         "chilled"), col = c("red", "blue"),
       pch = 16,
       title = "Treatment")

#There is still no linear relationship but nonchilled plants have relatively higher uptake level then chilled plants

plot(carbon$conc,
     carbon$uptake,
     xlim = c(0, 1300),
     xlab = "conc",
     ylab = "uptake",
     main = "Scatterplot by Plant",
     col = ifelse(carbon$Type=="Quebec",
                  "orange",
                  "green"),pch = 16) 
legend("topright",
       c("Quebec",
         "Mississippi"), col = c("orange", "green"),
       pch = 16,
       title = "Type")

#There is still no linear relationship but Quebec plants have relatively higher uptake level then Mississippi plants

plot(carbon$conc,
     carbon$uptake,
     xlab = "conc",
     ylab = "uptake",
     main = "Scatterplot by plant",
     col = rainbow(12)[carbon$Plant],pch = 16)

#There seem to be a nonlinear relationship between the 2 variables.


install.packages("ggplot2")
library("ggplot2")

#Use ggplot2 to create a boxplot comparing the CO2 uptake between the two treatments of grass- "chilled" and "nonchilled"
ggplot(carbon, aes(x=Treatment, y=uptake)) + geom_boxplot()

#Create a boxplot comparing the CO2 uptake between the two types of grass- "Quebec" and "Mississippi"

aggregate(uptake ~ Type, summary, data=carbon)
ggplot(carbon, aes(x=Type, y=uptake)) + geom_boxplot()

#Atleast half of the Plants from Mississippi (Median = 19.3) has less co2 uptake than Quebec (Median = 37.15).


#GGPLOT
#graphical anyalysis using ggplot2
#box plot 1
bp <<-ggplot(data=carbon, aes(y= uptake, x=Plant, fill=Type ) ) # Creates boxplots
bp <- bp + geom_boxplot() # Adds color
bp <- bp + ggtitle("CO2 uptake rates for the twelve plants") # Adds a title
bp <- bp +  ylab("uptake") + xlab("Plant/Type") # Adds kaveks
bp # displays the boxplots

#box plot 2
bp1 <<-ggplot(data=carbon, aes(y= uptake, x=Plant, fill=Treatment ) ) # Creates boxplots
bp1 <- bp1 + geom_boxplot() # Adds color
bp1 <- bp1 + ggtitle("CO2 uptake rates for the twelve plants") # Adds a title
bp1 <- bp1 +  ylab("uptake") + xlab("Plant/Treatment") # Adds kaveks
bp1 # displays the boxplots

#There are a number of outliers in each category here too. Compare to the first boxplot, there is difference in skewness of data in each category. Only Qc3, Mn1 and Mc1 don't have outliers in their value.

hist(carbon$uptake, freq=FALSE)
lines(density(carbon$uptake), col="red")
#uptake is not normally distributed.

ggplot(carbon, aes(x=as.numeric(uptake), y=as.numeric(conc))) +
        geom_polygon(data = carbon, alpha = 0.5, aes(fill = Type)) +
        geom_point(size = 2, stroke = 1, aes(shape = Treatment)) +
        geom_point(aes(color = Plant,shape = Treatment), size = 1) +
        scale_shape_manual(values = c(21, 22))





#Regression and Correlation

#Regression
#Using ggplot2 to create a scatter plot with conc on the x-axis, uptake on the y-axis, where each point is colored according to its Type ("Quebec" vs "Mississippi").
ggplot(carbon, aes(x=conc, y=uptake, color=Type)) + geom_point() + scale_x_log10()

#Performing a multiple linear regression explaining CO2 uptake by both the log of the concentration and the Type variable.
mfit = lm(uptake ~ log(conc) + Type, data=carbon)
summary(mfit)

#Using ggplot2 to create a scatter plot with conc on the x-axis, uptake on the y-axis, where each point is colored according to its Type ("Quebec" vs "Mississippi") and the shape of each point (the "pch" or "shape" aesthetic) is determined by the Treatment ("chilled" vs "nonchilled").
ggplot(carbon, aes(x=conc, y=uptake, color=Type, shape=Treatment)) + geom_point() + scale_x_log10()

#Performing a multiple linear regression explaining CO2 uptake by the log of the concentration and the Type and Treatment variables. 
mfit2 = lm(uptake ~ log(conc) + Type + Treatment, data=carbon)
summary(mfit2)

#Based on this graph, the combination of treatment and type leads to the lowest CO2 uptake is Mississippi+chilled


#correlation
#making a data frame variable with CO2 dataset
carbon<- CO2

#display the dataset
carbon

#changing the categorical data with specific number values for each category
carbon$Plant = factor(carbon$Plant,
                          levels = c('Qn1', 'Qn2','Qn3','Qc1','Qc2','Qc3','Mn1','Mn2','Mn3','Mc1','Mc2','Mc3'),
                          labels = c(1,2,3,4,5,6,7,8,9,10,11,12))

#we get values in integer format, but we need numeric values for cor() function
#using as.numeric() to change integer to numeric values for column Plant
carbon$Plant = as.numeric(carbon$Plant)

#cor() function computes the correlation coefficient
cor(carbon$uptake, carbon$Plant)

#cor.test() test for association/correlation between paired samples.
#It returns both the correlation coefficient and the significance level(or p-value) of the correlation .
cor.test(carbon$uptake, carbon$Plant)

# assign the uptake
Uptake<-carbon$uptake
Plant<-carbon$Plant


# Create the liner model
linearModel <- lm(Uptake ~ Plant)


# plot the Co2 data, this is not sorted
par(mfrow = c(2,1))

plot(Plant, Uptake, main = "Unsorted CO2 data")

# create a line through the non sorted data
abline(linearModel,col = "red")

# Create the linear model
plot(Uptake, Plant, main = "Unsorted CO2 data")
abline(linearModel,col = "red")

