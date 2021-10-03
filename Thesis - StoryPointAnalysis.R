# install libraries

if(!require("pacman")) install.packages("pacman")
p_load(tidyverse)
p_load(magrittr)
p_load(plotly)
p_load(ggplot2)
p_load(dplyr)
p_load(stats)
p_load(lsr)
p_load(TeachingDemos)
p_load(fastDummies)
p_load(caret)
p_load(OneR)
p_load(regclass)
p_load(faraway)
p_load(reshape2)
p_load(sjmisc)
p_load(sjPlot)
p_load(HistogramTools)
p_load(limma)
p_load(MASS)

# fetch and view data 

data <- read.csv("C:/Users/damia/Desktop/Damian/Thesis/Clean.csv")

# change Story Point to factor (although a number, it is not numeric)

data$StoryPoint <- as.character(data$StoryPoint)
table(data$StoryPoint)

# visualise and understand data

head(data)
str(data)

hist(data$Estimate, col = "light green", xlab = "Estimated Hours", main = "Histogram - Estimated Hours", las = 1)
hist(data$Actual, col = "dark green", xlab = "Actual Hours", main = "Histogram - Actual Hours", las = 1)

## qq plots

qqnorm(data$Estimate, pch = 1, frame = FALSE, col = "steelblue", las = 1, xlab = "Normal Quantiles",
       cex = 2.5, ylab = "Estimate Quantiles", main = "Normal Q-Q Plot (Estimates)")
qqline(data$Estimate, col = "light green", lwd = 5)

qqnorm(data$Actual, pch = 1, frame = FALSE, col = "steelblue", las = 1, xlab = "Normal Quantiles",
       cex = 2.5, ylab = "Actual Quantiles", main = "Normal Q-Q Plot (Actual Hours)")
qqline(data$Estimate, col = "dark green", lwd = 5)

## hypothesis test for normality

shapiro.test(data$Estimate)
shapiro.test(data$Actual)

str(data)
summary(data)
hist(data$Actual)

## scatter plot
 
plot_ly(data = data, x = ~Estimate, y = ~Actual, type = "scatter", mode = "markers") %>% 
  add_lines(x = ~Estimate, y = fitted(lm(Actual ~ Estimate, data = data)))


# box plot

plot_ly(data, y = ~Actual, x = ~StoryPoint, color = I("blue"), 
             alpha = 0.1, boxpoints = "suspectedoutliers") %>% 
            add_boxplot(color = ~StoryPoint) %>%
            layout(yaxis = list(title = "Actual Hours"))

# qqplot (y = data.frame(matrix(data$Estimate)), x = data.frame(matrix(data$StoryPoint)), plot.it = TRUE, col  = "light green",
#        deparse(subsittuet(x)), deparse(substitute(y)), xlab = "Story Point", ylab =  "Estimate")
# qqline(data$Estimate, col = "green", lwd = 5, datax = T)


# analyse correlation between estimates and actual

## Spearman's Rank for numeric variables

cor.test(x = data$Estimate, y = data$Actual, method = c("spearman"))

# bin Actual

data$ActualBins1 <- as.factor(bin(data = data$Actual, labels = c("0-2","2-4","4-6","6-8",">8"),
                        nbins = 5, method = "length"))

data$ActualBins2 <- bin(data = data$Actual, labels = c("0-1","1-2","2-3","3-4","4-5",
                                                       "5-6","6-7","7-8",">8"),
                        nbins = 9, method = "length")

data$ActualBins3 <- bin(data = data$Actual, labels = c("0-0.5","0.5-1","1-1.5","1.5-2",
                                                       "2.-2.5","2.5-3","3-3.5","3.5-4",
                                                       "4-4.5","4.5-5","5-5.5","5.5-6",
                                                       "6-6.5","6.5-7","7-7.5","7.5-8",">8"),
                        nbins = 17, method = "length")



dataBins1 <- data.frame(data$ActualBins1, data$StoryPoint)
cramersV(table(dataBins1))

dataBins2 <- data.frame(data$ActualBins2, data$StoryPoint)
cramersV(table(dataBins2))

dataBins3 <- data.frame(data$ActualBins3, data$StoryPoint)
cramersV(table(dataBins3))

## eta squared - for categorical/numeric variables
## eta sq tell how much variation in the dependent valuable is explained by
## variation in the independent variable

dataAnova <- aov(data$Actual ~ data$StoryPoint)
etasq <- etaSquared(dataAnova)
eta.assoc <- sqrt(etasq[1])

# z=test to see if mean values are greater at each level

pairwise.z.test <- data.frame(matrix(nrow = 12, ncol = 12, byrow = T, dimnames = list(levels(data))))



# sampledata2 <- sampledata[sampledata$StoryPoint != 0.1 & sampledata$StoryPoint != 0.75, ]
# pairwise returns all null values if any level contains only one value - remove corresponding levels
# pairwise.t.test(x = sampledata2$Actual, g = sampledata2$StoryPoint, p.adj = "none", alternative = c("greater"), paired = F)


# Build lm model 

## for the purposes of this exercise, given the characteristics of what is entailed in an Story Point estimate,
## (intensity, degree of diffculty etc of work), let the Actual Time work takes to be completed be dependent 
## upon the Story Point

## hence the model will be model a linear function whereby : Actual ~ Story Point + C

# create dummy columns for categriocal variable

dummy.data <- dummy_cols(.data = data, select_columns = c("StoryPoint"), remove_first_dummy = T)

# select necessary columns into a variable

dumm.data <- dummy.data[ , c(4,9:19)]

## check for multicollinearity

corr <- round(cor(dumm.data[,2:12]),2)

# heat map


melt_corr <- melt(corr)

ggplot(data = melt_corr, aes(x=Var1, y=Var2, fill=value)) + 
geom_tile() +
geom_text(aes(Var2, Var1, label = value), colour = "white", size = 3)

#_____________________________________________________________________________________________

# linear model

lm.actual <- lm(data = dumm.data, formula = Actual ~ .)

# model detail 

summary(lm.actual)

# second check for multicollinearity
vif(lm.actual)


par(mfrow = c(1,1))

# homoscedasticity and normality of residuals (not looking normal)
plot(lm.actual, col = "dark green", cex = 2)

# test residuals for normality
ks.test(lm.actual$residuals, pnorm)

ggplot(data.frame(lm.actual$residuals), aes(lm.actual$residuals)) + 
  geom_histogram(colour = "white", fill = "dark green")

#test residuals for independence
cor(x = lm.actual$residuals, 
    y = predict(lm.actual),
    method = c("pearson"))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#lm model residuals are not normally distributed. try log of actual

#lm.log.actual <- lm(data = dumm.data, formula = log(Actual) ~ .)

# model detail 

#summary(lm.log.actual)

# second check for multicollinearity
#vif(lm.actual)


# homoscedasticity and normality of residuals (not looking normal)
#plot(lm.log.actual, col = "dark green", cex = 2)


# test residuals for normality
#ks.test(lm.log.actual$residuals, pnorm)

#ggplot(data.frame(lm.log.actual$residuals), aes(lm.log.actual$residuals)) + 
#  geom_histogram(colour = "white", fill = "dark green") 

# test for independence

#cor(x = lm.log.actual$residuals,
#    y = predict(lm.log.actual),
#    method = c("pearson"))

#resid.results <- data.frame(cbind(resid = lm.log.actual$residuals,
#                                  pred = predict(lm.log.actual)))


#plot_ly(data = resid.results, x = ~zscore(pred, "norm"), y = ~zscore(resid, "norm"), type = "scatter", mode = "markers") %>% 
#  add_lines(x = ~Estimate, y = fitted(lm(Actual ~ Estimate, data = data)))


#________________________________________________________________________________________________
  
#Kruskal-Wallis test

# 3 asumptions

## 1 independent groups
## 2 dependent variable is continuous
## 3 no normality required
## 4 homoscedasticity 

# data2 <- data[data$StoryPoint != c(0.75, 8, 9),]
# bartlett.test(Actual ~ StoryPoint, data = data[data$StoryPoint != c(0.75, 8, 9),])


#1

kw.data <- kruskal.test(x = data$Actual, g = data$StoryPoint, Actual ~ StoryPoint, data = data)

#2

data$StoryPoint <- as.factor(data$StoryPoint)

# check for grouping

pairwise.wilcox.test(data$Actual, data$StoryPoint,
                     p.adjust.method = "BH", alternative = "two.sided")


# check if ordinal

pairwise.wilcox.test(data$Actual, data$StoryPoint,
                     p.adjust.method = "BH", alternative = "greater")


# ## bootstrapping
# 
# # set seed in order to replicate outcome
# set.seed(1234)
# 
# # select variables with multiple numeric levels
# count_of_ones <- data.frame(Records = apply(dumm.data[,2:12], 2, function(x) {length(x[x==1])}))
# 
# # Story Point 0.5, 1, 2, 3 and 5 hav multiple assoc hours and are good candidates for bootstrapping
# #select Story_0.5
# sp_0.5 <- data$Actual[data$StoryPoint==0.5]
# 
# #bootstrap
# #select Story_0.5
# bstrap_0.5 <- replicate(10000, mean(sample(sp_0.5, length(sp_0.5)-4, replace = T)))
# mean(bstrap_0.5)
# sd(bstrap_0.5)
# #bootstrap_0.5 distribution Quartiles
# quantile(bstrap_0.5, c(0.025, 0.975))
# #histogram
# ggplot(data.frame(bstrap_0.5), aes(x=bstrap_0.5)) + 
#   geom_histogram(colour = "white", fill = "dark green") +
#   geom_vline(xintercept=(mean(bstrap_0.5) - 2 * sd(bstrap_0.5)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=(mean(bstrap_0.5) + 2 * sd(bstrap_0.5)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=mean(bstrap_0.5), color = "red") +
#   geom_vline(xintercept=lm.actual[["coefficients"]][["StoryPoint_0.5"]], color = "black")
# 
# #select Story_1
# sp_1 <- data$Actual[data$StoryPoint==1]
# #bootstrap
# bstrap_1 <- replicate(10000, mean(sample(sp_1, length(sp_1)-4, replace = T)))
# mean(bstrap_1)
# sd(bstrap_1)
# #bootstrap_1 distribution Quartiles
# quantile(bstrap_1, c(0.025, 0.975))
# #histogram
# ggplot(data.frame(bstrap_1), aes(x=bstrap_1)) + 
#   geom_histogram(colour = "white", fill = "dark green") +
#   geom_vline(xintercept=(mean(bstrap_1) - 2 * sd(bstrap_1)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=(mean(bstrap_1) + 2 * sd(bstrap_1)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=mean(bstrap_1), color = "red") +
#   geom_vline(xintercept=lm.actual[["coefficients"]][["StoryPoint_1"]], color = "black")
# 
# #select Story_2
# sp_2 <- data$Actual[data$StoryPoint==2]
# #bootstrap
# bstrap_2 <- replicate(10000, mean(sample(sp_2, length(sp_2)-4, replace = T)))
# mean(bstrap_2)
# sd(bstrap_2)
# #bootstrap_2 distribution Quartiles
# quantile(bstrap_2, c(0.025, 0.975))
# #histogram
# ggplot(data.frame(bstrap_2), aes(x=bstrap_2)) + 
#   geom_histogram(colour = "white", fill = "dark green") +
#   geom_vline(xintercept=(mean(bstrap_2) - 2 * sd(bstrap_2)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=(mean(bstrap_2) + 2 * sd(bstrap_2)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=mean(bstrap_2), color = "red") +
#   geom_vline(xintercept=lm.actual[["coefficients"]][["StoryPoint_2"]], color = "black")
# 
# #select Story_3
# sp_3 <- data$Actual[data$StoryPoint==3]
# #bootstrap
# bstrap_3 <- replicate(10000, mean(sample(sp_3, length(sp_3)-4, replace = T)))
# mean(bstrap_3)
# sd(bstrap_3)
# #bootstrap_3 distribution Quartiles
# quantile(bstrap_3, c(0.025, 0.975))
# #histogram
# ggplot(data.frame(bstrap_3), aes(x=bstrap_3)) + 
#   geom_histogram(colour = "white", fill = "dark green") +
#   geom_vline(xintercept=(mean(bstrap_3) - 2 * sd(bstrap_3)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=(mean(bstrap_3) + 2 * sd(bstrap_3)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=mean(bstrap_3), color = "red") +
#   geom_vline(xintercept=lm.actual[["coefficients"]][["StoryPoint_3"]], color = "black")
# 
# #select Story_5
# sp_5 <- data$Actual[data$StoryPoint==5]
# #bootstrap
# bstrap_5 <- replicate(10000, mean(sample(sp_5, length(sp_5)-4, replace = T)))
# mean(bstrap_5)
# sd(bstrap_5)
# #bootstrap_5 distribution Quartiles
# quantile(bstrap_5, c(0.025, 0.975))
# #histogram
# ggplot(data.frame(bstrap_5), aes(x=bstrap_5)) + 
#   geom_histogram(colour = "white", fill = "dark green") +
#   geom_vline(xintercept=(mean(bstrap_5) - 2 * sd(bstrap_5)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=(mean(bstrap_5) + 2 * sd(bstrap_5)), linetype="dashed", color = "blue") +
#   geom_vline(xintercept=mean(bstrap_5), color = "red") +
#   geom_vline(xintercept=lm.actual[["coefficients"]][["StoryPoint_5"]], color = "black")

#________________________________________________________________________________________
#stepwise selection
lm.step <- stepAIC(lm.actual, direction = "both", trace = F)
summary(lm.step)
# homoscedasticity and normality of residuals (not looking normal)
plot(lm.step, col = "dark green", cex = 2)
# test residuals for normality
ks.test(lm.step$residuals, pnorm)
ggplot(data.frame(lm.step$residuals), aes(lm.step$residuals)) + 
  geom_histogram(colour = "white", fill = "dark green") 
# test for independence
cor(x = lm.step$residuals,
    y = predict(lm.step),
    method = c("pearson"))
