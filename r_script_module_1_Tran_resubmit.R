# Trang Tran, ALY6015, Module 1 Practice Resubmission, Apr 26

cat("\014")  # clears console
rm(list = ls())  # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse, RColorBrewer, car, corrplot, skimr)

#1/Load the Ames housing dataset
df <- read.csv("AmesHousing.csv")

#2/Perform EDA and use descriptive statistics to describe the data
colnames(df)
head(df)
skim(df) #data types, missing values, min, max, mean, sd, hist

# remove Identifier variables & columns with > 20% missing values
df <- select(df,-Order, -PID, -Alley, -Fireplace.Qu, -Pool.QC, -Fence, -Misc.Feature)

# convert qualitative variables into factors
df$MS.SubClass <- factor(df$MS.SubClass)
df$Overall.Qual <- factor(df$Overall.Qual)
df$Overall.Cond <- factor(df$Overall.Cond)

# Histogram of SalePrice
ggplot(df, aes(x = SalePrice)) + 
  geom_histogram(fill = "steelblue", color = "white") +
  labs(title = "Distribution of SalePrice", x = "SalePrice")

# Boxplot of SalePrice by Neighborhood
ggplot(df, aes(x = Neighborhood, y = SalePrice)) + 
  geom_boxplot() +
  labs(title = "SalePrice by Neighborhood", x = "Neighborhood", y = "SalePrice")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1,
                                   vjust = 1))

#3/Prepare the dataset for modeling by imputing missing values with the variable's mean
# impute mean in all numeric columns
nums <- colnames(df[, unlist(lapply(df, is.numeric)), drop = FALSE]) 
for (col_name in nums) {
  df[is.na(df[, col_name]), col_name] <- mean(df[, col_name], na.rm = TRUE)
}

#4/Use the "cor()" function to produce a correlation matrix of the numeric values
corrs <- round(cor(df[, unlist(lapply(df, is.numeric))]), 2)

#5/Produce a plot of the correlation matrix, and explain how to interpret it
corrplot(corrs, type = "upper", col = brewer.pal(n = 8, name = "RdYlBu"))

#6/Find the highest/ lowest/ closet-to-0.5 correlation with 'SalePrice'
corr_with_SalePrice <- sort(corrs["SalePrice",], decreasing = TRUE)

# Scatter plot of SalePrice vs. Gr.Liv.Area (highest correlation = 0.71)
ggplot(df, aes(x = Gr.Liv.Area, y = SalePrice)) + 
  geom_point(color = "darkgreen", alpha = 0.5) +
  labs(title = "SalePrice vs. Gr.Liv.Area")

# Scatter plot of SalePrice vs. BsmtFin.SF.2  (lowest correlation = 0.01)
ggplot(df, aes(x = BsmtFin.SF.2 , y = SalePrice)) + 
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "SalePrice vs. BsmtFin.SF.2 ")

# Scatter plot of SalePrice vs. Mas.Vnr.Area (correlation closest to 0.5)
ggplot(df, aes(x = Mas.Vnr.Area, y = SalePrice)) + 
  geom_point(color = "brown", alpha = 0.5) +
  labs(title = "SalePrice vs. Mas.Vnr.Area")

#7/Using at least 3 continuous variables, fit a regression model in R
fit <- lm(SalePrice ~ Gr.Liv.Area + Total.Bsmt.SF + Garage.Area, data = df)
print(summary(fit))

#8/Report the model in equation form and interpret each coefficient of the model
# SalePrice = -29593.64 + 68.86 * Gr.Liv.Area + 54.59 * Total.Bsmt.SF + 105.15 * Garage.Area

#9/Use the "plot()" function to plot your regression model. Interpret the four graphs that are produced
par(mfrow = c(2, 2))
plot(fit)

#10/Check your model for multicollinearity and report your findings. What steps would you
# take to correct multicollinearity if it exists?
vif <- print(vif(fit))
write.csv(vif, "~/Documents/vif.csv")

#11/Check your model for outliers and report your findings. Should these observations be
# removed from the model?
# Remove influential points
df <- slice(df, -2182, -2181, -1499)

#rerun the model
fit <- lm(SalePrice ~ Gr.Liv.Area + Total.Bsmt.SF + Garage.Area, data = df)
print(summary(fit))

#replot the model
par(mfrow = c(2, 2))
plot(fit)

#12/Attempt to correct any issues that you have discovered in your model
# Did your changes improve the model, why or why not?

#13/Use the all subsets regression method to identify the "best" model. State the preferred model in equation form
library(leaps)
library(MASS)

# Perform all-subsets regression to find the best model
regfit_2 <- regsubsets(SalePrice~ Lot.Area + Year.Built + Mas.Vnr.Area + BsmtFin.SF.1 +
                      BsmtFin.SF.2 + Total.Bsmt.SF + Gr.Liv.Area + Bsmt.Full.Bath +
                      Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + TotRms.AbvGrd +
                      Garage.Area + Wood.Deck.SF + Open.Porch.SF + Pool.Area, data = df)

fit2_sum <- summary(regfit_2)
names(fit2_sum)

# check the best number of predictor varibles
data.frame(Adj.R2 = which.max(fit2_sum$adjr2), CP = which.min(fit2_sum$cp),
           BIC = which.min(fit2_sum$bic), RSQ = which.max(fit2_sum$rsq),
           RSS = which.min(fit2_sum$rss)) #8 variables

# check the preferred model
coef(regfit_2, 8)

best_fit <- lm(SalePrice ~ Year.Built + Mas.Vnr.Area + BsmtFin.SF.1 + Total.Bsmt.SF+
                 Gr.Liv.Area + Bedroom.AbvGr + Kitchen.AbvGr + Garage.Area, data =df)
print(summary(best_fit))

#diagnostic plot
par(mfrow = c(2, 2))
plot(best_fit)

#14/Compare the preferred model from step 13 with your model from step 12
# How do they differ? Which model do you prefer and why?

