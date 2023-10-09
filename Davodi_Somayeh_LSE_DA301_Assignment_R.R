###                                                                          ###
######           LSE Data Analytics Online Career Accelerator            ###### 
##########  DA301:  Advanced Analytics for Organisational Impact     ##########
###############          Analyst: Somayeh Davodi                ###############
          ###########################################################
#########################      Introduction       #############################

# Turtle Games, is a game manufacturer and retailer, manufacturing and selling 
# their own products, along with sourcing and selling products manufactured by 
# other companies. Their product range includes books, board games, video games 
# and toys. They have a global customer base and have a business objective of 
# improving overall sales performance by utilising customer trends. As a data 
# analyst, my task is to provide actionable insights to Turtle Games answering 
# its business questions.

#########################    Business questions     ###########################

# Q1. What is the impact on sales per product? 
# Q2. The reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
# Q3. If there is any possible relationship(s) in sales between North America (NA), 
#    Europe (EU), and global sales.

##########################     Analysis Process       #########################
#######         1. Data exploration and data cleansing (EDA)             ######
###############################################################################

# Load and explore the data
# Set the working directory and read the file.
setwd("C:/Users/soma_/OneDrive/Desktop/LSE/Course 3_ABtesting_LinearReg_LogReg_DecTree_RandFrst_NLP/LSE_DA301_assignment_files")

# Install and import Tidyverse.
# install.packages('tidyverse')
library(tidyverse)

# Import the data set.
sales_data <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
dim(sales_data)
str(sales_data)
View(sales_data)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_data2 <- select(sales_data, -Ranking, -Year, -Genre, -Publisher)

# Change Product class to factor.
class(sales_data2$Product)
sales_data2$Product <- as.factor(sales_data2$Product)

# View the data frame.
View(sales_data2)

# View the descriptive statistics.
summary(sales_data2)
str(sales_data2)

###################################

# Review plots through quick plot to gain general insights into the data set.
# Fisrt lets import ggplot2 package and gridExtra for side by side plots.
# install.packages("ggplot2")
# install.packages('gridExtra')
library(ggplot2)
library(gridExtra)

## 2a) Scatterplots
# Create scatterplots.
scat_EU_NA <- qplot(NA_Sales, EU_Sales, data = sales_data2,
               xlab = 'North America Sales',
               ylab = 'EU Sales',
               main = 'EU Sales vs. North America', 
               colour= I('black'))

scat_NA_Global <- qplot(NA_Sales, Global_Sales, data = sales_data2,
               xlab = 'North America Sales',
               ylab = 'Global Sales',
               main = 'Global Sales vs. North America',
               colour =I('red'))

scat_EU_Global <- qplot(EU_Sales, Global_Sales, data = sales_data2,
               xlab = 'EU Sales',
               ylab = 'Global Sales',
               main = 'Global Sales vs. EU',
               colour= I('darkgreen'))

# Arrange and display the plots side by side.
sales_all_scatter <- grid.arrange(scat_EU_NA , 
                                  scat_NA_Global, 
                                  scat_EU_Global, 
                                  ncol = 3)
sales_all_scatter

# Observation: Strong positive correlation between Europe, NA and Global sales. 

###################################

## Histograms
# Create histograms.
hist_EU <- qplot(EU_Sales, data = sales_data2,
                 geom='histogram', bins=20,
                 fill=I('skyblue'), colour=I('black'), 
                 xlab='EU Sales') + 
  ggtitle('Sales Original Data')

hist_NA <- qplot(NA_Sales, data = sales_data2,
      geom='histogram', bins=20,
      fill=I('salmon'), colour=I('black'), 
      xlab='North America Sales') 

hist_Global <- qplot(Global_Sales, data = sales_data2,
              geom='histogram', bins=20,
              fill=I('green'), colour=I('black'), 
              xlab='Global Sales')

# Arrange and display the plots side by side
hist_all_sales <- grid.arrange(hist_EU, 
                               hist_NA, 
                               hist_Global, 
                               ncol = 3)
hist_all_sales

# Histograms observation: The three regions showed similar sort of sales 
# distribution with right-skewness.

###################################

## Box-plots.
# Analyse global sales relationship with NA and EU regions.
box_EU <- qplot(EU_Sales, data=sales_data2,
                geom='boxplot', fill=I('skyblue'),
                xlab="",xlim=c(0,20), main='EU Sales') + 
  ggtitle('Sales Original Data')

box_NA <- qplot(NA_Sales, data=sales_data2,
               geom='boxplot', fill=I('salmon'),
               xlab="", xlim=c(0,20), main='North America Sales')


box_Global <- qplot(Global_Sales, data=sales_data2, 
                geom='boxplot', fill=I('green'), 
                xlab="", xlim=c(0,20), main='Global sales')

# Arrange and display the plots side by side
box_all_sales <- grid.arrange(box_EU, 
                              box_NA, 
                              box_Global, 
                              ncol=1)
box_all_sales

# Boxplot observation: 
# Sales distribution of Europe has been less spread out (smaller
# IQR and range) and in general lower amounts of sales with 
# smaller median compared to NA. Global sales variation has been
# more significant compared to regional variations in sales.

################################################################################
#######         Q1: What is the impact on sales per product?             #######
################################################################################

## Find the top products sold globally.
# Subset dataframe for aggregated sales per product
sales_by_product <- sales_data2 %>% 
  aggregate(cbind(NA_Sales, EU_Sales, Global_Sales) ~ Product, FUN = sum) %>% 
  arrange(desc(Global_Sales))

# View the data frame.
head(sales_by_product)
View(sales_by_product)
str(sales_by_product)

# Explore the data frame.
dim(sales_by_product)
sales_per_product_discriptive_summary <- summary(sales_by_product[-1])
sales_per_product_discriptive_summary

# Plot global sales versus product_ID.

sales_productID_plot <- ggplot(sales_by_product , 
                               aes(x = Product, y=Global_Sales)) + 
  geom_point() +
  labs (x = 'Product_ID', 
        y = 'Global Sales (million £)',
        title = 'Global Sales per Products_ID') +
  theme_bw() 
sales_productID_plot


# Plot top 10 sold products.
Top_10_products <- sales_by_product %>% head(10)
top_product_plot <- ggplot(Top_10_products , 
                       aes(x = Product, y=Global_Sales)) + 
  geom_point() +
  labs (x = 'Product_ID', 
        y = 'Global Sales (million £)',
        title = 'Top 10 Globally Sold Products') +
  geom_text(aes(label = Product ), 
            nudge_x =0.25, nudge_y = 2, 
            size =3,
            colour='red') +  # Add labels
  theme_bw()
top_product_plot

grid.arrange(sales_productID_plot, top_product_plot, ncol = 2)

################################################################################
#######             Answer to Q1: Product impact on sales:               #######

# There is a declining trend between the product_ID and globalsales. However, 
# this is not making sense as product_ID is a categorical variable. Therefore, 
# to identify the most selling products, top 10 products were shown. To improve 
# analyse and investigat the products impact on sales, we can encode product_ID 
# and define dummy_variables and use machine learning models such as multiple 
# linear regression model.  
################################################################################

################################################################################
#######             Q2. The reliability of the data                      #######
#######         (e.g. normal distribution, Skewness, Kurtosis)           #######
################################################################################

## 2: Determine data distribution and normality.
# 2a) min, max, and mean values.
# EU
EU_sales <- sales_by_product$EU_Sales
min_EU <- round(min(EU_sales),2)
max_EU <- round(max(EU_sales),2)
mean_EU <- round(mean(EU_sales),2)

# NA
NA_sales <- sales_by_product$NA_Sales
min_NA <- round(min(NA_sales),2)
max_NA <- round(max(NA_sales),2)
mean_NA <- round(mean(NA_sales),2)

# Global
Global_sales <- sales_by_product$Global_Sales
min_global <- round(min(Global_sales),2)
max_global <- round(max(Global_sales),2)
mean_global <- round(mean(Global_sales),2)

# Print min, max, mean for EU, NA, Global sales respectively.
cat(" EU sales (million £):", "\n", 
    "Min:", min_EU, "\n", 
    "Max:", max_EU, "\n",
    "Mean:", mean_EU,"\n\n", 
    
    "North America sales (million £):", "\n", 
    "Min:", min_NA, "\n", 
    "Max:", max_NA, "\n",
    "Mean:", mean_NA,"\n\n",
    
    "Global sales (million £):", "\n", 
    "Min:", min_global, "\n", 
    "Max:", max_global, "\n",
    "Mean:", mean_global,"\n")

# View the descriptive statistics.
sales_discriptive_saummary <- summary(sales_by_product[,-1:-2])
sales_discriptive_saummary

##############################

# 2b) More in-depth visual analysis using ggplot.

# Relationship of two sales region compared to global sales.
install.packages('plotly')
library(plotly)
## Determine which plot is the best to compare game sales.
# Create scatterplots.
scat_EU_NA_Global <- ggplot(sales_by_product, aes(x = NA_Sales, y = EU_Sales, 
                                                  colour = Global_Sales)) +
  geom_point(size=1.5, show.legend = TRUE) +
  geom_smooth(aes(x= NA_Sales, y = EU_Sales), 
              method = "lm", 
              se = FALSE, 
              colour='black', size=0.5) +
  scale_color_gradient(low = "yellow", 
                       high = "red") +
  labs(title = 'EU Sales vs. NA Compared to Global Sales',
       subtitle = 'Global sale is shown with colour gradient indicator.',
       x = 'North America Sales (million £)',
       y = 'Europe Sales (million £)')

# Convert ggplot2 plot to plotly.
plotly_scat_EU_NA_Global <- ggplotly(scat_EU_NA_Global)

# Display the interactive plot
plotly_scat_EU_NA_Global

# Observation: There is a strong positive correlation between sales of EU, NA and 
# Global sales.

##############################

# Create histograms.
# EU
hist_EU_by_prod <- ggplot(sales_by_product, aes(x = EU_Sales)) +
  geom_histogram(fill='skyblue', colour='black', bins = 20) +
  labs(title = 'Sales Aggregated by Product',
       subtitle = 'Europe', 
       x = 'Sales (million £)',
       y = 'Frequency')

# NA
hist_NA_by_prod <- ggplot(sales_by_product, aes(x = NA_Sales)) +
  geom_histogram(fill='salmon', colour='black', bins = 20) +
  labs(title = '   ',
       subtitle = 'North America', 
       x = 'Sales (million £)', 
       y = ' ') 

# Global
hist_Global_by_prod <- ggplot(sales_by_product, aes(x = Global_Sales)) +
  geom_histogram(fill='green', colour='black', bins = 20) +
  labs(title = '   ',
       subtitle = 'Global', 
       caption = 'Note: Sales for each region is aggregated per product.',
       x = 'Sales (million £)'
       , y =' ') 

# Combine all three histograms in one visual.
grid.arrange(hist_EU_by_prod,
             hist_NA_by_prod,
             hist_Global_by_prod, ncol =3)

# Observation: Similar right skewness distribution for three sale regions.

##############################

# Create boxplots.
boxplot_EU = ggplot(sales_by_product, aes(x = EU_Sales, '' )) +
  geom_boxplot(outlier.colour = 'red', fill='skyblue') +
  labs(title = 'Sales (million £)', x = '', y = 'Europe') +
  xlim(0,30)

boxplot_NA = ggplot(sales_by_product, aes(x = NA_Sales, '')) +
  geom_boxplot(outlier.colour = 'red', fill='salmon') +
  labs(x = '', y = 'North America') +
  xlim(0,30)

boxplot_Global = ggplot(sales_by_product, aes(x = Global_Sales, '')) +
  geom_boxplot(outlier.colour = 'red', fill='green') +
  labs(caption = 'Note: Same sales interval applied to all three.',
       x = '', y = 'Global') +
  xlim(0,30)

# Combine all 3 boxplots in one.
grid.arrange(boxplot_EU, boxplot_NA, boxplot_Global, ncol =1)

# Observation: Variation of sales in Global level is more significant than 
# sub-categories of EU and NA. 
# Sales values in EU smaller and more consistent than NA. 

##############################

## 2c): Determine the normality of the data set.

## Create Q-Q Plots

# EU
qqnorm(sales_by_product$EU_Sales,
       ylab = 'EU Sales (million £)')
qqline(sales_by_product$EU_Sales, col='skyblue', lwd=2)

# NA
qqnorm(sales_by_product$NA_Sales,
       ylab = 'NA Sales (million £)') 
qqline(sales_by_product$NA_Sales, col='salmon', lwd=2)

# Global
qqnorm(sales_by_product$Global_Sales, 
       ylab = 'Global Sales (million £)')
qqline(sales_by_product$Global_Sales, col='green', lwd=2)

###########################  

## Perform Shapiro-Wilk test.
# Install and import Moments.
# instal.packages('moments')
library(moments)

# Perform Shapiro-Wilk test.
# EU
shapiro.test(sales_by_product$EU_Sales) 
# P-value < 0.05 obtained, which rejects the EU sales normality.

# NA
shapiro.test(sales_by_product$NA_Sales) 
# P-value < 0.05 obtained, which rejects the NA sales normality.

# Global
shapiro.test(sales_by_product$Global_Sales)

# P-value < 0.05 obtained, which rejects the Global sales normality.

##############################

## Determine Skewness and Kurtosis
# Skewness and Kurtosis.
#EU
EU_skew <-
  skewness(sales_by_product$EU_Sales)

EU_kurt <-
  kurtosis(sales_by_product$EU_Sales)

# NA
NA_skew <-
  skewness(sales_by_product$NA_Sales)
NA_kurt <-
  kurtosis(sales_by_product$NA_Sales)

# Global
Global_skew <-
  skewness(sales_by_product$Global_Sales)
Global_kurt <-
  kurtosis(sales_by_product$Global_Sales)

# Create a dataframe to show summary of skewness and kurtosis in one place.
skewness_kurtosis_df <- data.frame(Region = c('EU', 'NA', 'Global'), 
                          Skewness = round(c(EU_skew, NA_skew, Global_skew), 2),
                          Kurtosis = round(c(EU_kurt,NA_kurt, Global_kurt), 2))
skewness_kurtosis_df

##############################

### 2d) Determine correlation between sales columns.

## Calculate correlation coefficient.
cor_EU_NA <- 
  cor(sales_by_product$EU_Sales, sales_by_product$NA_Sales)
cor_EU_Global <-
  cor(sales_by_product$EU_Sales, sales_by_product$Global_Sales)
cor_NA_Global<-
  cor(sales_by_product$NA_Sales, sales_by_product$Global_Sales)

# Create a dataframe to show summary of correlation coefficients of each pairs of variables.
corr_coef_df <- 
  data.frame(Relation = c('EU_NA', 'EU_Global', 'NA_Global'), 
             Coefficients = round(c(cor_EU_NA, cor_EU_Global, cor_NA_Global),3))
corr_coef_df

##############################

## Plot the scatter plot for each pair of sales columns with best fit line.

#EU_NA
scat_EU_NA <- ggplot(sales_by_product, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(size = 1.5 , show.legend = FALSE) +
  geom_smooth(aes(x= NA_Sales, y = EU_Sales), 
              method = "lm", se = FALSE, 
              colour = 'black',size=0.5, show.legend = FALSE) +
  labs(title = 'EU Sales vs. NA',
       subtitle = sprintf("Correlation coefficient: %.3f", cor_EU_NA),
       x = 'North America Sales (million £)',
       y = 'Europe Sales (million £)')
       
# Global_NA
scat_Global_NA <- ggplot(sales_by_product, aes(x = NA_Sales, y = Global_Sales))+
  geom_point(colour='red', size = 1.5 , show.legend = FALSE) +
  geom_smooth(aes(x= NA_Sales, y = Global_Sales), 
              method = "lm", se = FALSE, 
              colour = 'red',size=0.5, show.legend = FALSE) +
  labs(title = 'Global Sales vs. NA',
       subtitle = sprintf("Correlation coefficient: %.3f", cor_NA_Global),
       x = 'North America Sales (million £)',
       y = 'Global Sales (million £)') 

# Global_EU
scat_Global_EU <- ggplot(sales_by_product, aes(x = EU_Sales, y = Global_Sales))+
  geom_point(colour='darkgreen', size = 1.5 , show.legend = FALSE) +
  geom_smooth(aes(x= NA_Sales, y = Global_Sales), 
              method = "lm", se = FALSE, 
              colour = 'darkgreen',size=0.5, show.legend = FALSE) +
  labs(title = 'Global Sales vs. EU',
       subtitle = sprintf("Correlation coefficient: %.3f", cor_EU_Global),
       x = 'Europe Sales (million £)',
       y = 'Global Sales (million £)') 

# Combine three scatter plots in one display.
grid.arrange(scat_EU_NA, 
             scat_Global_NA, 
             scat_Global_EU,
             ncol=3)

###############################################################################
##   Answers to Q2: The reliability of the data: normality and distribution? ##

# 1. Based on Q-Q Plots and Shapiro test none of the EU, NA and global_Sales 
#    showed normal distribution.
# 2. Relatively large value of positive skewness measured for all three regions
#    (about 3), which confirms the right skewness observed from histograms. 
# 3.Large Kurtosis value ( about 16) has been seen that aligns with the the 
#   large left side peaks in the histograms. 
# 4. Correlation coefficients as well as the scatter plots showed a positive 
#    linear relationship between each pair of regions. Global sales, however, 
#    showed a stronger correlation with either of NA sales or Europe sales 
#   compared to correlation between the EU and NA sales themselves.

###############################################################################
######    Q3. If there is any possible relationship(s) in sales           #####
######    between North America (NA), Europe (EU), and global sales.      #####
###############################################################################
# 3a) Simple linear regression model.

# Create a linear regression model on the original data.
# Store each column in a variable for simplicity.
EU_Sales <- sales_data2$EU_Sales
NA_Sales <- sales_data2$NA_Sales
Global_Sales <- sales_data2$Global_Sales

#######################
# 1. Global-EU

# Create SLR model.
model_Global_EU <- lm(Global_Sales ~ EU_Sales)
summary(model_Global_EU)

# Create a plot to check SLR normality.
qqnorm(residuals(model_Global_EU))
qqline(residuals(model_Global_EU), col='red')

# Observation:
# EU_Sales is a significant predictor for Global sales due to coefficients 
# p-value< 0.05. Also 77% of data can be explained with Global_EU SLR model. 
# R^2 = 0.771

#######################
# 2. Global-NA 

# Create SLR Model.
model_Global_NA <- lm(Global_Sales ~ NA_Sales)
summary(model_Global_NA)

# Create a plot to check SLR normality.
qqline(residuals(model_Global_NA), col='green')

# Observation:
# NA_Sales is a significant predictor for Global sales due to coefficients 
# p-value< 0.05. Also 87.4% of data was well explained with Global_NA SLR model. 
# R^2 = 0.874.

#######################
# 3b) Create a multiple linear regression model

# Select only numeric columns from the original data frame.
# Create MLR model.
model_mlr <- lm(formula = Global_Sales ~ EU_Sales + NA_Sales, 
                data=sales_data2)
summary(model_mlr)

# Plot MLR model normality.
qqnorm(residuals(model_mlr))
qqline(residuals(model_mlr), col='orange')

# Observation:
# MLR model showed that both EU_Sales and NA_Sales are significant predictors 
# for Global sales as their both coefficients p-value< 0.05. 
# In addition 96.9% of data were well explained with Global_EU_NA MLR model. 
# R^2 = 0.969
# Therefore between the three models multiple linear regression model was selected 
# to predict the Global sales.

#######################
# 3c) Predictions based on given values. 

# Define a data-frame for the given new values. 
head(sales_data2)
new_data <- 
  sales_data2 %>% 
  select(-Product, -Platform) %>% 
  filter(EU_Sales %in% c(23.80, 1.56, 0.65, 0.97, 0.52) &
        NA_Sales %in% c(34.02, 3.93, 2.73, 2.26, 22.08)) %>% 
  arrange(desc(NA_Sales))

# Predict test data.
test_predictors <- select(new_data,-Global_Sales)
pred_Global_sales <- round(predict(model_mlr, 
                                  newdata = test_predictors),2)

# Create a result dataframe for easier comparison.
results_df <- data.frame(obs_Global_sales = new_data$Global_Sales,
                         prd_Global_sales = pred_Global_sales)

# Find the residuals (error).
results_df$error <- 
  round((((results_df$prd_Global_sales - results_df$obs_Global_sales)/
     results_df$obs_Global_sales)*100),2)

# Find accuracy using Mean Absolute Percentage Error (MAPE).
mape <- mean(abs(results_df$error))
mape

# Observation: mpae of 10.3 indicates that MLR model is an excellent model for 
# Global sales predictions .
###############################################################################
######  Answer to Q3: Possible relationship(s) in sales and best model?   #####

# There is a positive strong correlation between Global sales and combined EU 
# sales and NA sales, however, if data from one of the regions available still a
# good correlation exists between global sales and either of EU and NA sales.

###############################################################################


