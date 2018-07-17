# Load Pricing Data from the folder location ------------------------------
Pricing_Data <- read.csv("Pricing_Analytics.csv")

# Load necessary libraries in the session  --------------------------------
library(s20x)
library(car)
library(corrgram)

# Lets take a look at first few records & Summary of the data -------------

View(Pricing_Data)

#gives the dimensions of the data set
dim(Pricing_Data)

#gives the names of the variables in the data set
names(Pricing_Data)

#displays top 10 observations of the data set
head(Pricing_Data)

#displays bottom top 10 observations of the data set
tail(Pricing_Data)

#displays structure of the variables in the data set
str(Pricing_Data)

#gives the summary of the variables of the data set 
summary(Pricing_Data)

# Change plotting pannel to accomodate 2 plots  ---------------------------
par(mfrow = c(1,2))

# Find out outlier with the help of box plot 
boxplot(Pricing_Data$sales_Cheese,horizontal = TRUE, xlab="cheese_sales")

# Create histogram to check & validate data distribution 
hist(Pricing_Data$sales_Cheese,main="",xlab="sales",prob=T)
lines(density(Pricing_Data$sales_Cheese),lty="dashed",lwd=2.5,col="red")

#Split data set based on family & natural ad for cheese promotions
Cheese_sales_For_nature_ad = subset(Pricing_Data,ad_type==0)
Cheese_sales_For_family_ad = subset(Pricing_Data,ad_type==1)

#Let's check the mean value of cheese sales for different ad categories 
mean(Cheese_sales_For_nature_ad$sales_Cheese)
mean(Cheese_sales_For_family_ad$sales_Cheese)

# Change plotting pannel to accomodate 2 plots  ---------------------------
par(mfrow = c(1,2))

# Check the shape of curve for the data of both ad categories 
hist(Cheese_sales_For_nature_ad$sales_Cheese,main="",xlab="sales with natural production theme ad",prob=T)
lines(density(Cheese_sales_For_nature_ad$sales_Cheese),lty="dashed",lwd=2.5,col="red")

hist(Cheese_sales_For_family_ad$sales_Cheese,main="",xlab="sales with family health caring theme ad",prob=T)
lines(density(Cheese_sales_For_family_ad$sales_Cheese),lty="dashed",lwd=2.5,col="red")


# check the normality of the data for natural ad sales category
shapiro.test(Cheese_sales_For_nature_ad$sales_Cheese)

# check the normality of the data for family ad sales category
shapiro.test(Cheese_sales_For_family_ad$sales_Cheese)

# Run T-test on both the samples to conclude whether population mean is same or not
t.test(Cheese_sales_For_nature_ad$sales_Cheese,Cheese_sales_For_family_ad$sales_Cheese)

#it says that ad as natural product is 27-92 units less than the add as family product
#so ad as family product is better

pairs(Pricing_Data,col="blue",pch=20)
pairs20x(Pricing_Data)
corrgram(Pricing_Data)
cor(Pricing_Data)

# Sales Driver Analysis & Price Elasticity Analysis  ----------------------
Pricing_Sales_Reg<-lm(sales_Cheese~price_Cheese+ad_type+price_icecream+price_milk,Pricing_Data)
summary(Pricing_Sales_Reg)

#pricing elasticity = delta q/delta p = % change in the quantity/ %change in price 
#pe = (delta q/q)/(delta p/p)=-51.23*(mean of price/mean of sales)

par(mfrow=c(2,2))

# Validate the assumption of linear model
plot(Pricing_Sales_Reg)

# Check Multicollinearity 
vif(Pricing_Sales_Reg)

# Optimization 
f = function(x) -51.24*x^2 + 1028.84 * x - 3863.2
optimize(f,lower=0,upper=20,maximum=TRUE)

# Prediction on the new dataset
New_Data <- data.frame(price_Cheese=10,ad_type=1,price_icecream=7.659,price_milk=9.738)
predict(Pricing_Sales_Reg,New_Data,interval="p")