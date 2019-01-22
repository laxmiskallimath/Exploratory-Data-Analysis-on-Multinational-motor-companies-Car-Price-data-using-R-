####Logistic Regression - Car price ################################################

#  Exploritary Data analysis ----------------------------------------------
# Set working Directory 
# setwd("C:/Users/Veeresh SK/Laxmi/RCA/Ravi sir stat/Logistic Reg Data/Car Price Prediction")
setwd("E:/Laxmi/RCA/Ravi sir stat/Logistic Reg Data/Car Price Prediction")
# Import Data 
Car_Price <- read.csv("Car Price.csv")

#Check the dimensions(no of rows and no of columns)
dim(Car_Price)

# Check names of dataset hr
names(Car_Price)

# Check top(first) rows of dataset hr 
head(Car_Price)

# Check structure of dataset(data structure of each variable)
str(Car_Price)

# Check summary of dataset 
summary(Car_Price)
fix(Car_Price)
# Step1:-Variable Identification  ------------------------------------------------
# Dependent variable is - Attrition 
str(Car_Price$Car_Price_category)
#If Car_Price category is Lowprice  = 0 
#If Car_Price category is Highrice  = 1 
Car_Price$Car_Price_category <- ifelse(Car_Price$Car_Price_category=="Lowprice",0,1)
Car_Price$Car_Price_category <- as.factor(Car_Price$Car_Price_category)
str(Car_Price$Car_Price_category)

x <- table(Car_Price$Car_Price_category)
x
prop.table(x) #data is skewed

# Data type conversion
# Symboling
str(Car_Price)
Car_Price$symboling <- as.factor(Car_Price$symboling)
str(Car_Price)
# num_of_doors 
Car_Price$num_of_doors <- as.factor(Car_Price$num_of_doors)
str(Car_Price$num_of_doors)
# Step2:- Missing Value Imputation ----------------------------------------

# Blank Spaces are there in this data set treat them as NA 
fix(Car_Price)#Look in to data set you find some blank spaces
Car_Price[Car_Price==""] <- NA
# Remove "?" mark in Num_of_doors
Car_Price$num_of_doors <- gsub("?",NA,Car_Price$num_of_doors,fixed = TRUE)
str(Car_Price$num_of_doors)

# Convert it to as .factor 
Car_Price$num_of_doors <- as.factor(Car_Price$num_of_doors)
str(Car_Price$num_of_doors)
# "?" is there in num_of_doors
sum(is.na(Car_Price$num_of_doors))
summary(Car_Price$num_of_doors)

# Car_Price[Car_Price=="?"]<- NA # Look in to the data set some punctuation like "?" are there 
sum(is.na(Car_Price))
sapply(Car_Price,function(x) sum(is.na(x)))

# Variables Containing Missing values 
# normalized_losses -128(int)
# engine_location -34(Factor/categorical)
# bore -3(int)
# stroke -3(num)
# horsepower -2(int)
# peak_rpm -2(int)
# num_of_doors -2

# How to treat charcter data type missing values Replace it by mode
# engine_location -34(Factor/categorical)
str(Car_Price$engine_location)
table(Car_Price$engine_location)
# Engine location "front" is more so replace NA values with "front"
Car_Price[is.na(Car_Price$engine_location),"engine_location"] <- "front"
sum(is.na(Car_Price$engine_location))

# num_of_doors -2
Car_Price[is.na(Car_Price$num_of_doors),"num_of_doors"] <- "four"
sum(is.na(Car_Price$num_of_doors))

# engine_type
summary(Car_Price$engine_type)
# Car_Price[Car_Price$engine_type=="l","l"] <- "ohc"
# Car_Price$engine_type <- gsub("l","ohc",Car_Price$engine_type,fixed = TRUE)
Car_Price$engine_type[which((Car_Price$engine_type=="l"))] <- "ohc"
summary(Car_Price$engine_type)
summary(Car_Price)

# How to treat numeric data type missing values by calculating mean
# normalized_losses -128(int)
Car_Price[is.na(Car_Price$normalized_losses),"normalized_losses"] <- mean(Car_Price$normalized_losses,na.rm = T)
sum(is.na(Car_Price$normalized_losses))

#  bore -3(int)
Car_Price[is.na(Car_Price$bore),"bore"] <- mean(Car_Price$bore,na.rm = T)
sum(is.na(Car_Price$bore))

# stroke -3(num)
Car_Price[is.na(Car_Price$stroke),"stroke"] <- mean(Car_Price$stroke,na.rm = T)
sum(is.na(Car_Price$stroke))

# horsepower -2(int)
Car_Price[is.na(Car_Price$horsepower),"horsepower"] <- mean(Car_Price$horsepower,na.rm = T)
sum(is.na(Car_Price$horsepower))

# peak_rpm -2(int)
Car_Price[is.na(Car_Price$peak_rpm),"peak_rpm"] <- mean(Car_Price$peak_rpm,na.rm = T)
sum(is.na(Car_Price$peak_rpm))

# Step 3:- Univariate Analysis --------------------------------------------
summary(Car_Price)

# Step 4 :- Bivariate Analysis  -------------------------------------------
# Make Seperate categorical and numerical variables dataframe 
library(dplyr)

Car_Price_Numerical <- select_if(Car_Price,is.numeric)
names(Car_Price_Numerical)

Car_Price_Categorical <- select_if(Car_Price,is.factor)
names(Car_Price_Categorical)

co <- cor(Car_Price$Car_Price_category,Car_Price_Numerical)
write.csv(co,file = "co.csv")

# # To make bivariate anlysis 
# For Numerical Variables perform independent t test 
# For categorical variables perform Chi sequare test 

# Numeric Variables 
# [1] "normalized_losses" "wheel_base"        "length"           
# [4] "width"             "height"            "curb_weight"      
# [7] "engine_size"       "bore"              "stroke"           
# [10] "compression_ratio" "horsepower"        "peak_rpm"         
# [13] "city_mpg"          "highway_mpg"       "bore_new"         
# [16] "stroke_new"        "horsepower_new"    "peak_rpm_new"   

# 1) "normalized_losses" 
t.test(Car_Price$normalized_losses~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (121.4909!=127.37)
# p-value = 0.2889 ,p>0.05
# Accept Ho, "normalized_losses" is not significant
boxplot(Car_Price$normalized_losses~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="normalized_losses",col=c("Blue","Violet"))

# 2) "wheel_base" 
t.test(Car_Price$wheel_base~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (94!=104.14)
# p-value = 1.579e-09,p<0.05
# Reject  Ho, "wheel_base" is significant
boxplot(Car_Price$wheel_base~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="wheel_base",col=c("Blue","Violet"))

# 3) "length"
t.test(Car_Price$length~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (178!=185)
# p-value = 0.09332,p<0.05
# Reject  Ho, "length" is  significant
boxplot(Car_Price$length~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="length",col=c("Blue","Violet"))

# 4) "width"
t.test(Car_Price$width~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (65!=68)
# p-value = 6.002e-10,p<0.05
# Reject  Ho, "width" is  significant
boxplot(Car_Price$width~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="width",col=c("Blue","Violet"))


# 5) "height"  
t.test(Car_Price$height~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (52!=54)
# p-value = 7.197e-05,p<0.05
# Reject  Ho, "height" is  significant
boxplot(Car_Price$height~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="height",col=c("Blue","Violet"))

# 6)"curb_weight" 
t.test(Car_Price$curb_weight~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 ( 2371.30!=3175.59)
# p-value = 2.805e-12,p<0.05
# Reject  Ho, "curb_weight" is  significant
boxplot(Car_Price$curb_weight~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="curb_weight",col=c("Blue","Violet"))


# 7) "engine_size" 
t.test(Car_Price$engine_size~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (116.4539 != 178.5897 )
# p-value = 3.711e-08,p<0.05
# Reject  Ho, "engine_size" is  significant
boxplot(Car_Price$engine_size~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="engine_size" ,col=c("Blue","Violet"))


# 8) "bore"            
t.test(Car_Price$bore~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 ( 3.276673 != 3.523846)
# p-value = 2.052e-07,p<0.05
# Reject  Ho, "bore" is  significant
boxplot(Car_Price$bore~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="bore"  ,col=c("Blue","Violet"))

# 9) "stroke"  
t.test(Car_Price$stroke~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (3.366715!=3.289744)
# p-value = 2.052e-07,p<0.05
# Accept  Ho, "stroke" is  not significant
boxplot(Car_Price$stroke~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="stroke"  ,col=c("Blue","Violet"))


# 10) "compression_ratio"
t.test(Car_Price$compression_ratio~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (8.919433!= 10.656410)
# p-value = 0.03495,p<0.05
#  Reject  Ho, "compression_ratio" is significant
boxplot(Car_Price$compression_ratio~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="compression_ratio" ,col=c("Blue","Violet"))


# 11) "horsepower"  
t.test(Car_Price$horsepower~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (96.36923 !=  152.30769)
# p-value = 2.866e-09,p<0.05
#  Reject  Ho,"horsepower" is significant
boxplot(Car_Price$horsepower~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="horsepower" ,col=c("Blue","Violet"))


# 12) "peak_rpm" 
t.test(Car_Price$peak_rpm~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (5190.530!=5053.846 )
# p-value = 0.1115,p>0.05
#  Accept  Ho,"peak_rpm" is not significant
boxplot(Car_Price$peak_rpm~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="peak_rpm" ,col=c("Blue","Violet"))

# 13) "city_mpg" 
t.test(Car_Price$city_mpg~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (26.55556 != 19.38462)
# p-value = 7.886e-10,p<0.05
#  Reject Ho,"city_mpg" is significant
boxplot(Car_Price$city_mpg~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="city_mpg" ,col=c("Blue","Violet"))

# 14) "highway_mpg" 
t.test(Car_Price$highway_mpg~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (32.31442!= 24.61538)
# p-value = 1.134e-09,p<0.05
#  Reject Ho,"highway_mpg" is significant
boxplot(Car_Price$highway_mpg~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="highway_mpg" ,col=c("Blue","Violet"))


# 15) "bore_new" 
t.test(Car_Price$bore_new~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (3.276619 != 3.523846)
# p-value = 2.042e-07,p<0.05
#  Reject Ho,"bore_new" is significant
boxplot(Car_Price$bore_new~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="bore_new" ,col=c("Blue","Violet"))


# 16)"stroke_new"  
t.test(Car_Price$stroke_new~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (3.366714 !=3.289744)
#  p-value = 0.135,p>0.05
#  Accept  Ho,"stroke_new" is not significant
boxplot(Car_Price$stroke_new~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="stroke_new" ,col=c("Blue","Violet"))


# 17) "horsepower_new" 
t.test(Car_Price$horsepower_new~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 (96.36879!=152.30769)
# p-value = 2.865e-09,p<0.05
# Reject  Ho,"horsepower_new" is significant
boxplot(Car_Price$horsepower_new~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="horsepower_new",col=c("Blue","Violet"))

# 18) "peak_rpm_new"
t.test(Car_Price$peak_rpm_new~Car_Price$Car_Price_category)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of  group 0 is not equal to mean of group 1 ( 5190.525!=5053.846 )
# p-value = 0.1115,p>0.05
# Accept Ho,"peak_rpm_new" is not significant
boxplot(Car_Price$peak_rpm_new~Car_Price$Car_Price_category,main="Boxplot",xlab="Car_Price_category",
        ylab="peak_rpm_new",col=c("Blue","Violet"))

# Bivariatenalysis of categorical variables 
# [1] "symboling"           "make"               
# [3] "fuel_type"           "aspiration"         
# [5] "num_of_doors"        "body_style"         
# [7] "drive_wheels"        "engine_location"    
# [9] "engine_type"         "num_of_cylinders"   
# [11] "fuel_system"         "Car_Price_category" 
# [13] "num_of_doors_new"    "drive_wheel_new"    
# [15] "engine_location_new"

# 1) "symboling"  
x1 <- table(Car_Price$Car_Price_category,Car_Price$symboling)
prop.table(x1,2)
chisq.test(x1)
# Reject H0. "symboling" is significant(p<0.05)

# 2) "make" 
x2 <- table(Car_Price$Car_Price_category,Car_Price$make)
prop.table(x2,2)
chisq.test(x2)
# Reject H0.  "make" is significant(p<0.05)

# 3) "fuel_type" 
x3 <- table(Car_Price$Car_Price_category,Car_Price$fuel_type)
prop.table(x3,2)
chisq.test(x3)
# Reject H0.  "make" is significant(p<0.05

# 4) "aspiration"
x4 <- table(Car_Price$Car_Price_category,Car_Price$aspiration)
prop.table(x4,2)
chisq.test(x4)
# Accept H0. "aspiration" is not significant(p>0.05)

# 5) "num_of_doors" 
x5 <- table(Car_Price$Car_Price_category,Car_Price$num_of_doors)
prop.table(x5,2)
chisq.test(x5)
# Reject H0.  "num_of_doors"  is significant(p<0.05)

# 6) "body_style" 
x6 <- table(Car_Price$Car_Price_category,Car_Price$body_style)
prop.table(x6,2)
chisq.test(x6)
# Reject H0.  "body_style"   is significant(p<0.05)

# 7) "drive_wheels" 
x7 <- table(Car_Price$Car_Price_category,Car_Price$drive_wheels)
prop.table(x7,2)
chisq.test(x7)
# Reject H0.  "drive_wheels"  is significant(p<0.05)

# 8)"engine_location"  
x8 <- table(Car_Price$Car_Price_category,Car_Price$drive_wheels)
prop.table(x8,2)
chisq.test(x8)
# Reject H0.  "engine_location"  is significant(p<0.05)

# 9) "engine_type" 
x9 <- table(Car_Price$Car_Price_category,Car_Price$engine_type)
prop.table(x9,2)
chisq.test(x9)
# Reject H0.  "engine_location"  is significant(p<0.05)

# 10) "num_of_cylinders" 
x10 <- table(Car_Price$Car_Price_category,Car_Price$num_of_cylinders)
prop.table(x10,2)
chisq.test(x10)
# Reject H0.  "num_of_cylinders"   is significant(p<0.05)

# 11) "fuel_system" 
x11 <- table(Car_Price$Car_Price_category,Car_Price$fuel_system)
prop.table(x11,2)
chisq.test(x11)
# Reject H0.  "fuel_system" is significant(p<0.05)

# 12) "num_of_doors_new" 
x12 <- table(Car_Price$Car_Price_category,Car_Price$num_of_doors_new)
prop.table(x12,2)
chisq.test(x12)
# Reject H0. "num_of_doors_new"  is significant(p<0.05)

# 13) "drive_wheel_new" 
x13 <- table(Car_Price$Car_Price_category,Car_Price$drive_wheel_new)
prop.table(x13,2)
chisq.test(x13)
# Reject H0. "drive_wheel_new"   is significant(p<0.05)

     
# 14) "engine_location_new"
x14 <- table(Car_Price$Car_Price_category,Car_Price$engine_location_new)
prop.table(x14,2)
chisq.test(x14)
# Reject H0. "drive_wheel_new"   is significant(p<0.05)

### Stratified Sampling-Split the data in to 70% and 30%
library(caTools)
Trainrows <- sample.split(Car_Price$Car_Price_category,SplitRatio = 0.7)
Train <- Car_Price[Trainrows,]
Test <- Car_Price[!Trainrows,]

#Check response rate/Proportion/frequency of each data set using table() function
table(Car_Price$Car_Price_category)
table(Train$Car_Price_category)
table(Test$Car_Price_category)

# Check proportion of each table 
prop.table(table(Train$Car_Price_category))
prop.table(table(Test$Car_Price_category))

# Check structure of Train data set
str(Train)
str(Train$Car_Price_category)

# Build a model 

# Remove insignificant Variables from bivariate analysis 
# Numeric Variables -Normalized losses,Stroke,Peak_rpm,Stroke_new,peak_rpm_new
# Ctegorical variables - Aspiration 
fit <-glm(Car_Price_category~ symboling+ make+ fuel_type+ num_of_doors+ body_style+ 
             drive_wheels+ engine_location+ wheel_base+ length+ width+ height+ curb_weight+
             engine_type+ num_of_cylinders+ engine_size+ fuel_system+ compression_ratio+ 
             horsepower+ city_mpg+ highway_mpg+ num_of_doors_new+ drive_wheel_new+ 
             engine_location_new+ bore_new+ horsepower_new,data=Train,family=binomial)
summary(fit)
step()
# Remove length,width,height 
# Citympg,higwaympg 
fit1 <-glm(Car_Price_category~ symboling+ make+ fuel_type+ num_of_doors+ body_style+ 
            drive_wheels+ engine_location+ wheel_base+ curb_weight+
            engine_type+ num_of_cylinders+ engine_size+ fuel_system+ compression_ratio+ 
            horsepower+ highway_mpg+ num_of_doors_new+ drive_wheel_new+ 
            engine_location_new+ bore_new+ horsepower_new,data=Train,family=binomial)



# Remove num_of_doors_new,engine_location_new,drive_wheel_new,horsepower_new,engine size,engine type,
# drive wheels,highwaymapg,enginelocation
fit2 <-glm(Car_Price_category~ symboling+ make+ fuel_type+ num_of_doors+ body_style 
             + wheel_base+ curb_weight+ num_of_cylinders+ fuel_system+ compression_ratio+ 
             horsepower,data=Train,family=binomial)

# Remove symboling ,fuel_type,fuel_system,make
fit3 <-glm(Car_Price_category~num_of_doors+ body_style 
           + wheel_base+ curb_weight+ num_of_cylinders+compression_ratio+ 
             horsepower,data=Train,family=binomial)
summary(fit3)

# Use only wheel base ,compression ratio ,horsepower 
fit4 <-glm(Car_Price_category~wheel_base+compression_ratio+ 
             horsepower,data=Train,family=binomial)
summary(fit4)

##Multicollinarity check
library(car)
vif(fit4)#vif values are less than 5 no multicolinearity  

##Validation
Train$Car_Price_category_Prob <- predict(fit4,Train,type = "response")
Train$Car_Price_category_NP <- ifelse(Train$Car_Price_category_Prob>=0.5,"highprice","lowprice")
View(Train)
X1 <- table(Train$Car_Price_category,Train$Car_Price_category_NP)
X1
prop.table(X1,1)

#If Car_Price category is Lowprice  = 0 
#If Car_Price category is Highrice  = 1

# In Train Data set 
# ##Specificity-0.98%
293/(293+3)

#Sensitivity0.55%
15/(15+12)

# Accuracy - 0.95%
(293+15)/(3+293+15+12)

##Test
Test$Car_Price_category_Prob <- predict(fit4,Test,type="response")
Test$Car_Price_category_NP <- ifelse(Test$Car_Price_category_Prob>=0.5,"highprice","Lowprice") 
View(Test)
X2 <- table(Test$Car_Price_category,Test$Car_Price_category_NP)
X2
prop.table(X2,1)

#In test data set 
##Specificity
123/(123+4)#0.96%
##Sensitivity
6/(6+6)#0.5%
##Accuracy =0.92%
(123+6)/(4+123+6+6)

##Concordance 
library(InformationValue)
Concordance(Train$Car_Price_category,Train$Car_Price_category_Prob)
Concordance(Test$Car_Price_category,Test$Car_Price_category_Prob)

##Somers'D
##% of concurdance - % of disconcurdance 
somersD(Train$Car_Price_category,Train$Car_Price_category_Prob)
somersD(Test$Car_Price_category,Test$Car_Price_category_Prob)

##ROC Curve
plotROC(Train$Car_Price_category,Train$Car_Price_category_Prob)
plotROC(Test$Car_Price_category,Test$Car_Price_category_Prob)


##Rank ordering and KS stat
Decile <- cut(Train$Car_Price_category_Prob,
              breaks = quantile(Train$Car_Price_category_Prob,
                                probs = seq(0,1,by=0.1)),
              include.lowest = T)
table(Decile,Train$Car_Price_category)
