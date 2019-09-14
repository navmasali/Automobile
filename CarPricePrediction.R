# Install the required packages
requiredPackages = c('MASS','car','reshape2','corrplot','ggplot2')

#Installing the required packages
for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}

# Import the US market- CarPrice data set

car_price <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

# Overview of the car_price dataset
str(car_price)

#Check for NA values in the dataset - No NA's values present
sum(is.na(car_price))

#Check for duplicates in the dataset - No duplicates present
nrow(car_price)
unique_car_price <- unique(car_price)

#Few variables need to converted into levels, hence import the dataset with factors

car_price <- read.csv("CarPrice_Assignment.csv")
str(car_price)

#Split the CarName variable into Brand(Manufacturer) and Make(Model)
car_name <- colsplit(car_price$CarName," ",c("brand","make"))

#Fixing the Manufacture names in the dataset
car_name$brand <- gsub("toyouta","toyota", car_name$brand)
car_name$brand <- gsub("vw","volkswagen", car_name$brand)
car_name$brand <- gsub("vokswagen","volkswagen", car_name$brand)
car_name$brand <- gsub("porcshce","porsche", car_name$brand)
car_name$brand <- gsub("Nissan","nissan", car_name$brand)
car_name$brand <- gsub("maxda","mazda", car_name$brand)

#Combine the columns to the dataset
car_price <- cbind(car_name,car_price[,-3])

#Convert the Brand,Make variable into factors
car_price$brand <- as.factor(car_price$brand)
car_price$make <- as.factor(car_price$make)
str(car_price)

#Convert the make variable to lowercase
car_price$make <- tolower(car_price$make)

#DUMMY VARIABLE CREATION. -- 2 levels
#Convert fueltype,aspiration,doornumber,enginelocation to levels

levels(car_price$fueltype) <- c(1,0)
car_price$fueltype <- as.numeric(levels(car_price$fueltype))[car_price$fueltype]

levels(car_price$aspiration) <- c(1,0)
car_price$aspiration <- as.numeric(levels(car_price$aspiration))[car_price$aspiration]

levels(car_price$doornumber) <- c(1,0)
car_price$doornumber <- as.numeric(levels(car_price$doornumber))[car_price$doornumber]

levels(car_price$enginelocation) <- c(1,0)
car_price$enginelocation <- as.numeric(levels(car_price$enginelocation))[car_price$enginelocation]


###Checking for outliers
# No siginificant outliers for curbweight
quantile(car_price$curbweight,seq(0,1,0.01))

#Capping the value 209 from 96th perentile for enginesize
quantile(car_price$enginesize,seq(0,1,0.01))
car_price$enginesize[which(car_price$enginesize > 209.00)] <- 290.00

# No significant outliers for boreratio
quantile(car_price$boreratio,seq(0,1,0.01))

# Capping the value 10.9400 from 91st percentile for compression ratio
quantile(car_price$compressionratio,seq(0,1,0.01))
car_price$compressionratio[which(car_price$compressionratio > 10.9400)] <- 10.9400

#Capping value 207.00 from 99th percentile for horsepower
quantile(car_price$horsepower,seq(0,1,0.01))
car_price$horsepower[which(car_price$horsepower > 207.00)] <- 207.00

#No significant change for peakrpm variable.
quantile(car_price$peakrpm,seq(0,1,0.01))

# Capping the value of price to 24316.60 from 92nd percentile
quantile(car_price$price,seq(0,1,0.01))
boxplot(car_price$price)
car_price$price[which(car_price$price > 24316.60)] <- 24316.60
boxplot(car_price$price)

#DUMMY VARIABLE CREATION. -- more than 2 levels

##Converting "Brand(Manufacturer)" into dummies 
dummy_1 <- data.frame(model.matrix(~brand, data = car_price))
dummy_1 <- dummy_1[,-1]

#Combining the brand dummies to the car_price dataset
car_price <- cbind(car_price[,-1], dummy_1)

##Doing the same for carbody,drivewheel, enginetype,cylindernumber,fuelsystem
#carbody
dummy_2 <- data.frame(model.matrix(~carbody, data =car_price))
dummy_2 <- dummy_2[,-1]
car_price <- cbind(car_price[,-7],dummy_2)

#drivewheel
dummy_3 <- data.frame(model.matrix(~drivewheel, data = car_price))
dummy_3 <- dummy_3[,-1]
car_price <- cbind(car_price[,-7],dummy_3)

#cylindernumber
dummy_4 <- data.frame(model.matrix(~cylindernumber, data= car_price))
dummy_4 <- dummy_4[,-1]
car_price <- cbind(car_price[,-14],dummy_4)

#enginetype
dummy_5 <- data.frame(model.matrix(~enginetype, data = car_price))
dummy_5 <- dummy_5[,-1]
car_price <- cbind(car_price[,-13],dummy_5)

#fuelsystem
dummy_6 <- data.frame(model.matrix(~fuelsystem, data = car_price))
dummy_6 <- dummy_6[,-1]
car_price <- cbind(car_price[,-14],dummy_6)

#Calculating the derived variable(average mpg) from citympg and highwaympg
car_price$avgmpg <- (car_price$citympg + car_price$highwaympg)/2

#Removing the variables make(mode), Car ID, citympg, highwaympg from dataset
cleaned_car_price <- car_price[,c(-1,-19,-20)]


###########################################################################

# separate training and testing data
set.seed(100)
train_indices <- sample(1:nrow(cleaned_car_price), 0.7*nrow(cleaned_car_price))

training_set <- cleaned_car_price[train_indices,]
testing_set <- cleaned_car_price[-train_indices,]

model_1 <-lm(price~.,data=training_set)
summary(model_1)

#We have a total of 65 variables, Stepwise function allows us to
#eliminate most insignificant variables at once.

step <- stepAIC(model_1, direction ="both")

step

model_2 <- lm(formula = price ~ car_ID + symboling + aspiration + enginelocation + 
                curbweight + enginesize + stroke + horsepower + brandbmw + 
                brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                brandsaab + brandsubaru + brandtoyota + brandvolkswagen + 
                brandvolvo + carbodywagon + drivewheelrwd + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                enginetypeohc + enginetypeohcv + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi, data = training_set)

summary(model_2)

#Check for multicollinearity among the variables
vif(model_2)

# Eliminate the most insignificant variables (enginetypeohcv,carbodywagon) and run the model again

model_3 <- lm(formula = price ~ car_ID + symboling + aspiration + enginelocation + 
                curbweight + enginesize + stroke + horsepower + brandbmw + 
                brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                brandsaab + brandsubaru + brandtoyota + brandvolkswagen + 
                brandvolvo + drivewheelrwd + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                enginetypeohc + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi, data = training_set)
summary(model_3)
vif(model_3)

#Eliminate the "fuelsystemspdi" variable - High VIF and low significance

model_4 <- lm(formula = price ~ car_ID + symboling + aspiration + enginelocation + 
                curbweight + enginesize + stroke + horsepower + brandbmw + 
                brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                brandsaab + brandsubaru + brandtoyota + brandvolkswagen + 
                brandvolvo + drivewheelrwd + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                enginetypeohc + fuelsystem2bbl + fuelsystemmpfi, data = training_set)
summary(model_4)
vif(model_4)

#Eliminate the "horsepower" - Low significance

model_5 <- lm(formula = price ~ car_ID + symboling + aspiration + enginelocation + 
                curbweight + enginesize + stroke + brandbmw + 
                brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                brandsaab + brandsubaru + brandtoyota + brandvolkswagen + 
                brandvolvo + drivewheelrwd + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                enginetypeohc + fuelsystem2bbl + fuelsystemmpfi, data = training_set)
summary(model_5)
vif(model_5)

#Eliminate the "fuelsystem2bbl,fuelsystemmpfi" - Low significance

model_6 <- lm(formula = price ~ car_ID + symboling + aspiration + enginelocation + 
                curbweight + enginesize + stroke + brandbmw + 
                brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                brandsaab + brandsubaru + brandtoyota + brandvolkswagen + 
                brandvolvo + drivewheelrwd + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                enginetypeohc, data = training_set)
summary(model_6)
vif(model_6)

#Eliminate the "symboling" - Low significance

model_7 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
                curbweight + enginesize + stroke + brandbmw + 
                brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                brandsaab + brandsubaru + brandtoyota + brandvolkswagen + 
                brandvolvo + drivewheelrwd + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                enginetypeohc, data = training_set)
summary(model_7)
vif(model_7)

brands <- data.frame(training_set[,19:39])
##Checking for correlation btw brands
corrplot(cor(brands), type = "lower")


##Eliminate the "brandtoyota" - High multicolinearity
model_8 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
                curbweight + enginesize + stroke + brandbmw + 
                brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                brandsaab + brandsubaru + brandvolkswagen + 
                brandvolvo + drivewheelrwd + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                enginetypeohc, data = training_set)
summary(model_8)
vif(model_8)

##Eliminate the "enginelocation" - Low significance
model_9 <- lm(formula = price ~ car_ID + aspiration + 
                curbweight + enginesize + stroke + brandbmw + 
                brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                brandsaab + brandsubaru + brandvolkswagen + 
                brandvolvo + drivewheelrwd + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                enginetypeohc, data = training_set)
summary(model_9)
vif(model_9)

#Eliminate the "brandporsche" - Low significance
model_10 <- lm(formula = price ~ car_ID + aspiration + 
                 curbweight + enginesize + stroke + brandbmw + 
                 brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                 brandmazda + brandmercury + brandmitsubishi + brandnissan + 
                 brandpeugeot + brandplymouth + brandporsche + brandrenault + 
                 brandsaab + brandsubaru + brandvolkswagen + 
                 brandvolvo + drivewheelrwd + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_10)
vif(model_10)

#Eliminate the "brandmercury,brandrenault,drivewheelrwd" - Low significant consistently from previous models
model_11 <- lm(formula = price ~ car_ID + aspiration + 
                 curbweight + enginesize + stroke + brandbmw + 
                 brandbuick + branddodge + brandhonda + brandisuzu + brandjaguar + 
                 brandmazda + brandmitsubishi + brandnissan + 
                 brandpeugeot + brandplymouth + brandporsche + 
                 brandsaab + brandsubaru + brandvolkswagen + 
                 brandvolvo + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_11)
vif(model_11)

#Eliminate the "brandhonda,brandnissan,brandbuick,brandplymouth" - Low significant consistently from previous models
model_12 <- lm(formula = price ~ car_ID + aspiration + 
                 curbweight + enginesize + stroke + brandbmw + 
                 branddodge + brandisuzu + brandjaguar + 
                 brandmazda + brandmitsubishi + 
                 brandpeugeot + brandporsche + 
                 brandsaab + brandsubaru + brandvolkswagen + 
                 brandvolvo + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_12)
vif(model_12)

#Eliminate the "cylindernumberfour" - High VIF
model_13 <- lm(formula = price ~ car_ID + aspiration + 
                 curbweight + enginesize + stroke + brandbmw + 
                 branddodge + brandisuzu + brandjaguar + 
                 brandmazda + brandmitsubishi + 
                 brandpeugeot + brandporsche + 
                 brandsaab + brandsubaru + brandvolkswagen + 
                 brandvolvo + cylindernumberfive + 
                 cylindernumbersix + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_13)
vif(model_13)

#Eliminate "brandjaguar,brandmazda" - Low significant consistently from previous models

model_14 <- lm(formula = price ~ car_ID + aspiration + 
                 curbweight + enginesize + stroke + brandbmw + 
                 branddodge + brandisuzu + brandmitsubishi + 
                 brandpeugeot + brandporsche + 
                 brandsaab + brandsubaru + brandvolkswagen + 
                 brandvolvo + cylindernumberfive + 
                 cylindernumbersix + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_14)
vif(model_14)

#Eliminate "curbweight" - High VIF

model_15 <- lm(formula = price ~ car_ID + aspiration + 
                 enginesize + stroke + brandbmw + 
                 branddodge + brandisuzu + brandmitsubishi + 
                 brandpeugeot + brandporsche + 
                 brandsaab + brandsubaru + brandvolkswagen + 
                 brandvolvo + cylindernumberfive + 
                 cylindernumbersix + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_15)
vif(model_15)

#Eliminate "brandisuzu,brandpeugeot,brandvolkswagen" - low significance

model_16 <- lm(formula = price ~ car_ID + aspiration + 
                 enginesize + stroke + brandbmw + branddodge + brandmitsubishi + 
                 brandporsche + brandsaab + brandsubaru + 
                 brandvolvo + cylindernumberfive + 
                 cylindernumbersix + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_16)
vif(model_16)

#Eliminate "car_ID,cylindernumbersix" - low significance

model_17 <- lm(formula = price ~ aspiration + 
                 enginesize + stroke + brandbmw + branddodge + brandmitsubishi + 
                 brandporsche + brandsaab + brandsubaru + 
                 brandvolvo + cylindernumberfive + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_17)
vif(model_17)

#Eliminate "stroke,branddodge" - low significance consistently from previous models

model_18 <- lm(formula = price ~ aspiration + 
                 enginesize + brandbmw + brandmitsubishi + 
                 brandporsche + brandsaab + brandsubaru + 
                 brandvolvo + cylindernumberfive + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_18)
vif(model_18)

#Eliminate "brandmitsubishi" - low significance decreasing consistently from previous models

model_19 <- lm(formula = price ~ aspiration + 
                 enginesize + brandbmw + brandporsche + brandsaab + brandsubaru + 
                 brandvolvo + cylindernumberfive + cylindernumbertwo + 
                 enginetypeohc, data = training_set)
summary(model_19)
vif(model_19)

#Eliminate "enginetypeohc" - Low significance
model_20 <- lm(formula = price ~ aspiration + 
                 enginesize + brandbmw + brandporsche + brandsaab + brandsubaru + 
                 brandvolvo + cylindernumberfive + cylindernumbertwo , data = training_set)
summary(model_20)
vif(model_20)

#Eliminate "brandsubaru" - Low significance
model_21 <- lm(formula = price ~ aspiration + 
                 enginesize + brandbmw + brandporsche + brandsaab +
                 brandvolvo + cylindernumberfive + cylindernumbertwo , data = training_set)
summary(model_21)
vif(model_21)

#Predicting with model_21
predict_1 <- predict(model_21, testing_set)
testing_set$test_price <- predict_1

#Predicting with model_19
predict_2 <- predict(model_19, testing_set)
testing_set$test_price1 <- predict_2

#Correlation between model_21 predicted prices and actual price
cor(testing_set$price,testing_set$test_price)
rsquared1 <- cor(testing_set$price,testing_set$test_price)^2
rsquared1

#Correlation between model_19 predicted prices and actual price
cor(testing_set$price,testing_set$test_price1)
rsquared2 <- cor(testing_set$price,testing_set$test_price1)^2
rsquared2

#Model19 seems to work better with high correlation between 
#actual test price and predicted test price
final_model <- model_19
summary(final_model)
cor(testing_set$price,testing_set$test_price1)
rsquared2 <- cor(testing_set$price,testing_set$test_price1)^2
rsquared2

#The residuals are quite random and can be ignored as plotted in the 
#below graph
plot(fitted(final_model), residuals(final_model),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(final_model), residuals(final_model)))

ggplot(testing_set, aes(car_ID)) +
  geom_line(aes(y = price, colour = "price")) +
  geom_line(aes(y=test_price1, colour="test_price"))

#Important derivation variables derived out of the model are as follow
# Aspiration
# enginesize
# brandbmw
# brandporsche
# brandsaab
# brandsubaru
# brandvolvo
# cylindernumberfive
# cylindernumbertwo
# enginetypeohc

# Important variables include the variables minus the brand(Manufacturer)
#as they are Company perspective.
#Variables such as aspiration,enginesize, cylinder five or two and enginetypeohc
#can be considered while entering the US market.
#And also need to take some inspiration and business conduct and quality
# of the companies like bmw,porsche,volvo,saab and subaru



