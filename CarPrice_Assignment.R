#Packages
if(!require("MASS")) {
  install.packages("MASS")
}

if(!require("car")) {
  install.packages("car")
}

if(!require("stringr")) {
  install.packages("stringr")
}

if(!require("dplyr")) {
  install.packages("dplyr")
}

if(!require("ggplot2")) {
  install.packages("ggplot2")
}

#Loading the packages
library(MASS);
library(car);
library(stringr);
library(dplyr);
library(ggplot2)

# Reading the dataset
cardataset <- read.csv("CarPrice_Assignment.csv");

#Understanding the data

nrow(cardataset);
ncol(cardataset);

str(cardataset);


# Data Preparation


#Checking for NA Values.
colSums(is.na(cardataset));


# Removing car_ID column as it will not be helpful 

cardataset <- cardataset[-1];


levels(cardataset$CarName);

# Extracting name of the car company
cardataset$CarName <- str_extract(cardataset$CarName,'\\S*');

# Changing the name to lowercase
cardataset$CarName <- str_to_lower(cardataset$CarName);

table(cardataset$CarName);

#Correcting the mis-spelled words

cardataset$CarName[cardataset$CarName == 'maxda'] <- 'mazda';
cardataset$CarName[cardataset$CarName == 'porcshce'] <- 'porsche';
cardataset$CarName[cardataset$CarName == 'toyouta'] <- 'toyota';
cardataset$CarName[cardataset$CarName == 'vw'] <- 'volkswagen';
cardataset$CarName[cardataset$CarName == 'vokswagen'] <- 'volkswagen';

table(cardataset$CarName);


str(cardataset$fueltype)

# Assigning gas 'fueltype' to 1 and diesel 'fueltype' to 0 and renaming the column to 'fueltype_is_gas'.
levels(cardataset$fueltype) <- c(0,1)

cardataset$fueltype_is_gas <- cardataset$fueltype;
cardataset <- cardataset[-3];
levels(cardataset$fueltype_is_gas)
#Converting to numeric type.
cardataset$fueltype_is_gas <- as.numeric(levels(cardataset$fueltype_is_gas))[cardataset$fueltype_is_gas];


# If a car has 4 doors it is assigned 1 else it is assigned 0.
# doornumber is renamed as doornumber_is_4.

levels(cardataset$doornumber) <- c(1,0);
cardataset$doornumber_is_4 <- cardataset$doornumber;
cardataset <- cardataset[-4];
levels(cardataset$doornumber_is_4);

#Converting to numeric type
cardataset$doornumber_is_4 <- as.numeric(levels(cardataset$doornumber_is_4))[cardataset$doornumber_is_4];


# Assigning front enginelocation as 1 and the rest as 0.
# enginelocation is renamed as enginelocation_is_front
levels(cardataset$enginelocation) <- c(1,0);
cardataset$enginelocation_is_front <- cardataset$enginelocation;
cardataset <- cardataset[-6]
levels(cardataset$enginelocation_is_front);

#Converting to numeric type
cardataset$enginelocation_is_front <- as.numeric(levels(cardataset$enginelocation_is_front))[cardataset$enginelocation_is_front];


# Assigning std aspiration to 1 and turbo to 0.
# Renaming aspiration to aspiration_is_std.
levels(cardataset$aspiration) <- c(1,0);
cardataset$aspiration_is_std <- cardataset$aspiration;
cardataset <- cardataset[-3];
levels(cardataset$aspiration_is_std);

#Converting to numeric type
cardataset$aspiration_is_std <- as.numeric(levels(cardataset$aspiration_is_std))[cardataset$aspiration_is_std];


#Creating dummy variables for carbody
dummy_1 <- data.frame(model.matrix( ~carbody, data = cardataset))
View(dummy_1);
dummy_1 <- dummy_1[-1];

#Merging it into cardatset.
cardataset <- cbind(cardataset[,-3], dummy_1)


#Creating dummy variable for engine type.
dummy_2 <- data.frame(model.matrix( ~enginetype, data = cardataset))
View(dummy_2);
dummy_2 <- dummy_2[-1];

#Merging it into cardatset.
cardataset <- cbind(cardataset[,-9], dummy_2)


#Creating dummy variable for drive wheel.
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = cardataset))
View(dummy_3);
dummy_3 <- dummy_3[-1];

#Merging it into cardatset.
cardataset <- cbind(cardataset[,-3], dummy_3)



#Creating dummy variable for fuel system.
dummy_4 <- data.frame(model.matrix( ~fuelsystem, data = cardataset))
View(dummy_4);
dummy_4 <- dummy_4[-1];

#Merging it into cardatset.
cardataset <- cbind(cardataset[,-10], dummy_4)


#Creating dummy variable for Cylinder number
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = cardataset))
View(dummy_5);
dummy_5 <- dummy_5[-1];

#Merging it into cardatset.
cardataset <- cbind(cardataset[,-8], dummy_5)

#Creating dummy variable for car name
dummy_6 <- data.frame(model.matrix( ~CarName, data = cardataset))
View(dummy_6);
dummy_6 <- dummy_6[-1];

#Merging it into cardatset.
cardataset <- cbind(cardataset[,-2], dummy_6)



# Classifying symboling as High, Medium and Low Risk
cardataset$symboling <- ifelse(cardataset$symboling<=-2,
                               'LowRisk',
                               ifelse(cardataset$symboling<=1,
                                      'MediumRisk',
                                      'HighRisk'
                               )
);
#Creating dummy variables for symboling
dummy_7 <- data.frame(model.matrix( ~symboling, data = cardataset))
View(dummy_7);
dummy_7 <- dummy_7[-1];

#Merging it into cardatset.
cardataset <- cbind(cardataset[,-1], dummy_7)

# Seperating training and testing data.

set.seed(100);

#Training data is 70% and testing data is 30%.
train_indices <- sample(1 : nrow(cardataset), 0.7 * nrow(cardataset));

car_train <- cardataset[train_indices,];
car_test <- cardataset[-train_indices,];




#Modelling

model_1 <- lm(price~., data=car_train);
summary(model_1);

step <- stepAIC(model_1, direction="both");

step;

# Model after StepAIC
model_2 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + enginelocation_is_front + aspiration_is_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                drivewheelrwd + fuelsystem2bbl + fuelsystemmpfi + cylindernumberfive + 
                cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo, data = car_train);
summary(model_2);
vif(model_2);

# Model- 2, Multiple R-squared:  0.9802,	Adjusted R-squared:  0.9732 


# Although curb weight has a high vif-24.39 but has a p-value with 2 stars(0.002511 **) .
#So, will consider it in next iterations. 


# Engine size has a high vif 19.51, but a very low p-value(2.57e-13 ***).
# So considering it in the model as it is significant.


# Removing carbodysedan, has it has a high vif of 14.5 and a high p-value of 0.030465 * .
model_3 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + enginelocation_is_front + aspiration_is_std + 
                carbodyhardtop + carbodyhatchback  + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                drivewheelrwd + fuelsystem2bbl + fuelsystemmpfi + cylindernumberfive + 
                cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo, data = car_train);
summary(model_3);
vif(model_3);

# Model-3, Multiple R-squared:  0.9793,	Adjusted R-squared:  0.9723
# Adjusted R-squared hasn't changes much. So proceeding.

#Curb weight p-value still hasn't changed much.

# carwidth, vif - 11.05, p-value- 0.002143 **. Will consider it later.

#citympg, vif - 8.11, p-value-0.111973. Removing this.

model_4 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm +  enginelocation_is_front + aspiration_is_std + 
                carbodyhardtop + carbodyhatchback  + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                drivewheelrwd + fuelsystem2bbl + fuelsystemmpfi + cylindernumberfive + 
                cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo, data = car_train);
summary(model_4);
vif(model_4);
#Model_4 Multiple R-squared:  0.9788,	Adjusted R-squared:  0.9719 
# Adjusted  R-squared hasn't changes much. So proceeding.


# curbweight has 2 stars. So, will consider it later.
# CarNametoyota has vif of 6.96 and p-value of 8.31e-07 *** . So keeping it.
# CarNamehonda has vif of 6.03 and p-value of 0.001983 ** . Considering it later.


# fuelsystemmpfi has vif of 5.55 and p-value of 0.273047
#drivewheelrwd has vif of 5.599 and p-value of 0.001729 ** 
#So removing fuelsystemmpfi first and considering drivewheelrwd later.

model_5 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm +  enginelocation_is_front + aspiration_is_std + 
                carbodyhardtop + carbodyhatchback  + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                drivewheelrwd + fuelsystem2bbl  + cylindernumberfive + 
                cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo, data = car_train);
summary(model_5);
vif(model_5);


# model_5 Multiple R-squared:  0.9786,	Adjusted R-squared:  0.9718 
#Adjusted  R-squared hasn't changes much. So proceeding.


# Coming back to curbweight which has high vif of 20 and p-value of 0.007149 ** 
# Checking if removal of this affects our model.

model_6 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                peakrpm +  enginelocation_is_front + aspiration_is_std + 
                carbodyhardtop + carbodyhatchback  + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                drivewheelrwd + fuelsystem2bbl  + cylindernumberfive + 
                cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo, data = car_train);
summary(model_6);
vif(model_6);

#model_6 Multiple R-squared:  0.9771,	Adjusted R-squared:  0.9701
# Adjusted R-square hasn't changed much. So safely removing this.


# drivewheelrwd has a vif of 5.07 and p-value has increased to 0.020228 * 
# So, removing this.
model_7 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                peakrpm +  enginelocation_is_front + aspiration_is_std + 
                carbodyhardtop + carbodyhatchback  + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                fuelsystem2bbl  + cylindernumberfive + 
                cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo, data = car_train);
summary(model_7);
vif(model_7);

#model_7 Multiple R-squared:  0.9759,	Adjusted R-squared:  0.9689 
# Adjusted R-square hasn't changed much. So safely removing this.


#fuelsystem2bbl has vif of 3.224373 and p-value of 0.089353 . Removing this.

model_8 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                peakrpm +  enginelocation_is_front + aspiration_is_std + 
                carbodyhardtop + carbodyhatchback  + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo, data = car_train);
summary(model_8);
vif(model_8);

#model_8 Multiple R-squared:  0.9753,	Adjusted R-squared:  0.9684 
# Adjusted R-square hasn't changed much. So safely removing this.

# CarNamevolvo has vif of 3.14 and p-value 0.004053 ** 
# Removing this.
model_9 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                peakrpm +  enginelocation_is_front + aspiration_is_std + 
                carbodyhardtop + carbodyhatchback  + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen , data = car_train);
summary(model_9);
vif(model_9);



# model_9 Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9662 
# Adjusted R-square hasn't changed much. So safely removing this.


# CarNamehonda has vif of 3.83 and p-value of 0.013540 *
# So removing this.
model_10 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhardtop + carbodyhatchback  + carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + 
                 CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen , data = car_train);
summary(model_10);
vif(model_10);

# model_10 Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9646
# Adjusted R-square hasn't changed much. So safely removing this.


# As all vif are now below 2.5 and those above have p-value with 3 stars(***)
# Considering p-value alone for removal.

# CarNamesaab has a very high p-value of 0.9. Removing this.

model_11 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhardtop + carbodyhatchback  + carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + 
                 CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + CarNamevolkswagen , data = car_train);

summary(model_11);
vif(model_11);

# model_11 Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9649 
# Adjusted R-square hasn't changed much. So safely removing this.


# carbodywagon has a high p-value of 0.85, Removing this.

model_12 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhardtop + carbodyhatchback  +  
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + 
                 CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + CarNamevolkswagen , data = car_train);

summary(model_12);
vif(model_12);

# model_12 Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9652 
# Adjusted R-square hasn't changed much. So safely removing this.


#cylindernumberfive has a p-value of 0.83468. Removing this.
model_13 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhardtop + carbodyhatchback  +  
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + 
                 CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + CarNamevolkswagen , data = car_train);

summary(model_13);
vif(model_13);
# model_13 Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9655
# Adjusted R-square hasn't changed much. So safely removing this.

#CarNameisuzu has a p-value of 0.2. So, removing this.
model_14 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhardtop + carbodyhatchback  +  
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + CarNamevolkswagen , data = car_train);

summary(model_14);
vif(model_14);
# model_14 Multiple R-squared:  0.9715,	Adjusted R-squared:  0.9654
# Adjusted R-square hasn't changed much. So safely removing this.


#CarNamemercury has a p-value of 0.1. So, removing this.
model_15 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhardtop + carbodyhatchback  +  
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + CarNamevolkswagen , data = car_train);

summary(model_15);
vif(model_15);

# model_15 Multiple R-squared:  0.9709,	Adjusted R-squared:  0.965 
# Adjusted R-square hasn't changed much. So safely removing this.

#carbodyhardtop has a p-value of 0.11. S0, removing this.
model_16 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhatchback  +  
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + CarNamevolkswagen , data = car_train);

summary(model_16);
vif(model_16);

# model_16 Multiple R-squared:  0.9703,	Adjusted R-squared:  0.9646 
# Adjusted R-square hasn't changed much. So safely removing this.

#CarNamevolkswagen has a p-value of 0.09. So, removing this.
model_17 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhatchback  +  
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota , data = car_train);

summary(model_17);
vif(model_17);

#model_17 Multiple R-squared:  0.9696,	Adjusted R-squared:  0.964 
# Adjusted R-square hasn't changed much. So safely removing this.


#CarNamerenault has a p-value of 0.144. So, removing this.
model_18 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 carbodyhatchback  +  
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNametoyota , data = car_train);

summary(model_18);
vif(model_18);

# model_18 Multiple R-squared:  0.9691,	Adjusted R-squared:  0.9637 
# Adjusted R-square hasn't changed much. So safely removing this.


# carbodyhatchback has a p-value of 0.04. So, removing this.
model_19 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNametoyota , data = car_train);

summary(model_19);
vif(model_19);


# model_19 Multiple R-squared:  0.968,	Adjusted R-squared:  0.9628
# Adjusted R-square hasn't changed much. So safely removing this.




#enginetypedohcv has a p-value of 0.02. So, removing this.
model_20 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNametoyota , data = car_train);

summary(model_20);
vif(model_20);


# model_20 Multiple R-squared:  0.9667,	Adjusted R-squared:  0.9615 
# Adjusted R-square hasn't changed much. So safely removing this.



# CarNamenissan has a p-value of 0.015. So, removing this.

model_21 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNameplymouth + 
                 CarNametoyota , data = car_train);

summary(model_21);
vif(model_21);


# model_21 Multiple R-squared:  0.965,	Adjusted R-squared:   0.96 
# Adjusted R-square hasn't changed much. So safely removing this.


#CarNamedodge has a p-value of 0.036067 *
# So, removing this

model_22 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick +  
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNameplymouth + 
                 CarNametoyota , data = car_train);

summary(model_22);
vif(model_22);

# model_22 Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9588
# Adjusted R-square hasn't changed much. So safely removing this.


# CarNameplymouth has a p-value of 0.05 .
# So, removing this


model_23 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick +  
                 CarNamejaguar + CarNamemazda +  CarNamemitsubishi +
                 CarNametoyota , data = car_train);

summary(model_23);
vif(model_23);

# model_23 Multiple R-squared:  0.9627,	Adjusted R-squared:  0.9579 
# Adjusted R-square hasn't changed much. So safely removing this.

# CarNametoyota has a p-value of 0.05 .
# So, removing this
model_24 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick +  
                 CarNamejaguar + CarNamemazda +  CarNamemitsubishi 
               , data = car_train);

summary(model_24);
vif(model_24);

# model_24 Multiple R-squared:  0.9615,	Adjusted R-squared:  0.957 
# Adjusted R-square hasn't changed much. So safely removing this.

# CarNamemazda has p-value 0.14
# So, removing this.
model_25 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick +  
                 CarNamejaguar + CarNamemitsubishi 
               , data = car_train);

summary(model_25);
vif(model_25);


#model_25 Multiple R-squared:  0.9609,	Adjusted R-squared:  0.9566 
# Adjusted R-square hasn't changed much. So safely removing this.


#enginetypel has a p-value of 0.07 .
#So, removing this.
model_26 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarNamebmw + CarNamebuick +  
                 CarNamejaguar + CarNamemitsubishi 
               , data = car_train);

summary(model_26);
vif(model_26);

# model_26 Multiple R-squared:  0.9599,	Adjusted R-squared:  0.9558 
# Adjusted R-square hasn't changed much. So safely removing this.




# cylindernumber3 has a p-value of 0.0099. So removing this.

model_27 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar + CarNamemitsubishi 
               , data = car_train);

summary(model_27);
vif(model_27);

#model_27 Multiple R-squared:  0.9578,	Adjusted R-squared:  0.9539 
# Adjusted R-square hasn't changed much. So safely removing this.

# CarNamemitsubishi has a p-value of 0.0056. So, removing this.
model_28 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + 
                 enginetypeohcf + enginetyperotor + 
                 CarNamebmw + CarNamebuick +  CarNamejaguar
               , data = car_train);

summary(model_28);
vif(model_28);

# model_28 Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9514
# Adjusted R-square hasn't changed much. So safely removing this.



# enginetypeohfc has p-value of 0.000108 *** and vif of 2.061315
# Trying to keep vif below 2. So removing this

model_29 <- lm(formula = price ~ carwidth  + enginesize + stroke + 
                 peakrpm +  enginelocation_is_front + aspiration_is_std + enginetyperotor + 
                 CarNamebmw + CarNamebuick +  CarNamejaguar
               , data = car_train);

summary(model_29);
vif(model_29);


#model_29 Multiple R-squared:  0.9497,	Adjusted R-squared:  0.9459 
# Adjusted R-square hasn't changed much. So safely removing this.


# Although there are other variables with high vif, corresponding p-values are very less.
# So considering them.


# Prediction
predictedprice<-predict(model_29,
                        car_test[,c('carwidth', 'enginesize','stroke', 'peakrpm','enginelocation_is_front',
                                    'aspiration_is_std','enginetyperotor',
                                    'CarNamebmw','CarNamebuick', 'CarNamejaguar'
                                    
                        )
                        ]
);

car_test$predicted_price <- predictedprice;


cor(car_test$price,car_test$predicted_price);
rsquare <- cor(car_test$price,car_test$predicted_price) ^ 2;

# The final model has a rsquare of 0.840 between predicated and actual price.

rsquare

# The final variables which are best for predicting a price are
# 'carwidth', 'enginesize','stroke', 'peakrpm','enginelocation_is_front',
# 'aspiration_is_std','enginetyperotor',
# 'CarNamebmw','CarNamebuick', 'CarNamejaguar'

car_test$error <- car_test$predicted_price - car_test$price 
plot(car_test$error)
# There is no pattern in error.

ggplot(car_test, aes(price)) + geom_freqpoly()
ggplot(car_test, aes(predicted_price)) + geom_freqpoly()

# Both actual and predicted almost follows the same pattern

plot(model_29)


