##img=raster
##trainData=vector
##responseCol=field trainData
##nsamples=number 1000
##output=output raster
#install.packages('caret')
#install.packages('sp')
#install.packages('raster')
#install.packages('e1071')
#install.packages('randomForest ')
#install.packages('rgdal')
#install.packages('snow')
#install.packages('survey')

#---------------------------------------
# Free Memory in R/RStudio: Clearing the panels
# 1.Clear plots
if(!is.null(dev.list())) dev.off() # equivalent to clear all plots in the plots panel


# 2.Cean workspace
rm(list=ls()) # equivalent to button 'clear objects from workspace' in the evironment panel

# 3.Clear console :
cat("\014")  # equivalent to Ctrl+L or clear console from the Edit menu

#memory.limit() # to know the storage capacity of r version
#memory.limit(size=56000) # increase r storage capacity to 7GB, doesn't work
#---------------------------------------

# Load the need libraries

library(sp)
library(raster)
library(caret)
library(rgdal)
library(snow)
library(randomForest)


img = brick("C:/Users/Fred/Documents/SubsetSentinel1SAR/SAR_Results/SAR_Convert1/Bakoa_GLCM_Textures/SAR_Subset_8uint/GLCM_Textures_Image_Stacks/Bakoa_S1A_VVVH_GLCM8uint_80bands.tif")

names(img) <- c(paste0("B", 1:79, coll = ""), "B80") # rename the 80 image bands as B1 to B80

trainData = shapefile("C:/Users/Fred/Documents/SubsetSentinel1SAR/SAR_Results/SAR_Convert1/Bakoa_GLCM_Textures/SamplesLuse/Lsamples_all1.shp")
responseCol = "LUClass"


dfAll = data.frame(matrix(vector(), 0, length(names(img)) + 1))

#system.time(
for (i in 1:length(unique(trainData[[responseCol]]))){
  category = unique(trainData[[responseCol]])[i]
  categorymap = trainData[trainData[[responseCol]] == category,]
  dataSet = extract(img, categorymap)
  dataSet = lapply(dataSet, function(x){cbind(x, LUClass = rep(category, nrow(x)))})
  df = do.call("rbind", dataSet)
  dfAll = rbind(dfAll, df)
}
#)


summary(dfAll$LUClass)  # get sample size (no. of pixels) for each class
# LUClass 7 has just 44 pixels while other class range from 106 to 934 pixels
# so it is worthwhile conducting a stratified sampling, other rf classification
# accuracy will be biased and representatiive towards classes with high sampled pixels


# Convert all prediction band values from categorical to numeric
# Extracted values using shapefiles save the values at categories for each polygon

#dput(head(dfAll)) # checking with variable is categorical or numeric
summary(dfAll) # variable check
#sdfAll$B1<- as.numeric(as.character(sdfAll$B1)) # convert B1 from factor to numeric
#i <- sapply(dfAll, is.factor) # select only variables which a factors
#dfAll[i] <- lapply(dfAll[i], as.character) # convert the factor variable to characters

dfAll[] <- lapply(dfAll, function(x) as.numeric(as.character(x))) #convert all variables to numberic 

dfAll$LUClass = as.factor(dfAll$LUClass)

#-----------------------------------------------------


# Stratified Random sampling design

#library(survey)

#mydesign = svydesign(id = ~1, strata = ~LUClass, data= dfAll, fpc = ~Nsample)
#summary(mydesign)

# Recode categorical variable before stratified sampling

dfAll$LClass = NA # Assign NA to all values in a new LClass column
dfAll$LClass[dfAll$LUClass == '1'] = 5 # Recode LUClass 1 == Cocoa Agroforest to LClass 5
dfAll$LClass[dfAll$LUClass == '2'] = 2 # Recode LUClass 2 == Earth Road to LClass to 2  
dfAll$LClass[dfAll$LUClass == '3'] = 5 # Recode LUClass 3 == OilPalm/Cocoa to LClass to 5  
dfAll$LClass[dfAll$LUClass == '4'] = 3 # Recode LUClass 4 == Savannah ImperataLClass to LClass 3
dfAll$LClass[dfAll$LUClass == '5'] = 7 # Recode LUClass 5 == Secondary ForestLClass to LClass 7 
dfAll$LClass[dfAll$LUClass == '6'] = 6 # Recode LUClass 6 == Subsistence Farming to LClass 6 
dfAll$LClass[dfAll$LUClass == '7'] = 4 # Recode LUClass 7 == Water to LClass 4 to LClass 4 
dfAll$LClass[dfAll$LUClass == '8'] = 1 # Recode LUClass 8 == Built up to LClass to 1 

table(dfAll$LClass) # view number of cases in each land use class
table(dfAll$LUClass)

write.csv2(dfAll, 'C:/....../RESULTS/VV_VH_GLCM_8uintALL_dataframe.csv')
#dfAll[complete.cases(dfAll), ] # removing rows wit missing values (NA) before conducting stratified sampling

dfAll = na.omit(dfAll) # remove rows in the dataframe witn a NA value in one of the columns

write.csv2(dfAll, 'C:/....../RESULTS/VV_VH_GLCM_8uintALL_dataframe.csv')

dfAll$LClass = as.factor(dfAll$LClass)
table(dfAll$LClass)
summary(dfAll)

#------------------------------------

# Created a Stratified Random Sample Dataframe from 70% for each land use class

p = 0.7 # probability of 70 % of original sample size

dsample = data.frame() # create empty sampled dataframe

system.time(
  for(i in levels(dfAll$LClass)){
    dsub = subset(dfAll, dfAll$LClass == i) # create a subset each land use class
    B = ceiling(nrow(dsub) * p) # compute 70% of total number rows in each class
    dsub = dsub[sample(1:nrow(dsub), B), ] # subsample 30% of initial size of each land use subset
    dsample = rbind(dsample, dsub) # append class subsample to dataframe
  }
)

table(dsample$LClass)
View(dsample)


#------------------------------------
#nsamples = 1000

#sdfAll <- subset(dfAll[sample(1:nrow(dfAll), nsamples), ])

#ind = sample(2, nrow(D_GLCM5_VH_Mod), replace = TRUE, prob = c(0.7, 0.3))

#beginCluster() # initialise parallel processing to make use of more cpu cores
# use Red, Rededge, and NIR bands as explanatory variables for random forest model
# modFit_rf <- train(as.factor(LUClass) ~ B3 + B4 + B5, method = "rf", data = sdfAll) 

sdfAll = dsample
names(sdfAll)
sdfAll = sdfAll[,-81]
summary(sdfAll)



sdfAll$LClass = as.factor(sdfAll$LClass)
names(sdfAll)
table(sdfAll$LClass)


#summary(sdfAll) # Second check of types and confirm levels of categorical variable(s)

#----------------------------------------------------------------------
# Separate training and test dataset and run RF model

ind = sample(2, nrow(sdfAll), replace = TRUE, prob = c(0.7, 0.3)) # partition date in ratio 70% and 30%
sdfAll_train = sdfAll[ind ==1,]
sdfAll_test = sdfAll[ind ==2,]

set.seed(202)
modFit_rf <- randomForest(LClass ~ ., data = sdfAll_train)


print(modFit_rf)

attributes(modFit_rf) # to see the attributes of the model
modFit_rf$confusion # to print the confusion matrix


# Evaluate Prediction and Confusion matrix based on training data
#install.packages("scales")
library(scales)
library(ggplot2)
library(caret) # use library for making model predictions and runing confusion matrix
pred_M1 = predict(modFit_rf, sdfAll_train) # run model prediction using training data
head(pred_M1)
head(sdfAll_train$LClass)
confusionMatrix(pred_M1, sdfAll_train$LClass)


# Prediction and Confusion Matrix - test data 
# We now obtain the true accuracy of the model, since test data is not seen by model trees
pred2_M1 = predict(modFit_rf, sdfAll_test) # run prediction using the test data (30% of stratified sample dataset)
confusionMatrix(pred2_M1, sdfAll_test$LClass)

# Evaluate Error Rate of Random Forest Model1
plot(modFit_rf)
plot(modFit_rf, main='OOB Error plot; VVVH 8bitGLCM Model')

#-------------------------------------------------------------

# Tunning the Random Forest based on Error Rate (plot of model)
# As the number of trees increase, the OOB error intially drops and later becomes constant 
# Based on OOB error plot, the erro decreases as the number of trees increase
# Based on the error plot, there is no further imporovement in OOB error after about 350 trees 


# We now turn the model using information from OOB error plot

# Tune mtry

t1 = tuneRF(sdfAll_train[,-81], sdfAll_train[,81],
            stepFactor = 0.2,
            plot = TRUE,
            ntreeTry = 550,
            trace = TRUE,
            improve = 0.05)

# Update model by reducing number of trees

modFit_rf2 <- randomForest(LClass ~ ., data = sdfAll_train,
                           ntree = 550,
                           mtry = 8, 
                           importatnce = TRUE,
                           proximity = TRUE)

pred_M2 = predict(modFit_rf2, sdfAll_train) # run model prediction using training data
confusionMatrix(pred_M1, sdfAll_train$LClass)
pred2_M2 = predict(modFit_rf2, sdfAll_test) # run prediction using the test data (30% of stratified sample dataset)
confusionMatrix(pred2_M2, sdfAll_test$LClass)

print(modFit_rf2)
plot(modFit_rf2)



# C) Run classification prediction based on final (best) model
#-------------------------------------
# we use the predict function to make a raster with preditions from the fitted model object (modFit_rf2)

library(snow) # need the "snow package" for model prediction

beginCluster() # initialise parallel processing to make use of more cpu cores
# use all 10 GLCM texture bands as explanatory variables for random forest model
system.time(preds_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf2))) # timing the prediction process
endCluster()

output = preds_rf  # Save predicted classification map as output
plot(preds_rf) # plot classification results


rootdir = 'C:/...../RESULTS/' # Set the root directory to save results


# Save classified map, for use in other GIS, in different image formats

writeRaster(output, 'C:/Users/Fred/Documents/.../RESULTS/BakSAR_VVVH_8bitglcm_RFclassified.tif',
            format="GTiff", overwrite=TRUE) # save classified map
writeRaster(output, 'C:/Users/Fred/Documents/.../RESULTS/BakSAR_VVVH_8bitglcm_RFclassification_Prediction.tif',
            format="GTiff", overwrite=TRUE) # save map of land cover/use predictions



# Plot histogram of the No. of Nodes for the classification Trees

hist(treesize(modFit_rf2),
     main = 'No. Tree Nodes: VVVH 8bitGLCM RFModel',
     col = 'grey')

varImpPlot(modFit_rf2,
           sort = T,
           n.var = 30,
           main = 'Top 30 Important Variables: 8bitGLCM RFModel') # to see which variables play an important role in the model2

var.imp_M2 <- data.frame(importance(modFit_rf2,
                                    type=2)) # type = 2 for MeanDecreaseGini
# make row names as columns
var.imp_M2$Variables <- row.names(var.imp_M2)
write.csv (var.imp_M2[order(var.imp_M2$MeanDecreaseGini,decreasing = T),], 'C:/......../RESULTS/VVVH_8bitglcm_ImportantVar.csv')

#prob_M2 = modFit_rf2$test$votes
#View(prob_M2)

# model to get classification probabilities of each land cover category

modFit_rf3 = randomForest(sdfAll_train, sdfAll_train$LClass, sdfAll_test, sdfAll_test$LClass, keep.forest = TRUE) 
prob_VV_VH = predict(modFit_rf3, sdfAll_train, type='prob')
View(prob_VV_VH)
write.csv(prob_VV_VH, 'C:/Users/Fred/Documents/..../RESULTS/VVVH_8bit_ClassProbability_trainData.csv')


#-----------------------------------------------------------------
# Analysing Important Variables

#setwd('C:/Users/Fred/Documents/SubsetSentinel1SAR/SAR_Results/SAR_Convert1/Bakoa_GLCM_Textures/Bakoa_S1A_GRDH_IntensityGlcm_StackAnalysis/RESULTS')

# To get quantitative values of Variables Importances (for all 170 Variables)
write.csv(importance(modFit_rf2, type=2), file="C:/....../RESULTS/VVVH_8bit_AllVariable_ImportanceValues.csv")
write.csv(importance(modFit_rf2), file="C:/......./RESULTS/VVVH_8bit_AllVariable_ImportanceValues1.csv")

# to find out which predictor variables are actually used in the random forest
write.csv(varUsed(modFit_rf2), file = "C:/........./RESULTS/VVVH_8bit_PredictorVariables_Frequency.csv")


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Get predictions of class probablilities from RF model

beginCluster() # initialise parallel processing to make use of more cpu cores
# use all 10 GLCM texture bands as explanatory variables for random forest model
# system.time(class_prob = clusterR(img, raster::predict, args = list(model = modFit_rf2, type='prob') )) # timing the prediction process
#class_prob = predict(img, raster::predict(), args = list(model = modFit_rf2))
class_prob = predict(modFit_rf2, sdfAll_test, type='prob')
class_prob2 = predict(img, modFit_rf2, type='prob', na.rm=TRUE, overwrite=TRUE)
endCluster()
#beginCluster()
#class_prob2 = predict(img, modFit_rf2, type='prob', na.rm=TRUE, overwrite=TRUE)
#endCluster()

write.csv(class_prob, file="C:/....../RESULTS/VVVH_8bit_Predicted_TestData_class_prob.csv")
writeRaster(class_prob2, 'C:/....../RESULTS/BakSAR_VVVH_8bit_PredictedClassProbabilityMap.tif',
            format="GTiff", overwrite=TRUE)
View(class_prob)

class_prob3 = modFit_rf2$predicted
View(class_prob3)
class_votes1 = modFit_rf2$votes 
View(class_votes1)
write.csv(class_votes1, file="C:/......../RESULTS/VVVH_8bitModel_class_votes.csv")

# Get the class probability (votes) for each pixel using all dataset
class_prob4 = predict(modFit_rf2, sdfAll, type='prob') # return the probability for each class
View(class_prob4)
write.csv(class_prob4, file = 'C:/......../RESULTS/VVVH_8bitPredicted_AllData_Class_prob.csv')

# Get the predicted class under 
class_votes2 = predict(modFit_rf2, sdfAll, type = "vote", norm.votes = TRUE) #selects the class based on which class had the larger value (probability vote)
View(class_votes2)



# Plot Classification Probability Map 

beginCluster() # initialise parallel processing to make use of more cpu cores
# use all 10 GLCM texture bands as explanatory variables for random forest model
system.time(preds_rf_prob <- clusterR(img, raster::predict, args = list(model = modFit_rf2, type='prob'))) # timing the prediction process
endCluster()

output2 = preds_rf_prob  # Save predicted classification map as output
plot(preds_rf_prob) # plot classification results
View(output2) # object type is a layer with NA values

writeRaster(output2, 'C:/......./RESULTS/BakSAR_VVVH_8bit_PredictedProbabilityMap.tif',
            format="GTiff", overwrite=TRUE)
writeRaster(output2, 'C:......../RESULTS/IMAGES4use/BakSAR_VVVH_8bit_PredictedProbabilityMap2.tif',
            format="GTiff", overwrite=TRUE)


#-----------------------------------------------------------------------
# Partial Dependence Plot
# It gives a graphical depiction of the marginal effect of a variable on the class probability

# E.g. PDP For the variable 19 (with highest meanGini) on class 1 - Cocoa Agroforest

partialPlot(modFit_rf2, sdfAll_train, B19, '5',
            main = 'VVVH8bit: Partial Dependence of class 5 on Band19') 

#PDP For the variable 19 on class 6 - Secondary Forest

partialPlot(modFit_rf2, sdfAll_train, B19, '6',
            main = 'VVVH8bit: Partial Dependence class 6 on Band19') 

partialPlot(modFit_rf2, sdfAll_train, B19,
            main = 'VVVH8bit: Partial Dependence on Band19') 

partialPlot(modFit_rf2, sdfAll_train, B19, '7',
            main = 'VVVH8bit: Partial Dependence of class 7 on Band19') 


#PDP For the variable 19 on class 3 - Savannah

partialPlot(modFit_rf2, sdfAll_train, B19, '3',
            main = 'VVVH8bit: Partial Dependence class 3 on Band19') 




