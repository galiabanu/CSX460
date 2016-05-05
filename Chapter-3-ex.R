library(AppliedPredictiveModeling)
data("segmentationOriginal")
segData <- subset(segmentationOriginal, Case == "Train")

cellID <- segData$Cell
class <- segData$Class
case <- segData$Case

> # Now remove the columns
segData <- segData[, -(1:3)]
statusColNum <- grep("Status", names(segData))
segData <- segData[, -statusColNum]

library(e1071)
# For one predictor:
skewness(segData$AngleCh1)

# Since all the predictors are numeric columns, the apply function can > # be used to compute the skewness across columns.
skewValues <- apply(segData, 2, skewness)
head(skewValues)
histogram(segData$AvgIntenCh1)

library(caret)
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

# The original data
head(segData$AreaCh1)
# After transformation
predict(Ch1AreaTrans, head(segData$AreaCh1))

#The base R function prcomp can be used for PCA. In the code below, the data are centered and scaled prior to PCA
pcaObject <- prcomp(segData, center = TRUE, scale. = TRUE)
# Calculate the cumulative percentage of variance which each component > # accounts for.
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]
head(pcaObject$x[, 1:5])
head(pcaObject$rotation[, 1:3])

trans <- preProcess(segData, method = c("BoxCox", "center", "scale", "pca")) 
trans
# Apply the transformations:
transformed <- predict(trans, segData)
# These values are different than the previous PCA components since > # they were transformed prior to PCA
head(transformed[, 1:5])


nearZeroVar(segData)
# When predictors should be removed, a vector of integers is > # returned that indicates which columns should be removed.
correlations <- cor(segData)
dim(correlations)
correlations[1:4, 1:4]

library(corrplot)
corrplot(correlations, order = "hclust")

highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)

