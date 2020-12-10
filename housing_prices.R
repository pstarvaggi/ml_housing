library(tidyverse)
library(randomForest)

train = read_csv('train.csv')

train %>% select(SalePrice) %>% ggplot(aes(x = log(SalePrice))) +
  geom_histogram()

train %>% ggplot(aes(x = OverallCond)) + geom_histogram()

train %>% ggplot(aes(x = LotFrontage, y = SalePrice)) + 
  geom_point()

train %>% ggplot(aes(x = LotArea, y = SalePrice)) + 
  geom_point()

train %>% ggplot(aes(x = Street, y = SalePrice)) + 
  geom_point()

train %>% ggplot(aes(x = LandSlope, y = SalePrice)) + 
  geom_point()

train %>% group_by(Neighborhood) %>% 
  summarize(median.sale.price = median(SalePrice)) %>%
  arrange(median.sale.price) %>%
  ggplot(aes(x = Neighborhood, y = median.sale.price)) + 
  geom_point()

train = data.frame(train, stringsAsFactors = TRUE)

#Find the proportion of missingness in each column
colSums(is.na(train))/dim(train)[1]

##### IMPUTE MISSING VALUES #####

library(Hmisc)
library(mice)
library(VIM)

md.pattern(train)


# Impute the missing LotFrontage values with the mean, but consider 
# although, consider doing a random sampling for imputation instead
train$LotFrontage[is.na(train$LotFrontage)] = 
  mean(train$LotFrontage, na.rm = TRUE)

# Remove Alley column all together since 0.9376712329 missingness
train = train[!(names(train) %in% 'Alley')]

# MasVnrType 0.0054794521 missingness; impute at random
train$MasVnrType = impute(train$MasVnrType, "random")

# MasVnrArea 0.0054794521 missingness; impute with mean
train$MasVnrArea[is.na(train$MasVnrArea)] =
  mean(train$MasVnrArea, na.rm = TRUE)

# BsmtQual 0.0253424658 missingness; impute at random
train$BsmtQual = impute(train$BsmtQual, "random")

# BsmtCond 0.0253424658 missingness; impute at random
train$BsmtCond = impute(train$BsmtCond, "random")

# BsmtExpore 0.0260273973 missingness; impute at random
train$BsmtExposure = impute(train$BsmtExposure, "random")

# BsmtFinType1 0.0253424658 missingness; impute at random
train$BsmtFinType1 = impute(train$BsmtFinType1, "random")

# BsmtFinType2 0.0260273973 missingness; impute at random
train$BsmtFinType2 = impute(train$BsmtFinType2, "random")

# Remove FireplaceQu since missingness is 0.4726027397
train = train[!(names(train) %in% 'FireplaceQu')]

# GarageType 0.0554794521 missingness; impute at random
train$GarageType = impute(train$GarageType, "random")

# GarageYrBlt 0.0554794521 missingness; impute at random
## Use YearBuilt instead???
train$GarageYrBlt = impute(train$GarageYrBlt, "random")

# GarageFinish 0.0554794521 missingness; impute at random
train$GarageFinish = impute(train$GarageFinish, "random")

# GarageQual 0.0554794521 missingness; impute at random
train$GarageQual = impute(train$GarageQual, "random")

# GarageCond 0.0554794521 missingness; impute at random
train$GarageCond = impute(train$GarageCond, "random")

# Electrical 0.0006849315 missingness; impute at random
train$Electrical = impute(train$Electrical, "random")

# for missing PoolQC, Change NA to 'No' (applies to 0.9952054795 of entries)
train$PoolQC[is.na(train$PoolQC)] = 'No' 

# for missing Fence, Change NA to 'No' (applies to 0.8075342466 of entries)
train$Fence[is.na(train$Fence)] = 'No' 

# for missing MiscFeature, Change NA to 'No' (applies to 0.9630136986 of entries)
train$MiscFeature[is.na(train$MiscFeature)] = 'No' 

str(train)

##### RANDOM FOREST REGRESSION ######

rf.train  = randomForest(SalePrice ~ ., data = train, mtry = 18)
rf.train

class(importance(rf.train))

rf.features = data.frame(importance(rf.train))

rf.features = rf.features %>% arrange(desc(IncNodePurity))

# Export the data for analysis in Python.
# write.csv(rf.features, 'rf_features.csv')
# write.csv(train, 'imputed_train.csv')

rf.features

rf.rsq = numeric(length(rownames(rf.features)))
for (i in 1:length(rownames(rf.features))){
  rf.train = randomForest(SalePrice ~ ., data = train[c(1:i,79)], mtry = ceil(sqrt(i)))
  rf.rsq[i] = rf.train$rsq[500]
  }



# From Nick D to Everyone: (20:16)
# OverallQual             0.00861
# GrLivArea               0.00561
# GarageCars              0.00491
# TotalBsmtSF             0.00341
# YearBuilt               0.00291
# YearRemodAdd            0.00251
# Fireplaces              0.00201
# GarageArea              0.00181
# FireplaceQu_None        0.00181
# CentralAir_Y            0.00161
# MSZoning_RL             0.00151
# GarageType_Attchd       0.00121
# BsmtFullBath            0.00111
# KitchenQual_TA          0.00101
# LotArea                 0.00091
# WoodDeckSF              0.00091
# OverallCond             0.00081
# BsmtExposure_Gd         0.00081
# BsmtFinType1_GLQ        0.00081
# Neighborhood_Crawfor    0.00071


















