library(dplyr)

homes = read.csv('train.csv')

# testing data cleaning procedure on original train dataset
# remove columns that will not be used in analysis
remove.cols = function(DF) {
  DF = DF %>% dplyr::select(., -Id,
                            -Alley,
                            -PoolQC,
                            -PoolArea,
                            -MiscFeature,
                            -GarageYrBlt,
                            -Fence,
                            -'Unnamed: 0')
  return(DF)
}

remove.cols(homes)

 
# some values are missing because a house does not have a garage or basement
# these are imputed with 'None'
impute.none.cols = c('BsmtQual', 'BsmtCond', 'BsmtFinType1',
                     'BsmtFinType2', 'FireplaceQu', 'GarageFinish',
                     'GarageQual', 'GarageCond', 'GarageType',
                     'BsmtExposure')

impute.nones = function(DF) {
  DF[, impute.none.cols][is.na(DF[, impute.none.cols])] = 'None'
  return(DF)
}

homes2 = remove.cols(homes)
homes3 = impute.nones(homes2)

stillmissing = colSums(is.na(homes3))
stillmissing[stillmissing > 0] # Columns still containing missing values

impute.with.mode = function(DF, feature) {
  DF$feature[is.na(DF$feature)] =
    names(sort(table(DF$feature), decreasing = TRUE))[1]
  return(DF)
}

homes4 = impute.electrical(homes3)

stillmissing = colSums(is.na(homes4))
names(stillmissing[stillmissing > 0]) # Columns still containing missing values
                               # Use KNN imputation with k = 9 for remaining NAs
                               # Assumption: Houses of similar design will have
                               # similar values for the remaining columns.

hist(homes4$LotFrontage, breaks = 50) # there are several homes with
                                      # LotFrontage = 0, so NAs are likely true
                                      # missing values and not just 0s.

homes.knn = kNN(homes4,
                c('LotFrontage',
                  'MasVnrType',
                  'MasVnrArea'),
                k=9)
#kNN imputation for missing values

drops = c('LotFrontage_imp', 'MasVnrType_imp', 'MasVnrArea_imp')

homes.imp = homes.knn[, !names(homes.knn) %in% drops]

sum(colSums(is.na(homes.imp))) #no missing values remain

write.csv(homes.imp, 'homes_final.csv')


# a more general function for cleaning data
# missingness in test dataset is different from train

cleandata = function(DF) {
  DF = remove.cols(DF)
  DF = impute.nones(DF)
  DF = impute.with.mode(DF, Electrical)
  DF = impute.with.mode(DF, Utilities)
  DF = impute.with.mode(DF, Functional)
  
  stillmissing = colSums(is.na(DF))
  to.impute = names(stillmissing[stillmissing > 0])
  
  to.drop = paste0(to.impute, '_imp')
  
  DF.knn = kNN(DF, to.impute, k=9)
  DF.imp = DF.knn[, !names(DF.knn) %in% to.drop]
  return(DF.imp)
}

test.cleaned = cleandata(read.csv('test.csv'))
write.csv(test.cleaned, 'test_final.csv')
