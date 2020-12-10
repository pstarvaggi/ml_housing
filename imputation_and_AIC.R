homes = read.csv(file = 'homes2.csv', header = TRUE)

colSums(is.na(homes)) #259 missing in LotFrontage
                      #8 missing in MasVnrType and MasVnrArea

homes$MasVnrType[homes$MasVnrType == ''] <- NA #replace empty strings in
                                               #MasVnrType with NAs

library(VIM)

kNN(homes, k = 9)

homes.knn = kNN(homes, c('LotFrontage', 'MasVnrType', 'MasVnrArea'), k=9)
  #kNN imputation for missing values

drops = c('LotFrontage_imp', 'MasVnrType_imp', 'MasVnrArea_imp')

homes.imp = homes.knn[, !names(homes.knn) %in% drops]

colSums(is.na(homes.imp)) #no more missingness

homes.imp$SalePrice = log(homes.imp$SalePrice)

library(MASS)

model.empty = lm(SalePrice ~ 1, data = homes.imp) #The model with an intercept ONLY.
model.full = lm(SalePrice ~ ., data = homes.imp) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
summary(forwardAIC)
plot(forwardAIC)

write.csv(homes.imp, 'homes_imputed.csv')
