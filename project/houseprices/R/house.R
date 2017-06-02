#### 0  라이브러리 ####
library("corrplot")
library("dplyr")
library("Amelia")
library("rpart")
library("fBasics")
library("ggplot2")
library("stats")
library("userfriendlyscience")





#### 1. 데이터 로드 ####

train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
test  <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

test$SalePrice <- rep(NA, nrow(test))
house <- rbind(train, test)

str(house)





#### 2. 변수 설명 ####

# MSSubClass  건물 클래스
# MSZoning    구역 분류

# LotFrontage   도로까지의 거리 
# LotArea       사유지 면적(스퀘어 피트)
# Street        도로 유형
# Alley         골목길 유형                           있거나 / 없거나 
# LotShape      사유지 모양
# LandContour   사유지 평평
# Utilities     이용 가능한 유틸리티 종류
# LandSlope     사유지의 기울기
# Neighborhood  Ames 도시 내 물리적 위치(구역)
# Condition1    주요 도로 또는 철도까지 거리
# Condition2
# BldgType      주거 유형
# HouseStyle    주거 스타일

# OverallQual   전체 재질 및 마감 품질
# OverallCond   전체 상태 등급
# YearBuilt     건설 연도
# YearRemodAdd  리모델링 연도 

# RoofStyle   지붕 유형
# RoofMatl    지붕 재료
# Exterior1st 집안의 외장 
# Exterior2nd 주택의 외장
# ExterQual   외관 재질
# ExterCond   외부 물질의 현재 상태

# BsmtQual      지하실의 높이
# BsmtCond      지하실의 상태
# TotalBsmtSF   지하실 면적(총 평방 피트)
# BsmtFullBath  지하실 욕실(욕조 있는) 개수
# BsmtHalfBath  지하실 욕실(욕조 없는) 개수

# Heating       난방 종류
# HeatingQC     난방 품질
# CentralAir    중앙 에어컨
# Electrical    전기 시스템
# Fireplaces    벽난로 수
# FireplaceQu   벽난로 품질                           있거나 / 없거나

# X1stFlrSF     1층 면적(평방 피트)
# X2ndFlrSF     2층 면적(평방 피트)
# GrLivArea     지상의 생활 면적(스퀘어 피트)

# FullBath      욕실(욕조 있는) 개수
# HalfBath      욕실(욕조 없는) 개수
# BedroomAbvGr  침실 수
# KitchenAbvGr  부엌 수
# KitchenQual   부엌 품질
# TotRmsAbvGrd  욕실을 제외한 총 방 개수

# Functional    집의 기능 평가

# GarageType  차고 위치
# GarageYrBlt 차고 지어진 연도                        있거나 / 없거나
# GarageCars  차고 크기(수용 가능한 자동차 대수)
# GarageArea  차고 면적(평방 피트)
# GarageQual  차고 품질
# GarageCond  차고 상태

# PavedDrive      포장 도로
# WoodDeckSF      우드 데크 면적(평방 피트)
# OpenPorchSF     오픈 현관 면적(평방 피트)
# EncplosedPorch  막힌 현관 면적(평방 피트)
# 3SsnPorch       세 자리 현관 면적(평방 피트)
# ScreenPorch     스크린 현관 면적(평방 피트)
# PoolArea        수영장 면적(스퀘어 피트)
# PoolQC          수영장 품질                         있거나 / 없거나
# Fence           울타리 품질                         있거나 / 없거나

# MiscFeature   여기서 다뤄지지 않은 특징들           있거나 / 없거나

# MoSold        판매 월
# YrSold        판매 연도
# SaleType      판매 유형
# SaleCondition 판매 조건
# SalePrice     판매 가격





#### 3. 결측치 처리 ####

# 변수별 결측치 개수 확인
colSums(sapply(house, is.na))

# 결측치 시각화
missmap(house[, 2:80],
        main = "Missing values in houseset",
        y.labels = NULL, y.at = NULL)

# 결측치를 갖는 변수 저장
cols.na <- colSums(sapply(house, is.na))[colSums(sapply(house, is.na)) > 0]
# 범주형 변수
cat.var <- names(house[, names(cols.na)])[which(sapply(house[, names(cols.na)], is.character))]
# 숫자형 변수
num.var <- names(house[, names(cols.na)])[which(sapply(house[, names(cols.na)], is.numeric))]


## (1) MSZoning
table(house$MSZoning, useNA = "ifany")
table(is.na(house$MSZoning))

# Neighborhood, Condition 변수를 이용해 결측치를 처리
# MSZoning 변수가 범주형이므로 의사결정트리를 이용해 값을 예측하여 대체하기로 결정하였다.

# 예측에 사용할 변수 
col.pred <- c("MSZoning", "Neighborhood", "Condition1", "Condition2")
# 모델 생성
model <- rpart(as.factor(MSZoning) ~., 
               data = house[!is.na(house$MSZoning), col.pred],
               method = "class",
               na.action = na.omit)
# 최적화
plotcp(model)
opt.cp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]
model <- prune(model, cp = opt.cp)

# 모델로 예측한 값으로 결측치를 대체
house$MSZoning[is.na(house$MSZoning)] <- as.character(predict(model, house[is.na(house$MSZoning), col.pred], type = "class"))
table(is.na(house$MSZoning))


## (2) Alley
table(house$Alley, useNA = "ifany")  # Grvl, Pave

# 결측치를 갖는 경우 골목길이 없다고 간주하고 "None"으로 결측치를 대체하기로 결정하였다.
house$Alley[is.na(house$Alley)] <- rep("No Alley", 2721)
table(house$Alley, useNA = "ifany")


## (3) Utilities
table(house$Utilities, useNA = "ifany")

# 결측치가 2개 존재
# 변수가 정확히 무엇을 말하는 것인지 설명이 부족하고, 어떤 변수들과 관련이 있는지 알 수 없다.
# 제거해도 큰 영향이 없을 것이라고 판단하여 제거하기로 결정하였다.
house <- house[-which(is.na(house$Utilities)), ]


## (4) Exterior1st, Exterior2nd
table(house$Exterior1st, useNA = "ifany")
table(house$Exterior2nd, useNA = "ifany")

# 결측치가 각각 1개 존재
# 변수가 정확히 무엇을 말하는 것인지 설명이 부족하고, 어떤 변수들과 관련이 있는지 알 수 없다.
# 제거해도 큰 영향이 없을 것이라고 판단하여 제거하기로 결정하였다.
house <- house[-which(is.na(house$Exterior1st) | is.na(house$Exterior2nd)), ]


## (5) MasVnrType, MasVnrArea
summary(as.factor(house$MasVnrType))
summary(as.factor(house$MasVnrArea))

table(house$MasVnrArea[house$MasVnrType == "None"], useNA = "ifany")

# MasVnrType 변수가 "None" 값을 가지지만 MasVnrArea 변수의 값이 0이 아닌 데이터들이 7개 존재
# 7개 데이터 중 MasVnrArea 변수 값이 1인 데이터는 변수의 값을 0으로 대체하기로 결정하였고,
# MasVnrArea 변수가 큰 값을 가지는 데이터는 MasVnrType 변수의 값을 NA로 대체하기로 결정하였다.
house$MasVnrArea <- ifelse(house$MasVnrArea == 1, 0, house$MasVnrArea)
house[which(house$MasVnrArea > 0 & house$MasVnrType == "None" & !is.na(house$MasVnrType)), "MasVnrArea"] <-  rep(NA, 4)

table(house$MasVnrType, useNA = "ifany")
table(house$MasVnrArea, useNA = "ifany")
table(is.na(house$MasVnrType), is.na(house$MasVnrArea))

# MasVnrType, MasVnrArea 변수 모두 결측치를 갖는 데이터가 23개 존재
# MasVnrArea 변수만 결측치를 갖는 데이터가 4개, MasVnrType 변수만 결측치를 갖는 데이터가 1개 존재
# 23개 데이터의 결측치는 MasVnrType 변수에 "None", MasVnrArea 변수에 0을 넣어 대체하기로 결정하였고,
# 나머지 두 개 데이터는 어느 하나의 변수만으로 예측하기가 어려운 관계라고 판단되어 제거하기로 결정하였다.
house <- house[-which(is.na(house$MasVnrArea) & !is.na(house$MasVnrType)), ]
house <- house[-which(!is.na(house$MasVnrArea) & is.na(house$MasVnrType)), ]
house$MasVnrArea[is.na(house$MasVnrArea) & is.na(house$MasVnrType)] <- rep(0, 23)
house$MasVnrType[!is.na(house$MasVnrArea) & is.na(house$MasVnrType)] <- rep("None", 23)


## (6) BsmtQual, BsmtExposure, BsmtFinType1, BsmtFinSF1, BsmtFinType2, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, BsmtFullBath, BsmtHalfBath
table(is.na(house$BsmtExposure))
table(is.na(house$BsmtCond))
# 각각 81개의 결측치를 가지고 있다. 
table(is.na(house$BsmtQual))
# 80개의 결측치를 가지고 있다. 
table(is.na(house$BsmtFinType1))
# 78개의 결측치를 가진다. 
table(is.na(house$BsmtFinType2))
# 79개의 결측치를 가진다. 
table(is.na(house$BsmtFinSF1) & is.na(house$BsmtFinSF2) & is.na(house$BsmtUnfSF))
# FinSF1, FinSF2, BsmtUnfSF가 동시에 결측치인 경우는 한개이다. 
table(is.na(house$BsmtFullBath) & is.na(house$BsmtHalfBath))
# 욕조가 없는 경우는 2개이다. 
table(house$TotalBsmtSF == 0 & is.na(house$BsmtExposure))

# 지하실과 관련된 변수는 다음과 같다. 
col.bsmt <- c("TotalBsmtSF", "BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2", 
              "BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF")

# 먼저, Exposure이 없고 현재 지하실 면적이 없다면 지하실이 없는것과 마찬가지므로 이 값을 0으로 대체한다. 
house$TotalBsmtSF[is.na(house$BsmtExposure) & is.na(house$TotalBsmtSF)] <- 0

# 위에서와 마찬가지로 Exposure이 없고, 현재 지하실 면적이 없다면 
# Cond, Qual, FinType1, FinType2같은 경우 존재할 수 없으므로 이를 None으로 대체한다. 
col.bsmt <- c("BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2")
house[house$TotalBsmtSF == 0 & is.na(house$BsmtExposure), col.bsmt] <- 
  apply(house[house$TotalBsmtSF == 0 & is.na(house$BsmtExposure), col.bsmt], 2, function(x) x <- rep('No Bsmt',78))

# 앞서 수행한 대체 이후 남은 결측치를 살펴본다. 
house[is.na(house$BsmtExposure)|is.na(house$BsmtCond)|is.na(house$BsmtQual)|is.na(house$BsmtFinType2),
      c(col.bsmt,c("TotalBsmtSF","BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF"))]

# 9개정도의 row가 남아 있는데 이경우 이를 의사결정나무를 이용하여 값을 예측하여 결측치를 채우도록 하겠다. 
col.pred <- c("BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2","TotalBsmtSF","YearBuilt")

BsmtFinType2.rpart <- rpart(as.factor(BsmtFinType2) ~ .,
                            data = house[!is.na(house$BsmtFinType2),col.pred], 
                            method = "class", 
                            na.action=na.omit)

house$BsmtFinType2[is.na(house$BsmtFinType2)] <- as.character(predict(BsmtFinType2.rpart,                                               
                                                                      house[is.na(house$BsmtFinType2),col.pred], 
                                                                      type="class"))

BsmtQual.rpart <- rpart(as.factor(BsmtQual) ~ .,
                        data = house[!is.na(house$BsmtQual),col.pred], 
                        method = "class", 
                        na.action=na.omit)

house$BsmtQual[is.na(house$BsmtQual)] <- as.character(predict(BsmtQual.rpart,
                                                              house[is.na(house$BsmtQual),col.pred], 
                                                              type="class"))

BsmtCond.rpart <- rpart(as.factor(BsmtCond) ~ .,
                        data = house[!is.na(house$BsmtCond),col.pred], 
                        method = "class", 
                        na.action=na.omit)

house$BsmtCond[is.na(house$BsmtCond)] <- as.character(predict(BsmtCond.rpart, 
                                                              house[is.na(house$BsmtCond),col.pred], 
                                                              type="class"))

BsmtExposure.rpart <- rpart(as.factor(BsmtExposure) ~ .,
                            data = house[!is.na(house$BsmtExposure),col.pred], 
                            method = "class", 
                            na.action=na.omit)

house$BsmtExposure[is.na(house$BsmtExposure)] <- as.character(predict(BsmtExposure.rpart,                                               
                                                                      house[is.na(house$BsmtExposure),col.pred], 
                                                                      type="class"))

# 마지막으로 남은 결측치가 있는지 찾아본다. 
house[is.na(house$BsmtFinSF1)|is.na(house$BsmtFinSF2)|is.na(house$BsmtUnfSF), c(col.pred, c("BsmtFinSF1", "BsmtFinSF2","BsmtUnfSF", "BsmtFullBath","BsmtHalfBath"))]

# 이 데이터의 경우 TotalBsmt가 0이므로 지하실이 없다고 볼 수 있다. 따라서 이를 0으로 대체한다. 
house$BsmtFinSF1[is.na(house$BsmtFinSF1)|is.na(house$BsmtFinSF2)|is.na(house$BsmtUnfSF)] <- 0
house$BsmtFinSF2[is.na(house$BsmtFinSF1)|is.na(house$BsmtFinSF2)|is.na(house$BsmtUnfSF)] <- 0
house$BsmtUnfSF[is.na(house$BsmtFinSF1)|is.na(house$BsmtFinSF2)|is.na(house$BsmtUnfSF)] <- 0
house$BsmtFullBath[house$TotalBsmtSF == 0 & is.na(house$BsmtFullBath)] <- rep(0,2)
house$BsmtHalfBath[house$TotalBsmtSF == 0 & is.na(house$BsmtHalfBath)] <- rep(0,2)


## (7) Electrical
summary(house$Electrical)
table(house$Electrical, useNA = "ifany")

# 결측치가 1개 존재한다. 
# BldgType, HouseStyle, OverallQual, OverallCond, YearBuilt, YearRemodAdd 변수를 가지고 집의 전기 시스템을 예측하기로 결정하였다.
# Electrical 변수가 범주형이므로 의사결정트리를 이용해 값을 예측하기로 하였다.

# 예측에 사용할 변수
col.pred <- c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "Electrical")
# 변수 생성
model <- rpart(as.factor(Electrical) ~., 
               data = house[!is.na(house$Electrical), col.pred],
               method = "class",
               na.action = na.omit)
# 최적화
plotcp(model)
opt.cp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]
model <- prune(model, cp = opt.cp)

# 모델로 예측한 값으로 결측치를 대체
house$Electrical[is.na(house$Electrical)] <- as.character(predict(model, house[is.na(house$Electrical), col.pred], type = "class"))
table(is.na(house$Electrical))


## (8) KitchenQual
table(house$KitchenQual, useNA = "ifany")

# 결측치가 1개 존재한다.
# BldgType, HouseStyle, OverallQual, OverallCond, YearBuilt, YearRemodAdd 변수를 가지고 집의 전기 시스템을 예측하기로 결정하였다.
# Electrical 변수가 범주형이므로 의사결정트리를 이용해 값을 예측하기로 하였다.

# 예측에 사용할 변수
col.pred <- c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "KitchenQual")
# 변수 생성
model <- rpart(as.factor(KitchenQual) ~., 
               data = house[!is.na(house$KitchenQual), col.pred],
               method = "class",
               na.action = na.omit)
# 최적화
plotcp(model)
opt.cp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]
model <- prune(model, cp = opt.cp)

# 모델로 예측한 값으로 결측치를 대체
house$KitchenQual[is.na(house$KitchenQual)] <- as.character(predict(model, house[is.na(house$KitchenQual), col.pred], type = "class"))
table(is.na(house$KitchenQual))


## (9) Functional
table(is.na(house$Functional))
#결측치가 2개뿐이므로 이를 지워도 무방하다고 생각하여 이를 제거한다. 
house <- house[-which(is.na(house$Functional)), ]


## (10) FireplaceQu
summary(house$FireplaceQu)
table(house$FireplaceQu, useNA = "ifany")
table(house$Fireplaces , useNA = "ifany")
table(house$Fireplaces > 0, house$FireplaceQu, useNA = "ifany")

# FireplaceQu 변수는 Fireplaces 변수에 종속적이다.
# Fireplaces 변수가 결측치를 가지면 FireplaceQu 변수도 결측치를 갖는다.
# Fireplaces 변수가 결측치를 갖는 경우, 집에 벽난로가 없다는 것으로 간주하고 결측치를 "No Fireplace"로 대체하기로 결정하였다.
house$FireplaceQu[is.na(house$FireplaceQu)] <- rep("No Fireplace", 1415)


## (11) GarageType, GarageYrBlt, GarageFinish, GarageCars, GarageArea, GarageQual, GarageCond, 
table(house$GarageYrBlt , useNA = "ifany")
table(house$GarageFinish, useNA = "ifany")
table(house$GarageQual  , useNA = "ifany")
table(house$GarageCond  , useNA = "ifany")
table(is.na(house$GarageYrBlt) & is.na(house$GarageFinish) & is.na(house$GarageQual) & is.na(house$GarageCond))

# GarageYrBlt 변수가 결측치를 가지면 GarageFinish, GarageQual, GarageCond 변수도 결측치를 갖는다.
# GarageYrBlt 변수가 결측치를 갖는 경우를 집에 차고가 없다는 것으로 간주하고 결측치를 "No Garage"로 대체하기로 결정하였다.
house$GarageYrBlt[is.na(house$GarageYrBlt)]   <- rep("No Garage", 158)
house$GarageFinish[is.na(house$GarageFinish)] <- rep("No Garage", 158)
house$GarageQual[is.na(house$GarageQual)]     <- rep("No Garage", 158)
house$GarageCond[is.na(house$GarageCond)]     <- rep("No Garage", 158)

table(house$GarageType  , useNA = "ifany")
table(house$GarageArea  , useNA = "ifany")
table(house$GarageCars  , useNA = "ifany")
table(is.na(house$GarageType) & house$GarageArea == 0 & house$GarageCars == 0)
table(house$GarageType, house$GarageArea == 0, useNA = "ifany")
table(house$GarageType, house$GarageCars, useNA = "ifany")

# 집에 차고가 없음에도 불구하고 GarageType, GarageArea, GarageCars 변수에 값이 입력된 데이터가 2개 존재한다.
# 이 데이터는 GarageType 변수의 값은 "No Garage"로, GarageArea, GarageCars 변수의 값을 0으로 대체하기로 결정하였다.
house[which(house$GarageType == "Detchd" & is.na(house$GarageCars)), c("GarageCars", "GarageArea")]   <- 0
house[which(house$GarageYrBlt == "No Garage" & house$GarageCars != 0), c("GarageCars", "GarageArea")] <- 0
house[which(house$GarageType == "Detchd" & house$GarageYrBlt == "No Garage"), "GarageType"] <- "No Garage" 
house$GarageType[is.na(house$GarageType)] <- rep("No Garage", 156)


## (12) PoolQC
summary(as.factor(house$PoolQC))  # Excellent / Good / Typical or Average / Fair / No Pool
summary(as.factor(house$PoolArea))  
table(house$PoolArea > 0, house$PoolQC, useNA = "ifany")

# PoolQC 변수는 PoolArea 변수에 종속적이다.
# PoolArea 변수의 결측치는 2898개이고, PoolQc 변수의 결측치는 2901개이다.
# PoolArea 변수와 PoolQc 변수 모두 결측치를 가지면 'No Pool'로 대체하고,
# PoolArea 데이터는 있지만 PoolQc 변수가 결측치를 갖는 경우는 값을 예측하기로 결정하였다.

house[house$PoolArea == 0, ]$PoolQC <- rep("No Pool", 2896)

# OverallCond, YearBuilt, YearRemodAdd, PoolQC, PoolArea 변수의 값들을 이용해 PoolQc 변수의 결측치를 처리
# PoolQc 변수의 결측치 처리는 범주형 변수이므로 의사결정트리를 이용하기로 결정하였다.

# 예측에 사용할 변수
col.pred <- c("OverallCond", "YearBuilt", "YearRemodAdd", "PoolQC", "PoolArea")
# 모델 생성
model <- rpart(as.factor(PoolQC) ~.,
               data = house[!is.na(house$PoolQC), col.pred],
               method = "class",
               na.action = na.omit)
# 최적화
plotcp(model)
opt.cp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]
model <- prune(model, cp = opt.cp)

# 모델로 예측한 값으로 결측치를 대체
house$PoolQC[is.na(house$PoolQC)] <- as.character(predict(model, house[is.na(house$PoolQC), col.pred], type = "class"))
table(is.na(house$PoolQC))


## (13) Fence
table(house$Fence, useNA = "ifany")

# 결측치를 갖는 경우를 집에 펜스가 없다는 것으로 간주하고 "No Fence"로 결측치를 대체하기로 결정하였다.
house$Fence[is.na(house$Fence)] <- rep("No Fence", 2338)
table(house$Fence, useNA = "ifany")


## (14) MiscFeature
summary(as.factor(house$MiscFeature))
table(is.na(house$MiscFeature), useNA = "ifany")

# 이 데이터셋에서 특정 변수로 다뤄지지 않는 집의 기타 특징에 대한 변수이다.
# 결측치를 갖는 경우 기타 특징이 없음으로 간주하고 'None'으로 대체하기로 결정하였다.
house$MiscFeature[is.na(house$MiscFeature)] <- rep("None", 2805)
table(is.na(house$MiscFeature), useNA = "ifany")


## (15) SaleType
table(is.na(house$SaleType))
#결측값이 1개뿐이므로 이를 제거해도 무방하다고 생각하여 이를 제거한다. 
house <- house[-which(is.na(house$SaleType)), ]


## (16) LotFrontage
table(house$LotFrontage, useNA = "ifany")

# LotFrontage의 결측치는 484개로 전체 데이터의 약 17%를 차지한다.
# 결측치가 많음에도 불구하고 제거하기로 결정하였다. 
# 제거하기로 결정한 이유는,
# LotFrontage 변수의 값을 예측하는 데 사용할만한 변수가 없는데 
# 이런 상황에서 아무 변수나 이용해서 값을 대체할 경우 오히려 예측된 값들이 변수들 간의 상관관계를 왜곡시킬 것이라고 판단했기 때문이다.
house <- house[-which(!complete.cases(house$LotFrontage)), ]


#결측치가 남아있는지 살펴봄 
sapply(house[,1:80],function(x) sum(is.na(x)))

# 데이터를 다시 학습용과 훈련용으로 분리 
new.train <- house[which(!is.na(house$SalePrice)), ]
new.test  <- house[which(is.na(house$SalePrice)), ]





#### 4. 탐색적 데이터 분석 ####
# 변수가 많아 네명이 나눠서 EDA를 수행하였다.
# 조준희: MSSubClass ~ YearRemodAdd
# 정지원: RoofStyle  ~ TotalBsmtSF
# 김도희: Heating    ~ Fireplaces
# 김연수: GarageType ~ MiscVal


# 연수 EDA ####
cols.eda4 <- names(new.train[, 59:81])
str(new.train[, cols.eda4])

# 범주형 변수
cat.var <- names(new.train[, cols.eda4][which(sapply(new.train[, cols.eda4], is.character))])
# 숫자형 변수
num.var <- names(new.train[, cols.eda4][which(sapply(new.train[, cols.eda4], is.numeric))])
int.var <- names(new.train[, cols.eda4][which(sapply(new.train[, cols.eda4], is.integer))])


# 4.79. SalePrice ####
# 정규성 검정 
# 가장 먼저, 시각화를 통해 정규성을 띄고 있는지 확인한다.
par(mfrow = c(2, 2))
boxplot(x = new.train[, "SalePrice"], 
        main="Distribution of SalePrice") 
hist(x = new.train$SalePrice, 
     main="Hist of SalePrice", xlab="SalePrice") 
plot(x = density(new.train$SalePrice), 
     main="SalePrice") 
text(x = 5e+05, y = 6e-06, 
     paste("skewneww = ", round(skewness(new.train$SalePrice),4)))
text(x = 5e+05, y = 5e-06, 
     paste("kurtosis = ", round(kurtosis(new.train$SalePrice),4)))
abline(v = mean(new.train$SalePrice), col="blue")
abline(v = median(new.train$SalePrice), col="red")
legend(x = 5e+05, y = 2e-06, 
       legend=c("Mean","Median"), col=c("Blue","Red"), lty=1:2, cex=0.7)
qqnorm(y = new.train$SalePrice)
qqline(y = new.train$SalePrice, lwd=2, col="red")
# 분포가 왼쪽으로 치우쳐 F-분포 혹은 멱함수분포를 따르고 있으며,
# Q-Q Plot에서는 데이터들이 이론적 분포에 해당하는 빨간 직선과 멀리 존재하므로 
# 이 분포는 정규분포와는 거리가 멀다고 볼 수 있다.

# 따라서 데이터 변환을 통해 정규분포화 해준다.
par(mfrow = c(2, 2))
boxplot(x = sqrt(log(new.train[, "SalePrice"])), 
        main="Distribution of transformed SalePrice") 
hist(x = sqrt(log(new.train$SalePrice)), 
     main="Hist of transformed SalePrice", 
     xlab="Transformed SalePrice") 
plot(x = density(sqrt(log(new.train$SalePrice))), 
     main="Transformed SalePrice") 
text(x = 3.4, y = 6, 
     paste("skewneww = ", round(skewness(sqrt(log(new.train$SalePrice))), 4)))
text(x = 3.4, y = 5.5, 
     paste("kurtosis = ", round(kurtosis(sqrt(log(new.train$SalePrice))), 4)))
abline(v = mean(sqrt(log(new.train$SalePrice))), col="blue")
abline(v = median(sqrt(log(new.train$SalePrice))), col="red")
legend(x = 3.5, y = 2, 
       legend=c("Mean","Median"), col=c("Blue","Red"), lty=1:2, cex=0.7)
qqnorm(y = sqrt(log(new.train$SalePrice)))
qqline(y = sqrt(log(train$SalePrice)), lwd=2, col="red")
# 로그 변환 후 제곱근 변환을 했더니 분포가 정규분포에 근사하고 있으며,
# Q-Q Plot에서도 데이터들이 이론적 분포에 해당하는 빨간 직선에 근접하여 선형적으로 분포하고 있으므로
# 이 분포는 정규분포에 가깝다고 볼 수 있다.

# 이번에는 통계적인 방법으로 정규성 검정을 실시한다.
shapiro.test(sqrt(log(new.train$SalePrice)))
# p-value < 0.05 이므로 통계적으로는 이 분포가 정규분포라고 할 수 없다.
# 하지만 통계량만으로 판단하기에는 부족하므로
# 위의 모든 결과를 고려했을 때 완전한 정규분포는 아니지만 정규분포에 가깝다고 볼 수 있다.

# 다음으로, 표준화를 해준다.
new.train$SalePrice <- sqrt(log(new.train$SalePrice))
new.train$SalePrice <- scale(new.train$SalePrice)

# 마지막으로 이상치를 제거해준다.
UCL <- quantile(x = new.train$SalePrice, probs = 0.99865)
LCL <- quantile(x = new.train$SalePrice, probs = 0.00135)

outlier <- which((new.train$SalePrice >= UCL) | (new.train$SalePrice <= LCL)) 
new.train <- new.train[-outlier, ]


# 4.57. GarageType ####
table(as.factor(new.train$GarageType))
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = GarageType),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(GarageType ~.)

with(data = new.train, tapply(SalePrice, GarageType, skewness))
with(data = new.train, tapply(SalePrice, GarageType, kurtosis))

# 차고지가 있으면 집 값이 더 비싸다는 것을 알 수 있고,
# 차고지 형태가 Attchd, Detchd인 집이 거래량이 가장 많다는 것을 알 수 있다.
qplot(GarageType, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = GarageType)

qplot(GarageType, SalePrice, data = subset(new.train, (GarageType == "Attchd" | GarageType == "Detchd" | GarageType == "No Garage" | GarageType == "BuiltIn")),
      geom = c("boxplot", "jitter"),
      fill = GarageType)

# 집 값의 분포 범위는 차고지 형태가 Builtin일 때 가장 넓다.
qplot(GarageType, SalePrice, data = subset(new.train, (GarageType == "Attchd" | GarageType == "Detchd" | GarageType == "No Garage" | GarageType == "BuiltIn")),
      geom = c("boxplot"),
      fill = GarageType)

# 차고지 형태별로 집 값을 요약
tapply(new.train$SalePrice, new.train$GarageType, summary)

# ANOVA

# 차고지 형태별 집 값의 평균 차이는 통계적으로 유의함을 알 수 있다.
# 즉, 차고지 형태별 집 값의 평균은 같지 않다고 말할 수 있다.
summary(aov(SalePrice ~ as.factor(GarageType), data = new.train))

# 하지만 등분산성을 만족하지 못하므로 위의 분산분석 결과는 신뢰할 수 없다.
bartlett.test(SalePrice ~ as.factor(GarageType), data = new.train)

# Welch`s ANOVA를 사용하여 차고지 형태별 집 값의 평균 차가 유의한지 확인해본다.
# 검정 결과 차고지 형태별 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(GarageType), data = new.train,
            var.equal = FALSE)

# 사후 분석을 통해 어떤 차고지 형태의 평균 차가 유의한지 확인해본다.
oneway(as.factor(new.train$GarageType), y = new.train$SalePrice, posthoc = "games-howell")


# 4.58. GarageYrBlt ★★ ####
table(new.train$GarageYrBlt)

# 차고지 유무에 따른 집 값 분포를 보기 위해 GarageYrBlt 변수를 따로 분리하여 변수의 범주를 바꿔준다.
temp <- new.train[, c("GarageYrBlt", "SalePrice")]
temp[which(temp$GarageYrBlt != "No Garage"), "GarageYrBlt"] <- "Yes Garage"
temp$GarageYrBlt <- as.factor(temp$GarageYrBlt)

ggplot() +
  geom_histogram(data = temp, aes(x = SalePrice, fill = GarageYrBlt),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(GarageYrBlt ~.)

# 차고지가 있으면 집 값이 더 비싸며, 집 값 분포 범위도 더 넓다는 것을 시각적으로 확인할 수 있다.
qplot(GarageYrBlt, SalePrice, data = temp,
      geom = c("boxplot", "jitter"),
      fill = GarageYrBlt)

# 다음은 차고지가 지어진 연도와 집 값의 변화를 알아본다.
Grg.Y <- subset(new.train, GarageYrBlt != "No Garage")
Grg.Y$GarageYrBlt <- as.numeric(Grg.Y$GarageYrBlt)
Grg.Y$GarageYrBlt <- cut(Grg.Y$GarageYrBlt,
                         breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020),
                         labels = c("1900 ~ 1920", "1920 ~ 1940", "1940 ~ 1960", 
                                    "1960 ~ 1980", "1980 ~ 2000", "2000 ~"))

# 대체로 차고지가 지어진지 오래된 집일수록 집 값의 평균이 낮은 것을 확인할 수 있다.
ggplot() +
  geom_histogram(data = Grg.Y, aes(x = SalePrice, fill = GarageYrBlt),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(GarageYrBlt ~.)

qplot(GarageYrBlt, SalePrice, data = Grg.Y,
      geom = c("boxplot", "jitter"),
      fill = GarageYrBlt)

qplot(GarageYrBlt, SalePrice, data = Grg.Y,
      geom = c("boxplot"),
      fill = GarageYrBlt)

# 등분산성 검정 결과, p-value < 0.05 이므로 등분산성을 만족하지 못한다.
# Welch`s ANOVA를 사용하여 차고지 형태별 집 값의 평균 차가 유의한지 확인해본다.
bartlett.test(SalePrice ~ as.factor(GarageYrBlt), data = Grg.Y)

# 검정 결과 차고지가 지어진 연도별 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(GarageYrBlt), data = Grg.Y,
            var.equal = FALSE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(Grg.Y$GarageYrBlt), y = Grg.Y$SalePrice, posthoc = "games-howell")

# 평균 집 값의 차이가 유의한 세 집단으로 변수를 범주화 해준다.
new.train[new.train$GarageYrBlt != "No Garage", "GarageYrBlt"] <- cut(as.numeric(new.train[new.train$GarageYrBlt != "No Garage", "GarageYrBlt"]),
                                                                      breaks = c(1900, 1980, 2000, 2040),
                                                                      labels = c("1900 ~ 1980", "1980 ~ 2000", "2000 ~ "))
new.train <- new.train[-which(!complete.cases(new.train)), ]
qplot(GarageYrBlt, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = GarageYrBlt)

new.train$GarageYrBlt <- as.factor(new.train$GarageYrBlt)
levels(new.train$GarageYrBlt) <- c("1900 ~ 1980", "1980 ~ 2000", "2000 ~ ", "No Garage")

# 테스트 데이터 셋에 적용
new.test$GarageYrBlt <- as.character(new.test$GarageYrBlt)
new.test[new.test$GarageYrBlt != "No Garage", "GarageYrBlt"] <- cut(as.numeric(new.test[new.test$GarageYrBlt != "No Garage", "GarageYrBlt"]),
                                                                    breaks = c(1900, 1980, 2000, 2040),
                                                                    labels = c("1900 ~ 1980", "1980 ~ 2000", "2000 ~ "))
new.test$GarageYrBlt <- as.factor(new.test$GarageYrBlt)
levels(new.test$GarageYrBlt) <- c("1900 ~ 1980", "1980 ~ 2000", "2000 ~ ", "No Garage")


# 4.59. GarageFinish ####
table(as.factor(new.train$GarageFinish))

ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = GarageFinish),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(GarageFinish ~.)

# 차고지 유뮤가 집 값에 영향을 준다는 것은 시각적으로 확인할 수 있고,
# 차고지가 완성된 집과 완성되지 않은 집의 집 값 차이도 큰 것을 알 수 있다.
qplot(GarageFinish, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = GarageFinish)

# 등분산성 검정 결과, p-value < 0.05 이므로 등분산성을 만족하지 못한다.
# Welch`s ANOVA를 사용하여 차고지 완성 상태에 따른 집 값의 평균 차가 유의한지 확인해본다.
bartlett.test(SalePrice ~ as.factor(GarageFinish), data = new.train)

# 검정 결과 차고지 유무와 완성 상태에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(GarageFinish), data = new.train,
            var.equal = FALSE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(new.train$GarageFinish), y = new.train$SalePrice, posthoc = "games-howell")


# 4.60. GarageCars ####
table(as.factor(new.train$GarageCars))

# 수용할 수 있는 차량의 수가 많은 차고지를 가진 집일수록 집 값이 대체로 비싸다는 것을 알 수 있고,
# 수용할 수 있는 차량의 수가 3대인 차고지를 가진 집의 집 값이 가장 넓은 분포를 띄고 있다.
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = GarageCars),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(GarageCars ~.)

# 차량 1~2대를 수용할 수 있는 차고지를 가진 집이 가장 많이 거래되는 것도 알 수 있다.
qplot(as.factor(GarageCars), SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = GarageCars)

# 등분산성 검정 결과, p-value < 0.05 이므로 등분산성을 만족하지 못한다.
# Welch`s ANOVA를 사용하여 차고지 크기에 따른 집 값의 평균 차가 유의한지 확인해본다.
bartlett.test(SalePrice ~ as.factor(GarageCars), data = new.train)

# 검정 결과 차고지 크기에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(GarageCars), data = new.train,
            var.equal = FALSE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(new.train$GarageCars), y = new.train$SalePrice, posthoc = "games-howell")


# 4.61. GarageArea ★★ ####
table(as.factor(new.train$GarageArea))
summary(new.train$GarageArea)

# 차고지 면적이 넓을 수록 대체로 집 값이 증가하는 것을 시각적으로 확인할 수 있다.
ggplot() +
  geom_point(data = new.train, aes(x = GarageArea, y = SalePrice, color = GarageCars),
             shape = 1, size = 1.5)

# 차고지 면적을 300 단위로 나눠서 집 값과의 관계를 확인해본다.
temp <- new.train[, c("GarageArea", "SalePrice")]
temp$GarageArea <- cut(temp$GarageArea,
                       breaks = c(0, 300, 600, 900, 1200, 1500),
                       labels = c("0 ~ 300", "300 ~ 600", "600 ~ 900", "900 ~ 1200", "1200 ~"))

ggplot() +
  geom_histogram(data = temp, aes(x = SalePrice, fill = GarageArea),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(GarageArea ~.)

qplot(GarageArea, SalePrice, data = temp,
      geom = c("boxplot", "jitter"),
      fill = GarageArea)

# 거래량이 많은 0 ~ 300, 300 ~ 600, 900 ~ 1200 구간의 값만 다시 보면
# 차고지 면적이 넓을수록 평균 집 값이 커지고
# 집 값의 분포 범위도 넓어지는 것을 알 수 있다.
qplot(GarageArea, SalePrice, data = subset(temp, (GarageArea == "0 ~ 300" | GarageArea == "300 ~ 600" | GarageArea == "600 ~ 900")),
      geom = c("boxplot", "jitter"),
      fill = GarageArea)

# 등분산성 검정 결과, p-value < 0.05 이므로 등분산성을 만족하지 못한다.
# Welch`s ANOVA를 사용하여 차고지 면적에 따른 집 값의 평균 차가 유의한지 확인해본다.
bartlett.test(SalePrice ~ as.factor(GarageArea), data = temp)

# 검정 결과 차고지 면적에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(GarageArea), data = temp,
            var.equal = FALSE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(temp$GarageArea), y = temp$SalePrice, posthoc = "games-howell")

# 두 변수를 범주화하여 관계를 시각화
temp$SalePrice <- cut(temp$SalePrice,
                      breaks = c(0, 150000, 300000, 450000, 600000, 755000),
                      labels = c("150000 미만", "150000 ~ 300000", "300000 ~ 450000", "450000 ~ 600000", "600000 이상"))

# 히트맵을 통해 확인해본 결과, 
# 차고지 면적이 0 ~ 300인 집들은 대부분 150000 미만의 가격에서 거래되고
# 차고지 면적이 300 ~ 600인 집들은 대부분 300000 미만의 가격에서 거래되고
# 차고지 면적이 600 ~ 900인 집들은 대부분 300000 이상의 가격에서 거래됨을 알 수 있다.
temp.mat <- table(temp$GarageArea, temp$SalePrice)
heatmap(temp.mat, Rowv = NA, Colv = NA,
        col = heat.colors(100), scale = "column",
        xlab = "SalePrice", ylab = "GarageArea",
        margins = c(5, 10))

# 평균 집 값 차이가 유의한 그룹으로 묶이도록 변수를 범주화 해준다.
new.train$GarageArea <- cut(new.train$GarageArea,
                            breaks = c(0, 300, 600, 2000),
                            labels = c("0 ~ 300", "300 ~ 600", "600 ~"))

new.train[which(!complete.cases(new.train$GarageArea)), "GarageArea"] <- "0 ~ 300"
qplot(GarageArea, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = GarageArea)

# 테스트 데이터 셋에 적용
table(new.test$GarageArea, useNA = "ifany")
new.test$GarageArea <- cut(new.test$GarageArea,
                           breaks = c(0, 300, 600, 2000),
                           labels = c("0 ~ 300", "300 ~ 600", "600 ~"))
new.test[which(!complete.cases(new.test$GarageArea)), "GarageArea"] <- "0 ~ 300"


# 4.62. GarageQual  ####
table(new.train$GarageQual)

# 차고지 품질이 평균 이상은 되어야 차고지가 없는 집과 평균적인 집 값에 차이가 있다는 정도를 시각적으로 확인할 수 있다. 
qplot(GarageQual, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = GarageQual)

# 등분산성 검정 결과, p-value > 0.05 이므로 등분산성을 만족한다.
bartlett.test(SalePrice ~ as.factor(GarageQual), data = new.train)

# 검정 결과 차고지 품질에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(GarageQual), data = new.train,
            var.equal = TRUE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(new.train$GarageQual), y = new.train$SalePrice, posthoc = "tukey")


# 4.63. GarageCond  ####
table(new.train$GarageCond)

# 뚜렷한 특징을 확인할 수가 없다.
# 차고지 상태가 평균 이상은 되어야 차고지가 없는 집과 평균적인 집 값에 차이가 있다는 정도를 시각적으로 확인할 수 있다. 
qplot(GarageCond, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = GarageCond)

# 등분산성 검정 결과, p-value > 0.05 이므로 등분산성을 만족한다.
bartlett.test(SalePrice ~ as.factor(GarageCond), data = new.train)

# 검정 결과 차고지 상태에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(GarageCond), data = new.train,
            var.equal = TRUE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(new.train$GarageCond), y = new.train$SalePrice, posthoc = "tukey")


# 4.64. PaveDrive ★★ ####
table(new.train$PavedDrive)

# 시각적으로 봤을 때 포장된 도로가 있을 때와 없을 때 집 값에 차이가 있는 것처럼 보여진다.
qplot(PavedDrive, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = PavedDrive)

# 등분산성 검정 결과, p-value > 0.05 이므로 등분산성을 만족한다.
bartlett.test(SalePrice ~ as.factor(PavedDrive), data = new.train)

# 검정 결과 포장 도로 유무에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(PavedDrive), data = new.train,
            var.equal = TRUE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(new.train$PavedDrive), y = new.train$SalePrice, posthoc = "tukey")

# 평균 집 값 차이가 유의한 그룹으로 묶이도록 변수를 범주화 해준다.
new.train[which((new.train$PavedDrive == "N") | (new.train$PavedDrive == "P")), "PavedDrive"] <- "Partial or Not"

# 테스트 데이터 셋에 적용
table(new.test$PavedDrive, useNA = "ifany")
new.test[which((new.test$PavedDrive == "N") | (new.test$PavedDrive == "P")), "PavedDrive"] <- "Partial or Not"


# 4.65. WoodDeckSF ####
table(new.train$WoodDeckSF)
summary(new.train$WoodDeckSF)

# 뚜렷한 특징을 찾기가 힘들다.
ggplot() +
  geom_point(data = new.train, aes(x = WoodDeckSF, y = SalePrice, color = WoodDeckSF),
             shape = 1, size = 1.5)

# 우드덱 면적을 0 ~ 400까지 100 단위로 나눠서 집 값과의 관계를 확인해본다.
temp <- new.train[, c("WoodDeckSF", "SalePrice")]
temp$WoodDeckSF <- cut(temp$WoodDeckSF,
                       breaks = c(0, 100, 200, 300, 400),
                       labels = c("0 ~ 100", "100 ~ 200", "200 ~ 300", "300 ~ 400"))

ggplot() +
  geom_histogram(data = temp, aes(x = SalePrice, fill = WoodDeckSF),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(WoodDeckSF ~.)

# 우드덱이 없는 집보다는 우드덱이 있는 집이 대체로 가격이 높은 것처럼 보이기는 하지만
# 우드덱 면적에 따라 집 값이 크게 변하는 것처럼 보여지지는 않고,
# 면적에 따른 집 값의 차이도 유의미한지는 통계적으로 분석해봐야할 것같다.
qplot(WoodDeckSF, SalePrice, data = temp,
      geom = c("boxplot", "jitter"),
      fill = WoodDeckSF)

# 등분산성 검정 결과, p-value < 0.05 이므로 등분산성을 만족하지 못한다.
bartlett.test(SalePrice ~ as.factor(PavedDrive), data = new.train)

# Welch`s ANOVA를 사용하여 포장 도로 유무에 따른 집 값의 평균 차가 유의한지 확인해본다.
# 검정 결과 포장 도로 유무에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(PavedDrive), data = new.train,
            var.equal = FALSE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(new.train$PavedDrive), y = new.train$SalePrice, posthoc = "games-howell")


# 4.66. OpenPorchSF ####
table(new.train$OpenPorchSF)
summary(new.train$OpenPorchSF)

# 뚜렷한 특징을 찾기가 힘들다.
ggplot() +
  geom_point(data = new.train, aes(x = OpenPorchSF, y = SalePrice, color = OpenPorchSF),
             shape = 1, size = 1.5)

# 현관 면적을 0 ~ 200까지 50 단위로 나눠서 집 값과의 관계를 확인해본다.
temp <- new.train[, c("OpenPorchSF", "SalePrice")]
temp$OpenPorchSF <- cut(temp$OpenPorchSF,
                        breaks = c(0, 50, 100, 150, 200),
                        labels = c("0 ~ 50", "50 ~ 100", "100 ~ 150", "150 ~ 200"))

# 현관이 없는 집과 있는 집의 평균적인 집 값의 차이는 있어보이지만
# 똑같이 현관이 있는 집이라면 현관의 면적이 집 값에 영향을 주는 것처럼 보이지는 않는다.
ggplot() +
  geom_histogram(data = temp, aes(x = SalePrice, fill = OpenPorchSF),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(OpenPorchSF ~.)

qplot(OpenPorchSF, SalePrice, data = temp,
      geom = c("boxplot", "jitter"),
      fill = OpenPorchSF)

which(!complete.cases(new.train$OpenPorchSF))
table(new.train$OpenPorchSF)

# 등분산성 검정 결과, p-value < 0.05 이므로 등분산성을 만족하지 못한다.
bartlett.test(SalePrice ~ as.factor(OpenPorchSF), data = temp)

# Welch`s ANOVA를 사용하여 현관 면적에 따른 집 값의 평균 차가 유의한지 확인해본다.
# 검정 결과 현관 면적에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(OpenPorchSF), data = temp,
            var.equal = FALSE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(temp$OpenPorchSF), y = temp$SalePrice, posthoc = "games-howell")


# 4.67. EnclosedPorch ####
summary(new.train$EnclosedPorch)

# 뚜렷한 특징을 찾기가 힘들다.
ggplot() +
  geom_point(data = new.train, aes(x = EnclosedPorch, y = SalePrice, color = EnclosedPorch),
             shape = 1, size = 1.5)

# 현관 면적을 0 ~ 300까지 50 단위로 나눠서 집 값과의 관계를 확인해본다.
temp <- new.train[, c("EnclosedPorch", "SalePrice")]
temp$EnclosedPorch <- cut(temp$EnclosedPorch,
                          breaks = c(0, 50, 100, 150, 200, 250, 300),
                          labels = c("0 ~ 50", "50 ~ 100", "100 ~ 150", "150 ~ 200", "200 ~ 250", "250 ~ 300"))

# 마찬가지로 특징을 찾기가 어렵다.
ggplot() +
  geom_histogram(data = temp, aes(x = SalePrice, fill = EnclosedPorch),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(EnclosedPorch ~.)

# EnclosedPorch 변수는 집 값과 상관이 없는 것처럼 보인다.
qplot(EnclosedPorch, SalePrice, data = temp,
      geom = c("boxplot", "jitter"),
      fill = EnclosedPorch)

# 등분산성 검정 결과, p-value > 0.05 이므로 등분산성을 만족한다.
bartlett.test(SalePrice ~ as.factor(EnclosedPorch), data = temp)

# Welch`s ANOVA를 사용하여 현관 면적에 따른 집 값의 평균 차가 유의한지 확인해본다.
# 검정 결과 현관 면적에 상관 없이 집 값의 평균은 동일함을 통계적으로 확인할 수 있다.
oneway.test(SalePrice ~ as.factor(EnclosedPorch), data = temp,
            var.equal = TRUE)


# 4.68. X3SsnPorch ####
summary(new.train$X3SsnPorch)

# 뚜렷한 특징을 찾기가 힘들다.
ggplot() +
  geom_point(data = new.train, aes(x = X3SsnPorch, y = SalePrice, color = X3SsnPorch),
             shape = 1, size = 1.5)


# 4.69. ScreenPorch ####
summary(new.train$ScreenPorch)

# 뚜렷한 특징을 찾기가 힘들다.
ggplot() +
  geom_point(data = new.train, aes(x = ScreenPorch, y = SalePrice, color = ScreenPorch),
             shape = 1, size = 1.5)


# 4.70. PoolQC ####
table(new.train$PoolQC)

# 뚜련한 특징을 찾을 수 없다.
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = PoolQC),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(PoolQC ~.)

# 수영장은 집 값에 영향을 주지 않는 것같다.
qplot(PoolQC, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = PoolQC)

# 등분산성 검정 결과, p-value > 0.05 이므로 등분산성을 만족한다.
bartlett.test(SalePrice ~ as.factor(PoolQC), data = new.train)

# 검정 결과 수영장 상태에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(PoolQC), data = new.train,
            var.equal = TRUE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
# 수영장 상태가 매우 좋은 집과 수영장이 없는 집의 평균 집 값의 차이만 유의하다.
oneway(as.factor(new.train$PoolQC), y = sqrt(log(new.train$SalePrice)), posthoc = "tukey")



# 4.71. PoolArea ####
summary(new.train$PoolArea)

# 뚜렷한 특징을 찾기가 힘들다.
# 큰 수영장이 있다고 집 값이 비싸지거나 하지는 않는 것같다.
ggplot() +
  geom_point(data = new.train, aes(x = PoolArea, y = SalePrice, color = PoolArea),
             shape = 1, size = 1.5)


# 4.72. Fence ★★ ####
table(new.train$Fence)

# 펜스의 유무가 집 값에 영향을 주는 것처럼 보이지는 않는다.
qplot(Fence, SalePrice, data = new.train,
      geom = c("boxplot", "jitter"),
      fill = Fence)

# 등분산성 검정 결과, p-value < 0.05 이므로 등분산성을 만족하지 못한다.
bartlett.test(SalePrice ~ as.factor(Fence), data = new.train)

# Welch`s ANOVA를 사용하여 펜스 유무에 따른 집 값의 평균 차가 유의한지 확인해본다.
# 검정 결과 펜스 유무에 따른 집 값의 평균이 동일하지 않음이 통계적으로 유의하다는 것을 알 수 있다.
oneway.test(SalePrice ~ as.factor(Fence), data = new.train,
            var.equal = FALSE)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(new.train$Fence), y = new.train$SalePrice, posthoc = "games-howell")

# 펜스의 유무로 데이터를 범주화 해준다.
new.train[which(new.train$Fence != "No Fence"), "Fence"] <- "Yes Fence"

# 테스트 데이터 셋에 적용
table(new.test$Fence, useNA = "ifany")
new.test[which(new.test$Fence != "No Fence"), "Fence"] <- "Yes Fence"


# 4.73. MiscFeature ####
table(new.train$MiscFeature)

# 다른 기타 기능들도 집 값에는 영향을 주지 않는 것처럼 보인다.
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = MiscFeature),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(MiscFeature ~.)


# 4.74. MiscVal ####
summary(new.train$MiscVal)

# 전체적으로 보면 기타 기능의 유무와 집 값은 특별한 관계가 없지만
# 기타 기능을 가진 집이라면
# 기타 기능의 가치가 높을수록 집 값이 높아지는 경향이 있는 것처럼 보인다.
ggplot() +
  geom_point(data = new.train, aes(x = MiscVal, y = SalePrice, color = MiscVal),
             shape = 1, size = 1.5)


# 4.75. MoSold ####
# 4.76. YrSold ####
# 4.77. SaleType ####       
# 4.78. SaleCondition ####


eda1.train <- new.train
eda1.test  <- new.test


# 준희 EDA ####
str(new.train)
eda_1 <-colnames(new.train)[2:21]
eda_1 <-c(eda_1)
eda_df_1 <- new.train[,eda_1]
eda_df_1$SalePrice <- new.train$SalePrice


# 밀도 함수 그래프를 그려주는 함수
density_plotting <- function(category, numeri){
  ggplot(data = eda_df_1, aes(x = numeri)) + geom_density(aes(group = category, colour = category, fill = category), alpha = 0.3)
}

# 박스 플랏을 그려주는 함수
box_jitter <- function(category, numeri){
  qplot(category, numeri, geom = c('boxplot','jitter'), fill = category)
}
par(mfrow = c(1,1))

# 4.1. MSSubClass ####
table(eda_df_1$MSSubClass)
#수치형이지만 빌딩의 클래스를 나타내기 때문에 이를 범주형으로 판단했다. 
eda_df_1$MSSubClass <- as.factor(eda_df_1$MSSubClass)
plot(eda_df_1$MSSubClass, eda_df_1$SalePrice)
#간단한 박스플럿을 통해 확인해 본 결과 특히 20,60,120에서 차이가 나타난다는 것을 알 수 있다. 
#해당 클래스는 다른 클래스에 비해 데이터의 양이 크다는 것을 앞서 테이블을 통해 확인 가능하다. 
bartlett.test(eda_df_1$SalePrice ~ eda_df_1$MSSubClass)
#등분산이 아니다 
oneway.test(SalePrice ~ MSSubClass,data = eda_df_1, var.equal = FALSE)
#유의한 변수임을 알 수 있음
#각 클래스별 밀도함수를 그려서 파악해본다. 
density_plotting(eda_df_1$MSSubClass, eda_df_1$SalePrice)

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(eda_df_1$MSSubClass, y = new.train$SalePrice, posthoc = "games-howell")

# 집 값의 평균 차이가 유의하지 않은 범주들을 하나로 묶어준다.
eda_df_1$MSSubClass <- as.character(eda_df_1$MSSubClass)
#바꾸기 용의하도록 charater형으로 변환해준다. 
eda_df_1[which((eda_df_1$MSSubClass == 40) | (eda_df_1$MSSubClass == 45)), "MSSubClass"] <- 40
eda_df_1[which((eda_df_1$MSSubClass == 70) | (eda_df_1$MSSubClass == 75)), "MSSubClass"] <- 70
eda_df_1[which((eda_df_1$MSSubClass == 80) | (eda_df_1$MSSubClass == 85)), "MSSubClass"] <- 80
#평균차이가 거의 없는 범주는 묶어준다. 
plot(as.factor(eda_df_1$MSSubClass), eda_df_1$SalePrice)
bartlett.test(SalePrice ~ MSSubClass, data = eda_df_1)
#등분산이 아니다 
oneway.test(SalePrice ~ MSSubClass,data = eda_df_1, var.equal = FALSE)
#유의한 변수임을 알 수 있음

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(eda_df_1$MSSubClass), y = eda_df_1$SalePrice, posthoc = "games-howell")

eda_df_1[which((eda_df_1$MSSubClass == 70)  | (eda_df_1$MSSubClass == 80)  | (eda_df_1$MSSubClass == 90)), "MSSubClass"] <- "70 ~ 90"
eda_df_1[which((eda_df_1$MSSubClass == 160) | (eda_df_1$MSSubClass == 180) | (eda_df_1$MSSubClass == 190)), "MSSubClass"] <- "160 ~ 190"

eda_df_1[which((eda_df_1$MSSubClass == 30) | (eda_df_1$MSSubClass == 40)), "MSSubClass"] <- "30 ~ 40"

# 사후 분석을 통해 어느 카테고리의 평균 집 값 차이가 유의한지 확인해본다.
oneway(as.factor(eda_df_1$MSSubClass), y = eda_df_1$SalePrice, posthoc = "games-howell")
eda_df_1$MSSubClass <- as.factor(eda_df_1$MSSubClass)
#결과적으로 7개의 범주로 축약했다. 


# 4.2. MSZoning ★★ ####
table(eda_df_1$MSZoning)

plot(as.factor(eda_df_1$MSZoning), eda_df_1$SalePrice)
#박스플럿을 그렷을 때 가격의 평균은 크게 차이가 안나지만 RL에 이상치가 많은 것을 
#알 수 있다. 이를 파악해보기 위한 density plot을 그려본다. 
density_plotting(eda_df_1$MSZoning, eda_df_1$SalePrice)
#집단간 평균의 차이가 보이긴 하지만 유의한지 알수 없다. 
#따라서 anova로 이를 검정해보도록 한다. 
bartlett.test(eda_df_1$SalePrice~eda_df_1$MSSubClass)
#등분산이 아니다. 
oneway.test(SalePrice~MSZoning, data = eda_df_1, var.equal = F)
#유의한 변수이다. 
#사분 분석을 통해 어느 카테고리의 평균값 차이가 유의한지 알아본다. 
oneway(as.factor(eda_df_1$MSZoning), y = eda_df_1$SalePrice, posthoc = "games-howell")
#RH-RM-c의 경우  집단간 차이가 유의하지 않다. 따라서 집단을 합치겟다 .
eda_df_1[which((eda_df_1$MSZoning == 'RH') | (eda_df_1$MSZoning == 'RM') | (eda_df_1$MSZoning == 'C (all)')),'MSZoning'] <- 'C or RM or RH'
bartlett.test(eda_df_1$SalePrice~eda_df_1$MSSubClass)
#등분산이 아니다. 
oneway.test(SalePrice~MSZoning, data = eda_df_1, var.equal = F)
#유의한 변수이다. 
#사분 분석을 통해 어느 카테고리의 평균값 차이가 유의한지 알아본다. 
oneway(as.factor(eda_df_1$MSZoning), y = eda_df_1$SalePrice, posthoc = "games-howell")
#3개의 범주로 축약했다.

# 4.3. Street ####
table(eda_df_1$Street)
plot(as.factor(eda_df_1$Street), eda_df_1$SalePrice)
#평균은 큰차이를 보이지 않으나 Pave에 이상치가 많이 나타나는 것을 볼 수 있다. 
density_plotting(eda_df_1$Street, eda_df_1$SalePrice)
bartlett.test(eda_df_1$SalePrice~eda_df_1$Street)
#등분산을 만족한다. 
summary(aov(eda_df_1$SalePrice~eda_df_1$Street, data = eda_df_1))
#상대적으로 유의하지 않으며, Grvl의 개수가 너무 적기 때문에 이를 무시도록 한다. 


# 4.4. Alley ★★ ####
table(eda_df_1$Alley)
plot(as.factor(eda_df_1$Alley), eda_df_1$SalePrice)
#평균은 큰차이를 보이지 않으나 No Alley에 이상치가 많이 나타나는 것을 볼 수 있다. 
density_plotting(eda_df_1$Alley, eda_df_1$SalePrice)
#집단간 평균차이가 보인다. 유의한 차인지 검정한다. 
bartlett.test(eda_df_1[,'SalePrice'] ~ eda_df_1[,'Alley'])
#등분산을 만족하지 않는다. 
oneway.test(SalePrice~Alley, data = eda_df_1, var.equal = F)
#유의한 변수임을 알 수 있다. 
#어떤집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$Alley), y = eda_df_1$SalePrice, posthoc = "games-howell")
#Pave~No Alley간 차이가 유의하지 않음을 알 수 있다. 
eda_df_1[which((eda_df_1$Alley == 'Pave') | (eda_df_1$Alley == 'No Alley')), 'Alley'] <- 'Pave and No Alley'
#두범주를 하나로 합친다. 
bartlett.test(eda_df_1$SalePrice ~ eda_df_1$Alley)
#등분산은 여전히 만족하않는다. 
oneway.test(SalePrice~Alley, data = eda_df_1, var.equal = F)
#합쳐도 집단간 차이는 유의미하다. 결과적으로 범주를 두개로 함축하였다. 


# 4.5. LotShape ★★ ####
table(as.factor(eda_df_1$LotShape))
plot(as.factor(eda_df_1$LotShape), eda_df_1$SalePrice)
#큰차이는 없지만 경향을 볼 수 있다.
density_plotting(eda_df_1$LotShape, eda_df_1$SalePrice)
#IR2에서 이상치가 많이 나타남을 알 수 있다. 
bartlett.test(eda_df_1$SalePrice ~ eda_df_1$LotShape)
#등분산을 만족한다. 
summary(aov(eda_df_1$SalePrice ~ eda_df_1$LotShape, data = eda_df_1))
#유의한 변수이다. 
#어떤 집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$LotShape), y = eda_df_1$SalePrice, posthoc = "tukey")
#IR1, IR2, IR3집단간 차이는 통계적으로 유의하지 않다. 따라서 이 세집단 IR로 묶어준다. 
eda_df_1[which((eda_df_1$LotShape == 'IR1') | (eda_df_1$LotShape == 'IR2') | (eda_df_1$LotShape == 'IR3')), 'LotShape'] <- 'IR'
bartlett.test(eda_df_1$SalePrice ~ eda_df_1$LotShape)
#등분산을 만족한다. 
summary(aov(eda_df_1$SalePrice ~ eda_df_1$LotShape, data = eda_df_1))
#여전히 유의함을 알수 있다. 변수는 두개만 남았기 때문에 따로 집단간 차이 검정을 하지 않도록 하겠다. 


# 4.6. LandContour ★★ ####
plot(as.factor(eda_df_1$LandContour), eda_df_1$SalePrice)
#Lvl에서 이상치가 편중되어 있음을 볼수 있다. 
density_plotting(as.factor(eda_df_1$LandContour), eda_df_1$SalePrice)
bartlett.test(eda_df_1$SalePrice~eda_df_1$LotShape)
#등분산을 만족한다. 
summary(aov(eda_df_1$SalePrice ~ eda_df_1$LandContour, data = eda_df_1))
#유의하다. 어떤집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$LandContour), y = eda_df_1$SalePrice, posthoc = "tukey")
#Lvl과 Low사이의 차이가 전혀 유의하지 않으므로 Lvl and Low로 두집단을 묶어준다. 
eda_df_1[which((eda_df_1$LandContour == 'Lvl') | (eda_df_1$LandContour == 'Low')), 'LandContour'] <- 'Lvl and Low'
bartlett.test(eda_df_1$SalePrice ~ eda_df_1$LotShape)
#등분산성을 만족한다. 
summary(aov(eda_df_1$SalePrice ~ eda_df_1$LandContour, data = eda_df_1))
#이 변수가 유의하므로 어떤집단 간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$LandContour), y = eda_df_1$SalePrice, posthoc = "tukey")
#세집단 차이가 모두 유의함을 알 수 있다. 따라서 범주를 3개로 줄였다. 


# 4.7. Utilities ####
table(eda_df_1$Utilities)
#Utilities의 경우 모든 값이 AllPub으로 나타났다. 따라서 이 값이 영향을 끼치지 않는다고 볼 수 있다. 
#이 변수를 제외한다. 


# 4.8. LotConfig ★★ ####
plot(as.factor(eda_df_1$LotConfig), eda_df_1$SalePrice)
#Inside에서 이상치가 많이 나타난다. 
density_plotting(eda_df_1$LotConfig, eda_df_1$SalePrice)
bartlett.test(eda_df_1$SalePrice~eda_df_1$LotConfig)
#등분산을 만족한다. 
summary(aov(eda_df_1$SalePrice ~ eda_df_1$LotConfig, data = eda_df_1))
#유의하다. 어떤집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$LotConfig), y = eda_df_1$SalePrice, posthoc = "tukey")
#FR2, Corner, Inside간 평균의 차이가 유의하지 않으므로 이 세집단을  FR2 and Corner and Inside 으로 묶어준다. 
eda_df_1[which((eda_df_1$LotConfig == 'FR2') | (eda_df_1$LotConfig == 'Corner') | (eda_df_1$LotConfig == 'Inside')), 'LotConfig'] <- 'FR2 and Corner and Inside'
bartlett.test(eda_df_1$SalePrice ~ eda_df_1$LotConfig)
#등분산성을 만족한다. 
oneway(as.factor(eda_df_1$LotConfig), y = eda_df_1$SalePrice, posthoc = "tukey")
#FR3의 경우 애매하지만 박스플럿상 어느정도 차이있다고 판단할 수 있으므로 그대로 둔다. 결과정으로 범주형이 3개로 줄었다. 


# 4.9. LandSlope ####
plot(as.factor(eda_df_1$LandSlope), eda_df_1$SalePrice)
# Gtl에 이상치가 많이 나타남 평균차이는 크지 않은 것 같음 
density_plotting(as.factor(eda_df_1$LandSlope), eda_df_1$SalePrice)
bartlett.test(eda_df_1$SalePrice~eda_df_1$LandSlope)
#등분산을 만족하지 않는다. 
oneway.test(eda_df_1$SalePrice~eda_df_1$LandSlope, var.equal = F)
#유의하지 않다. 


# 4.10. Neighborhood ★★ ####
plot(as.factor(eda_df_1$Neighborhood), eda_df_1$SalePrice)
#평균의 차이가 꽤 나타는 변수들이 있다. 
density_plotting(eda_df_1$Neighborhood, eda_df_1$SalePrice)
#변수가 너무 많기 때문에 density plot을 그리기 어렵다. 
box_jitter(eda_df_1$Neighborhood, eda_df_1$SalePrice)
#관측치가 Norm에 치우쳐 있다. 
bartlett.test(eda_df_1$SalePrice~eda_df_1$Neighborhood)
#등분산을 만족하지 않는다. 
oneway.test(eda_df_1$SalePrice ~ eda_df_1$Neighborhood, var.equal = F)
#anova test로 검정한 결과 유의한 변수로 판단할 수 있다. 
#어떤 집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$Neighborhood), y = eda_df_1$SalePrice, posthoc = "games-howell")
table(as.factor(eda_df_1$Neighborhood))
#Bluested의 경우 2개로 구성되어 다른 집단간 검정이 불가능하다 따라서 이를 분석에서 제외하도록 하겠다. 
eda_df_1<-eda_df_1[-which(eda_df_1$Neighborhood == 'Blueste'),]
new.train <- new.train[-which(new.train$Neighborhood == 'Blueste'),]
bartlett.test(eda_df_1$SalePrice~eda_df_1$Neighborhood)
#등분산을 만족하지 않는다. 
oneway.test(eda_df_1$SalePrice ~ eda_df_1$Neighborhood, var.equal = F)
#anova test로 검정한 결과 유의한 변수로 판단할 수 있다. 
#어떤 집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$Neighborhood), y = eda_df_1$SalePrice, posthoc = "games-howell")
#Blmngtn, ClearCr, CollgCr, Crawfor, Gilbert, Mitchel, NWAmes, SawyerW, Somerst, Timber, Veenker, SWISU이
#그룹간 차이가 없다 따라서 이들을 neighbor1이라는 그룹으로 묶도록 하겠다. 
eda_df_1[which((eda_df_1$Neighborhood == 'Blmngtn') | (eda_df_1$Neighborhood == 'Gilbert') | (eda_df_1$Neighborhood == 'Somerst')
               | (eda_df_1$Neighborhood == 'ClearCr') | (eda_df_1$Neighborhood == 'Mitchel') | (eda_df_1$Neighborhood == 'Timber')
               | (eda_df_1$Neighborhood == 'CollgCr') | (eda_df_1$Neighborhood == 'NWAmes') | (eda_df_1$Neighborhood == 'Veenker')
               | (eda_df_1$Neighborhood == 'Crawfor') | (eda_df_1$Neighborhood == 'SawyerW') | (eda_df_1$Neighborhood == 'SWISU')),
         'Neighborhood'] <- 'neighbor1'
bartlett.test(eda_df_1$SalePrice~eda_df_1$Neighborhood)
#등분산을 만족하지 않는다. 
oneway.test(eda_df_1$SalePrice ~ eda_df_1$Neighborhood, var.equal = F)
#anova test로 검정한 결과 유의한 변수로 판단할 수 있다. 
#어떤 집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$Neighborhood), y = eda_df_1$SalePrice, posthoc = "games-howell")
#BrkSide, BrDale, IDOTRR, MeadowV, OldTown, Edwards이 집단간 차이가 통계적으로 유의하지 않으므로 neighbor2 그룹을 묶는다. 
eda_df_1[which((eda_df_1$Neighborhood == 'BrkSide') | (eda_df_1$Neighborhood == 'BrDale') | (eda_df_1$Neighborhood == 'IDOTRR')
               | (eda_df_1$Neighborhood == 'MeadowV') | (eda_df_1$Neighborhood == 'OldTown')| (eda_df_1$Neighborhood == 'Edwards')),
         'Neighborhood'] <- 'neighbor2'
bartlett.test(eda_df_1$SalePrice~eda_df_1$Neighborhood)
#등분산을 만족하지 않는다. 
oneway.test(eda_df_1$SalePrice ~ eda_df_1$Neighborhood, var.equal = F)
#anova test로 검정한 결과 유의한 변수로 판단할 수 있다. 
#어떤 집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$Neighborhood), y = eda_df_1$SalePrice, posthoc = "games-howell")
#NAmes, NPkVill, Sawyer간 그룹차가 유의하않으므로 neighbor3로 묶는다. 
eda_df_1[which((eda_df_1$Neighborhood == 'NAmes') | (eda_df_1$Neighborhood == 'NPkVill') | (eda_df_1$Neighborhood == 'Sawyer')),
         'Neighborhood'] <- 'neighbor3'
bartlett.test(eda_df_1$SalePrice~eda_df_1$Neighborhood)
#등분산을 만족하지 않는다. 
oneway.test(eda_df_1$SalePrice ~ eda_df_1$Neighborhood, var.equal = F)
#anova test로 검정한 결과 유의한 변수로 판단할 수 있다. 
#어떤 집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$Neighborhood), y = eda_df_1$SalePrice, posthoc = "games-howell")
#NoRidge, NridgHt, StoneBr의 그룹차가 유의하지 않으므로 neighbor4로 묶는다. 
eda_df_1[which((eda_df_1$Neighborhood == 'NoRidge') | (eda_df_1$Neighborhood == 'NridgHt') | (eda_df_1$Neighborhood == 'StoneBr')),
         'Neighborhood'] <- 'neighbor4'
bartlett.test(eda_df_1$SalePrice~eda_df_1$Neighborhood)
#등분산을 만족하지 않는다. 
oneway.test(eda_df_1$SalePrice ~ eda_df_1$Neighborhood, var.equal = F)
#anova test로 검정한 결과 유의한 변수로 판단할 수 있다. 
#어떤 집단간 차이가 유의한지 검정한다. 
oneway(as.factor(eda_df_1$Neighborhood), y = eda_df_1$SalePrice, posthoc = "games-howell")
#네가지 범주로 축약하는데 성공하였다. 


# 4.11. Condition1 ★★ ####
plot(as.factor(eda_df_1$Condition1), eda_df_1$SalePrice)
#평균 차이가 나타나는 변수들이 많다. 
density_plotting(eda_df_1$Condition1, eda_df_1$SalePrice)
#밀도함수로 나타내면 변수의 수가 많아 나타나지 않는다. 
box_jitter(eda_df_1$Condition1, eda_df_1$SalePrice)
bartlett.test(SalePrice~Condition1,data = eda_df_1)
eda_df_1<-eda_df_1[-which(eda_df_1$Condition1 == 'RRNe'),]
new.train <- new.train[-which(new.train$Condition1 == 'RRNe'),]
bartlett.test(SalePrice~Condition1,data = eda_df_1)
#등분산이 아니다.
oneway.test(SalePrice~Condition1, data = eda_df_1, var.equal = F)
#anova test로 검정한 결과 유의한 변수라는 것을 알 수 있다. 
oneway(as.factor(eda_df_1$Condition1), y = eda_df_1$SalePrice, posthoc = "games-howell")
#통계적으로 Norm을 제외하면 나머지 그룹은 그룹간 차이가 없다. 따라서 나머지 그룹을 non-Norm이라고 명칭하여 묶는다. 
eda_df_1[which((eda_df_1$Condition1 == 'Artery') | (eda_df_1$Condition1 == 'Feedr') | (eda_df_1$Condition1 == 'PosA')
               | (eda_df_1$Condition1 == 'PosN')| (eda_df_1$Condition1 == 'RRAe')| (eda_df_1$Condition1 == 'RRAn')| (eda_df_1$Condition1 == 'RRNn')),
         'Condition1'] <- 'non-norm'
bartlett.test(SalePrice~Condition1,data = eda_df_1)
#등분산이다. 
summary(aov(SalePrice~Condition2, data = eda_df_1))
#유의함을 알수 있다. 
oneway(as.factor(eda_df_1$Condition1), y = eda_df_1$SalePrice, posthoc = "tukey")


# 4.12. Condition2 ####
plot(as.factor(eda_df_1$Condition2), eda_df_1$SalePrice)
#Condition1과 마찬가지로 평균의 차이가 나타나며 
#Norm에 이상치가 치우쳐 있다는 점을 알 수 있다. 
density_plotting(eda_df_1$Condition2, eda_df_1$SalePrice)
############################################################??
box_jitter(eda_df_1$Condition2, eda_df_1$SalePrice)
bartlett.test(SalePrice~Condition2, data = eda_df_1)
eda_df_1<-eda_df_1[-which(eda_df_1$Condition2 == 'PosA'),]
new.train <- new.train[-which(new.train$Condition2 == 'PosA'),]
# df<-df[-which(df$Condition2 == 'PosA'),]
bartlett.test(SalePrice~Condition2, data = eda_df_1)
#등분산을 만족한다. 
summary(aov(SalePrice~Condition2, data = eda_df_1))
#anova test로 검정한 결과 유의한 변수지만 앞서 발견한 
#변수들의 비해 상대적으로 중요도가 낮으며 데이터의 대부분이 Norm에 몰려있다는 것을 알 수 있다.  
#따라서 이변수가 종속변수에 큰 영향을 끼치지 않음을알 수 있다. 이를 제외한다. 


# 4.13. BldgType ★★ ####
plot(as.factor(eda_df_1$BldgType), eda_df_1$SalePrice)
#평균의 차이는 크지 않지만 1Fam에서 이상치가 많이 존재함을 알 수 있다. 
density_plotting(eda_df_1$BldgType, eda_df_1$SalePrice)
bartlett.test(eda_df_1$SalePrice ~ eda_df_1$BldgType, data = eda_df_1)
#등분산을 만족하지 않는다. 
oneway.test(eda_df_1$SalePrice ~ eda_df_1$BldgType, data = eda_df_1, var.equal = F)
#유의한 변수임을 알 수 있다. 집단간 차이를 검정한다. 
oneway(as.factor(eda_df_1$BldgType), y = eda_df_1$SalePrice, posthoc = "games-howell")
#분석결과 1Fam과 TwnhsE과 유사하며 나머지 3개 그룹이 유사함을 알 수 있다. 각각 두그룹으로 이를 분리한다. 
eda_df_1[which((eda_df_1$BldgType == '1Fam') | (eda_df_1$BldgType == 'TwnhsE') ), 'BldgType'] <- '1Fam and TwnhsE'
eda_df_1[which((eda_df_1$BldgType == 'Twnhs') | (eda_df_1$BldgType == '2fmCon') | (eda_df_1$BldgType == 'Duplex')), 'BldgType'] <- 'others'
bartlett.test(SalePrice ~ BldgType, data = eda_df_1)
#등분산을 만족하지 않는다. 
oneway.test(SalePrice ~ BldgType, data = eda_df_1, var.equal = F)
oneway(as.factor(eda_df_1$BldgType), y = eda_df_1$SalePrice, posthoc = "games-howell")
#유의함을 알 수 있다. 2가지 범주로 줄이게 되었다. 


# 4.14. HouseStyle ★★ ####
plot(as.factor(eda_df_1$HouseStyle), eda_df_1$SalePrice)
#각 클래스별 평균차는 평이하지만 2Story, 1Story에서 결측치가 많이 존재함을 볼 수 있다. 
density_plotting(eda_df_1$HouseStyle, eda_df_1$SalePrice)
box_jitter(eda_df_1$HouseStyle, eda_df_1$SalePrice)
bartlett.test(SalePrice~HouseStyle, data = eda_df_1)
#등분산이 아니다. 
oneway.test(SalePrice~HouseStyle, data = eda_df_1, var.equal = F)
#유의한 변수임을 알 수 있다. 
oneway(as.factor(eda_df_1$HouseStyle), y = eda_df_1$SalePrice, posthoc = "games-howell")
#2.5Unf, 1.5Fin, SFoyer, 2.5Fin 1.5Unf 다섯집단의 차이가 유의하지않음을 알수있다. 따라서 이 다섯집단을 Foyer and .5라고 묶는다. 
eda_df_1[which((eda_df_1$HouseStyle == '2.5Unf') | (eda_df_1$HouseStyle == '1.5Fin') 
               | (eda_df_1$HouseStyle == 'SFoyer') | (eda_df_1$HouseStyle == '2.5Fin') | (eda_df_1$HouseStyle == '1.5Unf')), 'HouseStyle'] <- 'Foyer and .5'
oneway.test(SalePrice~HouseStyle, data = eda_df_1, var.equal = F)
oneway(as.factor(eda_df_1$HouseStyle), y = eda_df_1$SalePrice, posthoc = "games-howell")
#SLvl and 1Story로 묶어준다. 
eda_df_1[which((eda_df_1$HouseStyle == 'SLvl') | (eda_df_1$HouseStyle == '1Story')), 'HouseStyle'] <- 'SLvl and 1Story'
oneway.test(SalePrice~HouseStyle, data = eda_df_1, var.equal = F)
oneway(as.factor(eda_df_1$HouseStyle), y = eda_df_1$SalePrice, posthoc = "games-howell")
#평균차가 유의한 3집단으로 나눌 수 있었다. 


# 4.15. LotFrontage ####
#수치형 변수를 표준화한다. 
eda_df_1$LotFrontage <- scale(eda_df_1$LotFrontage)
# 이상치를 제거해준다.
UCL <- quantile(x = eda_df_1$LotFrontage, probs = 0.99865)
LCL <- quantile(x = eda_df_1$LotFrontage, probs = 0.00135)

outlier <- which((eda_df_1$LotFrontage >= UCL) | (eda_df_1$LotFrontage <= LCL)) 
eda_df_1 <- eda_df_1[-outlier, ]
new.train <- new.train[-outlier, ]
plot(as.factor(eda_df_1$LotFrontage), eda_df_1$SalePrice)
#plot을 그려보니 상관관계가 어느정도 있을 것으로 추정된다. 
cor(eda_df_1$LotFrontage, eda_df_1$SalePrice)
#상관계수를 보니 0.35정도로 그렇게 큰 관계를 나타내지 않는다. 


# 4.16. LotArea ####
#수치형 변수를 표준화한다. 
eda_df_1$LotArea <- scale(eda_df_1$LotArea)
# 이상치를 제거해준다.
UCL <- quantile(x = eda_df_1$LotArea, probs = 0.99865)
LCL <- quantile(x = eda_df_1$LotArea, probs = 0.00135)

outlier <- which((eda_df_1$LotArea >= UCL) | (eda_df_1$LotArea <= LCL)) 
eda_df_1 <- eda_df_1[-outlier, ]
new.train <- new.train[-outlier, ]
plot(eda_df_1$SalePrice, as.factor(eda_df_1$LotArea))
cor(eda_df_1$LotArea, eda_df_1$SalePrice)
#LotArea도 Frontage와 마찬가지로 0.3정도의 계수를 가짐을 알 수 있다. 


# 4.17. OveallQual ★★ ####
#수치형 변수를 표준화한다. 
eda_df_1$OverallQual <- scale(eda_df_1$OverallQual)
# 이상치를 제거해준다.
UCL <- quantile(x = eda_df_1$OverallQual, probs = 0.99865)
LCL <- quantile(x = eda_df_1$OverallQual, probs = 0.00135)

outlier <- which((eda_df_1$OverallQual >= UCL) | (eda_df_1$OverallQual <= LCL)) 
eda_df_1 <- eda_df_1[-outlier, ]
new.train <- new.train[-outlier, ]
plot(eda_df_1$SalePrice, eda_df_1$OverallQual)
cor(eda_df_1$SalePrice, eda_df_1$OverallQual)
#0.8정도로 매우 유의하다.  


# 4.18. OverallCond ★★ ####
plot(eda_df_1$OverallCond, eda_df_1$SalePrice)
cor(eda_df_1$OverallCond, eda_df_1$SalePrice)
plot(as.factor(eda_df_1$OverallCond), eda_df_1$SalePrice)
#Cond는 Qual에 비해 큰 차이는 없지만 2,3에서 확실히 평균이 낮은 분포를 보인다.  
density_plotting(as.factor(eda_df_1$OverallCond), eda_df_1$SalePrice)
bartlett.test(SalePrice~as.factor(OverallCond), data = eda_df_1)
#등분산을 만족하지 않는다. 
oneway.test(SalePrice~as.factor(OverallCond), data = eda_df_1, var.equal = F)
#유의하다. 집단간 차이가 어디서 유의한지 검정한다. 
oneway(as.factor(eda_df_1$OverallCond), y = eda_df_1$SalePrice, posthoc = "games-howell")
#2,3,4 그룹이 평균간 차이가 유의하지않으므로 2~4 그룹으로 묶는다. 
eda_df_1$OverallQual <- as.character(eda_df_1$OverallQual)
eda_df_1[which((eda_df_1$OverallCond == '3') | (eda_df_1$OverallCond == '2')| (eda_df_1$OverallCond == '4')), 'OverallCond'] <- '2~4'
oneway.test(SalePrice~as.factor(OverallCond), data = eda_df_1, var.equal = F)
#유의하다. 집단간 차이가 어디서 유의한지 검정한다. 
oneway(as.factor(eda_df_1$OverallCond), y = eda_df_1$SalePrice, posthoc = "games-howell")
#6,7,8,9 그룹의 집단간 차이가 유의하지 않으므로 6~9 그룹으로 묶는다. 
eda_df_1[which((eda_df_1$OverallCond == '6') | (eda_df_1$OverallCond == '7')| (eda_df_1$OverallCond == '8')| (eda_df_1$OverallCond == '9')), 'OverallCond'] <- '6~9'
oneway.test(SalePrice~as.factor(OverallCond), data = eda_df_1, var.equal = F)
#유의하다. 집단간 차이가 어디서 유의한지 검정한다. 
oneway(as.factor(eda_df_1$OverallCond), y = eda_df_1$SalePrice, posthoc = "games-howell")
#3개의 집단으로 줄이게 됨. 
eda_df_1$OverallQual <- as.factor(eda_df_1$OverallQual)


# 4.19. YearBuilt ★★ ####
#수치형 변수를 표준화한다. 
eda_df_1$YearBuilt <- scale(eda_df_1$YearBuilt)
# 이상치를 제거해준다.
UCL <- quantile(x = eda_df_1$YearBuilt, probs = 0.99865)
LCL <- quantile(x = eda_df_1$YearBuilt, probs = 0.00135)

outlier <- which((eda_df_1$YearBuilt >= UCL) | (eda_df_1$YearBuilt <= LCL)) 
eda_df_1 <- eda_df_1[-outlier, ]
new.train <- new.train[-outlier, ]
plot(eda_df_1$YearBuilt, eda_df_1$SalePrice)
#그래프 분포를 봤을 때, YearBuilt와 가격이 어느정도 양의 상관관계를 갖는는 것을 유추할수 있다. 
cor(eda_df_1$YearBuilt, eda_df_1$SalePrice)
#0.63정도로 어느정도 관계가 있음을 알 수 있다. 


# 4.20. YearRemodAdd ★★ ####
#이상치 처리를 하게 되면 너무 많은 데이터가 사라지므로 이를 범주형 데이터로 보고 검정한다. 
table(as.factor(eda_df_1$YearRemodAdd))
eda_df_1$YearRemodAdd <- as.factor(eda_df_1$YearRemodAdd)
bartlett.test(SalePrice~YearRemodAdd, data = eda_df_1)
#등분산을 만족하지 않는다. 
oneway.test(SalePrice ~ YearRemodAdd, data = eda_df_1, var.equal = F)
#유의함을 알수 있다. 어떤 집단간 차이가 유의한지 확인한ㄷ.
oneway(eda_df_1$YearRemodAdd, y = eda_df_1$SalePrice, posthoc = "games-howell")
#1997~1950년의 집단간 차이가 없으므로 이 집단을 1950~1970으로 묶어준다. 
eda_df_1$YearRemodAdd <- as.character(eda_df_1$YearRemodAdd)
eda_df_1[which((eda_df_1$YearRemodAdd == '1950')|(eda_df_1$YearRemodAdd == '1951')|
                 (eda_df_1$YearRemodAdd == '1952')|(eda_df_1$YearRemodAdd == '1953')|(eda_df_1$YearRemodAdd == '1954')|
                 (eda_df_1$YearRemodAdd == '1955')|(eda_df_1$YearRemodAdd == '1956')|(eda_df_1$YearRemodAdd == '1957')|
                 (eda_df_1$YearRemodAdd == '1958')|(eda_df_1$YearRemodAdd == '1959')|(eda_df_1$YearRemodAdd == '1960')|
                 (eda_df_1$YearRemodAdd == '1961')|(eda_df_1$YearRemodAdd == '1962')|(eda_df_1$YearRemodAdd == '1963')|
                 (eda_df_1$YearRemodAdd == '1964')|(eda_df_1$YearRemodAdd == '1965')|(eda_df_1$YearRemodAdd == '1966')|
                 (eda_df_1$YearRemodAdd == '1967')|(eda_df_1$YearRemodAdd == '1968')|(eda_df_1$YearRemodAdd == '1969')|
                 (eda_df_1$YearRemodAdd == '1970')|(eda_df_1$YearRemodAdd == '1971')|(eda_df_1$YearRemodAdd == '1972')|
                 (eda_df_1$YearRemodAdd == '1973')|(eda_df_1$YearRemodAdd == '1974')|(eda_df_1$YearRemodAdd == '1975')|
                 (eda_df_1$YearRemodAdd == '1976')|(eda_df_1$YearRemodAdd == '1977')|(eda_df_1$YearRemodAdd == '1978')|
                 (eda_df_1$YearRemodAdd == '1979')|(eda_df_1$YearRemodAdd == '1980')|(eda_df_1$YearRemodAdd == '1981')|
                 (eda_df_1$YearRemodAdd == '1982')|(eda_df_1$YearRemodAdd == '1983')|(eda_df_1$YearRemodAdd == '1984')|
                 (eda_df_1$YearRemodAdd == '1985')|(eda_df_1$YearRemodAdd == '1986')|(eda_df_1$YearRemodAdd == '1987')|
                 (eda_df_1$YearRemodAdd == '1988')|(eda_df_1$YearRemodAdd == '1989')|(eda_df_1$YearRemodAdd == '1990')|
                 (eda_df_1$YearRemodAdd == '1991')|(eda_df_1$YearRemodAdd == '1992')|(eda_df_1$YearRemodAdd == '1993')|
                 (eda_df_1$YearRemodAdd == '1994')|(eda_df_1$YearRemodAdd == '1995')|(eda_df_1$YearRemodAdd == '1996')| (eda_df_1$YearRemodAdd == '1997')
)
, 'YearRemodAdd'] <- '1950~1997'
eda_df_1$YearRemodAdd <- as.factor(eda_df_1$YearRemodAdd)
bartlett.test(SalePrice~YearRemodAdd, data = eda_df_1)
table(as.factor(eda_df_1$YearRemodAdd))
#등분산을 만족하지 않는다. 
oneway.test(SalePrice ~ YearRemodAdd, data = eda_df_1, var.equal = F)
#유의함을 알수 있다. 어떤 집단간 차이가 유의한지 확인한ㄷ.
oneway(eda_df_1$YearRemodAdd, y = eda_df_1$SalePrice, posthoc = "games-howell")
#1998~2010까지 그룹 간 평균차이가 유의하지 않으므로 이를 합친다.
eda_df_1$YearRemodAdd <- as.character(eda_df_1$YearRemodAdd)
eda_df_1[which(eda_df_1$YearRemodAdd != '1950~1997'),'YearRemodAdd'] <- '1998~'
bartlett.test(SalePrice~YearRemodAdd, data = eda_df_1)
#등분산을 만족한다. 
summary(aov(SalePrice~YearRemodAdd, data = eda_df_1))
#결과적으로 2개의 범주로 이를 함축하였다. 

#new.train 갱신
new.train[,eda_1] <- eda_df_1[,1:20]

#new.test 갱신
new.test[which((new.test$MSSubClass == 40) | (new.test$MSSubClass == 45)), "MSSubClass"] <- 40
new.test[which((new.test$MSSubClass == 70) | (new.test$MSSubClass == 75)), "MSSubClass"] <- 70
new.test[which((new.test$MSSubClass == 80) | (new.test$MSSubClass == 85)), "MSSubClass"] <- 80
new.test[which((new.test$MSSubClass == 70)  | (new.test$MSSubClass == 80)  | (new.test$MSSubClass == 90)), "MSSubClass"] <- "70 ~ 90"
new.test[which((new.test$MSSubClass == 160) | (new.test$MSSubClass == 180) | (new.test$MSSubClass == 190)), "MSSubClass"] <- "160 ~ 190"
new.test[which((new.test$MSSubClass == 30) | (new.test$MSSubClass == 40)), "MSSubClass"] <- "30 ~ 40"
new.test[which((new.test$MSZoning == 'RH') | (new.test$MSZoning == 'RM') | (new.test$MSZoning == 'C (all)')),'MSZoning'] <- 'C or RM or RH'
new.test[which((new.test$Alley == 'Pave') | (new.test$Alley == 'No Alley')), 'Alley'] <- 'Pave and No Alley'
new.test[which((new.test$LotShape == 'IR1') | (new.test$LotShape == 'IR2') | (new.test$LotShape == 'IR3')), 'LotShape'] <- 'IR'
new.test[which((new.test$LandContour == 'Lvl') | (new.test$LandContour == 'Low')), 'LandContour'] <- 'Lvl and Low'
new.test[which((new.test$LotConfig == 'FR2') | (new.test$LotConfig == 'Corner') | (new.test$LotConfig == 'Inside')), 'LotConfig'] <- 'FR2 and Corner and Inside'
new.test[which((new.test$BldgType == '1Fam') | (new.test$BldgType == 'TwnhsE') ), 'BldgType'] <- '1Fam and TwnhsE'
new.test[which((new.test$BldgType == 'Twnhs') | (new.test$BldgType == '2fmCon') | (new.test$BldgType == 'Duplex')), 'BldgType'] <- 'others'
new.test[which((new.test$HouseStyle == '2.5Unf') | (new.test$HouseStyle == '1.5Fin') 
               | (new.test$HouseStyle == 'SFoyer') | (new.test$HouseStyle == '2.5Fin') | (new.test$HouseStyle == '1.5Unf')), 'HouseStyle'] <- 'Foyer and .5'
new.test[which((new.test$OverallCond == '3') | (new.test$OverallCond == '2')| (new.test$OverallCond == '4')), 'OverallCond'] <- '2~4'
new.test[which((new.test$OverallCond == '6') | (new.test$OverallCond == '7')| (new.test$OverallCond == '8')| (new.test$OverallCond == '9')), 'OverallCond'] <- '6~9'
new.test[which((new.test$Neighborhood == 'Blmngtn') | (new.test$Neighborhood == 'Gilbert') | (new.test$Neighborhood == 'Somerst')
               | (new.test$Neighborhood == 'ClearCr') | (new.test$Neighborhood == 'Mitchel') | (new.test$Neighborhood == 'Timber')
               | (new.test$Neighborhood == 'CollgCr') | (new.test$Neighborhood == 'NWAmes') | (new.test$Neighborhood == 'Veenker')
               | (new.test$Neighborhood == 'Crawfor') | (new.test$Neighborhood == 'SawyerW') | (new.test$Neighborhood == 'SWISU')),
         'Neighborhood'] <- 'neighbor1'
new.test[which((new.test$Neighborhood == 'BrkSide') | (new.test$Neighborhood == 'BrDale') | (new.test$Neighborhood == 'IDOTRR')
               | (new.test$Neighborhood == 'MeadowV') | (new.test$Neighborhood == 'OldTown')| (new.test$Neighborhood == 'Edwards')),
         'Neighborhood'] <- 'neighbor2'
new.test[which((new.test$Neighborhood == 'NAmes') | (new.test$Neighborhood == 'NPkVill') | (new.test$Neighborhood == 'Sawyer')),
         'Neighborhood'] <- 'neighbor3'
new.test[which((new.test$Neighborhood == 'NoRidge') | (new.test$Neighborhood == 'NridgHt') | (new.test$Neighborhood == 'StoneBr')),
         'Neighborhood'] <- 'neighbor4'
new.test[which((new.test$Condition1 == 'Artery') | (new.test$Condition1 == 'Feedr') | (new.test$Condition1 == 'PosA')
               | (new.test$Condition1 == 'PosN')| (new.test$Condition1 == 'RRAe')| (new.test$Condition1 == 'RRAn')| (new.test$Condition1 == 'RRNn')),
         'Condition1'] <- 'non-norm'
new.test[which((new.test$YearRemodAdd == '1950')|(new.test$YearRemodAdd == '1951')|
                 (new.test$YearRemodAdd == '1952')|(new.test$YearRemodAdd == '1953')|(new.test$YearRemodAdd == '1954')|
                 (new.test$YearRemodAdd == '1955')|(new.test$YearRemodAdd == '1956')|(new.test$YearRemodAdd == '1957')|
                 (new.test$YearRemodAdd == '1958')|(new.test$YearRemodAdd == '1959')|(new.test$YearRemodAdd == '1960')|
                 (new.test$YearRemodAdd == '1961')|(new.test$YearRemodAdd == '1962')|(new.test$YearRemodAdd == '1963')|
                 (new.test$YearRemodAdd == '1964')|(new.test$YearRemodAdd == '1965')|(new.test$YearRemodAdd == '1966')|
                 (new.test$YearRemodAdd == '1967')|(new.test$YearRemodAdd == '1968')|(new.test$YearRemodAdd == '1969')|
                 (new.test$YearRemodAdd == '1970')|(new.test$YearRemodAdd == '1971')|(new.test$YearRemodAdd == '1972')|
                 (new.test$YearRemodAdd == '1973')|(new.test$YearRemodAdd == '1974')|(new.test$YearRemodAdd == '1975')|
                 (new.test$YearRemodAdd == '1976')|(new.test$YearRemodAdd == '1977')|(new.test$YearRemodAdd == '1978')|
                 (new.test$YearRemodAdd == '1979')|(new.test$YearRemodAdd == '1980')|(new.test$YearRemodAdd == '1981')|
                 (new.test$YearRemodAdd == '1982')|(new.test$YearRemodAdd == '1983')|(new.test$YearRemodAdd == '1984')|
                 (new.test$YearRemodAdd == '1985')|(new.test$YearRemodAdd == '1986')|(new.test$YearRemodAdd == '1987')|
                 (new.test$YearRemodAdd == '1988')|(new.test$YearRemodAdd == '1989')|(new.test$YearRemodAdd == '1990')|
                 (new.test$YearRemodAdd == '1991')|(new.test$YearRemodAdd == '1992')|(new.test$YearRemodAdd == '1993')|
                 (new.test$YearRemodAdd == '1994')|(new.test$YearRemodAdd == '1995')|(new.test$YearRemodAdd == '1996')| (new.test$YearRemodAdd == '1997')
), 'YearRemodAdd'] <- '1950~1997'
new.test[which(new.test$YearRemodAdd != '1950~1997'),'YearRemodAdd'] <- '1998~'

eda2.train <- new.train
eda2.test  <- new.test 


# 지원 EDA ####
# 4.21. RoofStyle ####
table(new.train$RoofStyle)

#bar graph
ggplot() + geom_bar(data=new.train, aes(x=RoofStyle, fill=RoofStyle))
#density
ggplot() + geom_density(data=new.train,aes(x=SalePrice, color=RoofStyle,fill=RoofStyle),alpha=0.5)
#histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = RoofStyle),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(RoofStyle ~.)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=RoofStyle, y=SalePrice, fill=RoofStyle)) +
  geom_jitter(data=new.train, aes(x=RoofStyle, y=SalePrice, fill=RoofStyle))

#대부분 RoofStyle은 Gable이다.
#RoofStyle에 따른 SalePrice 범위의 차이가 있는 것 처럼 보인다.

bartlett.test(SalePrice~RoofStyle,data = new.train)
#등분산성을 만족하지않는다.
oneway.test(SalePrice~RoofStyle, data = new.train, var.equal = FALSE)
#유의한 변수이다. 
# 하지만 Gable 범주를 제외한 나머지 범주의 데이터가 매우 적어 모델을 만드는 데 사용할 변수로 적합하지 않다고 판단하였다.


# 4.22. RoofMatl ####
summary(new.train$RoofMatl)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=RoofMatl, fill=RoofMatl))
#density
ggplot() + geom_density(data=new.train,aes(x=SalePrice, color=RoofMatl,fill=RoofMatl),alpha=0.5)
#histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = RoofMatl),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(RoofMatl ~.)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=RoofMatl, y=SalePrice, fill=RoofMatl)) +
  geom_jitter(data=new.train, aes(x=RoofMatl, y=SalePrice, fill=RoofMatl))
#대부분 CompShg : 개수 및 비율 확인 
nrow(eda_2[new.train$RoofMatl=="CompShg",])
#98%가 CompShg
nrow(eda_2[new.train$RoofMatl=="CompShg",])/nrow(eda_2) 
head(new.train)

#RoofMatl 대부분이 CompShg이고 ClyTile,Membran,Roll의 경우엔 한 개씩 밖에 없다.
#다른 값들의 충분한 데이터가 없어 종속변수에 유의한 영향을 끼치는 지 확인하기 어려울 것 같다. 

bartlett.test(SalePrice~RoofMatl,data = new.train)
new.train<-new.train[ -which(new.train$RoofMatl=="ClyTile"|new.train$RoofMatl=="Membran"|new.train$RoofMatl=="Roll"),]
df<-new.train[ -which(df$RoofMatl=="ClyTile"|df$RoofMatl=="Membran"|df$RoofMatl=="Roll"),]
#등분산성을 만족하지 않는다. 
oneway.test(SalePrice~RoofMatl, data=new.train)
#유의하지 않은 변수이다. 


# 4.23. Exterior1st ####
table(new.train$Exterior1st, useNA = "ifany")
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=Exterior1st, fill=Exterior1st))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=Exterior1st,fill=Exterior1st),alpha=0.5)

#boxplot
ggplot() + 
  geom_boxplot(data=new.train, aes(x=Exterior1st, y=SalePrice, fill=Exterior1st)) +
  geom_jitter(data=new.train, aes(x=Exterior1st, y=SalePrice, fill=Exterior1st))

#값에 따른 SalePrice의 변화가 있는 것처럼 보이나 정확히는 판단하기 어렵다.
# 일단 범주가 매우 다양한데 일부 범주에만 데이터가 몰려있기 때문에 모델을 만드는 데 사용할 변수로 적합하지 않다고 판단했다.


# 4.24. Exterior2nd ####
table(new.train$Exterior2nd)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=Exterior2nd, fill=Exterior2nd))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=Exterior2nd,fill=Exterior2nd),alpha=0.5)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=Exterior2nd, y=SalePrice, fill=Exterior2nd)) +
  geom_jitter(data=eda2, aes(x=Exterior2nd, y=SalePrice, fill=Exterior2nd))
#Exterior 두 가지 이상 재료 쓰는 데이터 확인
#178개의 데이터가 두 가지 이상 material 사용 
# 일단 범주가 매우 다양한데 일부 범주에만 데이터가 몰려있기 때문에 모델을 만드는 데 사용할 변수로 적합하지 않다고 판단했다.


# 4.25. MasVnrType ####
summary(new.train$MasVnrType)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=MasVnrType, fill=MasVnrType))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=MasVnrType,fill=MasVnrType),alpha=0.5)
#histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = MasVnrType),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(MasVnrType ~.)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=MasVnrType, y=SalePrice, fill=MasVnrType)) +
  geom_jitter(data=new.train, aes(x=MasVnrType, y=SalePrice, fill=MasVnrType))

#Masonry Veneer를 사용하지 않는 경우가 많다.
#값에 따른 SalePrice의 변화가 있는 것처럼 보이나 정확히는 판단하기 어렵다.

bartlett.test(SalePrice~MasVnrType, data=new.train)
#등분산성을 만족하지 않는다.
oneway.test(SalePrice~MasVnrType, data=new.train)
#유의하지 않다. 


# 4.26. MasVnrArea ####
#MasVnrArea 막대그래프 
ggplot() + geom_bar(data=new.train,aes(x=MasVnrArea)) 
#MasVnrArea SalePrice 관계
ggplot() + geom_point(data=new.train, aes(x=MasVnrArea,y=SalePrice))
#MasVnrArea와 SalePrice 상관계수 0.49 : 약한 상관관계
cor(eda2$MasVnrArea,eda2$SalePrice)
#종속변수에 거의 영향을 끼치지 않을 것이다. 


# 4.27. ExterQual ★★ ####
table(new.train$ExterQual, useNA = "ifany")
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=ExterQual, fill=ExterQual))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=ExterQual,fill=ExterQual),alpha=0.5)
#histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = ExterQual),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(ExterQual ~.)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=ExterQual, y=SalePrice, fill=ExterQual)) +
  geom_jitter(data=new.train, aes(x=ExterQual, y=SalePrice, fill=ExterQual))

#Exterior material Quality 가 Exellent인 경우 SalePrice의 범위 차이가 뚜렷하다.
#ExterQual가 종속변수에 유의미한 영향을 끼칠 것 같다. 

bartlett.test(SalePrice~ExterQual, data=new.train)
#등분산성을 만족한다.
oneway.test(SalePrice~ExterQual, data=new.train, var.equal = TRUE)
#유의한 변수이다. 
oneway(as.factor(new.train$ExterQual), y = new.train$SalePrice, posthoc = "tukey")
# 모든 범주의 평균 집 값 차이가 통계적으로 유의함을 알 수 있다.


# 4.28. ExterCond ####
table(new.train$ExterCond)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=ExterCond, fill=ExterCond))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=ExterCond,fill=ExterCond),alpha=0.5)
#histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = ExterCond),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(ExterCond ~.)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=ExterCond, y=SalePrice, fill=ExterCond)) +
  geom_jitter(data=new.train, aes(x=ExterCond, y=SalePrice, fill=ExterCond))

#대부분 condition of the exterior material이 average이다.
# 범주 간 평균 차이가 크게 나지 않는 것처럼 보여진다.
# 또한, TA 범주에만 데이터가 몰려 있고, 일부 변수에는 데이터가 없기 때문에 모델을 만드는 데 적합하지 않을 것이라고 판단했다.


# 4.29. Foundation ★★ ####
table(new.train$Foundation)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=Foundation, fill=Foundation))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=Foundation,fill=Foundation),alpha=0.5)
#histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = Foundation),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(Foundation ~.)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=Foundation, y=SalePrice, fill=Foundation)) +
  geom_jitter(data=new.train, aes(x=Foundation, y=SalePrice, fill=Foundation))

#값에 따라 SalePrice의 범위가 다르다.
#Type of foundation이 SalePrice에 영향을 끼칠 것 같으나 정확히 판단하기 어렵다.

bartlett.test(SalePrice~Foundation, data=new.train)
#등분산성을 만족하지 않는다
oneway.test(SalePrice~Foundation, data=new.train, var.equal = FALSE)
#유의한 변수이다. 

oneway(as.factor(new.train$Foundation), y = new.train$SalePrice, posthoc = "games-howell")
# BrkTil, Slab을 하나의 범주로 묶고,
# CBlock, Stone, Wood를 하나의 범주로 묶어준다.
new.train[which((new.train$Foundation == "BrkTil") | (new.train$Foundation == "Slab")), "Foundation"] <- "BrkTil or Slab"
new.train[which((new.train$Foundation == "CBlock") | (new.train$Foundation == "Stone") | (new.train$Foundation == "Wood")), "Foundation"] <- "CBlock or Stone or Wood"

# 테스트 데이터 셋에 적용
table(new.test$Foundation, useNA = "ifany")
new.test[which((new.test$Foundation == "BrkTil") | (new.test$Foundation == "Slab")), "Foundation"] <- "BrkTil or Slab"
new.test[which((new.test$Foundation == "CBlock") | (new.test$Foundation == "Stone") | (new.test$Foundation == "Wood")), "Foundation"] <- "CBlock or Stone or Wood"


# 4.30. BsmtQual ★★ ####
table(new.train$BsmtQual)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=BsmtQual, fill=BsmtQual))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=BsmtQual,fill=BsmtQual),alpha=0.5)
#histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = BsmtQual),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(BsmtQual ~.)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=BsmtQual, y=SalePrice, fill=BsmtQual)) +
  geom_jitter(data=new.train, aes(x=BsmtQual, y=SalePrice, fill=BsmtQual))

#값에 따라 SalePrice의 범위가 뚜렷하다.
#height이 좋을 수록 SalePrice가 더 높은 것을 볼 수 있다.
#종속변수에 유의한 영향을 끼칠 것 같다.
#height의 quality 판단이 객관적인 수치범위에 의한 것으로 범주형으로 된 변수를 수치형으로 나눠서 봐도 좋을 것 같다.
#평균 height로 하면 좋을 것 같다.

bartlett.test(SalePrice~BsmtQual, data=new.train)
#등분산성을 만족한다.
oneway.test(SalePrice~BsmtQual, data=new.train)
#유의한 변수이다.
oneway(as.factor(new.train$BsmtQual), y = new.train$SalePrice, posthoc = "tukey")
# 사후분석 결과 Fa 범주와 No Bsmt 범주의 평균 집 값의 차이가 유의하지 않은 것으로 나온다.
# 하지만 데이터 수가 적어 신뢰도가 떨어지고, 두 범주는 하나로 합칠 수가 없기 때문에 이 상태로 사용한다. 


# 4.31. BsmtCond ####
table(new.train$BsmtCond)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=BsmtCond, fill=BsmtCond))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=BsmtCond,fill=BsmtCond),alpha=0.5)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=BsmtCond, y=SalePrice, fill=BsmtCond)) +
  geom_jitter(data=new.train, aes(x=BsmtCond, y=SalePrice, fill=BsmtCond))

#대부분 basement condition 값이 typical이다.
# 지하실의 상태가 좋거나 좋지 않은 경우로 나눴을 때 의미있는 변수가 될 것 같은데
# 지하실의 상태가 좋지 않는 경우에 대한 데이터가 없어서 모델을 만드는 데 적합하지 않다고 판단했다.


# 4.32. BsmtExposure ★★ ####
table(new.train$BsmtExposure)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=BsmtExposure, fill=BsmtExposure))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=BsmtExposure,fill=BsmtExposure),alpha=0.5)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=BsmtExposure, y=SalePrice, fill=BsmtExposure)) +
  geom_jitter(data=new.train, aes(x=BsmtExposure, y=SalePrice, fill=BsmtExposure))

#값에 따른 SalePrice의 범위에 차이가 있는 것 같다.
#종속변수에 유의한 영향을 끼칠 것 같으나 정확히 판단하기 어렵다.

bartlett.test(SalePrice~BsmtExposure,data=new.train)
#등분산성을 만족하지 않는다.
oneway.test(SalePrice~BsmtExposure,data=new.train, var.equal = FALSE)
#유의한 변수이다. 

oneway(as.factor(new.train$BsmtExposure), y = new.train$SalePrice, posthoc = "games-howell")
# Av와 Gd 범주를 하나로 묶고, Mn과 No 범주를 하나로 묶는다.
new.train[which((new.train$BsmtExposure == "Av") | (new.train$BsmtExposure == "Gd")), "BsmtExposure"] <- "Av or Gd"
new.train[which((new.train$BsmtExposure == "Mn") | (new.train$BsmtExposure == "No")), "BsmtExposure"] <- "Mn or No"

# 테스트 데이터 셋에 적용
table(new.test$BsmtExposure, useNA = "ifany")
new.test[which((new.test$BsmtExposure == "Av") | (new.test$BsmtExposure == "Gd")), "BsmtExposure"] <- "Av or Gd"
new.test[which((new.test$BsmtExposure == "Mn") | (new.test$BsmtExposure == "No")), "BsmtExposure"] <- "Mn or No"


# 4.33. BsmtFinType1 ★★ ####
table(new.train$BsmtFinType1)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=BsmtFinType1, fill=BsmtFinType1))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=BsmtFinType1,fill=BsmtFinType1),alpha=0.5)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=BsmtFinType1, y=SalePrice, fill=BsmtFinType1)) +
  geom_jitter(data=new.train, aes(x=BsmtFinType1, y=SalePrice, fill=BsmtFinType1))

#값에 따른 SalePrice의 범위 차이가 적다.
#정확히 종속변수에 유의한 영향을 끼치는지 정확히 판단하기 어렵다.

bartlett.test(SalePrice~BsmtFinType1,data=new.train)
#등분산성을 만족하지 않는다.
oneway.test(SalePrice~BsmtFinType1,data=new.train, var.equal = FALSE)
#유의한 변수이다. 

oneway(as.factor(new.train$BsmtFinType1), y = new.train$SalePrice, posthoc = "games-howell")
# 아주 좋은 등급이거나 지하실이 없지 않는 이상 나머지 범주들에 대해서는 평균 집 값 차이가 유의미하지 않다. 
# 따라서 GLQ, Not GLQ, No Bsmt로 범주화 해준다.
new.train[which((new.train$BsmtFinType1 == "ALQ") | (new.train$BsmtFinType1 == "BLQ") | 
                  (new.train$BsmtFinType1 == "LwQ") | (new.train$BsmtFinType1 == "Rec") | (new.train$BsmtFinType1 == "Unf")), "BsmtFinType1"] <- "Not GLQ"

# 테스트 데이터 셋에 적용
table(new.test$BsmtFinType1, useNA = "ifany")
new.test[which((new.test$BsmtFinType1 == "ALQ") | (new.test$BsmtFinType1 == "BLQ") | 
                  (new.test$BsmtFinType1 == "LwQ") | (new.test$BsmtFinType1 == "Rec") | (new.test$BsmtFinType1 == "Unf")), "BsmtFinType1"] <- "Not GLQ"


# 4.34. BsmtFinSF1 ####
summary(new.train$BsmtFinSF1)

# 표준화
row.num <- which(new.train$BsmtFinSF1 > 0)
new.train[row.num, "BsmtFinSF1"] <- scale(new.train[row.num, "BsmtFinSF1"])

# 마지막으로 이상치를 제거해준다.
UCL <- quantile(x = new.train[row.num, "BsmtFinSF1"], probs = 0.99865)
LCL <- quantile(x = new.train[row.num, "BsmtFinSF1"], probs = 0.00135)

outlier <- which((new.train[row.num, "BsmtFinSF1"] >= UCL) | (new.train[row.num, "BsmtFinSF1"] <= LCL)) 
new.train <- new.train[-outlier, ]

# 테스트 데이터 셋에 적용
summary(new.test$BsmtFinSF1)
row.num <- which(new.test$BsmtFinSF1 > 0)
new.test[row.num, "BsmtFinSF1"] <- scale(new.test[row.num, "BsmtFinSF1"])
UCL <- quantile(x = new.test[row.num, "BsmtFinSF1"], probs = 0.99865)
LCL <- quantile(x = new.test[row.num, "BsmtFinSF1"], probs = 0.00135)

outlier <- which((new.test[row.num, "BsmtFinSF1"] >= UCL) | (new.test[row.num, "BsmtFinSF1"] <= LCL)) 
new.test <- new.test[-outlier, ]

#BsmtFinSF1 SalePrice 관계
ggplot() + geom_point(data=new.train, aes(x=SalePrice,y=BsmtFinSF1))
# 양의 상관관계가 있는 것처럼 보여진다.
# 따라서 면적이 0인 데이터를 제외하고 다시 확인해본다.
temp <- new.train[-which(new.train$BsmtFinSF1 == 0), c("BsmtFinSF1", "SalePrice")]
ggplot() + geom_point(data=temp, aes(x=SalePrice,y=BsmtFinSF1))

cor(temp$BsmtFinSF1,temp$SalePrice)
#상관계수가 그렇게 높지 않아서 큰 영향을 줄 것같지는 않다.


# 4.35. BsmtFinType2 ####
table(new.train$BsmtFinType2)
#bar graph
ggplot() + geom_bar(data=new.train, aes(x=BsmtFinType2, fill=BsmtFinType2))
#density
ggplot() + geom_density(data=new.train, aes(x=SalePrice, color=BsmtFinType2,fill=BsmtFinType2),alpha=0.5)
#boxplot
ggplot() + geom_boxplot(data=new.train, aes(x=BsmtFinType2, y=SalePrice, fill=BsmtFinType2)) +
  geom_jitter(data=new.train, aes(x=BsmtFinType2, y=SalePrice, fill=BsmtFinType2))
# 완성된 Type2 지하실을 가진 집이 거의 없기 때문에 모델을 만드는 데는 적합하지 않을 것이라고 판단하였다.


# 4.36. BsmtFinSF2 ####
summary(new.train$BsmtFinSF2)
#BsmtFinSF2 막대그래프 
ggplot() + geom_bar(data=new.train,aes(x=BsmtFinSF2)) 
#BsmtFinSF2 SalePrice 관계
ggplot() + geom_point(data=new.train, aes(x=BsmtFinSF2,y=SalePrice))
#BsmtFinSF2 SalePrice 상관계수 -0.02 : 상관관계가 없다. 
cor(new.train$BsmtFinSF2,new.train$SalePrice)

#상관계수가 -0.02로 매우 낮아 종속변수에 영향을 끼치지 않을 것 같다. 


# 4.37. BsmtUnfSF ####
summary(new.train$BsmtUnfSF)
#BsmtUnfSF 막대그래프 
ggplot() + geom_bar(data=new.train,aes(x=BsmtUnfSF)) 
#BsmtUnfSF SalePrice 관계
ggplot() + geom_point(data=new.train, aes(x=BsmtUnfSF,y=SalePrice))
#BsmtUnfSF SalePrice 상관계수 0.22 : 상관관계가 거의 없다. 
cor(new.train$BsmtUnfSF,new.train$SalePrice)
#종속변수에 유의한 영향을 끼치지 않을 것 같다. 


# 4.38. TotalBsmtSF ★★ ####
summary(new.train$TotalBsmtSF)
#TotalBsmtSF SalePrice 관계
ggplot() + geom_point(data=new.train, aes(x=SalePrice, y=TotalBsmtSF))
# 면적이 0인 데이터를 제외하고 다시 확인해본다.
temp <- new.train[-which(new.train$TotalBsmtSF == 0), c("TotalBsmtSF", "SalePrice")]
ggplot() + geom_point(data=temp, aes(x=SalePrice, y=TotalBsmtSF))
cor(temp$TotalBsmtSF, temp$SalePrice)
#TotalBsmtSF SalePrice 상관계수 0.62 : 상관관계가 높다. 
#Total square feet of basement area는 종속변수에 유의한 영향을 끼칠 것이다. 

# 표준화
row.num <- which(new.train$TotalBsmtSF > 0)
new.train[row.num, "TotalBsmtSF"] <- scale(new.train[row.num, "TotalBsmtSF"])

# 마지막으로 이상치를 제거해준다.
UCL <- quantile(x = new.train[row.num, "TotalBsmtSF"], probs = 0.99865)
LCL <- quantile(x = new.train[row.num, "TotalBsmtSF"], probs = 0.00135)

outlier <- which((new.train[row.num, "TotalBsmtSF"] >= UCL) | (new.train[row.num, "TotalBsmtSF"] <= LCL)) 
new.train <- new.train[-outlier, ]

# 테스트 데이터 셋에 적용
summary(new.test$TotalBsmtSF)
row.num <- which(new.test$TotalBsmtSF > 0)
new.test[row.num, "TotalBsmtSF"] <- scale(new.test[row.num, "TotalBsmtSF"])
UCL <- quantile(x = new.test[row.num, "TotalBsmtSF"], probs = 0.99865)
LCL <- quantile(x = new.test[row.num, "TotalBsmtSF"], probs = 0.00135)

outlier <- which((new.test[row.num, "TotalBsmtSF"] >= UCL) | (new.test[row.num, "TotalBsmtSF"] <= LCL)) 
new.test <- new.test[-outlier, ]


# 도희 EDA ####
# 4.39. Heating ####
table(new.train$Heating, useNA = "ifany")

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = Heating, fill = Heating))
# density
ggplot() + 
  geom_density(data = new.train, 
               aes(x = SalePrice, color = Heating, fill = Heating), 
               alpha=0.5)
# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = Heating),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(Heating ~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = Heating, y = SalePrice, fill = Heating)) +
  geom_jitter(data = new.train, 
              aes(x = Heating, y = SalePrice, fill = Heating))
# GasA를 제외한 나머지 범주의 경우 데이터 수가 매우 적기 때문에
# Heating 변수의 분산이 SalePrice 변수의 분산에 영향을 주는지를 통계적으로 분석하는 것이 의미가 없다.
# 따라서 SalePrice를 예측하는 데 중요한 변수가 아니라고 판단하였다.


# 4.40. HeatingQC ★★ ####
table(new.train$HeatingQC, useNA = "ifany")

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = HeatingQC, fill = HeatingQC))

# density
ggplot() + 
  geom_density(data = new.train,
               aes(x = SalePrice, color = HeatingQC, fill = HeatingQC),
               alpha=0.5)

# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = HeatingQC),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(HeatingQC ~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = HeatingQC, y = SalePrice, fill = HeatingQC)) +
  geom_jitter(data = new.train, 
              aes(x = HeatingQC, y = SalePrice, fill = HeatingQC))
# 난방 품질이 Excellent인 표본들이 다른 표본들에 비해 평균적으로 주택 가격이 높음. 

# 분산분석을 통해 난방 품질에 따른 집 값 변동이 유의미한지 확인한다.
new.train <- new.train[-which(new.train$HeatingQC == "Po"), ]
bartlett.test(SalePrice ~ HeatingQC, data = new.train)
# 등분산성 가정이 성립되지 않으므로 Welch`s ANOVA를 실시한다.
oneway.test(SalePrice ~ HeatingQC, data = new.train, 
            var.equal = FALSE)
# 검정 결과 난방 품질이 집 값 변동에 유의미한 영향을 주는 것을 알 수 있다.

# 사후분석을 통해 평균 집 값 차이가 유의미한 난방 품질을 확인한다.
oneway(as.factor(new.train$HeatingQC), y = new.train$SalePrice, posthoc = "games-howell")
# 난방 품질이 TA 이거나 Gd 인 범주 간에는 평균 집 값에 유의미한 차이가 없다.
# 따라서 두 범주를 합쳐 모든 범주 간 평균 집 값의 차이가 유의미하게 해준다.
new.train[which((new.train$HeatingQC == "Gd") | (new.train$HeatingQC == "TA")), "HeatingQC"] <- "Gd or TA"

# 테스트 데이터 셋에 적용
table(new.test$HeatingQC, useNA = "ifany")
new.test[which((new.test$HeatingQC == "Gd") | (new.test$HeatingQC == "TA")), "HeatingQC"] <- "Gd or TA"
new.test <- new.test[-which(new.test$HeatingQC == "Po"), ]


# 4.41. CentralAir ★★ ####
table(new.train$CentralAir)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = CentralAir, fill = CentralAir))

# density
ggplot() + 
  geom_density(data = new.train,
               aes(x = SalePrice, color = CentralAir, fill = CentralAir),
               alpha=0.5)

# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = CentralAir),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(CentralAir ~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, aes(x = CentralAir, y = SalePrice, fill = CentralAir)) +
  geom_jitter(data = new.train, aes(x = CentralAir, y = SalePrice, fill = CentralAir))

# 분산분석을 실시하기 위해 등분산성 검정을 한다.
bartlett.test(SalePrice ~ CentralAir, data = new.train)
# 검정 결과 p-value > 0.05 이므로 등분산성 가정이 성립한다고 할 수 있다.
oneway.test(SalePrice ~ CentralAir, data = new.train)
# 분산분석 결과 중앙에어컨 시스템 유무가 집 값의 변동에 유의미한 영향을 주는 것을 알 수 있다.


# 4.42. Electrical ####
table(new.train$Electrical)

# bar graph
ggplot() + 
  geom_bar(data = new.train, aes(x = Electrical, fill = Electrical))

# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = Electrical),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(Electrical ~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = Electrical, y = SalePrice, fill = Electrical)) +
  geom_jitter(data = new.train, 
              aes(x = Electrical, y = SalePrice, fill = Electrical))
# SBrkr을 제외한 나머지 범주에 속하는 데이터 수가 상대적으로 적기 때문에
# SalePrice 변수의 분산에 영향을 미치는지를 통계적으로 분석하는 것이 의미가 없을 것이라 생각하였고,
# 따라서 예측에 중요하지 않은 변수라고 판단하였다.


# 4.43. 1stFlrSF ★★ ####
summary(new.train$X1stFlrSF)

# plot
plot(new.train$SalePrice, new.train$X1stFlrSF, 
     xlab = "SalePrice", ylab = "1stFlrSF", main = "Relationship between saleprice and 1stFlrSF")

# 상관분석을 실시하여 1stFlrSF 변수가 SalePrice 변수와 어떤 상관관계를 갖는지 확인한다.
cor(new.train$X1stFlrSF, new.train$SalePrice)
# 상관계수가 0.6인 것으로 보아 1stFlrSF가 종속변수에 영향을 어느 정도 영향을 끼치는 것으로 보인다. 
# 따라서 SalePrice 변수를 예측하는 데 유의미한 변수라고 판단하였다.

# 표준화
new.train$X1stFlrSF <- scale(new.train$X1stFlrSF)

# 마지막으로 이상치를 제거해준다.
UCL <- quantile(x = new.train$X1stFlrSF, probs = 0.99865)
LCL <- quantile(x = new.train$X1stFlrSF, probs = 0.00135)

outlier <- which((new.train$X1stFlrSF >= UCL) | (new.train$X1stFlrSF <= LCL)) 
new.train <- new.train[-outlier, ]

# 테스트 데이터 셋에 적용
summary(new.test$X1stFlrSF)
new.test$X1stFlrSF <- scale(new.test$X1stFlrSF)
UCL <- quantile(x = new.test$X1stFlrSF, probs = 0.99865)
LCL <- quantile(x = new.test$X1stFlrSF, probs = 0.00135)

outlier <- which((new.test$X1stFlrSF >= UCL) | (new.test$X1stFlrSF <= LCL)) 
new.test <- new.test[-outlier, ]


# 4.44. 2ndFlrSF ★★ ####
summary(new.train$X2ndFlrSF)

# plot
plot(new.train$SalePrice, new.train$X2ndFlrSF, xlab = "SalePrice", ylab = "2ndFlrSF", main = "Relationship between saleprice and 2ndFlrSF")
# 2층이 있는 집의 경우 어느 정도의 양의 상관관계가 존재하는 것으로 보여진다.
# 따라서 2층이 있는 집의 데이터를 분리하여 상관분석을 실시한다.
temp <- new.train[which(new.train$X2ndFlrSF != 0), c("X2ndFlrSF", "SalePrice")]
cor(temp$X2ndFlrSF, temp$SalePrice)
# 상관계수가 0.69로 두 변수 간에는 의미있는 양의 상관관계가 있다고 볼 수 있다.

# 표준화
row.num <- which(new.train$X2ndFlrSF > 0)
new.train[row.num, "X2ndFlrSF"] <- scale(new.train[row.num, "X2ndFlrSF"])

# 마지막으로 이상치를 제거해준다.
UCL <- quantile(x = new.train[row.num, "X2ndFlrSF"], probs = 0.99865)
LCL <- quantile(x = new.train[row.num, "X2ndFlrSF"], probs = 0.00135)

outlier <- which((new.train[row.num, "X2ndFlrSF"] >= UCL) | (new.train[row.num, "X2ndFlrSF"] <= LCL)) 
new.train <- new.train[-outlier, ]

# 테스트 데이터 셋에 적용
summary(new.test$X2ndFlrSF)
row.num <- which(new.test$X2ndFlrSF > 0)
new.test[row.num, "X2ndFlrSF"] <- scale(new.test[row.num, "X2ndFlrSF"])
UCL <- quantile(x = new.test[row.num, "X2ndFlrSF"], probs = 0.99865)
LCL <- quantile(x = new.test[row.num, "X2ndFlrSF"], probs = 0.00135)

outlier <- which((new.test[row.num, "X2ndFlrSF"] >= UCL) | (new.test[row.num, "X2ndFlrSF"] <= LCL)) 
new.test <- new.test[-outlier, ]


# 4.45. LowQualFinSF ####
summary(new.train$LowQualFinSF)

# plot
plot(new.train$SalePrice, new.train$LowQualFinSF, xlab = "SalePrice", ylab = "LowQualFinSF", main = "Relationship between saleprice and LowQualFinSF")
# LowQualFinSF 값이 0이 아닌 데이터들을 보더라도 큰 상관관계가 없는 것으로 보인다.
# 중요하지 않은 변수라고 판단하였다.


# 4.46. GrLivArea ★★ ####
summary(new.train$GrLivArea)

# plot
plot(new.train$SalePrice, new.train$GrLivArea, xlab = "SalePrice", ylab = "GrLivArea", main = "Relationship between saleprice and GrLivArea")
# 두 변수 간에 상당한 양의 상관관계가 있는 것으로 보여진다.
# 따라서 상관분석을 실시한다.
cor(new.train$GrLivArea, new.train$SalePrice)
# 상관 계수가 0.76인 것으로 보아 GrLivArea가 종속변수에 상당한 영향을 끼친다.

# 표준화
new.train$GrLivArea <- scale(new.train$GrLivArea)

# 마지막으로 이상치를 제거해준다.
UCL <- quantile(x = new.train$GrLivArea, probs = 0.99865)
LCL <- quantile(x = new.train$GrLivArea, probs = 0.00135)

outlier <- which((new.train$GrLivArea >= UCL) | (new.train$GrLivArea <= LCL)) 
new.train <- new.train[-outlier, ]

# 테스트 데이터 셋에 적용
summary(new.test$GrLivArea)
new.test$GrLivArea <- scale(new.test$GrLivArea)
UCL <- quantile(x = new.test$GrLivArea, probs = 0.99865)
LCL <- quantile(x = new.test$GrLivArea, probs = 0.00135)

outlier <- which((new.test$GrLivArea >= UCL) | (new.test$GrLivArea <= LCL)) 
new.test <- new.test[-outlier, ]


# 4.47. BsmtFullBath ####
table(new.train$BsmtFullBath)
new.train$BsmtFullBath <- as.factor(new.train$BsmtFullBath)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = BsmtFullBath, fill = BsmtFullBath))

# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = BsmtFullBath),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(BsmtFullBath ~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = BsmtFullBath, y = SalePrice, fill = BsmtFullBath)) +
  geom_jitter(data = new.train, 
              aes(x = BsmtFullBath, y = SalePrice, fill = BsmtFullBath))

# 분산분석을 위해 등분산성 검정을 실시한다.
bartlett.test(SalePrice ~ BsmtFullBath, data = new.train[-which(new.train$BsmtFullBath == 2), ])
# 검정 결과 p-value > 0.05이므로 등분산성 가정이 성립한다고 할 수 있다,
# 따라서 분산분석을 실시한다.
oneway.test(SalePrice ~ BsmtFullBath, data = new.train)
# 유의한 변수가 아니다. 


# 4.48. BsmtHalfBath ####
table(new.train$BsmtHalfBath)
new.train$BsmtHalfBath <- as.factor(new.train$BsmtHalfBath)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = BsmtHalfBath, fill = BsmtHalfBath))
# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = BsmtHalfBath),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(BsmtHalfBath ~.)
# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = BsmtHalfBath, y = SalePrice, fill = BsmtHalfBath)) +
  geom_jitter(data = new.train, 
              aes(x = BsmtHalfBath, y = SalePrice, fill = BsmtHalfBath))
# 대부분의 집이 BsmtHalfBath 값이 0이고, 값이 1인 데이터가 매우 적어 이 변수가 유의한 영향을 미치는지 별도로 분석하지 않았다.
# 모델의 설명력을 높이는 데 중요한 변수는 아니라고 판단하였다.


# 4.49. FullBath ★★ ####
table(new.train$FullBath)
new.train$FullBath <- as.factor(new.train$FullBath)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = FullBath, fill = FullBath))

# histogram
ggplot() +
  geom_histogram(data = new.train, 
                 aes(x = SalePrice, fill = FullBath),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(FullBath ~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = FullBath, y = SalePrice, fill = FullBath)) +
  geom_jitter(data = new.train, 
              aes(x = FullBath, y = SalePrice, fill = FullBath))
# 욕조가 있는 욕실의 개수에 따라 평균 집 값의 차이가 있는 것으로 보여진다.
# 분산분석을 통해 그 차이가 유의한지 확인한다.
bartlett.test(SalePrice ~ FullBath, data = new.train)
# 등분산성 가정이 성립하지 않는다.
oneway.test(SalePrice ~ FullBath, data = new.train, var.equal = FALSE)
# 분산분석 결과 p-value < 0.05이므로 집 값의 분산에 유의미한 영향을 미치는 변수라고 할 수 있다.

# 사후 분석을 통해 집 값의 분산에 유의미한 영향을 주는 변수의 범주를 확인한다.
oneway(as.factor(new.train$FullBath), y = new.train$SalePrice, posthoc = "games-howell")
# 욕실 개수가 0일 때와 3일 때는 데이터 개수가 적어서 분석 결과를 신뢰할 수 없다.
# 따라서 변수의 범주를 0 ~ 1과 2 ~으로 나눠준다.
new.train$FullBath <- as.character(new.train$FullBath)
new.train[which((new.train$FullBath == "0") | (new.train$FullBath == "1")), "FullBath"] <- "0 or 1"
new.train[which((new.train$FullBath == "2") | (new.train$FullBath == "3")), "FullBath"] <- "2 ~"
new.train$FullBath <- as.factor(new.train$FullBath)

# 테스트 데이터 셋에 적용
table(new.test$FullBath)
new.test[which((new.test$FullBath == "0") | (new.test$FullBath == "1")), "FullBath"] <- "0 or 1"
new.test[which((new.test$FullBath == "2") | (new.test$FullBath == "3") | (new.test$FullBath == "4")), "FullBath"] <- "2 ~"
new.test$FullBath <- as.factor(new.test$FullBath)


# 4.50. HalfBath ★★ ####
table(new.train$HalfBath)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = HalfBath, fill = HalfBath))

# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = HalfBath),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(HalfBath ~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = as.factor(HalfBath), y = SalePrice, fill = HalfBath)) +
  geom_jitter(data = new.train, 
              aes(x = as.factor(HalfBath), y = SalePrice, fill = HalfBath))
# 욕조가 없는 욕실의 개수에 따라 평균 집 값의 차이가 있는 것으로 보여진다. 
# 분산분석을 통해 그 차이가 유의한지 확인한다.
bartlett.test(SalePrice ~ HalfBath, data = new.train)
# 등분산성 가정이 성립하지 않는다.
oneway.test(SalePrice ~ HalfBath, data = new.train, var.equal = FALSE)
# 분산분석 결과 p-value < 0.05이므로 집 값의 분산에 유의미한 영향을 미치는 변수라고 할 수 있다.

# 사후 분석을 통해 집 값의 분산에 유의미한 영향을 주는 변수의 범주를 확인한다.
oneway(as.factor(new.train$HalfBath), y = new.train$SalePrice, posthoc = "games-howell")
# 분석 결과 욕조가 없는 욕실의 개수가 0개일 때와 1개일 때의 평균 집 값의 차이만 유의함을 알 수 있다.
# 따라서 변수를 욕조가 없는 욕실 없음과 있음으로 나눠준다.
new.train[which((new.train$HalfBath == "0")), "HalfBath"] <- "No HalfBath"
new.train[which((new.train$HalfBath == "1") | (new.train$HalfBath == "2")), "HalfBath"] <- "Yes HalfBath"
new.train$HalfBath <- as.factor(new.train$HalfBath)

# 테스트 데이터 셋에 적용
new.test[which((new.test$HalfBath == "0")), "HalfBath"] <- "No HalfBath"
new.test[which((new.test$HalfBath == "1") | (new.test$HalfBath == "2")), "HalfBath"] <- "Yes HalfBath"
new.test$HalfBath <- as.factor(new.test$HalfBath)


# 4.51. BedroomAbvGr ★★ ####
table(new.train$BedroomAbvGr, useNA = "ifany")
new.train$BedroomAbvGr <- as.character(new.train$BedroomAbvGr)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = BedroomAbvGr, fill = BedroomAbvGr))

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = BedroomAbvGr, y = SalePrice, fill = BedroomAbvGr)) +
  geom_jitter(data = new.train, 
              aes(x = BedroomAbvGr, y = SalePrice, fill = BedroomAbvGr))
# 침실 개수에 따라 평균 집 값의 차이가 있는 것으로 보여진다. 
# 분산분석을 통해 그 차이가 유의한지 확인한다.
new.train[which((new.train$BedroomAbvGr == "5") | (new.train$BedroomAbvGr == "6") | (new.train$BedroomAbvGr == "7")  | (new.train$BedroomAbvGr == "8")), "BedroomAbvGr"] <- "5 ~"
bartlett.test(SalePrice ~ BedroomAbvGr, data = new.train)
# 등분산성 가정이 성립하지 않으므로 Welch`s ANOVA를 실시한다.
oneway.test(SalePrice ~ BedroomAbvGr, data = new.train, var.equal = FALSE)
# 분산분석 결과 p-value < 0.05이므로 집 값의 분산에 유의미한 영향을 미치는 변수라고 할 수 있다.

# 사후 분석을 통해 집 값의 분산에 유의미한 영향을 주는 변수의 범주를 확인한다.
oneway(as.factor(new.train$BedroomAbvGr), y = new.train$SalePrice, posthoc = "games-howell")
# 침실 개수를 0 ~ 2개, 3 ~개로 범주화 해준다.
new.train[which((new.train$BedroomAbvGr == "0") | (new.train$BedroomAbvGr == "1") | (new.train$BedroomAbvGr == "2")), "BedroomAbvGr"] <- "0 ~ 2"
new.train[which((new.train$BedroomAbvGr == "4") | (new.train$BedroomAbvGr == "5 ~")), "BedroomAbvGr"] <- "4 ~"
new.train[which((new.train$BedroomAbvGr == "3") | (new.train$BedroomAbvGr == "4 ~")), "BedroomAbvGr"] <- "3 ~"
new.train$HalfBath <- as.factor(new.train$HalfBath)

# 테스트 데이터 셋에 적용
table(new.test$BedroomAbvGr, useNA = "ifany")
new.test[which((new.test$BedroomAbvGr == "5") | (new.test$BedroomAbvGr == "6") | (new.test$BedroomAbvGr == "7")  | (new.test$BedroomAbvGr == "8")), "BedroomAbvGr"] <- "5 ~"
new.test[which((new.test$BedroomAbvGr == "0") | (new.test$BedroomAbvGr == "1") | (new.test$BedroomAbvGr == "2")), "BedroomAbvGr"] <- "0 ~ 2"
new.test[which((new.test$BedroomAbvGr == "4") | (new.test$BedroomAbvGr == "5 ~")), "BedroomAbvGr"] <- "4 ~"
new.test[which((new.test$BedroomAbvGr == "3") | (new.test$BedroomAbvGr == "4 ~")), "BedroomAbvGr"] <- "3 ~"
new.test$HalfBath <- as.factor(new.test$HalfBath)


# 4.52. KitchenAbvGr ★★ ####
table(new.train$KitchenAbvGr)
new.train$KitchenAbvGr <- as.character(new.train$KitchenAbvGr)
new.train <- new.train[-which(new.train$KitchenAbvGr == 0), ]
new.train[which((new.train$KitchenAbvGr == "2") | (new.train$KitchenAbvGr == "3")), "KitchenAbvGr"] <- "2 ~"

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = KitchenAbvGr, fill = KitchenAbvGr))
# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = KitchenAbvGr),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(KitchenAbvGr~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = KitchenAbvGr, y = SalePrice, fill = KitchenAbvGr)) +
  geom_jitter(data = new.train, 
              aes(x = KitchenAbvGr, y = SalePrice, fill = KitchenAbvGr))
# 부엌의 개수에 따라 평균 집 값의 차이가 있는 것으로 보여진다. 
# 분산분석을 통해 그 차이가 유의한지 확인한다.
bartlett.test(SalePrice ~ KitchenAbvGr, data = new.train)
# 등분산성 가정이 성립하지 않으므로 Welch`s ANOVA를 실시한다.
oneway.test(SalePrice ~ KitchenAbvGr, data = new.train, var.equal = FALSE)
# 분산분석 결과 p-value < 0.05이므로 집 값의 분산에 유의미한 영향을 미치는 변수라고 할 수 있다.

# 사후 분석을 통해 집 값의 분산에 유의미한 영향을 주는 변수의 범주를 확인한다.
oneway(as.factor(new.train$KitchenAbvGr), y = new.train$SalePrice, posthoc = "games-howell")
# 분석 결과 부엌의 개수가 1개일 때와 2개 이상일 때의 평균 집 값의 차이가 유의함을 알 수 있다.
new.train$HalfBath <- as.factor(new.train$HalfBath)

# 테스트 데이터 셋에 적용
table(new.test$KitchenAbvGr, useNA = "ifany")
new.test <- new.test[-which(new.test$KitchenAbvGr == 0), ]
new.test[which((new.test$KitchenAbvGr == "2") | (new.test$KitchenAbvGr == "3")), "KitchenAbvGr"] <- "2 ~"


# 4.53. KitchenQual ★★ ####
table(new.train$KitchenQual)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = KitchenQual, fill = KitchenQual))

# density
ggplot() + 
  geom_density(data = new.train,
               aes(x = SalePrice, color = KitchenQual, fill = KitchenQual),
               alpha=0.5)

# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = KitchenQual),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(KitchenQual ~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = KitchenQual, y = SalePrice, fill = KitchenQual)) +
  geom_jitter(data = new.train, 
              aes(x = KitchenQual, y = SalePrice, fill = KitchenQual))
# 부엌의 상태에 따라 평균 집 값의 차이가 있는 것으로 보여진다. 
# 분산분석을 통해 그 차이가 유의한지 확인한다.
bartlett.test(SalePrice ~ KitchenQual, data = new.train)
# 등분산성 가정한다.
oneway.test(SalePrice ~ KitchenQual, data = new.train, var.equal = TRUE)
# 분산분석 결과 p-value < 0.05이므로 집 값의 분산에 유의미한 영향을 미치는 변수라고 할 수 있다.

# 사후 분석을 통해 집 값의 분산에 유의미한 영향을 주는 변수의 범주를 확인한다.
oneway(as.factor(new.train$KitchenQual), y = new.train$SalePrice, posthoc = "tukey")
new.train$KitchenQual <- as.factor(new.train$KitchenQual)

# 테스트 데이터 셋에 적용
new.test$KitchenQual <- as.factor(new.test$KitchenQual)


# 4.54. TotRmsAbvGrd ★★  ####
table(new.train$TotRmsAbvGrd)
new.train$TotRmsAbvGrd <- as.character(new.train$TotRmsAbvGrd)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = TotRmsAbvGrd, fill = TotRmsAbvGrd))

# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = TotRmsAbvGrd),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(TotRmsAbvGrd~.)

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = TotRmsAbvGrd, y = SalePrice, fill = TotRmsAbvGrd)) +
  geom_jitter(data = new.train, 
              aes(x = TotRmsAbvGrd, y = SalePrice, fill = TotRmsAbvGrd))
# 방 개수에 따라 평균 집 값의 차이가 있는 것으로 보여진다. 
# 분산분석을 통해 그 차이가 유의한지 확인한다.
new.train[which((new.train$TotRmsAbvGrd == "12") | (new.train$TotRmsAbvGrd == "14")), "TotRmsAbvGrd"] <- "12 ~"
bartlett.test(SalePrice ~ TotRmsAbvGrd, data = new.train)
# 등분산성 가정이 성립하지 않으므로 Welch`s ANOVA를 실시한다.
oneway.test(SalePrice ~ TotRmsAbvGrd, data = new.train, var.equal = FALSE)
# 분산분석 결과 p-value < 0.05이므로 집 값의 분산에 유의미한 영향을 미치는 변수라고 할 수 있다.

# 사후 분석을 통해 집 값의 분산에 유의미한 영향을 주는 변수의 범주를 확인한다.
oneway(as.factor(new.train$TotRmsAbvGrd), y = new.train$SalePrice, posthoc = "games-howell")
# 방의 개수를 4이하, 5, 6, 7, 8이상으로 범주화한다.
new.train[which((new.train$TotRmsAbvGrd == "3") | (new.train$TotRmsAbvGrd == "4")), "TotRmsAbvGrd"] <- "~ 4"
# 방이 10개 이상인 경우를 하나의 범주로 합쳐준다.
new.train[which((new.train$TotRmsAbvGrd == "8") | (new.train$TotRmsAbvGrd == "9") | (new.train$TotRmsAbvGrd == "10") | (new.train$TotRmsAbvGrd == "11") | (new.train$TotRmsAbvGrd == "12 ~")), "TotRmsAbvGrd"] <- "8 ~"
new.train$TotRmsAbvGrd <- as.factor(new.train$TotRmsAbvGrd)

# 테스트 데이터 셋에 적용
table(new.test$TotRmsAbvGrd, useNA = "ifany")
new.test[which((new.test$TotRmsAbvGrd == "12") | (new.test$TotRmsAbvGrd == "14")), "TotRmsAbvGrd"] <- "12 ~"
new.test[which((new.test$TotRmsAbvGrd == "3") | (new.test$TotRmsAbvGrd == "4")), "TotRmsAbvGrd"] <- "~ 4"
new.test[which((new.test$TotRmsAbvGrd == "8") | (new.test$TotRmsAbvGrd == "9") | (new.test$TotRmsAbvGrd == "10") | (new.test$TotRmsAbvGrd == "11") | (new.test$TotRmsAbvGrd == "12 ~")), "TotRmsAbvGrd"] <- "8 ~"
new.test$TotRmsAbvGrd <- as.factor(new.test$TotRmsAbvGrd)


# 4.55. Functional ####
table(new.train$Functional)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = Functional, fill = Functional))

# histogram
ggplot() +
  geom_histogram(data = new.train, aes(x = SalePrice, fill = Functional),
                 binwidth = 30000, colour = "black", alpha = 0.7) +
  facet_grid(Functional~.)
# Typ 범주를 제외한 나머지 범주의 데이터가 매우 적어 이 변수는 회귀 모델을 설명하는 데 적합하지 않은 변수라고 판단하였다.


# 4.56. Fireplaces ★★ ####
table(new.train$Fireplaces)
new.train$Fireplaces <- as.character(new.train$Fireplaces)

# bar graph
ggplot() + 
  geom_bar(data = new.train, 
           aes(x = Fireplaces, fill = Fireplaces))

# boxplot
ggplot() + 
  geom_boxplot(data = new.train, 
               aes(x = Fireplaces, y = SalePrice, fill = Fireplaces)) +
  geom_jitter(data = new.train, 
              aes(x = Fireplaces, y = SalePrice, fill = Fireplaces))
# 벽난로 개수에 따라 평균 집 값의 차이가 있는 것으로 보여진다. 
# 분산분석을 통해 그 차이가 유의한지 확인한다.
new.train[which((new.train$Fireplaces == "2") | (new.train$Fireplaces == "3")), "Fireplaces"] <- "2 ~ 3"
bartlett.test(SalePrice ~ Fireplaces, data = new.train)
# 등분산성 가정이 성립하지 않으므로 Welch`s ANOVA를 실시한다.
oneway.test(SalePrice ~ Fireplaces, data = new.train, var.equal = FALSE)
# 분산분석 결과 p-value < 0.05이므로 집 값의 분산에 유의미한 영향을 미치는 변수라고 할 수 있다.

# 사후 분석을 통해 집 값의 분산에 유의미한 영향을 주는 변수의 범주를 확인한다.
oneway(as.factor(new.train$Fireplaces), y = new.train$SalePrice, posthoc = "games-howell")
# 벽난로의 유무에 따라서만 집 값의 평균에 유의미한 차가 있는 것을 알 수 있다.
# 따라서 범주를 0과 1이상으로 바꿔준다.
new.train[which((new.train$Fireplaces == "0")), "Fireplaces"] <- "No Fireplaces"
new.train[which((new.train$Fireplaces == "1") | (new.train$Fireplaces == "2 ~ 3")), "Fireplaces"] <- "Yes Fireplaces"
new.train$Fireplaces <- as.factor(new.train$Fireplaces)

# 테스트 데이터 셋에 적용
table(new.test$Fireplaces, useNA = "ifany")
new.test[which((new.test$Fireplaces == "2") | (new.test$Fireplaces == "3") | (new.test$Fireplaces == "4")), "Fireplaces"] <- "2 ~ 3"
new.test[which((new.test$Fireplaces == "0")), "Fireplaces"] <- "No Fireplaces"
new.test[which((new.test$Fireplaces == "1") | (new.test$Fireplaces == "2 ~ 3")), "Fireplaces"] <- "Yes Fireplaces"
new.test$Fireplaces <- as.factor(new.test$Fireplaces)





#### 정리 ####
write.csv(new.train, file = "NewTrain.csv", row.names = FALSE)
write.csv(new.test, file = "NewTest.csv", row.names = FALSE)

