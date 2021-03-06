#### 0  라이브러리 ####
library("corrplot")
library("dplyr")
library("Amelia")
library("rpart")
library("fBasics")
library("ggplot2")
library("stats")
library("userfriendlyscience")
library("car")




#### 1. 데이터 로드 ####

train <- read.csv(file = "NewTrain.csv", stringsAsFactors = FALSE, header = TRUE)
test  <- read.csv(file = "NewTest.csv",  stringsAsFactors = FALSE, header = TRUE)
test$SalePrice <- -1

house <- rbind(train, test)

# 결측치 확인
which(!complete.cases(house))





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





#### 3. 변수 선택 ####
house <- house[, c("MSSubClass"  , "MSZoning"    , "Alley"       , "LotShape"    , "LotArea",
                   "LandContour" , "LotConfig"   , "Neighborhood", "Condition1"  ,
                   "BldgType"    , "HouseStyle"  , "OverallQual" , "OverallCond" ,
                   "YearBuilt"   , "YearRemodAdd", "ExterQual"   , "Foundation"  ,
                   "BsmtQual"    , "BsmtExposure", "BsmtFinType1", "TotalBsmtSF" ,
                   "HeatingQC"   , "CentralAir"  , "X1stFlrSF"   , "X2ndFlrSF"   ,
                   "GrLivArea"   , "FullBath"    , "HalfBath"    , "BedroomAbvGr",
                   "KitchenAbvGr", "KitchenQual" , "TotRmsAbvGrd", "Fireplaces"  ,
                   "GarageYrBlt" , "GarageArea"  , "PavedDrive"  , "Fence"       , "SalePrice")]
str(house)





#### 4. 모델링 ####
# 4.1. model.1 ####
model.1 <- lm(SalePrice ~ 
                factor(MSSubClass)  + factor(MSZoning)     + factor(Alley)        + factor(LotShape)    +
                factor(LandContour) + factor(LotConfig)    + factor(Neighborhood) + factor(Condition1)  +
                factor(BldgType)    + factor(HouseStyle)   + OverallQual          + factor(OverallCond) +
                YearBuilt           + factor(YearRemodAdd) + factor(ExterQual)    + factor(Foundation)  +
                factor(BsmtQual)    + factor(BsmtExposure) + factor(BsmtFinType1) + TotalBsmtSF         +
                factor(HeatingQC)   + factor(CentralAir)   + X1stFlrSF            + X2ndFlrSF           +
                GrLivArea           + factor(FullBath)     + factor(HalfBath)     + factor(BedroomAbvGr)+
                factor(KitchenAbvGr)+ factor(KitchenQual)  + factor(TotRmsAbvGrd) + factor(Fireplaces)  +
                factor(GarageYrBlt) + factor(GarageArea)   + factor(PavedDrive)   + factor(Fence), 
              data = house[house$SalePrice != -1, ])
summary(model.1)



# 4.2. model.2 ####
# 첫 번째 모델에서 유의하지 않은 변수들을 제거한 뒤 모델을 만들어 본다.
model.2 <- lm(SalePrice ~ 
                factor(MSSubClass)  + factor(MSZoning)   + factor(LandContour) + factor(LotConfig)   +
                factor(Neighborhood)+ factor(Condition1) + OverallQual         + factor(OverallCond) +
                factor(YearRemodAdd)+ factor(Foundation) + factor(BsmtQual)    + TotalBsmtSF         +
                factor(CentralAir)  + X1stFlrSF           + X2ndFlrSF          +
                GrLivArea           + factor(FullBath)    + factor(HalfBath)   + factor(BedroomAbvGr)+
                factor(KitchenAbvGr)+ factor(KitchenQual) + factor(Fireplaces) +
                factor(GarageYrBlt) + factor(GarageArea), 
              data = house[house$SalePrice != -1, ])
summary(model.2)



# 4.3 model.3 ####
# 두 번째 모델에서 사용한 변수들을 가지고 새로운 파생변수를 만든다.
agg.MSSubClass   <- aggregate(SalePrice ~ MSSubClass  , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.MSZoning     <- aggregate(SalePrice ~ MSZoning    , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.LandContour  <- aggregate(SalePrice ~ LandContour , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.LotConfig    <- aggregate(SalePrice ~ LotConfig   , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.Neighborhood <- aggregate(SalePrice ~ Neighborhood, data = house[house$SalePrice != -1, ], FUN = "mean")
agg.Condition1   <- aggregate(SalePrice ~ Condition1  , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.OverallCond  <- aggregate(SalePrice ~ OverallCond , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.YearRemodAdd <- aggregate(SalePrice ~ YearRemodAdd, data = house[house$SalePrice != -1, ], FUN = "mean")
agg.Foundation   <- aggregate(SalePrice ~ Foundation  , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.BsmtQual     <- aggregate(SalePrice ~ BsmtQual    , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.CentralAir   <- aggregate(SalePrice ~ CentralAir  , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.FullBath     <- aggregate(SalePrice ~ FullBath    , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.HalfBath     <- aggregate(SalePrice ~ HalfBath    , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.BedroomAbvGr <- aggregate(SalePrice ~ BedroomAbvGr, data = house[house$SalePrice != -1, ], FUN = "mean")
agg.KitchenAbvGr <- aggregate(SalePrice ~ KitchenAbvGr, data = house[house$SalePrice != -1, ], FUN = "mean")
agg.KitchenQual  <- aggregate(SalePrice ~ KitchenQual , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.Fireplaces   <- aggregate(SalePrice ~ Fireplaces  , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.GarageYrBlt  <- aggregate(SalePrice ~ GarageYrBlt , data = house[house$SalePrice != -1, ], FUN = "mean")
agg.GarageArea   <- aggregate(SalePrice ~ GarageArea  , data = house[house$SalePrice != -1, ], FUN = "mean")

colnames(agg.MSSubClass)[2]   <- "MSSubClass.weight"
colnames(agg.MSZoning)[2]     <- "MSZoning.weight"
colnames(agg.LandContour)[2]  <- "LandContour.weight"
colnames(agg.LotConfig)[2]    <- "LotConfig.weight"
colnames(agg.Neighborhood)[2] <- "Neighborhood.weight"
colnames(agg.Condition1)[2]   <- "Condition1.weight"
colnames(agg.OverallCond)[2]  <- "OverallCond.weight"
colnames(agg.YearRemodAdd)[2] <- "YearRemodAdd.weight"
colnames(agg.Foundation)[2]   <- "Foundation.weight"
colnames(agg.BsmtQual)[2]     <- "BsmtQual.weight"
colnames(agg.CentralAir)[2]   <- "CentralAir.weight"
colnames(agg.FullBath)[2]     <- "FullBath.weight"
colnames(agg.HalfBath)[2]     <- "HalfBath.weight"
colnames(agg.BedroomAbvGr)[2] <- "BedroomAbvGr.weight"
colnames(agg.KitchenAbvGr)[2] <- "KitchenAbvGr.weight"
colnames(agg.KitchenQual)[2]  <- "KitchenQual.weight"
colnames(agg.Fireplaces)[2]   <- "Fireplaces.weight"
colnames(agg.GarageYrBlt)[2]  <- "GarageYrBlt.weight"
colnames(agg.GarageArea)[2]   <- "GarageArea.weight"

new.house <- left_join(house, agg.MSSubClass  , by = c("MSSubClass"   = "MSSubClass"))
new.house <- left_join(new.house, agg.MSZoning    , by = c("MSZoning"     = "MSZoning"))
new.house <- left_join(new.house, agg.LandContour , by = c("LandContour"  = "LandContour"))
new.house <- left_join(new.house, agg.LotConfig   , by = c("LotConfig"    = "LotConfig"))
new.house <- left_join(new.house, agg.Neighborhood, by = c("Neighborhood" = "Neighborhood"))
new.house <- left_join(new.house, agg.Condition1  , by = c("Condition1"   = "Condition1"))
new.house <- left_join(new.house, agg.OverallCond , by = c("OverallCond"  = "OverallCond"))
new.house <- left_join(new.house, agg.YearRemodAdd, by = c("YearRemodAdd" = "YearRemodAdd"))
new.house <- left_join(new.house, agg.Foundation  , by = c("Foundation"   = "Foundation"))
new.house <- left_join(new.house, agg.BsmtQual    , by = c("BsmtQual"     = "BsmtQual"))
new.house <- left_join(new.house, agg.CentralAir  , by = c("CentralAir"   = "CentralAir"))
new.house <- left_join(new.house, agg.FullBath    , by = c("FullBath"     = "FullBath"))
new.house <- left_join(new.house, agg.HalfBath    , by = c("HalfBath"     = "HalfBath"))
new.house <- left_join(new.house, agg.BedroomAbvGr, by = c("BedroomAbvGr" = "BedroomAbvGr"))
new.house <- left_join(new.house, agg.KitchenAbvGr, by = c("KitchenAbvGr" = "KitchenAbvGr"))
new.house <- left_join(new.house, agg.KitchenQual , by = c("KitchenQual"  = "KitchenQual"))
new.house <- left_join(new.house, agg.Fireplaces  , by = c("Fireplaces"   = "Fireplaces"))
new.house <- left_join(new.house, agg.GarageYrBlt , by = c("GarageYrBlt"  = "GarageYrBlt"))
new.house <- left_join(new.house, agg.GarageArea  , by = c("GarageArea"   = "GarageArea"))
new.house <- new.house[-which(!complete.cases(new.house)), ]

# 기존의 범주형 변수들을 제거하고 새로운 파생변수들을 넣어 모델을 만들어 본다.
model.3 <- lm(SalePrice ~ 
                MSSubClass.weight   + MSZoning.weight   + LandContour.weight + LotConfig.weight    +
                Neighborhood.weight + Condition1.weight + OverallQual        + OverallCond.weight  +
                YearRemodAdd.weight + Foundation.weight + BsmtQual.weight    + TotalBsmtSF         +
                CentralAir.weight   + X1stFlrSF         + X2ndFlrSF          +
                GrLivArea           + FullBath.weight   + HalfBath.weight    + BedroomAbvGr.weight +
                KitchenAbvGr.weight + KitchenQual.weight+ Fireplaces.weight  +
                GarageYrBlt.weight  + GarageArea.weight, 
              data = new.house[new.house$SalePrice != -1, ])
summary(model.3)

# 다중공선성 분석
model.var = c(11,20,23,24,25,37:56)
model.3.cor <- cor(new.house[, model.var])
corrplot(model.3.cor, shade.col = NA,
         tl.col = "black", tl.srt = 45)
vif(model.3)



# 4.4 model.4 ####
# vif 값이 가장 큰 X1stFlrSF 변수를 제거하고 모델을 만들어 본다.
model.4 <- lm(SalePrice ~ 
                MSSubClass.weight   + MSZoning.weight   + LandContour.weight + LotConfig.weight    +
                Neighborhood.weight + Condition1.weight + OverallQual        + OverallCond.weight  +
                YearRemodAdd.weight + Foundation.weight + BsmtQual.weight    + TotalBsmtSF         +
                CentralAir.weight   + X2ndFlrSF +
                GrLivArea           + FullBath.weight   + HalfBath.weight    + BedroomAbvGr.weight +
                KitchenAbvGr.weight + KitchenQual.weight+ Fireplaces.weight  +
                GarageYrBlt.weight  + GarageArea.weight, 
              data = new.house[new.house$SalePrice != -1, ])
summary(model.4)
vif(model.4)



# 4.5 model.5 ####
# 네 번째 모델에서 다중공선성의 문제는 없으므로 변수선택법(Stepwise)을 이용하여 모델을 만들어 본다.
null <- lm(SalePrice ~ 1, data = new.house[new.house$SalePrice != -1, ])
model.5 <- step(null, direction = "both", scope = list(upper = model.4))
summary(model.5)
vif(model.5)
# OverallCond.weight, Foundation.weight, GarageYrBlt.weight 변수가 제거된 것을 확인할 수 있다.


# 4.6 model.6 ####
# 새로운 파생변수들을 생성하여 다섯 번째 모델에 추가해 본다.
# 건폐율 BuildingCoverageRatio
new.house <- new.house %>% mutate(BuildingCoverageRatio = GrLivArea / LotArea * 100)
# 용적률 FloorAreaRatio
new.house <- new.house %>% mutate(FloorAreaRatio = (X1stFlrSF + X2ndFlrSF) / LotArea * 100)
# 두 변수를 표준화해준다. 
new.house$FloorAreaRatio <- scale(new.house$FloorAreaRatio)
new.house$BuildingCoverageRatio <- scale(new.house$BuildingCoverageRatio)

summary(new.house$FloorArea)
str(new.house)

# 건폐율과 용적률 변수를 제곱근 변환하여 모델에 추가하면 변수가 유의해지고 설명력이 올라간다.
# 하지만 유의하지 않은 변수들이 많이 생긴다.
model.6 <- lm(SalePrice ~ 
                MSSubClass.weight   + MSZoning.weight   + LandContour.weight + LotConfig.weight    +
                Neighborhood.weight + Condition1.weight + OverallQual        + OverallCond.weight  +
                BsmtQual.weight    + TotalBsmtSF         +
                CentralAir.weight   + X2ndFlrSF +
                GrLivArea           + FullBath.weight   + HalfBath.weight    + BedroomAbvGr.weight +
                KitchenAbvGr.weight + KitchenQual.weight+ Fireplaces.weight  +
                GarageArea.weight + BuildingCoverageRatio + FloorAreaRatio,
              data = new.house[new.house$SalePrice != -1, ])
summary(model.6)
vif(model.6)



# 4.7 model.7 ####
# 여섯 번째 모델에서 다중공선성의 문제는 없으므로 변수선택법(Stepwise)을 이용하여 모델을 만들어 본다.
null <- lm(SalePrice ~ 1, data = new.house[new.house$SalePrice != -1, ])
model.7 <- step(null, direction = "both", scope = list(upper = model.6))
summary(model.7)
vif(model.7)
