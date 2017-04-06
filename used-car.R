rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(e1071)
auto_new = read_table("autos.csv",header = T,sep = ",",row.names = F)
auto =  read_csv("autos.csv")
autosummary = read_csv("cnt_km_year_powerPS_minPrice_maxPrice_avgPrice_sdPrice.csv")
str(auto)
levels(auto$offerType)
#offerType: angebot供給者(賣方) Gesuch需求者(買方)
#abtest: cabrio敞蓬車 coupe雙座汽車 kleinwagen小客車 kombi廂型車 limousine加長型轎車 suv休旅車
#vehicleType: andere其他 cabrio敞蓬車 coupe雙座汽車 kleinwagen小客車 kombi廂型車 limousine加長型轎車 suv休旅車

# unique(auto$seller)
# unique(auto$abtest)
# unique(auto$vehicleType) #車種
# unique(auto$gearbox) #gearbox變速箱 automatik自排 manuell手排
# unique(auto$model) #廠牌旗下車系名稱
# unique(auto$fuelType) #燃料種類: benzin汽油 lpg液化石油氣 cpg壓縮天然氣 diesel柴油 hybrid油電混合 elektro電力發動
# unique(auto$brand) #汽車廠牌
# unique(auto$notRepairedDamage) #是否有沒修好的破損: ja有  nein沒有

auto$price = as.numeric(auto$price)
auto1 = filter(auto,seller=="privat",
                    offerType=="Angebot",
                     vehicleType %in% c("bus","cabrio","coupe","kleinwagen","kombi","limousine","suv"),
                     gearbox %in% c("automatik","manuell"),
                     fuelType %in% c("benzin","cng","diesel","elektro","hybrid","lpg"),
                     notRepairedDamage %in% c("ja","nein"))
auto1 = mutate(auto1,ad_exist_time=as.numeric(as.Date(auto1$lastSeen)-as.Date(auto1$dateCreated)))

trainset = auto1[c(1:150000),]
validationset = auto1[c(150001:200000),]

trainset$vehicleType = as.factor(trainset$vehicleType)
trainset$gearbox = as.factor(trainset$gearbox)
trainset$fuelType = as.factor(trainset$fuelType)
trainset$notRepairedDamage = as.factor(trainset$notRepairedDamage)
trainset$brand = as.factor(trainset$brand)
trainset$brand = relevel(trainset$brand,"sonstige_autos") #將sonstige_autos(其他品牌)定義為基準類 

stpowerPS = (trainset$powerPS-mean(trainset$powerPS))/sd(trainset$powerPS)
stprice = (trainset$price-mean(trainset$price))/sd(trainset$price)
stkilometer = (trainset$kilometer-mean(trainset$kilometer))/sd(trainset$kilometer)
stad_exist_time = (trainset$ad_exist_time-mean(trainset$ad_exist_time))/sd(trainset$ad_exist_time)

stpowerPS1 = (validationset$powerPS-mean(validationset$powerPS))/sd(validationset$powerPS)
stprice1 = (validationset$price-mean(validationset$price))/sd(validationset$price)
stkilometer1 = (validationset$kilometer-mean(validationset$kilometer))/sd(validationset$kilometer)
stad_exist_time1 = (validationset$ad_exist_time-mean(validationset$ad_exist_time))/sd(validationset$ad_exist_time)



trainsetmodel = data.frame(stprice,stpowerPS,stkilometer,stad_exist_time,
                           vehicleType=trainset$vehicleType,gearbox=trainset$gearbox,
                           fuelType=trainset$fuelType,notRepairedDamage=trainset$notRepairedDamage,
                           brand = trainset$brand)

validationsetmodel = data.frame(stprice1,stpowerPS1,stkilometer1,stad_exist_time1,
                                vehicleType=trainset$vehicleType,gearbox=trainset$gearbox,
                                fuelType=trainset$fuelType,notRepairedDamage=trainset$notRepairedDamage,
                                brand = trainset$brand)

trainsetY = trainset$price
linearregression = lm(stprice~.,data = trainsetmodel)
step(linearregression)
linearregression1 = lm()
summary(step(linearregression))


modelsvr = svm(stprice~.,data = trainsetmodel,type = "eps",cost=1.5)
pred_result = predict(modelsvr, validationsetmodel)
validationR2 = 1 - sum((validationsetmodel$stprice1-pred_result)^2)/sum((mean(trainsetmodel$stprice)-validationsetmodel$stprice1)^2)
