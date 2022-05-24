#WEATHER CRAWLING CODE 다른 파일에.
setwd("C:/김민주/EWHA/2020 VITAMIN/Jeju project")
#데이터 불러오기
train<-read.csv("data.csv", header = T, fileEncoding = "utf-8")
test<-read.csv("test.csv", header = T, fileEncoding = "utf-8")
test$X18.20_ride <- c(rep(NA,nrow(test))) #data로 합치기 위해서 target 변수인 X18.20_ride를 추가해서 NA로 채웠습니다
bts<-read.csv("bus_bts.csv", header = T, fileEncoding = "utf-8")
# weather_data <- read.csv('weather.csv')
head(train)
head(test)
head(bts)

#date(날짜)변수의 datatype을 Date로 바꿔주기 as.Date()함수 사용
#요일 변수 추가해주기 format() 함수 사용
train$date<-as.Date(train$date)
train$weekday<-format(train$date, format="%a")
test$date<-as.Date(test$date)
test$weekday<-format(test$date, format="%a")
str(train$date); str(train$weekday)
str(test$date); str(test$weekday)

#route_station 변수 생성: bus_route_id + station_code
#paste0() 함수로 chr type 변수 합치기(구분: ",")
train$bus_route_id<-as.character(train$bus_route_id)
train$station_code<-as.character(train$station_code)
train$route_station<-paste0(train$bus_route_id,seq=",",train$station_code)
test$bus_route_id<-as.character(test$bus_route_id)
test$station_code<-as.character(test$station_code)
test$route_station<-paste0(test$bus_route_id,seq=",",test$station_code)
str(train)
str(test)

#bus_route_id_weekday 변수 생성: bus_route_id + weekday
train$bus_route_id_weekday<-paste0(train$bus_route_id,sep=",",train$weekday)
test$bus_route_id_weekday<-paste0(test$bus_route_id,sep=",",test$weekday)
str(train)
str(test)

#station_code_weekday 변수 생성: station_code + weekday
train$station_code_weekday<-paste0(train$station_code,sep=",",train$weekday)
test$station_code_weekday<-paste0(test$station_code,sep=",",test$weekday)
str(train)
str(test)

#route_station_weekday 변수생성 : route_station + weekday
train$route_station_weekday<-paste0(train$route_station,sep=",",train$weekday)
test$route_station_weekday<-paste0(test$route_station,sep=",",test$weekday)
str(train)
str(test)

#on_time 변수 생성: time변수에서 '시'부분만 인덱싱이 필요
#install.packages("stringr") #문자열 변수 인덱싱을 위한 패키지
library(stringr)
bts$on_time<-str_extract(bts$geton_time, "[0-9]{2}") #숫자 중 처음 2자리 숫자를 추출
bts[c(1:5),c("geton_time","on_time")] #잘 됐는지 확인

#승하차 시간대 통합변수 (t~t+2)
#변수명으로 숫자가 먼저 올 수 없어서 68a 대신 a68 형식으로 만들었습니다.
train$a68<-apply(train[c("X6.7_ride","X7.8_ride")],1,sum)
train$a810<-apply(train[c("X8.9_ride","X9.10_ride")],1,sum)
train$a1012<-apply(train[c("X10.11_ride","X11.12_ride")],1,sum)
train$b68<-apply(train[c("X6.7_takeoff","X7.8_takeoff")],1,sum)
train$b810<-apply(train[c("X8.9_takeoff","X9.10_takeoff")],1,sum)
train$b1012<-apply(train[c("X10.11_takeoff","X11.12_takeoff")],1,sum)
str(train)
test$a68<-apply(test[c("X6.7_ride","X7.8_ride")],1,sum)
test$a810<-apply(test[c("X8.9_ride","X9.10_ride")],1,sum)
test$a1012<-apply(test[c("X10.11_ride","X11.12_ride")],1,sum)
test$b68<-apply(test[c("X6.7_takeoff","X7.8_takeoff")],1,sum)
test$b810<-apply(test[c("X8.9_takeoff","X9.10_takeoff")],1,sum)
test$b1012<-apply(test[c("X10.11_takeoff","X11.12_takeoff")],1,sum)
str(test)

train_backup <- train
train <- train_backup
test_backup <- test
test <- test_backup
bts_backup <- bts

# id_statistic----------------------------------

library(dplyr)

id_statistic <- function(id,col1,col2){
  id = enquo(id)
  #col_1=enquo(col1)
  #col_2=enquo(col2)
 
  # mean, sum
  rs_mean = train %>% group_by(!!id) %>% summarise(col1 = mean(X18.20_ride, na.rm = T))
  rs_sum = train %>% group_by(!!id) %>% summarise(col2 = sum(X18.20_ride, na.rm = T))
  rs_mean_sum = merge(rs_mean, rs_sum)
  names(rs_mean_sum)[2] = c(col1)
  names(rs_mean_sum)[3] = c(col2)
  
  # merge
  tr = left_join(train, rs_mean_sum)
  te = left_join(test, rs_mean_sum)
  
  COL1 <- te[[col1]]
  COL2 <- te[[col2]]
  COL1[is.na(COL1)] <- mean(COL1,na.rm = T)
  COL2[is.na(COL2)] <- mean(COL2,na.rm = T)
  
  te[[col1]] <- COL1
  te[[col2]] <- COL2
  
  # NA를 mean값으로 대체
  #te$col1[is.na(te$col1)] <- mean(rs_mean$col1,na.rm=T)
  #te$col2[is.na(te$col2)] <- mean(as.numeric(rs_mean$col2,na.rm=T))
  #te[[col1]] <- ifelse(is.na(te[[col1]]),mean(as.numeric(rs_mean[[col1]],na.rm=T)),te$col1)
  #te[[col2]] <- ifelse(is.na(te[[col2]]),mean(as.numeric(rs_sum[[col2]],na.rm=T)),te$col2)
 
  #te[col1][is.na(te[col1]),] <- mean(rs_mean[col1], na.rm = T)
  #te[[col2]][is.na(te[[col2]])] <- mean(as.numeric(rs_sum[[col2]]), na.rm = T)
  return(list(tr,te))
}

train = id_statistic(route_station, '1820_rs_mean', '1820_rs_sum')[[1]]
test = id_statistic(route_station, '1820_rs_mean','1820_rs_sum')[[2]]
        
train = id_statistic(bus_route_id, '1820_r_mean', '1820_r_sum')[[1]]
test = id_statistic(bus_route_id, '1820_r_mean', '1820_r_sum')[[2]]

train = id_statistic(station_code, '1820_s_mean', '1820_s_sum')[[1]]
test = id_statistic(station_code, '1820_s_mean', '1820_s_sum')[[2]]

train = id_statistic(weekday, '1820_w_mean', '1820_w_sum')[[1]]
test = id_statistic(weekday, '1820_w_mean', '1820_w_sum')[[2]]

#--TRYIT---------------------------------------------------------------------------------
TRYIT <- function(id,A,B){
  id = enquo(id)
#  A = enquo(A)
  rs_mean = train_backup %>% group_by(!!id) %>% summarise(A=mean(X18.20_ride, na.rm = T))
  colnames(rs_mean) <- c('id',A)
  rs_sum = train_backup %>% group_by(!!id) %>% summarise(B=sum(X18.20_ride, na.rm = T))
  colnames(rs_sum) <- c('id',B)
  result = inner_join(rs_mean,rs_sum)
#  colnames(result) = c(id,A,B)
  return(result)}



#NA처리 안된거..
TRYIT <- function(id,A,B){
  id2 = enquo(id)
  #  A = enquo(A)
  rs_mean = train_backup %>% group_by(!!id2) %>% summarise(a=mean(X18.20_ride, na.rm = T))
#  colnames(rs_mean) <- c(id,A)
  rs_sum = train_backup %>% group_by(!!id2) %>% summarise(b=sum(X18.20_ride, na.rm = T))
#  colnames(rs_sum) <- c(id,B)
  result = inner_join(rs_mean,rs_sum)
  names(result)[2] = c(A)
  names(result)[3] = c(B)
  
  #result2 = left_join(train_backup,result)
  result3 = left_join(test_backup,result)
  return(result3)}

k= TRYIT(route_station, 'X1820_rs_mean', 'X1820_rs_sum')


TRYIT <- function(id,A,B){
  id2 = enquo(id)
  #  A = enquo(A)
  rs_mean = train_backup %>% group_by(!!id2) %>% summarise(a=mean(X18.20_ride, na.rm = T))
  #  colnames(rs_mean) <- c(id,A)
  rs_sum = train_backup %>% group_by(!!id2) %>% summarise(b=sum(X18.20_ride, na.rm = T))
  #  colnames(rs_sum) <- c(id,B)
  result = inner_join(rs_mean,rs_sum)
  names(result)[2] = c(A)
  names(result)[3] = c(B)
  
  #result2 = left_join(train_backup,result)
  result3 = left_join(test_backup,result)
  #result3$A <- ifelse(is.na(result$A),1,result$A)
  
  #p <- mean(as.numeric(result3$a),na.rm = T)
  #result[a][is.na(result3[a]),] <- p #ifelse(is.na(result[A]),1,result3[A])
  #return(head(result3[A]))}
  return(result3)}

k= TRYIT(route_station, 'X1820_rs_mean', 'X1820_rs_sum')

# mean_statistics ------------------------------------------------------

mean_statistics <- function(){
  f = train %>% group_by(bus_route_id_weekday) %>% summarise(mean_bus_weekday_ride = mean(X18.20_ride,na.rm=T))
  tr = left_join(train, f, by = 'bus_route_id_weekday')
  te = left_join(test, f, by='bus_route_id_weekday') %>% mutate(mean_bus_weekday_ride=ifelse(is.na(mean_bus_weekday_ride),mean(mean_bus_weekday_ride,na.rm = T),mean_bus_weekday_ride))
  
  f = train %>% group_by(station_code_weekday) %>% summarise(mean_station_weekday_ride = mean(X18.20_ride,na.rm=T))
  tr = left_join(tr, f, by = 'station_code_weekday')
  te = left_join(te, f, by='station_code_weekday') %>% mutate(mean_station_weekday_ride=ifelse(is.na(mean_station_weekday_ride),mean(mean_station_weekday_ride,na.rm = T),mean_station_weekday_ride))
  
  f = train %>% group_by(route_station_weekday) %>% summarise(mean_route_station_weekday_ride = mean(X18.20_ride,na.rm=T))
  tr = left_join(tr, f, by = 'route_station_weekday')
  te = left_join(te, f, by='route_station_weekday') %>% mutate(mean_route_station_weekday_ride=ifelse(is.na(mean_route_station_weekday_ride),mean(mean_route_station_weekday_ride,na.rm = T),mean_route_station_weekday_ride))
  
  return(list(tr,te))  
}

train = mean_statistics()[[1]]
test = mean_statistics()[[2]]

#congestion 함수 만들기(bus_route_id를 기준으로 18~20의 혼잡도 계산)-----------------------------------
congestion <- function(){
  df = train %>% group_by(bus_route_id) %>% summarise(passenger=sum(X18.20_ride, na.rm = T))
  test_congestion<-function(x){
    if (x>10000){
      return(7)
    } else if(x>5000){
      return(6)
    } else if(x>2000){
      return(5)
    } else if(x>700){
      return(4)
    } else if(x>200){
      return(3)
    } else if(x>50){
      return(2)
    } else {
      return(1)
    }
  }
  df$congestion<-c()
  for (i in 1:nrow(df)){
    df$congestion[i]<-test_congestion(df$passenger[i])
  }
  df=df[,c("bus_route_id","congestion")]
  tr = left_join(train, df)
  te = left_join(test, df)
  te$congestion[is.na(te$congestion)]<-4
  return(list(tr,te))
}
train = congestion()[[1]]
test = congestion()[[2]]

#location=latitude+longitude-----------------------------------------------
train$latitude<-as.character(train$latitude)
train$longitude<-as.character(train$longitude)
train$location<-paste0(train$latitude,seq=",",train$longitude)
test$latitude<-as.character(test$latitude)
test$longitude<-as.character(test$longitude)
test$location<-paste0(test$latitude,seq=",",test$longitude)
str(train)
str(test)

#merge key-----------------------------------------------------------------
train$cue<-0
test$cue<-1
str(train)
str(test)

#오전 시간의 여러 데이터 활용한 변수(morning)-------------------------------------------
#Data set을 합쳐주기 이전에 test set에는 타겟 값인 X18.20_ride 변수가 없기 때문에, 데이터 불러오기 부분에서 X18.20_ride을 추가하고 NA로 채움.
data <- rbind(train,test)
View(data)

##a
data$a1012_sum <- ave(data$a1012, data$route_station, FUN = function(x) sum(x))
data$a1012_mean <- ave(data$a1012, data$route_station, FUN = function(x) mean(x))
##b
data$b1012_sum <- ave(data$b1012, data$route_station, FUN = function(x) sum(x))
data$b1012_mean <- ave(data$b1012, data$route_station, FUN = function(x) mean(x))
##c
data$`10.11_ride_sum` <- ave(data$X10.11_ride, data$route_station, FUN = function(x) sum(x))
data$`10.11_ride_mean` <- ave(data$X10.11_ride, data$route_station, FUN = function(x) mean(x))
##d
data$`10.11_takeoff_sum` <- ave(data$X10.11_takeoff, data$route_station, FUN = function(x) sum(x))
data$`10.11_takeoff_mean` <- ave(data$X10.11_takeoff, data$route_station, FUN = function(x) mean(x))
##e
data$`11.12_ride_sum` <- ave(data$X11.12_ride, data$route_station, FUN = function(x) sum(x))
data$`11.12_ride_mean` <- ave(data$X11.12_ride, data$route_station, FUN = function(x) mean(x))
##f
data$`11.12_takeoff_sum` <- ave(data$X11.12_takeoff, data$route_station, FUN = function(x) sum(x))
data$`11.12_takeoff_mean` <- ave(data$X11.12_takeoff, data$route_station, FUN = function(x) mean(x))
##g
data$`1820_r_mean_sum` <- ave(data$`1820_r_mean`, data$route_station, FUN = function(x) sum(x))
data$`1820_r_mean_mean` <- ave(data$`1820_r_mean`, data$route_station, FUN = function(x) mean(x))
##h
data$`1820_r_sum_sum` <- ave(data$`1820_r_sum`, data$route_station, FUN = function(x) sum(x))
data$`1820_r_sum_mean` <- ave(data$`1820_r_sum`, data$route_station, FUN = function(x) mean(x))
##i
data$`1820_rs_mean_sum` <- ave(data$`1820_rs_mean`, data$route_station, FUN = function(x) sum(x))
data$`1820_rs_mean_mean` <- ave(data$`1820_rs_mean`, data$route_station, FUN = function(x) mean(x))
##j
data$`1820_rs_sum_sum` <- ave(data$`1820_rs_sum`, data$route_station, FUN = function(x) sum(x))
data$`1820_rs_sum_mean` <- ave(data$`1820_rs_sum`, data$route_station, FUN = function(x) mean(x))
##k
data$`1820_s_mean_sum` <- ave(data$`1820_s_mean`, data$route_station, FUN = function(x) sum(x))
data$`1820_s_mean_mean` <- ave(data$`1820_s_mean`, data$route_station, FUN = function(x) mean(x))
##l
data$`1820_s_sum_sum` <- ave(data$`1820_s_sum`, data$route_station, FUN = function(x) sum(x))
data$`1820_s_sum_mean` <- ave(data$`1820_s_sum`, data$route_station, FUN = function(x) mean(x))
##m
data$`1820_w_mean_sum` <- ave(data$`1820_w_mean`, data$route_station, FUN = function(x) sum(x))
data$`1820_w_mean_mean` <- ave(data$`1820_w_mean`, data$route_station, FUN = function(x) mean(x))
##n
data$`1820_w_sum_sum` <- ave(data$`1820_w_sum`, data$route_station, FUN = function(x) sum(x))
data$`1820_w_sum_mean` <- ave(data$`1820_w_sum`, data$route_station, FUN = function(x) mean(x))
##o
data$a68_sum <- ave(data$a68, data$route_station, FUN = function(x) sum(x))
data$a68_mean <- ave(data$a68, data$route_station, FUN = function(x) mean(x))
##p
data$b68_sum <- ave(data$b68, data$route_station, FUN = function(x) sum(x))
data$b68_mean <- ave(data$b68, data$route_station, FUN = function(x) mean(x))
##q
data$`6.7_ride_sum` <- ave(data$X6.7_ride, data$route_station, FUN = function(x) sum(x))
data$`6.7_ride_mean` <- ave(data$X6.7_ride, data$route_station, FUN = function(x) mean(x))
##r
data$`6.7_takeoff_sum` <- ave(data$X6.7_takeoff, data$route_station, FUN = function(x) sum(x))
data$`6.7_takeoff_mean` <- ave(data$X6.7_takeoff, data$route_station, FUN = function(x) mean(x))
##s
data$`7.8_ride_sum` <- ave(data$X7.8_ride, data$route_station, FUN = function(x) sum(x))
data$`7.8_ride_mean` <- ave(data$X7.8_ride, data$route_station, FUN = function(x) mean(x))
##t
data$`7.8_takeoff_sum` <- ave(data$X7.8_takeoff, data$route_station, FUN = function(x) sum(x))
data$`7.8_takeoff_mean` <- ave(data$X7.8_takeoff, data$route_station, FUN = function(x) mean(x))
##u
data$`a810_sum` <- ave(data$a810, data$route_station, FUN = function(x) sum(x))
data$`a810_mean` <- ave(data$a810, data$route_station, FUN = function(x) mean(x))
##v
data$b810_sum <- ave(data$b810, data$route_station, FUN = function(x) sum(x))
data$b810_mean <- ave(data$b810, data$route_station, FUN = function(x) mean(x))
##w
data$`8.9_ride_sum` <- ave(data$X8.9_ride, data$route_station, FUN = function(x) sum(x))
data$`8.9_ride_mean` <- ave(data$X8.9_ride, data$route_station, FUN = function(x) mean(x))
##x
data$`8.9_takeoff_sum` <- ave(data$X8.9_takeoff, data$route_station, FUN = function(x) sum(x))
data$`8.9_takeoff_mean` <- ave(data$X8.9_takeoff, data$route_station, FUN = function(x) mean(x))
##y
data$`9.10_ride_sum` <- ave(data$X9.10_ride, data$route_station, FUN = function(x) sum(x))
data$`9.10_ride_mean` <- ave(data$X9.10_ride, data$route_station, FUN = function(x) mean(x))
##z
data$`9.10_takeoff_sum` <- ave(data$X9.10_takeoff, data$route_station, FUN = function(x) sum(x))
data$`9.10_takeoff_mean` <- ave(data$X9.10_takeoff, data$route_station, FUN = function(x) mean(x))
str(data)

#Label encoding feature--------------------------------------------------------------------------------------------------
#LabelEncoder() 함수: 범주형 변수가 문자열 data type을 가지고 있을 경우, 클래스 레이블을 정수형으로 인코딩해준다고 합니다.
#이후 회귀트리 모델에 어떤 data type이 적용되는지 몰라서 일단 factor화 해주었습니다.
data$bus_route_id2 <- as.factor(data$bus_route_id)
data$station_code2 <- as.factor(data$station_code)
data$route_station_weekday2 <- as.factor(data$route_station_weekday)
data$route_station2 <- as.factor(data$route_station)
str(data[c('bus_route_id2','station_code2','route_station_weekday2','route_station2')])

# Weather-----------------------------------------------------------------------------------------------------------------
library(dplyr)
weather <- function(){
  weather_data <- read.csv('weather.csv')
  
  #weather_data에 date달아주기
  weather_data$id <- c(1:46)
  a <- data.frame(date = unique(data$date[!is.na(data$date)]),id=c(1:46))
  weather_data = inner_join(weather_data,a) %>% select(-id)
  
  #일강수_10이 factor이기 때문에 바로 <U+00A0>을 숫자 0으로 바꾸면 나머지 값들도 전부 값이 바뀌어버림. 따라서 factor->character형식으로 바꾼다. 
  weather_data$일강수_10 <- as.character(weather_data$일강수_10) 
  #이제 <U+00A0>을 0으로 처리한다. 
  weather_data$일강수_10 <- ifelse(weather_data$일강수_10=='<U+00A0>',0,weather_data$일강수_10)
  #일강수_10 column을 numeric으로 전환하면 완료.
  weather_data$일강수_10 <- as.numeric(weather_data$일강수_10)
  
  #정리된 weather_data를 data에 join
  df = inner_join(data,weather_data,by='date')
  
  #현재일기_10 labelencoding후 scaling
  df_encode <- df$현재일기_10
  df_encoded <- as.vector(scale(as.numeric(df_encode)))
  df$현재일기_10 <- df_encoded
  
  #현재일기, 체감온도, 일강수 모두 numeric변수여서 astype(float)부분 생략하였습니다. 
  is.numeric(df$현재일기_10) ;is.numeric(df$체감온도_10); is.numeric(df$일강수_10)
  
  return(df)
  }

data = weather()
data_backup <- data

# weekday(data)-----------------------------
library(psych)
# weekday : 일월화수목금토 순으로 0~6으로 변환
#data$date <- as.Date(data$date)
data$weekday <- factor(data$weekday,levels=c('월','화','수','목','금','토','일')) #이 순서로 레벨 재조정
head(data$weekday) #레벨정렬 확인
data$weekday <- as.factor(as.numeric(data$weekday)-1) # weekday를 숫자화 (0-월, 1-화, 2-수, 3-목, 4-금, 5-토, 6-일) 후 factor처리
head(data$weekday) #레벨정렬 확인

# weekday_0 ... weekday_6 column생성 후 dummy variable생성후 data와 cbind
weekday_dummy <- dummy.code(data$weekday)
head(weekday_dummy)
colnames(weekday_dummy) <-c('weekday_1','weekday_0','weekday_2','weekday_4','weekday_3','weekday_6','weekday_5')
head(weekday_dummy)
data <- cbind(data,weekday_dummy)

#in-out ----------------------------------------
data$in_out <- ifelse(data$in_out=='시내',0,1)
#data_backup <- data

a <- data %>% group_by(weekday) %>% summarise(mean(X18.20_ride,na.rm = T))
b <- aggregate(data=data, X18.20_ride~weekday, mean, na.rm=T)

#출근 시간의 총 승객 수------------------------------------------------
data$ride_sum<-data$X6.7_ride+data$X7.8_ride+data$X8.9_ride+data$X9.10_ride+data$X10.11_ride+data$X11.12_ride
data$takeoff_sum<-data$X6.7_takeoff+data$X7.8_takeoff+data$X8.9_takeoff+data$X9.10_takeoff+data$X10.11_takeoff+data$X11.12_takeoff
str(data)
#날짜 및 시간대 별 총 승객 수--------------------------------------------
#1등 코드에는 6~7_all_ride_number, 요런식으로 되어 있는데 그렇게 하면 오류가 떠서 all_ride_number_6.7, 요런식으로 바꿨습니다(10~11시 탑승객수 합까지!)
f<-data %>% group_by(date) %>% summarise(all_ride_number_6.7=sum(X6.7_ride,na.rm=T))
data<-left_join(data, f)
f<-data %>% group_by(date) %>% summarise(all_ride_number_7.8=sum(X7.8_ride,na.rm=T))
data<-left_join(data, f)
f<-data %>% group_by(date) %>% summarise(all_ride_number_8.9=sum(X8.9_ride,na.rm=T))
data<-left_join(data, f)
f<-data %>% group_by(date) %>% summarise(all_ride_number_9.10=sum(X9.10_ride,na.rm=T))
data<-left_join(data, f)
f<-data %>% group_by(date) %>% summarise(all_ride_number_10.11=sum(X10.11_ride,na.rm=T))
data<-left_join(data, f)
str(data)
#주말, 주중--------------------------------------------
table(is.na(data$weekday))
h<-function(x){
  if(x==5|x==6){
    return(1)
  } else {
    return(0)
  }
}
data$weekend<-c()
for (i in 1:nrow(data)){
  data$weekend[i]<-h(data$weekday[i])
} #이 코드 돌리는데 조금 오래 걸립니당,,!
str(data)
data_backup <- data

#연휴--------------------------------------------
g<-function(x){
  if(x=="2019-09-12"|x=='2019-09-13'|x=='2019-09-14'|x=='2019-10-03'|x=='2019-10-09'){
    return(1)
  } else {
    return(0)
  }
}
data$holiday<-c()
for (i in 1:nrow(data)){
  data$holiday[i]<-g(data$date[i])
} #이 코드도 돌리는데 조금 오래 걸립니당,,!
str(data)

#요일 별 평균 승객 수--------------------------------------------
week_mean<-function(){
  df<-data
  mean_weekday<-aggregate(data=data, X18.20_ride~weekday, mean, na.rm=T)
  df$weekdaymean<-1
  index0<-which(data$weekday==0)
  index1<-which(data$weekday==1)
  index2<-which(data$weekday==2)
  index3<-which(data$weekday==3)
  index4<-which(data$weekday==4)
  index5<-which(data$weekday==5)
  index6<-which(data$weekday==6)
  df$weekdaymean[index0]<-mean_weekday[1,2]
  df$weekdaymean[index1]<-mean_weekday[2,2]
  df$weekdaymean[index2]<-mean_weekday[3,2]
  df$weekdaymean[index3]<-mean_weekday[4,2]
  df$weekdaymean[index4]<-mean_weekday[5,2]
  df$weekdaymean[index5]<-mean_weekday[6,2]
  df$weekdaymean[index6]<-mean_weekday[7,2]
  return(df)
}
data<-week_mean()
str(data)

#시내 및 시외버스 별 평균 탑승 금액------------------------------------
inindex<-which(data$in_out==0)
outindex<-which(data$in_out==1)
mean_in_out<-aggregate(data=data, X18.20_ride~in_out, mean, na.rm=T)
data$in_out_mean[inindex]<-mean_in_out[1,2]
data$in_out_mean[outindex]<-mean_in_out[2,2]


#----------------------------------------------배차간격----------------------------------
train$bus_route_id = as.numeric(train$bus_route_id) # bus_route_id에 대해 cha -> num 변환
test$bus_route_id = as.numeric(test$bus_route_id)
library(lubridate)
bts$geton_time2 <- bts$geton_time #bts$geton_time2 열 만들기
bts$user_count <- as.numeric(as.character(bts$user_count)) #user_count를 numeric으로 변환

#반복문이 너무 오래걸려서 일단은 위의 내용을 python으로 돌려 파일로 가져옴
library(data.table)
f<-fread("f.csv", data.table = F)
# "HH:MM:SS"를 초단위로 바꿔주는 함수
get_sec <- function(time_str){
  func_time <- strptime(time_str, "%H:%M:%S")
  hour <- func_time$hour
  minute <- func_time$min
  seconds <- func_time$sec
  return(hour*3600 + minute*60 + seconds)  
}
#f를 파일로 가져왔을때 인덱스에 순서가 밀려서 다시 초기화 (크게 중요한 부분은 아님)
f$index <- 1:nrow(f)
#----------------------배차간격을 계산해주는 함수----------------------
#-------interval에 f를 복사해주고 time변수를 초단위로 바꾸어 time4에 저장해준다.
#-------time4는 초단위인데 분단위로 바꿔주기 위해 60을 나눠준다.
#-------그중 3분 이상 180분 이하의 데이터만 다시 처리하여 저장해준다.
#-------bus_route_id를 그룹화하여 해당 그룹마다 평균값을 구해주고 반올림해준다.
#-------처리가 끝나면 csv파일로 저장해준다.
#library(dplyr)
#bus_interval <- function(){
#  interval <- f
#  time4 = vector('numeric', length = nrow(interval))
#  for(i in 1:nrow(interval)){
#    time4[i] <- get_sec(interval$time[i])
#  }
#  interval$time4 <- time4/60
#  interval <- subset(interval, time4 > 3 & time4 < 180)
#  interval = interval %>% group_by(bus_route_id) %>% summarise(bus_interval = ceiling(mean(time4)))
#  interval$bus_interval <- as.numeric(as.character(interval$bus_interval))
#  write.csv(interval, file = 'C:/김민주/EWHA/2020 VITAMIN/Jeju project/bus_interval_final.csv', row.names = FALSE)
#  print("success..!")
#}
#bus_itvl = bus_interval()

library(data.table)
bus_interval_var<-fread("C:/김민주/EWHA/2020 VITAMIN/Jeju project/bus_interval_final.csv", data.table = F)
bus_interval_var$bus_route_id <- as.character(bus_interval_var$bus_route_id)
View(bus_interval_var)
#--------- data와 bus_interval_var 합치기
data <- left_join(data, bus_interval_var, by="bus_route_id")
str(data)
data$bus_interval <- replace(data$bus_interval, is.na(data$bus_interval)==T,9999)
sum(is.na(data$bus_interval))

#여기까지 끝낸거 DATA file로 output함. 
data_backup <- data
write.csv(data, file = 'C:/김민주/EWHA/2020 VITAMIN/Jeju project/DATA.csv',row.names = FALSE) #안됨. 


#카테고리별 승객 수-----------------------------------------
library(tidyr)
category_people <- function(){
  bts$bus_route_id <- as.character(bts$bus_route_id)
  
  f = bts %>% group_by(bus_route_id,user_category) %>% summarise(`승객수`=sum(user_count))
  g = f %>% ungroup() %>% spread(key=user_category,value=`승객수`)
  g[is.na(g)] <- 0
  names(g) <- c('bus_route_id', 'adult','kids','teen','elder','jang','jang2','ugong','ugong2')
  g <- select(g, c(bus_route_id,adult,kids,teen,elder))
  
  #merge
  df = left_join(data,g,by="bus_route_id")
  df$adult[is.na(df$adult)] <- 2363.078
  df$kids[is.na(df$kids)] <- 60.42698
  df$teen[is.na(df$teen)] <- 448.2778
  df$elder[is.na(df$elder)] <- 751.3095
  
  return(df)
}

data = category_people()

#Category별 승객의 비율-------------------------------------
category_people_ratio <- function(){
  bts$bus_route_id <- as.character(bts$bus_route_id)
  a = bts %>% group_by(bus_route_id) %>% summarise(`전체`=sum(user_count))
  b = bts %>% group_by(bus_route_id,user_category) %>% summarise(`승객수`=sum(user_count))
  c = inner_join(a,b,by="bus_route_id")
  c$`비율` = c$`승객수`/c$`전체`
  c = c %>% select(-c(`전체`,`승객수`)) %>% spread(key=user_category,value=`비율`)
  c[is.na(c)] <- 0
  names(c) <- c('bus_route_id', 'adult_prop','kids_prop','teen_prop','elder_prop','jang_prop','jang2_prop','ugong_prop','ugong2_prop')
  f <- select(c,c(bus_route_id, adult_prop,kids_prop,teen_prop,elder_prop))
  
  df = left_join(data,f,by="bus_route_id")
  
  df$adult_prop[is.na(df$adult_prop)] <- 0.552
  df$kids_prop[is.na(df$kids_prop)] <- 0.0244 
  df$teen_prop[is.na(df$teen_prop)] <- 0.139
  df$elder_prop[is.na(df$elder_prop)] <- 0.261
 
  return(df)
  
}
# memory.limit(1000000)
data=category_people_ratio()

data_backup_tillcategory <- data


#--------------data에 비교문과 반목문 통해 주소 합치기--------------
#-----data를 기준으로 data_test set을 돌리기. -> left join으로 합칠것 기준은 location
data_address <- read.csv("data_address.csv")
life_address <- read.csv("life_address.csv")
#-----------------data_address의 동, 시 전처리-----------------
data_address$latitude<-as.character(data_address$latitude)
data_address$longitude<-as.character(data_address$longitude)
data_address$location<-paste0(data_address$latitude,seq=",",data_address$longitude)
for(i in 1:nrow(data_address)){
  data_address$si[i] <- strsplit(as.character(data_address$address_name[i]), split = ' ')[[1]][2]
  data_address$dong[i] <- strsplit(as.character(data_address$address_name[i]), split = ' ')[[1]][3]
}#지오코딩 프로그램을 통해 얻어온 주소로 동과 시를 추출하여 전처리
#-----------------life_address의 동, 시 전처리-----------------
View(life_address)
life_address$x_axis<-as.character(life_address$x_axis)
life_address$y_axis<-as.character(life_address$y_axis)
life_address$location<-paste0(life_address$y_axis,seq=",",life_address$x_axis) #위도+경도
for(i in 1:nrow(life_address)){
  life_address$si[i] <- strsplit(as.character(life_address$address_name[i]), split = ' ')[[1]][2]
  life_address$dong[i] <- strsplit(as.character(life_address$address_name[i]), split = ' ')[[1]][3]
}#지오코딩 프로그램을 통해 얻어온 주소로 동과 시를 추출하여 전처리
#-----------------함수내용-----------------
library(dplyr)
jeju_love <- function(){
  loc_data = data_address[,c('location', 'dong', 'si')]  #data_address set에서 location, dong, si만 추출
  loc_life = life_address[,c('location', 'dong', 'si')]  #life_address set에서 location, dong, si만 추출
  df <- left_join(data, loc_data, by="location" )
  jeju_life <- read.csv("jeju_financial_life_data.csv")
  jeju_life$x_axis<-as.character(substr(jeju_life$x_axis,1,11))  #jeju_financial_life_data.csv에서 위경도의 1~11번째 글자만 가져옴
  jeju_life$y_axis<-as.character(substr(jeju_life$y_axis,1,11))
  jeju_life$location<-paste0(jeju_life$y_axis,seq=",",jeju_life$x_axis) #위도+경도
  jeju_life2 <- left_join(jeju_life, loc_life, by="location") #결측치 매우 많음. 약 80%
  dong_mean = jeju_life2 %>% group_by(dong) %>%    #각 동의 평균
    filter(!is.na(dong)) %>% 
    summarise(mean_job_majorc = mean(job_majorc,na.rm=T), 
              mean_job_smallc = mean(job_smallc,na.rm=T), 
              mean_job_public = mean(job_public,na.rm=T), 
              mean_job_profession = mean(job_profession,na.rm=T), 
              mean_job_self = mean(job_self,na.rm=T),
              mean_vehicle_own_rat = mean(vehicle_own_rat,na.rm=T), 
              mean_avg_income = mean(avg_income,na.rm=T), 
              mean_med_income = mean(med_income,na.rm=T), 
              mean_avg_spend = mean(avg_spend,na.rm=T))
  dong_sum = jeju_life2 %>% group_by(dong) %>%     #각 동마다의 합
    filter(!is.na(dong)) %>% 
    summarise(sum_job_majorc = sum(job_majorc,na.rm=T), 
              sum_job_smallc = sum(job_smallc,na.rm=T), 
              sum_job_public = sum(job_public,na.rm=T), 
              sum_job_profession = sum(job_profession,na.rm=T), 
              sum_job_self = sum(job_self,na.rm=T),
              sum_vehicle_own_rat = sum(vehicle_own_rat,na.rm=T), 
              sum_avg_income = sum(avg_income,na.rm=T), 
              sum_med_income = sum(med_income,na.rm=T), 
              sum_avg_spend = sum(avg_spend,na.rm=T))
  dong_rate = jeju_life2 %>% group_by(dong) %>%     #각 동 기준으로 열내에서 비율
    filter(!is.na(dong)) %>% 
    summarise(rate_job_majorc = sum(job_majorc,na.rm=T), 
              rate_job_smallc = sum(job_smallc,na.rm=T), 
              rate_job_public = sum(job_public,na.rm=T), 
              rate_job_profession = sum(job_profession,na.rm=T), 
              rate_job_self = sum(job_self,na.rm=T),
              rate_vehicle_own_rat = sum(vehicle_own_rat,na.rm=T), 
              rate_avg_income = sum(avg_income,na.rm=T), 
              rate_med_income = sum(med_income,na.rm=T), 
              rate_avg_spend = sum(avg_spend,na.rm=T))
  for(i in 1:nrow(dong_rate)){   #비율을 구하기 위한 내용
    dong_rate$rate_job_majorc[i] <- dong_sum$sum_job_majorc[i]/sum(dong_sum$sum_job_majorc)
    dong_rate$rate_job_smallc[i] <- dong_sum$sum_job_smallc[i]/sum(dong_sum$sum_job_smallc)
    dong_rate$rate_job_public[i] <- dong_sum$sum_job_public[i]/sum(dong_sum$sum_job_public)
    dong_rate$rate_job_profession[i] <- dong_sum$sum_job_profession[i]/sum(dong_sum$sum_job_profession)
    dong_rate$rate_job_self[i] <- dong_sum$sum_job_self[i]/sum(dong_sum$sum_job_self)
    dong_rate$rate_vehicle_own_rat[i] <- dong_sum$sum_vehicle_own_rat[i]/sum(dong_sum$sum_vehicle_own_rat)
    dong_rate$rate_avg_income[i] <- dong_sum$sum_avg_income[i]/sum(dong_sum$sum_avg_income)
    dong_rate$rate_med_income[i] <- dong_sum$sum_med_income[i]/sum(dong_sum$sum_med_income)
    dong_rate$rate_avg_spend[i] <- dong_sum$sum_avg_spend[i]/sum(dong_sum$sum_avg_spend)
  }
  m_1 = left_join(dong_mean, dong_sum, by="dong")
  m_2 = left_join(m_1, dong_rate, by="dong")
  df = left_join(df, m_2, by="dong")  #df에 평균,합,비율 데이터 set을 합침.
  df$mean_job_majorc <- replace(df$mean_job_majorc, is.na(df$mean_job_majorc)==T,0.024219)
  df$mean_job_smallc <- replace(df$mean_job_smallc, is.na(df$mean_job_smallc)==T,0.145757)
  df$mean_job_public <- replace(df$mean_job_public, is.na(df$mean_job_public)==T,0.032768)
  df$mean_job_profession <- replace(df$mean_job_profession, is.na(df$mean_job_profession)==T,0.014855)
  df$mean_job_self <- replace(df$mean_job_self, is.na(df$mean_job_self)==T,0.222090)
  df$mean_vehicle_own_rat <- replace(df$mean_vehicle_own_rat, is.na(df$mean_vehicle_own_rat)==T,0.041161)
  df$mean_avg_income <- replace(df$mean_avg_income, is.na(df$mean_avg_income)==T,34221420)
  df$mean_med_income <- replace(df$mean_med_income, is.na(df$mean_med_income)==T,30645290)
  df$mean_avg_spend <- replace(df$mean_avg_spend, is.na(df$mean_avg_spend)==T,4224923)
  df$sum_job_majorc <- replace(df$sum_job_majorc, is.na(df$sum_job_majorc)==T,3.717861e+00)
  df$sum_job_smallc <- replace(df$sum_job_smallc, is.na(df$sum_job_smallc)==T,2.078142e+01)
  df$sum_job_public <- replace(df$sum_job_public, is.na(df$sum_job_public)==T,4.747755e+00)
  df$sum_job_profession <- replace(df$sum_job_profession, is.na(df$sum_job_profession)==T,2.169554e+00)
  df$sum_job_self <- replace(df$sum_job_self, is.na(df$sum_job_self)==T,3.044199e+01)
  df$sum_vehicle_own_rat <- replace(df$sum_vehicle_own_rat, is.na(df$sum_vehicle_own_rat)==T,5.609080e+00)
  df$sum_avg_income <- replace(df$sum_avg_income, is.na(df$sum_avg_income)==T,4.998226e+09)
  df$sum_med_income <- replace(df$sum_med_income, is.na(df$sum_med_income)==T,4.455924e+09)
  df$sum_avg_spend <- replace(df$sum_avg_spend, is.na(df$sum_avg_spend)==T,6.147678e+08)
  df$rate_job_majorc <- replace(df$rate_job_majorc, is.na(df$rate_job_majorc)==T,1.388889e-02)
  df$rate_job_smallc <- replace(df$rate_job_smallc, is.na(df$rate_job_smallc)==T,1.388889e-02)
  df$rate_job_public <- replace(df$rate_job_public, is.na(df$rate_job_public)==T,1.388889e-02)
  df$rate_job_profession <- replace(df$rate_job_profession, is.na(df$rate_job_profession)==T,1.388889e-02)
  df$rate_job_self <- replace(df$rate_job_self, is.na(df$rate_job_self)==T,1.388889e-02)
  df$rate_vehicle_own_rat <- replace(df$rate_vehicle_own_rat, is.na(df$rate_vehicle_own_rat)==T,1.388889e-02)
  df$rate_avg_income <- replace(df$rate_avg_income, is.na(df$rate_avg_income)==T,1.388889e-02)
  df$rate_med_income <- replace(df$rate_med_income, is.na(df$rate_med_income)==T,1.388889e-02)
  df$rate_avg_spend <- replace(df$rate_avg_spend, is.na(df$rate_avg_spend)==T,1.388889e-02)
  return(df)
}
data = jeju_love()
str(data)

#한얼꺼 cbind 함 
#1.좌표데이터를 이용한 변수 (3시간반)
#2.측정소와 정류장 사이 거리 계산 (1시간반)
#3.날씨 관련 변수(30분~1시간)
#4.rainy_day

data_haneol <- read.csv("data_haneol.csv",header = T)
data <- cbind(data, data_haneol)


#수요가 많을 것으로 예상되는 정류장--------------------------------
g<-data[grep("고등학교", data$station_name),]
highschool<-list(unique(g$station_name))
g<-data[grep("대학교", data$station_name),]
university<-list(unique(g$station_name))
f<-function(x){
  if (x %in% unlist(highschool)){
    return(1)
  } else if (x %in% unlist(university)){
    return(1)
  } else{
    return(0)
  }
}
data$school<-c()
for (i in 1:nrow(data)){
  data$school[i]<-f(data$station_name[i])
}
g<-data[grep("환승", data$station_name),]
tranfer<-list(unique(g$station_name))
g<-data[grep("공항", data$station_name),]
airport<-list(unique(g$station_name))
g<-data[grep("터미널", data$station_name),]
terminal<-list(unique(g$station_name))
f<-function(x){
  if (x %in% unlist(tranfer)){
    return(1)
  } else if (x %in% unlist(airport)){
    return(1)
  } else if (x %in% unlist(terminal)){
    return(1)
  } else{
    return(0)
  }
}
data$transfer<-c()
for (i in 1:nrow(data)){
  data$transfer[i]<-f(data$station_name[i])
}
#동-라벨인코딩---------------------------------------------
data$dong2<-as.factor(data$dong)

#승 하차 시간대 통합 변수(t~t+3)----------------------------
data$a69<-data$X6.7_ride+data$X7.8_ride+data$X8.9_ride
data$a912<-data$X9.10_ride+data$X10.11_ride+data$X11.12_ride
data$b69<-data$X6.7_takeoff+data$X7.8_takeoff+data$X8.9_takeoff
data$b912<-data$X9.10_takeoff+data$X10.11_takeoff+data$X11.12_takeoff
str(data)



### MODELING --------------------------------------------
#1 안쓰이는 변수 제거 및 train,test 나누기

data$dong2 <- as.numeric(data$dong2)
data$bus_route_id2 <- as.numeric(data$bus_route_id2)
data$station_code2 <- as.numeric(data$station_code2)
data$route_station_weekday2 <- as.numeric(data$route_station_weekday2)
str(data,list.len=nrow(data)) #전부 numeric

data_backup <- data
data <- data_backup

library(dplyr)
data_jeju <- data %>% filter(`si_제주시`==1)
set.seed(123)
data_jeju <- data_jeju[sample(nrow(data_jeju), 60000),]
nrow(data_jeju)

data_seo <- data %>% filter(`si_서귀포시`==1)
set.seed(123)
data_seo <- data_seo[sample(nrow(data_seo), 60000),]
nrow(data_seo)

data_jejudo <- rbind(data_jeju, data_seo) #제주시 + 서귀포시 12만
str(data_jejudo)

#train , test data 나누기
data_train <- data_jejudo %>% filter(cue==0)
data_test <- data_jejudo %>% filter(cue==1)

#제주,서귀포 구분없이 필요없는 공통 열 제거
data_train_clean <- data_train %>% select(-c(X,id,date,bus_route_id,station_code,station_name,weekday,route_station,bus_route_id_weekday,station_code_weekday,route_station_weekday,mean_bus_weekday_ride,mean_station_weekday_ride,mean_route_station_weekday_ride,location,cue,route_station2,dong,si,dist_jeju,dist_gosan,dist_seongsan,dist_po,dist_name))
data_test_clean <- data_test %>% select(-c(X,id,date,bus_route_id,station_code,station_name,weekday,route_station,bus_route_id_weekday,station_code_weekday,route_station_weekday,mean_bus_weekday_ride,mean_station_weekday_ride,mean_route_station_weekday_ride,location,cue,route_station2,dong,si,dist_jeju,dist_gosan,dist_seongsan,dist_po,dist_name))

data_train_clean$dong2[is.na(data_train_clean$dong2)] <- 0

str(data_train_clean)
# 90개 column들을 가진 dataset 4개 만들기
xvarname <- data_train_clean %>% select(-X18.20_ride) %>% names()
yvarname <- "X18.20_ride"

set.seed(123)
input_var1 <- sample(xvarname, 40)
set.seed(234)
input_var2 <- sample(xvarname, 40)
set.seed(345)
input_var3 <- sample(xvarname, 40)
set.seed(456)
input_var4 <- sample(xvarname, 40)


input1 <- data_train_clean %>% select(input_var1,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
input2 <- data_train_clean %>% select(input_var2,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
input3 <- data_train_clean %>% select(input_var3,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
input4 <- data_train_clean %>% select(input_var4,X18.20_ride) #40개 변수 뽑은 dataset (y포함)

test_x1 <- data_test_clean %>% select(input_var1)
test_x2 <- data_test_clean %>% select(input_var2)
test_x3 <- data_test_clean %>% select(input_var3)
test_x4 <- data_test_clean %>% select(input_var4)


#2 1퍼뽑아서 *4 (seed 4번) = mtry  4개 (oob)
set.seed(123)
train_123 <- sample_n(input1,nrow(input1)*0.01)
x_train_123 <- train_123 %>% select(-X18.20_ride)
y_train_123 <- train_123$X18.20_ride

set.seed(234)
train_234 <- sample_n(input2,nrow(input2)*0.01)
x_train_234 <- train_234 %>% select(-X18.20_ride)
y_train_234 <- train_234$X18.20_ride

set.seed(345)
train_345 <- sample_n(input3,nrow(input3)*0.01)
x_train_345 <- train_345 %>% select(-X18.20_ride)
y_train_345 <- train_345$X18.20_ride

set.seed(456)
train_456 <- sample_n(input4,nrow(input4)*0.01)
x_train_456 <- train_456 %>% select(-X18.20_ride)
y_train_456 <- train_456$X18.20_ride


library(randomForest)
set.seed(123)
bestmtry_123 <- tuneRF(x_train_123,y_train_123,stepFactor=1.5,improve=1e-5,ntree=100)
print(bestmtry_123)

set.seed(234)
bestmtry_234 <- tuneRF(x_train_234,y_train_234,stepFactor=1.5,improve=1e-5,ntree=100)
print(bestmtry_234)

set.seed(345)
bestmtry_345 <- tuneRF(x_train_345,y_train_345,stepFactor=1.5,improve=1e-5,ntree=100)
print(bestmtry_345)

set.seed(456)
bestmtry_456 <- tuneRF(x_train_456,y_train_456,stepFactor=1.5,improve=1e-5,ntree=100)
print(bestmtry_456)

#RF FIT
input1_x <- input1 %>% select(-X18.20_ride)
rf.fit_123 = randomForest(y= input1$X18.20_ride, x=input1_x, mtry = 19 , ntree = 100, importance = T)

input2_x <- input2 %>% select(-X18.20_ride)
rf.fit_234 = randomForest(y= input2$X18.20_ride, x=input2_x, mtry = 6 , ntree = 100, importance = T)

input3_x <- input3 %>% select(-X18.20_ride)
rf.fit_345 = randomForest(y= input3$X18.20_ride, x=input3_x, mtry = 9, ntree = 100, importance = T)

input4_x <- input4 %>% select(-X18.20_ride)
rf.fit_456 = randomForest(y= input4$X18.20_ride, x=input4_x, mtry = 28 , ntree = 100, importance = T)

#RF TEST
testset <- data %>% filter(cue==1)
test_clean <- testset %>% select(-c(X,id,date,bus_route_id,station_code,station_name,weekday,route_station,bus_route_id_weekday,station_code_weekday,route_station_weekday,mean_bus_weekday_ride,mean_station_weekday_ride,mean_route_station_weekday_ride,location,cue,route_station2,dong,si,dist_jeju,dist_gosan,dist_seongsan,dist_po,dist_name))
test_clean$dong2[is.na(test_clean$dong2)] <- 0

test_input1 <- test_clean %>% select(input_var1,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
test_input2 <- test_clean %>% select(input_var2,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
test_input3 <- test_clean %>% select(input_var3,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
test_input4 <- test_clean %>% select(input_var4,X18.20_ride) #40개 변수 뽑은 dataset (y포함)

TEST_x1 <- test_clean %>% select(input_var1)
TEST_x2 <- test_clean %>% select(input_var2)
TEST_x3 <- test_clean %>% select(input_var3) #si_서귀포시에서 NA뜸
TEST_x3[is.na(TEST_x3)] <- 0
TEST_x4 <- test_clean %>% select(input_var4)
TEST_x4[is.na(TEST_x4)] <- 0

pred1 <- predict(rf.fit_123,TEST_x1)
pred2 <- predict(rf.fit_234,TEST_x2)
pred3 <- predict(rf.fit_345,TEST_x3)
pred4 <- predict(rf.fit_456,TEST_x4)

#Geometric_mean
final_pred <- (pred1*pred2*pred3*pred4)**(1/4)
final_pred <- (pred1+pred2+pred3+pred4)/4

test_with_finalpred <-read.csv("test.csv", header = T, fileEncoding = "utf-8")
test_with_finalpred$y <- final_pred
library(dplyr)
test_with_finalpred <- test_with_finalpred[,c(1,21)]
write.csv(test_with_finalpred, "submission_제출양식.csv",row.names = F, col.names = T) #이후 column이름 바꿈.

final_pred_yeon <- read.csv("final_pred_연욱.csv",header=T)
test_with_finalpred$yeon <- final_pred_yeon
test_with_final_pred_yeon <- test_with_finalpred[,c(1,3)]
write.csv(test_with_final_pred_yeon, "submission_연욱.csv",row.names=F)
