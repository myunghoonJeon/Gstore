########################[ 신/구 사용자의 유입 플랫폼 ]#############################################
getNewVisitChannleCount <- function(df,flag){
  if(flag==TRUE){
    ggt1 <- df %>% subset(!is.na(newVisits))  
  }
  else{
    ggt1 <- df %>% subset(is.na(newVisits))
  }
  ggt <- ggt1 %>% group_by(channelGrouping) %>% summarise(count = n(), rate=round((count/count(ggt1)*100),1))
  ggt <- arrange(ggt,desc(count))
  ggt
}
########################[ 신/구 사용자의 디바이스 정보 ]#############################################
getChannelCount <- function(df,ct){
  sg <- df %>% subset(channelGrouping==ct)
  sgt <- sg %>% group_by(deviceCategory) %>% summarise(count = n(), rate=round((count/count(sg)*100),1))
  sgt <- arrange(sgt,desc(count))
  sgt
}

########################[ 디바이스 별 클릭 수 패턴 ]#############################################
getHitCountByDevice <- function(df){
  df %>% select(deviceCategory, hitNumber)
  df[is.na(df)] <-0
  d <- df %>% group_by(deviceCategory) %>% summarise(count = n(), hitAvg = mean(hitNumber))
  d
}
########################[ 비 정상 클릭 수 패턴 ]#############################################
checkHitnumberByUser <- function(df,device,num){
  test <- df %>% filter(hitNumber > num & deviceCategory==device)
  test <- test %>% group_by(case) %>% summarise(count = n())
  test <- arrange(test,desc(count))
  test
}

checkHitnumberByChannel <- function(df,device,num){
  test <- df %>% filter(hitNumber > num & deviceCategory==device)
  test <- test %>% group_by(medium) %>% summarise(count = n())
  test <- arrange(test,desc(count))
  test
}
df <- gt
getCountrylistByPament <- function(df,flag){
  if(flag==TRUE){
    df <- df %>% subset(eCommerceAction.option=='Payment')
  }
  else{
    df <- df %>% subset(eCommerceAction.option!='Payment')
  }
  cl <- df %>% group_by(country) %>% summarise(count =n())
  cl <- arrange(cl,desc(count))
  cl
}
########################[ 대기 ]#############################################
# getCountByDate <- function(df,flag){
#   if(flag=='y'){
#     py <- df %>% subset(eCommerceAction.option=='Payment') %>% group_by(year) %>% summarise(count = n())
#     npy <- df %>% subset(eCommerceAction.option!='Payment') %>% group_by(year) %>% summarise(count = n())
#   }else if(flag=='m'){
#     pm <- df %>% group_by(month) %>% summarise(count = n())
#     npm <-
#   }
#       
# }



###################################################################################################