myfun <- function(){
  #rember : library(ggplot2), library(plyr)
  data <- data.frame(
    year = numeric(0), gender = character(0), section_id = numeric(0), 
    team_name = character(0), tot_point = numeric(0), win_round = numeric(0), 
    atk_point = numeric(0), tot_atk = numeric(0), block = numeric(0), 
    ser_point = numeric(0), tot_ser = numeric(0), rec_good = numeric(0), 
    tot_rec = numeric(0), dig_good = numeric(0), tot_dig = numeric(0), 
    set_good = numeric(0), tot_set = numeric(0), tot_get = numeric(0)
  )
  for( i in 13:19){
    temp <- read.csv( paste("data/TVL",i,".csv",sep = ''), header = FALSE)
    temp <- cbind(rep(i, nrow(temp)), temp)
    colnames(temp) <- colnames(data)
    data <- rbind(data,temp)
  }
  
  data <- ddply(data, .variables = c("team_name","gender"), .fun = pure_tean_name)
  
  data <- ddply(data, .variables = c("year","gender","section_id"), .fun = cal_win)
  
  teamAB <- ddply(data, .variables = c("year","gender","AvsB"), .fun = AB_round ) 
  View(teamAB)
  
  #section <- ddply(data, .variables = c("year","section_id", "gender"), .fun = AB_Realation)
  
}


pure_tean_name <- function(data){
  name = data$team_name
  A = c("台電男排","屏東台電","台電女排","高雄台電","台電公司")
  B = c("臺中長力","長力男排","臺中太陽神","中國人纖","新北中纖")
  C = c("MIZUNO","雲林Mizuno","ATTACKLINE","臺北鯨華")
  D = c("極速超跑", "桃園石易","桃園台灣產險","桃園臺灣產險","桃園臺產","桃園臺產隼鷹")
  E = c("愛山林建設","愛山林","conti","臺北Conti")
  gender = any(data$gender == "女")
  if(any(A == name)){
    ifelse(gender== T, data$team_name<- "高雄台電", data$team_name<- "屏東台電")
  }
  else if(any(B == name)){
    ifelse(gender== T, data$team_name<- "新北中纖", data$team_name<- "臺中太陽神")
  }
  else if(any(C == name)){
    ifelse(gender== T, data$team_name<- "臺北鯨華", data$team_name<- "雲林Mizuno")
  }
  else if(any(D== name)){
    ifelse(gender== T, data$team_name<- "極速超跑", data$team_name<- "桃園臺產隼鷹")
  }
  else if(any(E == name)){
    ifelse(gender== T, data$team_name<- "愛山林", data$team_name<- "臺北Conti")
  }
  else{
    data$team_name = "del"
  }
  
  return(data)
}

cal_win <- function(data){
  
  if(any(data$team_name=="del")){
    return()
  }
  name <- sort(data$team_name)
  if(nrow(data)!=2){
    print("error")
  }else{
    AvsB <- paste(name[1],"VS",name[2])
  }
  
  A_win <- data$win_round[which(data$team_name==name[1])]
  B_win <- data$win_round[which(data$team_name==name[2])]
  tot_round <- sum(data$win_round)
  return(data.frame(AvsB,A_win,B_win, tot_round))
}

AB_round <- function(data){
  A_win <- sum(data$A_win)
  B_win <- sum(data$B_win)
  tot_round <- sum(data$tot_round)
  return(data.frame(A_win,B_win,tot_round))
}