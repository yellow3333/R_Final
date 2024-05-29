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
  
  data <- ddply(data, .variables = c("year","gender","section_id"), .fun = cal_win)
  
  #print(data)
  
  block <-data$block
  data <- cbind(data[,-9],block)
  
  data <- ddply(data, .variables = c("team_name","gender"), .fun = pure_tean_name)
  
  #print(data)
  
  section_data <- ddply(data, .variables = c("year","gender","team_name"), .fun = analysis_section)
  section_data <- section_data[section_data$team_name!="del",]
  
  #print(section_data)
  
  #d_ply(section_data, .variables="gender", .fun = out_img)
  
  team_data <-ddply(section_data,.variables = c("gender","team_name"), .fun = analysis_team)
  View(team_data)
  
}

analysis_section <- function(data){
  for( i in 4:8){
    temp <- mean(frquency(data[,i*2-1],data[,i*2]))
    if(i == 4){
      container <- data.frame(temp)
    }
    else{
      container <- cbind(container, temp)
    }
  }
  colnames(container) <- c("atk", "ser", "rec", "dig", "set")
  win_round <- mean(sum(data$win_round)/sum(data$tot_round))
  win_section <- sum(data$is_win)
  
  return (data.frame(container, block=mean(data$block), win_round, win_section, tot_section = nrow(data)))
}

frquency <- function(point,total){
  ans <- point/total
  ans[is.nan(ans)==T] <- 0
  return (round(ans*100,2))
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

out_img <- function(data){
  if (any(data$gender == "女")){
    gender = "女排"
  }else{
    gender = "男排"
  }
  
  action_c <- c("攻擊","發球","接發","防禦","舉球","攔網")
  action_e <- colnames(data)[4:9]
  
  for( i in  1:6){
    g <- ggplot(data, aes(year, data[,3+i], color = team_name))+ geom_point()+geom_line()
    g <- g+labs(title = paste(gender, action_c[i]), y = action_e[i])
    print(g)
  }
}

analysis_team <- function(data){
  for( i in 4:10){
    temp <- round(mean(data[,i]),2)
    if(i == 4){
      container <- data.frame(temp)
    }
    else{
      container <- cbind(container, temp)
    }
  }
  colnames(container) <- colnames(data)[4:10]
  win_section <- round(mean(data[,11]/data[,12]),2)
  
  return (data.frame(container, win_section))
}

cal_win <- function(data){
  tot_round <- sum(data$win_round)
  is_win <- data$win_round == 3
  return(data.frame(data, tot_round,is_win))
}