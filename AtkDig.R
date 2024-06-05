#Yeh: row28 37 40, function : year_Avg, choose_condition
library(ggplot2)
library(plyr)
myfun <- function(){
  #rember : library(ggplot2), library(plyr)
  data <- data.frame(
    year = numeric(0), gender = character(0), section_id = numeric(0), 
    team_name = character(0), total_point = numeric(0), win_of_round = numeric(0), 
    atk_point = numeric(0), total_atk = numeric(0), block = numeric(0), 
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
  
  block <-data$block
  data <- cbind(data[,-9],block)
  
  data <- ddply(data, .variables = c("team_name","gender"), .fun = get_teamid)
  data <- data[data$team_name!="del",]
  
  #remove data which isn't record 
  data <- data[!is.na(data$tot_get),]
  
  section_data <- ddply(data, .variables = c("year","gender","team_name"), .fun = analysis_section)
  
  #d_ply(section_data, .variables="gender", .fun = out_img)
  
  team_data <-ddply(section_data,.variables = c("gender","team_name"), .fun = analysis_team)
  
  #分別列出7年,每一隊在每一項專項中的年度平均
  year_data <- ddply(data, .variables = c('year', 'gender', "team_name"), .fun = year_Avg)
  
  #挑選出每一年中，a隊攻擊 > b隊攻擊 且 a隊防守 < b隊防守 之(a,b)組合
  condition_data <- ddply(year_data, .variables = c('year', 'gender'), .fun = choose_condition)
  
  teamAB <- myfun2()
  
  result_data <- ddply(teamAB, .variables = c("year", "gender", "A_team", "B_team"), .fun = search, team_set = condition_data)
  
  #View(data)
  #print(team_data)
  View(teamAB)
  #return()
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
  
  return (data.frame(container, block=mean(data$block)))
}

frquency <- function(point,total){
  ans <- point/total
  ans[is.nan(ans)==T] <- 0
  return (round(ans*100,2))
}

get_teamid <- function(data){
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
  atk = round(mean(data$atk),2)
  dig = round(mean(data$dig),2)
  ser = round(mean(data$ser),2)
  rec = round(mean(data$rec),2)
  block = round(mean(data$block),2)
  set = round(mean(data$set),2)
  return( data.frame(atk, dig, ser, rec, block, set))
}

year_Avg <- function(data){
  Atk <- round(mean(data$atk_point / data$total_atk), 2)
  Def <- round(mean(data$dig_good / data$tot_dig), 2)
  Rec <- round(mean(data$rec_good / data$tot_rec), 2)
  Ser <- round(mean(data$ser_point / data$tot_ser), 2)
  Set <- round(mean(data$set_good / data$tot_set), 2)
  Block <- round(sum(data$block) / length(data$block), 2)
  return(data.frame('year' = data$year[1], 'gender' = data$gender[1], 'team_name' = data$team_name[1], Atk, Def, Rec, Ser, Set, Block))
}

choose_condition <- function(data){
  out <- data.frame(A_team = numeric(0), B_team = numeric(0), 
                    A_Atk = numeric(0), B_Atk = numeric(0), 
                    A_Def = numeric(0), B_Def = numeric(0))
  l <- nrow(data)
  for(i in 1:(l - 1)){
    name <- data$team_name[i]
    atk <- data$Atk[i]
    def <- data$Def[i]
    for(k in (i + 1):l){
      comp_name <- data$team_name[k]
      comp_atk <- data$Atk[k]
      comp_def <- data$Def[k]
      
      if(atk > comp_atk && def < comp_def){
        temp <- data.frame(A_team = name, B_team = comp_name, 
                           A_Atk = atk, B_Atk = comp_atk, 
                           A_Def = def, B_Def = comp_def)
        out <- rbind(out, temp)
      }
      else if(atk < comp_atk && def > comp_def){
        temp <- data.frame(A_team = comp_name, B_team = name, 
                           A_Atk = comp_atk, B_Atk = atk, 
                           A_Def = comp_def, B_Def = def)
        out <- rbind(out, temp)
      }
    }
  }
  return(out)
}

myfun2 <- function(){
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
    temp <- read.csv( paste("C:/Users/user/Documents/data/TVL",i,".csv",sep = ''), header = FALSE)
    temp <- cbind(rep(i, nrow(temp)), temp)
    colnames(temp) <- colnames(data)
    data <- rbind(data,temp)
  }
  
  data <- ddply(data, .variables = c("team_name","gender"), .fun = pure_tean_name)
  
  data <- ddply(data, .variables = c("year","gender","section_id"), .fun = cal_win)
  
  teamAB <- ddply(data, .variables = c("year","gender","A_team", "B_team"), .fun = AB_round ) 
  return (teamAB)
  
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
    A_team <- name[1]
    B_team <- name[2]
  }
  
  A_win <- data$win_round[which(data$team_name==name[1])]
  B_win <- data$win_round[which(data$team_name==name[2])]
  tot_round <- sum(data$win_round)
  return(data.frame(A_team, B_team,A_win,B_win, tot_round))
}

AB_round <- function(data){
  A_win <- sum(data$A_win)
  B_win <- sum(data$B_win)
  tot_round <- sum(data$tot_round)
  return(data.frame(A_win,B_win,tot_round))
}

search <- function(data, team_set){
  
  out <- ddply(team_set, .variables = c("year", "gender", "A_team", "B_team"), .fun = search2, data = data)
  return(out)
}

search2 <- function(set, data){
  if(set$gender == data$gender & set$year == data$year){
    if(data$A_team == set$A_team && data$B_team == set$B_team){
      return(cbind(set, data.frame(A_win = data$A_win, B_win = data$B_win, tot_round = data$tot_round)))
    }else if(data$B_team == set$A_team && data$A_team == set$B_team){
      return(cbind(set, data.frame(A_win = data$B_win, B_win = data$A_win, tot_round = data$tot_round)))
    }
  }
  return()
}
  
myfun()