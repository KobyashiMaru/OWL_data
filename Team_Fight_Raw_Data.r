library(readr)


Team_Fight_Raw_Data <- read_csv("C:/Users/bronc/Desktop/OWL_data/Team_Fight_Raw_Data.csv")
Team_Fight_Raw_Data = as.data.frame(Team_Fight_Raw_Data)


Team_Fight_Raw_Data$Blue_Team = substring(Team_Fight_Raw_Data$Blue_Team, 2)
Team_Fight_Raw_Data$Red_Team = substring(Team_Fight_Raw_Data$Red_Team, 2)

#==================判定誰贏========================

attach(Team_Fight_Raw_Data)
win = c()
i = 0
for (i in seq(from = 1, to = dim(Team_Fight_Raw_Data)[1], by = 1)) {
  if (KB[i] > KR[i]){
    win[i] = "B"
  } else if (KB[i] < KR[i]){
    win[i] = "R"
  } else{
    win[i] = "T"
  }
}


detach(Team_Fight_Raw_Data)
Team_Fight_Raw_Data = cbind(Team_Fight_Raw_Data, win)


#==================判定誰贏========================


#==================找出不同的隊伍名字===============

teams = c()
teams[1] = Team_Fight_Raw_Data$Blue_Team[1]
i = 0
j = 1


multiply = function(X){         #做一個把向量值乘起來的函數
  i = 0
  multiplier = 1
  for (i in seq(1:length(X))){
    multiplier = multiplier * X[i]
  } 
  multiplier
}


for (i in seq(from = 2, to = length(Team_Fight_Raw_Data$Blue_Team), by = 1)){
  if (multiply(Team_Fight_Raw_Data$Blue_Team[i] != teams) == TRUE){
    teams[j + 1] = Team_Fight_Raw_Data$Blue_Team[i]
    j = j + 1
  }
}

#========================找出不同的隊伍名字====================



attach(Team_Fight_Raw_Data)

i = 1
j = 1

Categorzied_data = data.frame()

for (j in seq(from = 1, to = length(teams))){
  for (i in seq(from = 1, to = dim(Team_Fight_Raw_Data)[1], by = 1)){
    if ((teams[j] == Blue_Team[i]) || ((teams[j] == Red_Team[i]))){
      Categorzied_data = rbind(Categorzied_data, Team_Fight_Raw_Data[i, ])
    }
  }
}
detach(Team_Fight_Raw_Data)



i = 0
Blue_Team_Win = c()

for (i in seq(from = 1, to = length(win))){
  if (win[i] == "B"){
    Blue_Team_Win[i] = 1
  } else {
    Blue_Team_Win[i] = 0
  }
}

i = 0
for (i in seq(from = 1, to = length(Team_Fight_Raw_Data$FB))){
  if (Team_Fight_Raw_Data$FB[i] == "B"){
    Team_Fight_Raw_Data$FB[i] = 1    
  } else {
    Team_Fight_Raw_Data$FB[i] = 0
  }
}

attach(Team_Fight_Raw_Data)

reg = glm(Blue_Team_Win ~ length + UB + UR + FB, family = binomial(link = "logit"))

detach(Team_Fight_Raw_Data)
