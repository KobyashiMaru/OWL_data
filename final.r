library(readr)
library(dummies)
library(readxl)
library(ROCR)


#====================separate data set to training set and testing set===========
training_set_and_testing_set = function(X, testing_set_number){
  not_in_the_seq = function(A, B){
    A = A[!A %in% B]
    A
  }
  length_of_data = seq(1:dim(X)[1])
  testing_set_sequence = sample(length_of_data, testing_set_number)
  testing_set_sequence = sort(testing_set_sequence)
  training_set_sequence = not_in_the_seq(length_of_data, testing_set_sequence)
  list(training_set_seq = training_set_sequence, testing_set_seq = testing_set_sequence)
  
}
#====================separate data set to training set and testing set===========


#===================setting a function of accuracy matrix==============
accuracy_matrix_function = function(prediction_of_reg_map, reg_map, label){
  
  if (grepl("Win", x = description_of_main_title) == TRUE){
    label[which(label == 0.5)] = 1
  } else{
    label[which(label == 0.5)] = 0
  }
  
  
  pred = prediction(prediction_of_reg_map, label)
  acc.perf = performance(pred, measure = "acc")
  ind = which.max( slot(acc.perf, "y.values")[[1]] )
  acc = slot(acc.perf, "y.values")[[1]][ind]
  cutoff = slot(acc.perf, "x.values")[[1]][ind]
  cutoff = 0.5
  prediction_of_testing_set = reg_map$fitted.values[testing_seq]
  label_of_testing_set = Blue_Team_Win[testing_seq]
  label_of_testing_set[which(label_of_testing_set == 0.5)] = 0
  
  i = 0
  j = 1
  prediction_yes_correct = c()
  prediction_no_correct = c()
  prediction_yes_incorrect = c()
  prediction_no_incorrect = c()
  
  for (i in seq(1:length(prediction_of_testing_set))){
    if ((prediction_of_testing_set[i] > cutoff) && (label_of_testing_set[i] == 1) == TRUE){
      prediction_yes_correct = append(prediction_yes_correct, prediction_of_testing_set[i])
    } else if ((prediction_of_testing_set[i] > cutoff) && (label_of_testing_set[i] == 0) == TRUE){
      prediction_yes_incorrect = append(prediction_yes_incorrect, prediction_of_testing_set[i])
    } else if ((prediction_of_testing_set[i] < cutoff) && (label_of_testing_set[i] == 1) == TRUE){
      prediction_no_incorrect = append(prediction_no_incorrect, prediction_of_testing_set[i])   
    } else {
      prediction_no_correct = append(prediction_no_correct, prediction_of_testing_set[i])
    }
  }
  prediction_matrix = list(pred_yes_correct = length(prediction_yes_correct), 
                           pred_yes_incorrect = length(prediction_yes_incorrect), 
                           pred_no_incorrect = length(prediction_no_incorrect), 
                           pred_no_correct = length(prediction_no_correct),
                           cutoff_point = c(cutoff), 
                           accuracy = (length(prediction_yes_correct) + length(prediction_no_correct)) / length(testing_seq))
  prediction_matrix
}
#===================setting a function of accuracy matrix==============

#===========setting a function of setting dummy variable matrix===============
dummy_varible_matrix_function = function(Map_and_round_type){
  
  dummy_varible_matrix = dummy(Map_and_round_type)
  dummy_varible_matrix = dummy_varible_matrix[,1: (dim(dummy_varible_matrix)[2] - 1)] #set the last dummy variable as 0
  dummy_varible_matrix = as.matrix(dummy_varible_matrix)
  
  
  colnames(dummy_varible_matrix) = gsub("Map_and_round_type", "", colnames(dummy_varible_matrix))
  return(dummy_varible_matrix)
}
#===========setting a function of setting dummy variable matrix===============


#====================import data ====================================

Team_Fight_Raw_Data_All_Maps <- read_csv("F:/JARVIS/Documents/OWL_data/Team_Fight_Raw_Data_All_Maps.csv")
Team_Fight_Raw_Data_All_Maps = as.data.frame(Team_Fight_Raw_Data_All_Maps)


Team_Fight_Raw_Data_All_Maps$Blue_Team = substring(Team_Fight_Raw_Data_All_Maps$Blue_Team, 2)
Team_Fight_Raw_Data_All_Maps$Red_Team = substring(Team_Fight_Raw_Data_All_Maps$Red_Team, 2)

#====================import data ====================================

#==================decide what makes a teamfight win========================



attach(Team_Fight_Raw_Data_All_Maps)
win = c()
i = 0
for (i in seq(from = 1, to = dim(Team_Fight_Raw_Data_All_Maps)[1], by = 1)) {
  if (KB[i] > KR[i]){
    win[i] = "B"
  } else if (KB[i] < KR[i]){
    win[i] = "R"
  } else{
    win[i] = "T"
  }
}


detach(Team_Fight_Raw_Data_All_Maps)
Team_Fight_Raw_Data_All_Maps = cbind(Team_Fight_Raw_Data_All_Maps, win)

Team_Fight_Raw_Data_All_Maps = Team_Fight_Raw_Data_All_Maps[(!Team_Fight_Raw_Data_All_Maps$win %in% "T"), ]
win = win[!(win %in% "T")]

#==================decide what makes a teamfight win and delete the tie teamfight========================


#==================find how many teams are there===============

teams = c()
teams[1] = Team_Fight_Raw_Data_All_Maps$Blue_Team[1]
i = 0
j = 1


multiply = function(X){
  i = 0
  multiplier = 1
  for (i in seq(1:length(X))){
    multiplier = multiplier * X[i]
  }
  multiplier
}


for (i in seq(from = 2, to = length(Team_Fight_Raw_Data_All_Maps$Blue_Team), by = 1)){
  if (multiply(Team_Fight_Raw_Data_All_Maps$Blue_Team[i] != teams) == TRUE){
    teams[j + 1] = Team_Fight_Raw_Data_All_Maps$Blue_Team[i]
    j = j + 1
  }
}

#==================find how many teams are there===============

#=======================make blue team win as 1 lose as 0=========
i = 0
Blue_Team_Win = c()

for (i in seq(from = 1, to = length(win))){
  if (win[i] == "B"){
    Blue_Team_Win[i] = 1
  } else{
    Blue_Team_Win[i] = 0
  }
}
#=======================make blue team win as 1 lose as 0=========

#=======================if blue team makes first blood as 1======================
i = 0
for (i in seq(from = 1, to = length(Team_Fight_Raw_Data_All_Maps$FB))){
  if (Team_Fight_Raw_Data_All_Maps$FB[i] == "B"){
    Team_Fight_Raw_Data_All_Maps$FB[i] = 1
  } else {
    Team_Fight_Raw_Data_All_Maps$FB[i] = 0
  }
}
#=======================if blue team makes first blood as 1======================


#====================forge a data frame=============
Map_and_round_type = paste(Team_Fight_Raw_Data_All_Maps$Map, Team_Fight_Raw_Data_All_Maps$Round_Type, sep = "_")
dummy_varible_matrix = dummy_varible_matrix_function(Map_and_round_type)

new_data = cbind(Team_Fight_Raw_Data_All_Maps[6:11], dummy_varible_matrix, Blue_Team_Win)


#====================forge a data frame=============


#====================make a logistic model with new data frame=================
reg_map = glm(Blue_Team_Win ~ length + UB + UR + FB + dummy_varible_matrix,
              data = new_data,family = binomial(link = "logit"))

#====================make a logistic model with new data frame=================

#====================find how many map and round types===================
data_with_dummy_variables = new_data
write.csv(data_with_dummy_variables, file = "OWL_data_with_dummy_variables.csv")

data_without_dummy_variables = cbind(Team_Fight_Raw_Data_All_Maps[6:11], Map_and_round_type, Blue_Team_Win)
write.csv(data_without_dummy_variables, file = "OWL_data_without_dummy_variables.csv")


maps = c()
maps[1] = Map_and_round_type[1]
i = 0
j = 1


for (i in seq(from = 2, to = length(Map_and_round_type), by = 1)){
  if (multiply(Map_and_round_type[i] != maps) == TRUE){
    maps[j + 1] = Map_and_round_type[i]
    j = j + 1
  }
}
#====================find how many map and round types===================

#===========setting a function of drawing ROC curve and show accuracy matrix=======
drawing_a_ROC_curve = function(prediction_of_reg_map, description_of_main_title, label){
  
  
  if (grepl("Win", x = description_of_main_title) == TRUE){
    label[which(label == 0.5)] = 1
  } else{
    label[which(label == 0.5)] = 0
  }
  
  
  pred = prediction(prediction_of_reg_map, label)
  perf = performance(pred, measure = "auc")
  auc = as.character(perf@y.values)
  main_title = paste(description_of_main_title, "(AUC = ", auc, ")", sep = "")
  perf = performance(pred, measure = "tpr", x.measure = "fpr")
  
  plot(perf, col = "red", main = main_title)
  abline(a = 0, b = 1)
  return(accuracy_matrix_function(prediction_of_reg_map, reg_map, label))
}

#===========setting a function of drawing ROC curve and show accuracy matrix=======


#==================Drawing ROC curve===============

separated_data = training_set_and_testing_set(new_data, testing_set_number = 1000)
training_seq = separated_data$training_set_seq
testing_seq = separated_data$testing_set_seq
prediction_of_reg_map = reg_map$fitted.values[training_seq]
prediction_of_reg_map = as.vector(prediction_of_reg_map)
label = new_data$Blue_Team_Win[training_seq]


description_of_main_title = "All Maps All Modes"
drawing_a_ROC_curve(prediction_of_reg_map,
                    description_of_main_title,
                    label)

#==================Drawing ROC curve===============

#==================separate by game modes as 4 data frame ==========

control_map = c("Ilios_Lighthouse", "Ilios_Ruins", "Ilios_Well",
                "Nepal_Shrine", "Nepal_Sanctum", "Nepal_Village",
                "Lijiang Tower_Garden", "Lijiang Tower_Night Market", "Lijiang Tower_Control Center",
                "Oasis_University", "Oasis_Gardens", "Oasis_City Center",
                "Busan_Downtown", "Busan_Sanctuary", "Busan_MEKA Base")

escort_map = c("Dorado_Attack", "Dorado_Defense",
               "Route 66_Attack", "Route 66_Defense",
               "Junkertown_Attack", "Junkertown_Defense",
               "Watchpoint: Gibraltar_Attack", "Watchpoint: Gibraltar_Defense",
               "Rialto_Attack", "Rialto_Defense")

hybrid_map = c("Numbani_Attack", "Numbani_Defense",
               "King's Row_Attack", "King's Row_Defense",
               "Blizzard World_Attack", "Blizzard World_Defense",
               "Eichenwalde_Attack", "Eichenwalde_Defense",
               "Hollywood_Attack", "Hollywood_Defense")

assault_map = c("Temple of Anubis_Attack", "Temple of Anubis_Defense",
                "Volskaya Industries_Attack", "Volskaya Industries_Defense",
                "Horizon Lunar Colony_Attack", "Horizon Lunar Colony_Defense",
                "Hanamura_Attack", "Hanamura_Defense")

Team_Fight_Data_control_Maps <- read_excel("F:/JARVIS/Documents/OWL_data/OWL_data_control_map.xlsx")
Team_Fight_Data_control_Maps = as.data.frame(Team_Fight_Data_control_Maps)
Team_Fight_Data_control_Maps = Team_Fight_Data_control_Maps[(!Team_Fight_Data_control_Maps$Blue_Team_Win %in% 0.5), ]


Team_Fight_Data_escort_Maps <- read_excel("F:/JARVIS/Documents/OWL_data/OWL_data_escort_map.xlsx")
Team_Fight_Data_escort_Maps = as.data.frame(Team_Fight_Data_escort_Maps)
Team_Fight_Data_escort_Maps = Team_Fight_Data_escort_Maps[(!Team_Fight_Data_escort_Maps$Blue_Team_Win %in% 0.5), ]

Team_Fight_Data_hybrid_Maps <- read_excel("F:/JARVIS/Documents/OWL_data/OWL_data_hybrid_map.xlsx")
Team_Fight_Data_hybrid_Maps = as.data.frame(Team_Fight_Data_hybrid_Maps)
Team_Fight_Data_hybrid_Maps = Team_Fight_Data_hybrid_Maps[(!Team_Fight_Data_hybrid_Maps$Blue_Team_Win %in% 0.5), ]

Team_Fight_Data_assault_Maps <- read_excel("F:/JARVIS/Documents/OWL_data/OWL_data_assault_map.xlsx")
Team_Fight_Data_assault_Maps = as.data.frame(Team_Fight_Data_assault_Maps)
Team_Fight_Data_assault_Maps = Team_Fight_Data_assault_Maps[(!Team_Fight_Data_assault_Maps$Blue_Team_Win %in% 0.5), ]

#==================separate by game modes as 4 data frame ==========

#==================forge 4 dataframes=====================
dummy_varible_matrix_control_map = dummy_varible_matrix_function(Map_and_round_type = Team_Fight_Data_control_Maps$Map_and_round_type)

control_map_data = cbind(Team_Fight_Data_control_Maps[2:7], dummy_varible_matrix_control_map, Team_Fight_Data_control_Maps$Blue_Team_Win)
colnames(control_map_data)[length(colnames(control_map_data))] = "Blue_Team_Win"


dummy_varible_matrix_escort_map = dummy_varible_matrix_function(Map_and_round_type = Team_Fight_Data_escort_Maps$Map_and_round_type)

escort_map_data = cbind(Team_Fight_Data_escort_Maps[2:7], dummy_varible_matrix_escort_map, Team_Fight_Data_escort_Maps$Blue_Team_Win)
colnames(escort_map_data)[length(colnames(escort_map_data))] = "Blue_Team_Win"


dummy_varible_matrix_hybrid_map = dummy_varible_matrix_function(Map_and_round_type = Team_Fight_Data_hybrid_Maps$Map_and_round_type)

hybrid_map_data = cbind(Team_Fight_Data_hybrid_Maps[2:7], dummy_varible_matrix_hybrid_map, Team_Fight_Data_hybrid_Maps$Blue_Team_Win)
colnames(hybrid_map_data)[length(colnames(hybrid_map_data))] = "Blue_Team_Win"


dummy_varible_matrix_assault_map = dummy_varible_matrix_function(Map_and_round_type = Team_Fight_Data_assault_Maps$Map_and_round_type)

colnames(dummy_varible_matrix_assault_map) = gsub("Map_and_round_type", "", colnames(dummy_varible_matrix_assault_map))

assault_map_data = cbind(Team_Fight_Data_assault_Maps[2:7], dummy_varible_matrix_assault_map, Team_Fight_Data_assault_Maps$Blue_Team_Win)
colnames(assault_map_data)[length(colnames(assault_map_data))] = "Blue_Team_Win"

#==================forge 4 dataframes=====================


#=================make 4 logistic regressions====================
reg_control_map = glm(Blue_Team_Win ~  length + UB + UR + FB + dummy_varible_matrix_control_map,
                      data = control_map_data,family = binomial(link = "logit"))

reg_control_map_no_length = glm(Blue_Team_Win ~ UB + UR + FB + dummy_varible_matrix_control_map,
                                data = control_map_data,family = binomial(link = "logit"))

reg_escort_map = glm(Blue_Team_Win ~  UB + UR + FB + dummy_varible_matrix_escort_map,
                     data = escort_map_data,family = binomial(link = "logit"))

reg_hybrid_map = glm(Blue_Team_Win ~  UB + UR + FB + dummy_varible_matrix_hybrid_map,
                     data = hybrid_map_data,family = binomial(link = "logit"))

reg_assault_map = glm(Blue_Team_Win ~  UB + UR + FB + dummy_varible_matrix_assault_map,
                      data = assault_map_data,family = binomial(link = "logit"))

#=================make 4 logistic regressions====================

#=================test the aic of reg_control_map and reg_control_map_no_length which is less=====

if (reg_control_map_no_length$aic <= reg_control_map$aic){
  cat("The AIC of control maps with length is ", reg_control_map$aic)
  cat("\n")
  cat("The AIC of control maps without length is ", reg_control_map_no_length$aic)
  cat("\n")
  print("aic of reg_control_map is greater than reg_control_map_no_length, we choose the model of no length")
  reg_control_map = reg_control_map_no_length
} else {
  print("aic of reg_control_map is less than reg_control_map_no_length, we choose the model with length")
  cat("The AIC of control maps with length is ", reg_control_map$aic )
  cat("\n")
  cat("The AIC of control maps without length is ", reg_control_map_no_length$aic)
  cat("\n")
}

#=================test the aic of reg_control_map and reg_control_map_no_length which is less=====

#=================drawing ROC curves and show the accuracy matrix for 4 models============

separated_data = training_set_and_testing_set(control_map_data, testing_set_number = 500)
training_seq = separated_data$training_set_seq
testing_seq = separated_data$testing_set_seq
prediction_of_reg_control_map = reg_control_map$fitted.values[training_seq]
prediction_of_reg_control_map = as.vector(prediction_of_reg_control_map)
label_control_map = control_map_data$Blue_Team_Win[training_seq]



par(mfcol = c(2, 2))

description_of_main_title = "Control Maps"
drawing_a_ROC_curve(prediction_of_reg_map = prediction_of_reg_control_map,
                    description_of_main_title,
                    label = label_control_map)


separated_data = training_set_and_testing_set(escort_map_data, testing_set_number = 500)
training_seq = separated_data$training_set_seq
testing_seq = separated_data$testing_set_seq
prediction_of_reg_escort_map = reg_escort_map$fitted.values[training_seq]
prediction_of_reg_escort_map = as.vector(prediction_of_reg_escort_map)
label_escort_map = escort_map_data$Blue_Team_Win[training_seq]



description_of_main_title = "Escort Maps"
drawing_a_ROC_curve(prediction_of_reg_map = prediction_of_reg_escort_map,
                    description_of_main_title,
                    label = label_escort_map)


separated_data = training_set_and_testing_set(hybrid_map_data, testing_set_number = 500)
training_seq = separated_data$training_set_seq
testing_seq = separated_data$testing_set_seq
prediction_of_reg_hybrid_map = reg_hybrid_map$fitted.values[training_seq]
prediction_of_reg_hybrid_map = as.vector(prediction_of_reg_hybrid_map)
label_hybrid_map = hybrid_map_data$Blue_Team_Win[training_seq]


description_of_main_title = "Hybrid Maps"
drawing_a_ROC_curve(prediction_of_reg_map = prediction_of_reg_hybrid_map,
                    description_of_main_title,
                    label = label_hybrid_map)


separated_data = training_set_and_testing_set(assault_map_data, testing_set_number = 500)
training_seq = separated_data$training_set_seq
testing_seq = separated_data$testing_set_seq
prediction_of_reg_assault_map = reg_assault_map$fitted.values[training_seq]
prediction_of_reg_assault_map = as.vector(prediction_of_reg_assault_map)
label_assault_map = assault_map_data$Blue_Team_Win[training_seq]


description_of_main_title = "Assault Maps"
drawing_a_ROC_curve(prediction_of_reg_map = prediction_of_reg_assault_map,
                    description_of_main_title,
                    label = label_assault_map)

#=================drawing ROC curves and show the accuracy matrix for 4 models============

#=================separate data into defense and attak modes===========
i = grepl("Defense", data_without_dummy_variables$Map_and_round_type)
defense_round_data = data_without_dummy_variables[i, ]

i = grepl("Attack", data_without_dummy_variables$Map_and_round_type)
attack_round_data = data_without_dummy_variables[i, ]

dummy_varible_matrix_defense_map = dummy_varible_matrix_function(Map_and_round_type = defense_round_data$Map_and_round_type)

dummy_varible_matrix_attack_map = dummy_varible_matrix_function(Map_and_round_type = attack_round_data$Map_and_round_type)

#=================separate data into defense and attak modes===========



#==================make logistis regression of two modes===============
reg_defense_map = glm(Blue_Team_Win ~ length + UB + UR + FB + dummy_varible_matrix_defense_map,
                      data = defense_round_data,family = binomial(link = "logit"))

reg_attack_map = glm(Blue_Team_Win ~ length + UB + UR + FB + dummy_varible_matrix_attack_map,
                     data = attack_round_data,family = binomial(link = "logit"))
#==================make logistis regression of two modes===============


#==================drawing ROC curves and show the accuracy matrix for three modes=============
separated_data = training_set_and_testing_set(defense_round_data, testing_set_number = 500)
training_seq = separated_data$training_set_seq
testing_seq = separated_data$testing_set_seq
prediction_of_reg_defense_map = reg_defense_map$fitted.values[training_seq]
prediction_of_reg_defense_map = as.vector(prediction_of_reg_defense_map)
label_defense_map = defense_round_data$Blue_Team_Win[training_seq]



par(mfcol = c(2, 2))

description_of_main_title = "Defense Round Maps"
drawing_a_ROC_curve(prediction_of_reg_map = prediction_of_reg_defense_map,
                    description_of_main_title,
                    label = label_defense_map)


separated_data = training_set_and_testing_set(attack_round_data, testing_set_number = 500)
training_seq = separated_data$training_set_seq
testing_seq = separated_data$testing_set_seq
prediction_of_reg_attack_map = reg_attack_map$fitted.values[training_seq]
prediction_of_reg_attack_map = as.vector(prediction_of_reg_attack_map)
label_attack_map = attack_round_data$Blue_Team_Win[training_seq]


description_of_main_title = "Attack Round Maps"
drawing_a_ROC_curve(prediction_of_reg_map = prediction_of_reg_attack_map,
                    description_of_main_title,
                    label = label_attack_map)


separated_data = training_set_and_testing_set(control_map_data, testing_set_number = 500)
training_seq = separated_data$training_set_seq
testing_seq = separated_data$testing_set_seq
prediction_of_reg_control_map = reg_control_map$fitted.values[training_seq]
prediction_of_reg_control_map = as.vector(prediction_of_reg_control_map)
label_control_map = control_map_data$Blue_Team_Win[training_seq]


description_of_main_title = "Control Maps"
drawing_a_ROC_curve(prediction_of_reg_map = prediction_of_reg_control_map,
                    description_of_main_title,
                    label = label_control_map)

#==================drawing ROC curves and show the accuracy matrix for three modes=============