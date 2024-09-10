# Richter's Predictor: Modeling Earthquake Damage
# https://www.drivendata.org/competitions/57/nepal-earthquake/data/
# personal goal: practice tidyverse and caret; learn tidymodels

# --------------------------------------------------------------------------
# project setup
# --------------------------------------------------------------------------

rm(list = ls())

setwd("C:/GitHub/earthquake_damage/")

require(tidyverse)


# --------------------------------------------------------------------------
# load data and do basic exploration
# --------------------------------------------------------------------------

# load data
train_labels <- read.csv("./data/train_labels.csv") 
train_values <- read.csv("./data/train_values.csv") 
test_values <- read.csv("./data/test_values.csv")

# confirm dimensions
dim(train_labels) #260601x2
dim(train_values) #260601x39
dim(test_values) #86868x39

# build train and clean
train <- inner_join(train_labels, train_values, by = "building_id")
#rm(list = c("train_labels", "train_values"))

#combine train and test set
train$dataset <- "train"

test <- test_values
test$dataset <- "test"
                  
d <- bind_rows(train, test)

d %>%
  group_by(dataset) %>%
  count(damage_grade) # as expected, test buildings have NA

# missing values?
sum(is.na(d))

mv <- sapply(d, function(df){sum(is.na(df)==TRUE);})
mv <- data.frame(mv, variable = names(mv))
mv %>% arrange(desc(mv)) #good, only damage_grade for test group has                           


# --------------------------------------------------------------------------
# data exploration and preparation
# --------------------------------------------------------------------------

# separate attributes by type for eda and prediction power exploration

vars_binary <- c("has_superstructure_adobe_mud",           
                 "has_superstructure_mud_mortar_stone",   
                 "has_superstructure_stone_flag",          
                 "has_superstructure_cement_mortar_stone", 
                 "has_superstructure_mud_mortar_brick",   
                 "has_superstructure_cement_mortar_brick", 
                 "has_superstructure_timber",              
                 "has_superstructure_bamboo",             
                 "has_superstructure_rc_non_engineered",   
                 "has_superstructure_rc_engineered",       
                 "has_superstructure_other",              
                 "has_secondary_use",                     
                 "has_secondary_use_agriculture",          
                 "has_secondary_use_hotel",                
                 "has_secondary_use_rental",              
                 "has_secondary_use_institution",          
                 "has_secondary_use_school",               
                 "has_secondary_use_industry",            
                 "has_secondary_use_health_post",          
                 "has_secondary_use_gov_office",          
                 "has_secondary_use_use_police",          
                 "has_secondary_use_other")

vars_numeric <- c("geo_level_1_id",                        
                  "geo_level_2_id",
                  "geo_level_3_id",
                  "age",
                  "area_percentage",                        
                  "height_percentage")

vars_discrete <- c("land_surface_condition",                 
                   "foundation_type",
                   "roof_type",
                   "ground_floor_type",                      
                   "other_floor_type",
                   "position", 
                   "plan_configuration",
                   "legal_ownership_status")

# iterate over all binary variables and identify which have power of prediction
# and perform data prep as needed(binning)

df_binary <- d %>% 
  select(damage_grade, starts_with("has"))

for (i in 2:ncol(df_binary)){
  print("--------------------------------------------------")
  cat(paste0(i, ": ",colnames(df_binary[i])))
  t <- table(df_binary[, i], df_binary[, 1]) #[, 1] == damage_grade
  print(t)
  print(round(prop.table(t, 2),2))
}

# 2: has_superstructure_adobe_mud - good power
# 3: has_superstructure_mud_mortar_stone - great power   
# 4: has_superstructure_stone_flag - small power    
# 5: has_superstructure_cement_mortar_stone - tiny power  
# 6: has_superstructure_mud_mortar_brick - small power
# 7: has_superstructure_cement_mortar_brick - big power 
# 8: has_superstructure_timber - some power   
# 9: has_superstructure_bamboo - small power   
# 10: has_superstructure_rc_non_engineered - some power   
# 11: has_superstructure_rc_engineered good power  
# 12: has_superstructure_other - tiny power   
# 13: has_secondary_use - some power  
# 14: has_secondary_use_agriculture - tiny power   
# 15: has_secondary_use_hotel - some power  
# 16: has_secondary_use_rental - tiny power   
# 17: has_secondary_use_institution - no power  
# 18: has_secondary_use_school - no power   
# 19: has_secondary_use_industry - no power   
# 20: has_secondary_use_health_post - no power  
# 21: has_secondary_use_gov_office - no power   
# 22: has_secondary_use_use_police - no power  
# 23: has_secondary_use_other - no power

# remove from df the noise variables with no power
binary_vars_to_remove <- c("has_secondary_use_institution",
                           "has_secondary_use_school",
                           "has_secondary_use_industry",
                           "has_secondary_use_health_post",
                           "has_secondary_use_gov_office",
                           "has_secondary_use_use_police",
                           "has_secondary_use_other")



# iterate over all discrete variables and identify which have power of prediction
# and perform data prep as needed (binning)

df_discrete <- d %>% 
  select(damage_grade, all_of(vars_discrete))

for (i in 2:ncol(df_discrete)){
  print("--------------------------------------------------")
  cat(paste0(i, ": ",colnames(df_discrete[i])))
  t <- table(df_discrete[, i], df_discrete[, 1]) #[, 1] == damage_grade
  print(t)
  print(round(prop.table(t, 2),2))
}

#voy


# no change
"building_id"

# y
"damage_grade"


# tbd
"count_floors_pre_eq"
"count_families"  
"dataset" 

# coece variables to right type                 


# remove all vars from above not useful
#d <- d %>% 
#select(-any_of(binary_vars_to_remove))




# caret and tidymodels 
# tree
# glm
# rf/gbm
# nn
# kn

