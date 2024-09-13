# Richter's Predictor: Modeling Earthquake Damage
# https://www.drivendata.org/competitions/57/nepal-earthquake/data/
# personal goal: practice tidyverse and caret; learn tidymodels

# --------------------------------------------------------------------------
# project setup
# --------------------------------------------------------------------------

rm(list = ls())

setwd("C:/GitHub/earthquake_damage/")

require(tidyverse)
require(corrplot)
require(rpart)
require(rpart.plot)


get_percentiles <- function(column){
  print(quantile(column, 
                 prob = c(0, 0.01, 0.05, 0.10, 0.25, 0.50, 
                          0.75, 0.90, 0.95, 0.99, 1)))
}


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
test$damage_grade <- 999 # to avoid NAs and will ignore later
                  
d <- bind_rows(train, test)

d %>%
  group_by(dataset) %>%
  count(damage_grade) # as expected, test buildings have NA

# missing values?
sum(is.na(d))

mv <- sapply(d, function(df){sum(is.na(df)==TRUE);})
mv <- data.frame(mv, variable = names(mv))
mv %>% arrange(desc(mv)) #no missing values    

# clean a bit
rm("train", 
   "test",
   "train_labels",
   "train_values",
   "test_values")


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
                  "height_percentage"
                  )
                  # "count_floors_pre_eq", #discrete?
                  # "count_families") #discrete?

vars_discrete <- c("land_surface_condition",                 
                   "foundation_type",
                   "roof_type",
                   "ground_floor_type",                      
                   "other_floor_type",
                   "position", 
                   "plan_configuration",
                   "legal_ownership_status",
                   "count_floors_pre_eq", #num?
                   "count_families") #num?

# correlation among numeric vars?
r <- d %>%
  select(-all_of(vars_discrete),-building_id, -dataset, count_floors_pre_eq,
         count_families) %>%
  cor()

round(r,2)

r_df <- data.frame(r)
r_df$x <- row.names(r_df)

write_csv(r_df, 
          file = "./temp/r_df.csv") #df to see in xls


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

rm("df_binary")

# summary of potential for prediction of binary variables with power S/M/L?
# 2: has_superstructure_adobe_mud - S
# 3: has_superstructure_mud_mortar_stone - L
# 4: has_superstructure_stone_flag - S
# 5: has_superstructure_cement_mortar_stone - XS
# 6: has_superstructure_mud_mortar_brick - S
# 7: has_superstructure_cement_mortar_brick - L
# 8: has_superstructure_timber - M
# 9: has_superstructure_bamboo - S
# 10: has_superstructure_rc_non_engineered - M
# 11: has_superstructure_rc_engineered - M
# 12: has_superstructure_other - XS
# 13: has_secondary_use - S
# 14: has_secondary_use_agriculture - S
# 15: has_secondary_use_hotel - S
# 16: has_secondary_use_rental - XS
# 17: has_secondary_use_institution - NO 
# 18: has_secondary_use_school - NO
# 19: has_secondary_use_industry - NO
# 20: has_secondary_use_health_post - NO
# 21: has_secondary_use_gov_office - NO
# 22: has_secondary_use_use_police - NO
# 23: has_secondary_use_other - NO

# remove from df the noise variables with no power
vars_to_remove_binary <- c("has_secondary_use_institution",
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

rm("df_discrete")

# summary of potential for prediction of discrete variables 
# tag S/M/H power of prediction
# 2: land_surface_condition - XS
# 3: foundation_type - M, bin u_w
# 4: roof_type - M
# 5: ground_floor_type - H, bin m_z
# 6: other_floor_type - H 
# 7: position - XS, remove?
# 8: plan_configuration - XS, bin: acfmnos, d, qu?
# 9: legal_ownership_status - S, bin: rw
# 10: 10: count_floors_pre_eq - L, bin 4 o 5+  
# 11: count_families - M, bin 2 o 3+

# no discrete variables to remove for now

# bin variables per EDA above
# 3: foundation_type - M, bin u_w
# 5: ground_floor_type - H, bin m_z
# 8: plan_configuration - XS, bin: acfmnos, d, qu?
# 9: legal_ownership_status - S, bin: rw
# 10: 10: count_floors_pre_eq - L, bin 4 o 5+  
# 11: count_families - M, bin 2 o 3+
d <- d %>%
  mutate(foundation_type_imp = case_match(foundation_type,
                                      c("u", "w") ~ "u_w",
                                      .default = foundation_type
                                      ),
         ground_floor_type_imp = case_match(ground_floor_type,
                                       c("m", "z") ~ "m_z",
                                       .default = ground_floor_type
                                       ),
         plan_configuration_imp = case_match(plan_configuration,
                                             c("a", "c", "f", "m", "n", "o", "s") ~ "a_c_f_m_n_o_s",
                                             .default = plan_configuration
                                             ),
         legal_ownership_status_imp = case_match(legal_ownership_status,
                                                 c("r", "w") ~ "r_w",
                                                 .default = legal_ownership_status),
         count_floors_pre_eq_imp = case_when(count_floors_pre_eq >= 4 ~ 4,
                                             .default = count_floors_pre_eq), #convert to factor later
         count_families_imp = case_when(count_families >= 3 ~ 3,
                                    .default = count_families) #convert to factor later
         )

# discrete vars to remove because they were imputed
vars_to_remove_discrete <- c("foundation_type",
                             "ground_floor_type",
                             "plan_configuration",
                             "legal_ownership_status",
                             "count_floors_pre_eq",
                             "count_families")

# iterate over all numeric variables and identify which have power of prediction
# and perform data prep as needed (trim, scale change)

df_numeric <- d %>% 
  select(damage_grade, all_of(vars_numeric))

for (i in 2:ncol(df_numeric)){
  print("--------------------------------------------------")
  cat(paste0(i, ": ",colnames(df_numeric[i])))

  print(summary(df_numeric[, i]))
  
  print(
    df_numeric %>%
    group_by(damage_grade) %>%
    summarise(median = median(df_numeric[, i]),
#              quantiles = quantile(df_numeric[, i])
              )
  )
  
  hist(df_numeric[, i], xlab=names(df_numeric[i]))
  
  boxplot(df_numeric[, i] ~ df_numeric$damage_grade,
                xlab=names(df_numeric[i]), horizontal = TRUE)
  
}

rm("df_numeric")

# summary of what was observed in loop and modify var as needed

# "geo_level_1_id",                        
# 0-30, median 12, not normal but can't transform - bin?, L power

# "geo_level_2_id",
# 0-1427, median 701, can't transform - remove no power.

# "geo_level_3_id",
# 0-12567, median 6271 - can't transform - remove no power

# "age",
# 0-995, median 15, need to trim due to outliers
get_percentiles(d$age) #cap at 100 (p99)
d$age_imp <- ifelse(d$age > 100, 100, d$age)

boxplot(d$age_imp ~ d$damage_grade,horizontal = TRUE)
# not normal but older buildings tend to get more damaged - some power

# "area_percentage",              
get_percentiles(d$area_percentage) # 1-100, median 7, cap p95 = 16
d$area_percentage_imp <- ifelse(d$area_percentage > 16, 16, d$area_percentage)
boxplot(d$area_percentage_imp ~ d$damage_grade,horizontal = TRUE)
# some power, damage=1 has higher median, # normal-ish

# "height_percentage",
get_percentiles(d$height_percentage) # 2-32, median 5, cap p99 = 11
d$height_percentage_imp <- ifelse(d$height_percentage > 11, 11, d$height_percentage)
boxplot(d$height_percentage_imp ~ d$damage_grade,horizontal = TRUE)
# tiny power but medians are similar. Will keep but unlikely a good pred

vars_to_remove_numeric <- c("geo_level_2_id", 
                            "geo_level_3_id", 
                            "age", "area_percentage", 
                            "height_percentage")

d <- d %>%
  select(-all_of(c(vars_to_remove_numeric, 
                   vars_to_remove_discrete,
                   vars_to_remove_binary)))

glimpse(d)


# ----------------------------------------------------------------------------
# prep for modeling
# ----------------------------------------------------------------------------

x <- d #delete later

# convert chr and num discrete to factor
vars_to_convert_to_factor <- c("damage_grade",
                               "land_surface_condition",
                               "roof_type",
                               "other_floor_type",
                               "position",
                               "foundation_type_imp",
                               "ground_floor_type_imp",
                               "plan_configuration_imp",
                               "legal_ownership_status_imp",
                               "count_floors_pre_eq_imp",
                               "count_families_imp"
                               )

d <- d %>% 
  mutate_at(vars_to_convert_to_factor, as.factor)

# vars_to_convert_to_binary that start with has
d <- d %>%
  mutate_at(vars(starts_with("has_")), as.logical)

# confirm only binary, factor and true num are present
glimpse(d)

# split train/test 
mytrain <- d %>%
  filter(dataset == "train") %>%
  select(-dataset)

mytest <- d %>%
  filter(dataset == "test") %>%
  select(-c(dataset, damage_grade))

rm(d)


# ----------------------------------------------------------------------------
# feature selection with tree
# ----------------------------------------------------------------------------

set.seed(97702)
mdl1_rpart <- rpart(damage_grade ~ . - building_id, 
                    data = mytrain, 
                    method = "class",
                    control = rpart.control(cp = 0))

# voy, figure out how to rpint

#fix don't run
rpart.plot(mdl1_rpart,
           type = 1,
           box.palette = "0",
           clip.right.labs = FALSE)

#mdl1_rpart

mdl1_rpart_predictions <- predict(mdl1_rpart, mytest, type = "class")
head(mdl1_rpart_predictions, 10)

mdl1_rpart_predictions <- data.frame(mdl1_rpart_predictions)
mdl1_rpart_predictions$building_id <- mytest$building_id

mdl1_rpart_predictions <- mdl1_rpart_predictions %>%
  select(building_id, mdl1_rpart_predictions)

mdl1_rpart_predictions <- mdl1_rpart_predictions %>%
  select(building_id, mdl1_rpart_predictions) %>%
  rename(damage_grade = mdl1_rpart_predictions
         )

write.csv(mdl1_rpart_predictions,
          file = "./output/mdl1_rpart_predictions.csv",
          row.names = FALSE)

#voy

# remove corr predictors using caret, y ver primero


# caret and tidymodels 
# glm
# rf/gbm
# nn
# kn

#to dos


