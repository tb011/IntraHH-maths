#' @name IntraHHMaths
#'
#' @title Mathematical model to assess the impact of different levels of unequal intra-household food distribution within a household
#'
#' @param HCES Household Consumption Survey
#' 
#' @param FCT Food Composition Table
#'
#' @param Vulnerable_group Vulnerable group of interest: WRA, PSC, SAC, MEN
#'
#' @details This model has been simulated on hypothetical data.
#'
#' @import #packages
#'
#' @return model
#'
#' @export

# Function definitions (as used in main function)

## Data Ingestion and initial calculations
# DONE Step 1a: Input datasets (HCES & FCT)
# DONE Step 1b: Merge datasets together on food sub-group
# TODO Step 2a: Select eligible HHs 
# TODO Step 2b: Calculate AFE/AME
# TODO Step 2c: Calculate amount of food eaten according to AME/AFE
# DONE Step 3: Calculate total energy consumed from food per day
# IN-PROGRESS Step 4: Create energy_breakdown_table (shows how much energy comes from each food sub-group)

## Increase decrease foods
# Step 5: Create increase/decrease matrix
          # 1) Increase food-group (Increase 1 food group a time (Max 1 input), rest stay the same)
          # 2) Decrease food-group (Decrease 1 food group (max 1 input), rest stays the same)
          # 3) Increase 1 food group AND decrease 1 food group, rest stays same - legumes/cereal example) 
# Step 6: Create food_portions_ratio matrix for all increase and decrease ratios ( 0 - 100% ) 
          # Input csv file with default values (Equal, 10%, 20% ... 100%)
# Step 7: Create adjusted_grams_food matrix
# Step 8: Create adjusted_dietary_intakes for each micronutrient
# Step 9: TO DISCUSS: calculate the number of HH deficient / the cut off point / curve 

readData <- function(hces, fct) {
  hces <- read.csv(hces, fileEncoding = "UTF-8-BOM")
  fct <- read.csv(fct, fileEncoding = "UTF-8-BOM")
  dataset <- merge(hces, fct, by = "food_code")
  return(dataset)
}

#function to calculate AFE/AME based on target group. 
  # if WRA/PSC == F/ SAC == F use AFE, 
  # else if MEN, PSC == M/ SAC == M
  # else (stop) "AFE/AME cannot be calculated for this target group"

calcConsumedfpd <- function(dataset, food_group_adjust) {
  # wrap into an if else statement if food_group_compensate = empty then energy_food_group_compensate = 0 
  dataset[ ,"energy_consumed_food_pd"] <- dataset[ ,"amount_eaten_per_day"] / 100 * dataset[ ,"energy_kcal"]
  total_energy <- sum(dataset[ ,"energy_consumed_food_pd"])
  energy_food_group_adjust <- sum(dataset[which(dataset[ ,"food_group"]== food_group_adjust), "energy_consumed_food_pd"]) 
  energy_food_group_compensate <- 0 # placeholder (filled in via if else statement)
  energy_remaining_food <- (total_energy - energy_food_group_adjust) 
  energy_breakdown_table <- data.frame(total_energy, 
                                       energy_food_group_adjust,
                                       energy_food_group_compensate,
                                       energy_remaining_food)
  return(list(dataset = dataset, energy_breakdown_table = energy_breakdown_table))
}

# Main IntraHHMaths function
# IntraHHMaths <- function(HCES,
#                          FCT,
#                          target_group 
#                          food_group_adjust,
#                          food_group_compensate,
#                          ...) {
  
  food_group_adjust = "Starchy Staples" # for testing purposes
  
  dataset <- readData(hces = "../data/HCES.csv",
                      fct = "../data/FCT.csv")
  
  initial_calculations <- calcConsumedfpd(dataset, food_group_adjust)
  
  dataset <- initial_calculations$dataset
  
  energy_breakdown_table <-initial_calculations$energy_breakdown_table
  
#   return(model)
# }

# Function call example ;
# food_group_compensate parameter empty for scenario 1
# IntraHHMaths <- function(HCES,
#                          FCT,
#                          target_group = "WRA"
#                          food_group_adjust = "Starchy Staples")
