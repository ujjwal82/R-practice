

library(dplyr)
library(broom)
library(h2o)        # Professional grade ML pkg
library(tidyquant)  # Loads tidyverse and several other pkgs 
library(readxl)     # Super simple excel reader
library(lime)       # Explain complex black-box ML models
library(recipes)    # Preprocessing for machine learning

hr_data_raw_tbl <- read_excel(path = "HR Analytics/data/WA_Fn-UseC_-HR-Employee-Attrition.xlsx")

hr_data_organized_tbl <- hr_data_raw_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select(Attrition, everything())

recipe_obj <- hr_data_organized_tbl %>%
  recipe(formula = Attrition ~ .) %>%
  step_rm(EmployeeNumber) %>%
  step_zv(all_predictors()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  prep(data = hr_data_organized_tbl)

hr_data_bake_tbl <- bake(recipe_obj, newdata = hr_data_organized_tbl) 

h2o.init( port = 3030)


hr_data_bake_h2o <- as.h2o(hr_data_bake_tbl)

hr_data_split <- h2o.splitFrame(hr_data_bake_h2o, ratios = c(0.7, 0.15), seed = 1234)

train_h2o <- h2o.assign(hr_data_split[[1]], "train" ) # 70%
valid_h2o <- h2o.assign(hr_data_split[[2]], "valid" ) # 15%
test_h2o  <- h2o.assign(hr_data_split[[3]], "test" )  # 15%

y <- "Attrition"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 15
)

automl_leader <- automl_models_h2o@leader

explainer <- lime::lime(
  as.data.frame(train_h2o[,-1]), 
  model          = automl_leader, 
  bin_continuous = FALSE
)

explanation <- lime::explain(
  x              = as.data.frame(test_h2o[1:10,-1]), 
  explainer      = explainer, 
  n_labels       = 1, 
  n_features     = 4,
  n_permutations = 500,
  kernel_width   = 1
)

explanation

###
# Export the final result to PDF file.
###
pdf(file = "HR Analytics/HR analytics.pdf", 
    onefile = T, 
    paper = 'A4r',
    width = 28,
    height = 20)

plot_features(explanation) +
  labs(title = "HR Predictive Analytics: LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

dev.off()
