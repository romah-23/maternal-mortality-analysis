# Manuscript: Temporal, Regional and Sociodemographic Analysis
# of Maternal Mortality due to Abortion in Brazil (1996-2022)

# Loading necessary packages
library(readxl)
library(openxlsx) 
library(dplyr)     
library(prais)     

============================================================================== 
  # 1. Regional Analysis (North, Northeast, Southeast, South, Central-West, and Brazil)
  
  # Downloading Excel file
  df <- read.xlsx("table_region")
  
  # Transforming case units to logarithmic scale
  df <- df %>%
    mutate(north_log = log(north + 1),
           northeast_log = log(northeast + 1),
           southeast_log = log(southeast + 1),
           south_log = log(south + 1),
           centralwest_log = log(centralwest + 1),
           brazil_reg_log = log(brazil_reg + 1))
  
  # Applying the Prais-Winsten regression
  pw_north <- prais_winsten(north_log ~ year, data = df, index = "year")
  pw_northeast <- prais_winsten(northeast_log ~ year, data = df, index = "year")
  pw_southeast <- prais_winsten(southeast_log ~ year, data = df, index = "year")
  pw_south <- prais_winsten(south_log ~ year, data = df, index = "year")
  pw_centralwest <- prais_winsten(centralwest_log ~ year, data = df, index = "year")
  pw_brazil_reg <- prais_winsten(brazil_reg_log ~ year, data = df, index = "year")
  
  
  # Extracting predicted values (dependent variable = y)
  df <- df %>%
    mutate(Predicted_north = exp(pw_north$fitted.values) - 1,
           Predicted_northeast = exp(pw_northeast$fitted.values) - 1,
           Predicted_southeast = exp(pw_southeast$fitted.values) - 1,
           Predicted_south = exp(pw_south$fitted.values) - 1,
           Predicted_centralwest = exp(pw_centralwest$fitted.values) - 1,
           Predicted_brazil_reg = exp(pw_brazil_reg$fitted.values) - 1)
  
  # Creating a list to store results
  results_reg <- list()
  
  # Names of the regions
  regions <- c("north", "northeast", "southeast", "south", "centralwest", "brazil")
  
  # Calculating beta, standard error, and p-value for each region
  for (region in regions) {
    model <- get(paste0("pw_", region))
    results_reg[[region]] <- list(
      beta = coef(summary(model))["year", "Estimate"],
      standard_error = coef(summary(model))["year", "Std. Error"],
      p_value = summary(model)$coefficients["year", "Pr(>|t|)"]
    )
  }
  
  # Calculating the APC for each region using the beta coefficients
  apc_region <- list()
  
  for (region in names(results_reg)) {
    beta <- results_reg[[region]]$beta
    apc_region[[region]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Arbitrating critical t-value for 95% CI
  t_value <- 1.96
  
  # Initializing lists to store the 95% CI
  IC95_reg <- list()
  
  # Calculating upper and lower 95% CI for each region
  for (region in names(results_reg)) {
    beta <- results_reg[[region]]$beta
    standard_error <- results_reg[[region]]$standard_error
    
    IC95_upper <- round((-1 + 10^(beta + t_value * standard_error)) * 100, 2)
    IC95_lower <- round((-1 + 10^(beta - t_value * standard_error)) * 100, 2)
    
    IC95_reg[[region]] <- list(IC95_upper = IC95_upper, IC95_lower = IC95_lower)
  }
  
  # Function for calculating R²
  calculate_r_squared <- function(observed, predicted) {
    valid_indices <- !is.na(observed) & !is.na(predicted)
    observed <- observed[valid_indices]
    predicted <- predicted[valid_indices]
    
    if(length(observed) < 2) {
      return(NA)
    }
    
    # Calculating Total Sum of Squares (SST)
    mean_observed <- mean(observed)
    SST <- sum((observed - mean_observed)^2)
    
    # Calculating Sum of Squared Errors (SSE)
    SSE <- sum((observed - predicted)^2)
    
    # Calculating R²
    R_squared <- 1 - (SSE / SST)
    
    return(R_squared)
  }
  
  # Applying R² function for each region
  R2_reg <- data.frame(
    R2_north = calculate_r_squared(df$north, df$Predicted_north),
    R2_northeast = calculate_r_squared(df$northeast, df$Predicted_northeast),
    R2_southeast = calculate_r_squared(df$southeast, df$Predicted_southeast),
    R2_south = calculate_r_squared(df$south, df$Predicted_south),
    R2_centralwest = calculate_r_squared(df$centralwest, df$Predicted_centralwest),
    R2_brazil_reg = calculate_r_squared(df$brazil_reg, df$Predicted_brazil_reg)
  )
  
  ============================================================================== 
    # 2. Age Analysis [f1(10-14), f2(15-19), f3(20-29), f4(30-39), f5(40-49)
    # and Brazil]
    
    # Downloading Excel file
    df_age <- read.xlsx("table_age")
  
  # Transforming case units to logarithmic scale
  df_age <- df_age %>%
    mutate(f1_log = log(f1 + 1),
           f2_log = log(f2 + 1),
           f3_log = log(f3 + 1),
           f4_log = log(f4 + 1),
           f5_log = log(f5 + 1),
           brazil_age_log = log(brazil_age + 1))
  
  # Applying the Prais-Winsten regression
  pw_f1 <- prais_winsten(f1_log ~ year, data = df_age, index = "year")
  pw_f2 <- prais_winsten(f2_log ~ year, data = df_age, index = "year")
  pw_f3 <- prais_winsten(f3_log ~ year, data = df_age, index = "year")
  pw_f4 <- prais_winsten(f4_log ~ year, data = df_age, index = "year")
  pw_f5 <- prais_winsten(f5_log ~ year, data = df_age, index = "year")
  pw_brazil_age <- prais_winsten(brazil_age_log ~ year, data = df_age, index = "year")
  
  # Extracting predicted values (dependent variable = y)
  df_age <- df_age %>%
    mutate(Predicted_f1 = exp(pw_f1$fitted.values) - 1,
           Predicted_f2 = exp(pw_f2$fitted.values) - 1,
           Predicted_f3 = exp(pw_f3$fitted.values) - 1,
           Predicted_f4 = exp(pw_f4$fitted.values) - 1,
           Predicted_f5 = exp(pw_f5$fitted.values) - 1,
           Predicted_brazil_age = exp(pw_brazil_age$fitted.values) - 1)
  
  # Creating a list to store results
  results_age <- list()
  
  # Names of age ranges
  groups <- c("f1", "f2", "f3", "f4", "f5", "brazil_age")
  
  # Calculating beta, standard error, and p-value for each age range
  for (group in groups) {
    model <- get(paste0("pw_", group))
    results_age[[group]] <- list(
      beta = coef(summary(model))["year", "Estimate"],
      standard_error = coef(summary(model))["year", "Std. Error"],
      p_value = summary(model)$coefficients["year", "Pr(>|t|)"]
    )
  }
  
  # Calculating the APC for each age range using the beta coefficients
  apc_age <- list()
  
  for (group in names(results_age)) {
    beta <- results_age[[group]]$beta
    apc_age[[group]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Initializing lists to store the 95% CI
  IC95_age <- list()
  
  # Calculating upper and lower 95% CI for each age range
  for (age in names(results_age)) {
    beta <- results_age[[age]]$beta
    standard_error <- results_age[[age]]$standard_error
    
    IC95_upper <- round((-1 + 10^(beta + t_value * standard_error)) * 100, 2)
    IC95_lower <- round((-1 + 10^(beta - t_value * standard_error)) * 100, 2)
    
    IC95_age[[age]] <- list(IC95_upper = IC95_upper, IC95_lower = IC95_lower)
  }
  
  # Applying R² function for each age range
  R2_age <- data.frame(
    R2_f1 = calculate_r_squared(df_age$f1, df_age$Predicted_f1),
    R2_f2 = calculate_r_squared(df_age$f2, df_age$Predicted_f2),
    R2_f3 = calculate_r_squared(df_age$f3, df_age$Predicted_f3),
    R2_f4 = calculate_r_squared(df_age$f4, df_age$Predicted_f4),
    R2_f5 = calculate_r_squared(df_age$f5, df_age$Predicted_f5),
    R2_brazil_age = calculate_r_squared(df_age$brazil_age, df_age$Predicted_brazil_age)
  )
  
  ============================================================================== 
    # 3. Civil Status Analysis (single, married, widowed, separated, and Brazil)
    
    # Downloading Excel file
    df_marital_status <- read.xlsx("table_marital_status")
  
  # Transforming case units to logarithmic scale
  df_marital_status <- df_marital_status %>%
    mutate(single_log = log(single + 1),
           married_log = log(married + 1),
           widowed_log = log(widowed + 1),
           separated_log = log(separated + 1),
           brazil_ms_log = log(brazil_ms + 1))
  
  # Applying the Prais-Winsten regression
  pw_single <- prais_winsten(single_log ~ year, data = df_marital_status, index = "year")
  pw_married <- prais_winsten(married_log ~ year, data = df_marital_status, index = "year")
  pw_widowed <- prais_winsten(widowed_log ~ year, data = df_marital_status, index = "year")
  pw_separated <- prais_winsten(separated_log ~ year, data = df_marital_status, index = "year")
  pw_brazil_ms <- prais_winsten(brazil_ms_log ~ year, data = df_marital_status, index = "year")
  
  # Extracting predicted values (dependent variable = y)
  df_marital_status <- df_marital_status %>%
    mutate(Predicted_single = exp(pw_single$fitted.values) - 1,
           Predicted_married = exp(pw_married$fitted.values) - 1,
           Predicted_widowed = exp(pw_widowed$fitted.values) - 1,
           Predicted_separated = exp(pw_separated$fitted.values) - 1,
           Predicted_brazil_ms = exp(pw_brazil_ms$fitted.values) - 1)
  
  # Creating a list to store results
  results_ms <- list()
  
  # Names of civil statuses
  statuses <- c("single", "married", "widowed", "separated", "brazil_ms")
  
  # Calculating beta, standard error, and p-value for each civil status
  for (status in statuses) {
    model <- get(paste0("pw_", status))
    results_ms[[status]] <- list(
      beta = coef(summary(model))["year", "Estimate"],
      standard_error = coef(summary(model))["year", "Std. Error"],
      p_value = summary(model)$coefficients["year", "Pr(>|t|)"]
    )
  }
  
  # Calculating the APC for each civil status using the beta coefficients
  apc_ms <- list()
  
  for (status in names(results_ms)) {
    beta <- results_ms[[status]]$beta
    apc_ms[[status]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Initializing lists to store the 95% CI
  IC95_ms <- list()
  
  # Calculating upper and lower 95% CI for each civil status
  for (status in names(results_ms)) {
    beta <- results_ms[[status]]$beta
    standard_error <- results_ms[[status]]$standard_error
    
    IC95_upper <- round((-1 + 10^(beta + t_value * standard_error)) * 100, 2)
    IC95_lower <- round((-1 + 10^(beta - t_value * standard_error)) * 100, 2)
    
    IC95_ms[[status]] <- list(IC95_upper = IC95_upper, IC95_lower = IC95_lower)
  }
  
  # Applying R² function for each civil status
  R2_ms <- data.frame(
    R2_single = calculate_r_squared(df_marital_status$single, df_marital_status$Predicted_single),
    R2_married = calculate_r_squared(df_marital_status$married, df_marital_status$Predicted_married),
    R2_widowed = calculate_r_squared(df_marital_status$widowed, df_marital_status$Predicted_widowed),
    R2_separated = calculate_r_squared(df_marital_status$separated, df_marital_status$Predicted_separated),
    R2_brazil_ms = calculate_r_squared(df_marital_status$brazil_ms, df_marital_status$Predicted_brazil_ms)
  )
  
  ============================================================================== 
    # 4. Race/Ethnicity Analysis (White, Black, Yellow, Brown, Indigenous, and Brazil)
    
    # Downloading Excel file
    df_race <- read.xlsx("table_race")
  
  # Transforming case units to logarithmic scale
  df_race <- df_race %>%
    mutate(white_log = log(white + 1),
           black_log = log(black + 1),
           yellow_log = log(yellow + 1),
           brown_log = log(brown + 1),
           indigenous_log = log(indigenous + 1),
           brazil_race_log = log(brazil_race + 1))
  
  # Applying the Prais-Winsten regression
  pw_white <- prais_winsten(white_log ~ year, data = df_race, index = "year")
  pw_black <- prais_winsten(black_log ~ year, data = df_race, index = "year")
  pw_yellow <- prais_winsten(yellow_log ~ year, data = df_race, index = "year")
  pw_brown <- prais_winsten(brown_log ~ year, data = df_race, index = "year")
  pw_indigenous <- prais_winsten(indigenous_log ~ year, data = df_race, index = "year")
  pw_brazil_race <- prais_winsten(brazil_race_log ~ year, data = df_race, index = "year")
  
  # Extracting predicted values (dependent variable = y)
  df_race <- df_race %>%
    mutate(Predicted_white = exp(pw_white$fitted.values) - 1,
           Predicted_black = exp(pw_black$fitted.values) - 1,
           Predicted_yellow = exp(pw_yellow$fitted.values) - 1,
           Predicted_brown = exp(pw_brown$fitted.values) - 1,
           Predicted_indigenous = exp(pw_indigenous$fitted.values) - 1,
           Predicted_brazil_race = exp(pw_brazil_race$fitted.values) - 1)
  
  # Creating a list to store results
  results_race <- list()
  
  # Names of race/ethnicity categories
  races <- c("white", "black", "yellow", "brown", "indigenous", "brazil_race")
  
  # Calculating beta, standard error, and p-value for each race/ethnicity
  for (race in races) {
    model <- get(paste0("pw_", race))  # Getting the corresponding model
    results_race[[race]] <- list(
      beta = coef(summary(model))["year", "Estimate"],
      standard_error = coef(summary(model))["year", "Std. Error"],
      p_value = summary(model)$coefficients["year", "Pr(>|t|)"]
    )
  }
  
  # Calculating the APC for each race/ethnicity using the beta coefficients
  apc_race <- list()
  
  for (race in names(results_race)) {
    beta <- results_race[[race]]$beta
    apc_race[[race]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Initializing lists to store the 95% CI
  IC95_race <- list()
  
  # Calculating upper and lower 95% CI for each race/ethnicity
  for (race in names(results_race)) {
    beta <- results_race[[race]]$beta
    standard_error <- results_race[[race]]$standard_error
    
    IC95_upper <- round((-1 + 10^(beta + t_value * standard_error)) * 100, 2)
    IC95_lower <- round((-1 + 10^(beta - t_value * standard_error)) * 100, 2)
    
    IC95_race[[race]] <- list(IC95_upper = IC95_upper, IC95_lower = IC95_lower)
  }
  
  # Applying R² function for each race/ethnicity
  R2_race <- data.frame(
    R2_white = calculate_r_squared(df_race$white, df_race$Predicted_white),
    R2_black = calculate_r_squared(df_race$black, df_race$Predicted_black),
    R2_yellow = calculate_r_squared(df_race$yellow, df_race$Predicted_yellow),
    R2_brown = calculate_r_squared(df_race$brown, df_race$Predicted_brown),
    R2_indigenous = calculate_r_squared(df_race$indigenous, df_race$Predicted_indigenous),
    R2_brazil_race = calculate_r_squared(df_race$brazil_race, df_race$Predicted_brazil_race)
  )
  
  ============================================================================== 
    # 5. Education Analysis [e1(1-3), e2(4-7), e3(8-11), e4(12 or more), none, and Brazil]
    
    # Downloading Excel file
    df_education <- read.xlsx("table_education")
  
  # Transforming case units to logarithmic scale
  df_education <- df_education %>%
    mutate(e1_log = log(e1 + 1),
           e2_log = log(e2 + 1),
           e3_log = log(e3 + 1),
           e4_log = log(e4 + 1),
           none_log = log(none + 1),
           brazil_education_log = log(brazil_education + 1))
  
  # Applying the Prais-Winsten regression
  pw_e1 <- prais_winsten(e1_log ~ year, data = df_education, index = "year")
  pw_e2 <- prais_winsten(e2_log ~ year, data = df_education, index = "year")
  pw_e3 <- prais_winsten(e3_log ~ year, data = df_education, index = "year")
  pw_e4 <- prais_winsten(e4_log ~ year, data = df_education, index = "year")
  pw_none <- prais_winsten(none_log ~ year, data = df_education, index = "year")
  pw_brazil_education <- prais_winsten(brazil_education_log ~ year, data = df_education, index = "year")
  
  # Extracting predicted values (dependent variable = y)
  df_education <- df_education %>%
    mutate(Predicted_e1 = exp(pw_e1$fitted.values) - 1,
           Predicted_e2 = exp(pw_e2$fitted.values) - 1,
           Predicted_e3 = exp(pw_e3$fitted.values) - 1,
           Predicted_e4 = exp(pw_e4$fitted.values) - 1,
           Predicted_none = exp(pw_none$fitted.values) - 1,
           Predicted_brazil_education = exp(pw_brazil_education$fitted.values) - 1)
  
  # Creating a list to store results
  results_education <- list()
  
  # Names of education ranges
  education_levels <- c("e1", "e2", "e3", "e4", "none", "brazil_education")
  
  # Calculating beta, standard error, and p-value for each education level
  for (level in education_levels) {
    model <- get(paste0("pw_", level))  # Getting the corresponding model
    results_education[[level]] <- list(
      beta = coef(summary(model))["year", "Estimate"],
      standard_error = coef(summary(model))["year", "Std. Error"],
      p_value = summary(model)$coefficients["year", "Pr(>|t|)"]
    )
  }
  
  # Calculating the APC for each education level using the beta coefficients
  apc_education <- list()
  
  for (level in names(results_education)) {
    beta <- results_education[[level]]$beta
    apc_education[[level]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Initializing lists to store the 95% CI
  IC95_education <- list()
  
  # Calculating upper and lower 95% CI for each education level
  for (level in names(results_education)) {
    beta <- results_education[[level]]$beta
    standard_error <- results_education[[level]]$standard_error
    
    IC95_upper <- round((-1 + 10^(beta + t_value * standard_error)) * 100, 2)
    IC95_lower <- round((-1 + 10^(beta - t_value * standard_error)) * 100, 2)
    
    IC95_education[[level]] <- list(IC95_upper = IC95_upper, IC95_lower = IC95_lower)
  }
  
  # Applying R² function for each education level
  R2_education <- data.frame(
    R2_e1 = calculate_r_squared(df_education$e1, df_education$Predicted_e1),
    R2_e2 = calculate_r_squared(df_education$e2, df_education$Predicted_e2),
    R2_e3 = calculate_r_squared(df_education$e3, df_education$Predicted_e3),
    R2_e4 = calculate_r_squared(df_education$e4, df_education$Predicted_e4),
    R2_none = calculate_r_squared(df_education$none, df_education$Predicted_none),
    R2_brazil_education = calculate_r_squared(df_education$brazil_education, df_education$Predicted_brazil_education)
  )
  
  ============================================================================== 
    # 6. Regional Graph
    
    # Loading necessary packages
    library(ggplot2)
  library(tidyr)
  library(scales)
  
  # Transformed data
  df_long <- df %>%
    pivot_longer(cols = c(north, northeast, southeast, south, centralwest, brazil_reg,
                          Predicted_north, Predicted_northeast, Predicted_southeast, Predicted_south, Predicted_centralwest, Predicted_brazil),
                 names_to = "type",
                 values_to = "value") %>%
    mutate(type = factor(type, levels = c("north", "northeast", "southeast", "south", "centralwest", "brazil_reg",
                                          "Predicted_north", "Predicted_northeast", "Predicted_southeast", "Predicted_south", "Predicted_centralwest", "Predicted_brazil"),
                         labels = c("North - Observed", "Northeast - Observed", "Southeast - Observed", "South - Observed", "Central-West - Observed", "Brazil - Observed",
                                    "North - Predicted", "Northeast - Predicted", "Southeast - Predicted", "South - Predicted", "Central-West - Predicted", "Brazil - Predicted")))
  
  # Filtering data to display only observed and predicted from Brazil
  df_long_filtered <- df_long %>%
    filter(grepl("Observed", type) | type == "Brazil - Predicted")
  
  # Defining colors and styles
  colors <- c("North - Observed" = "#5B9BD5",
              "Northeast - Observed" = "#A5A5A5",
              "Southeast - Observed" = "#4472C4",
              "South - Observed" = "#255E91",
              "Central-West - Observed" = "#636363",
              "Brazil - Observed" = "#000000",
              "Brazil - Predicted" = "#000000")
  
  styles <- c("North - Observed" = "solid",
              "Northeast - Observed" = "solid",
              "Southeast - Observed" = "solid",
              "South - Observed" = "solid",
              "Central-West - Observed" = "solid",
              "Brazil - Observed" = "solid",
              "Brazil - Predicted" = "dashed")
  
  # Centralizing the year variable around 1996
  df$year_centered <- df$year - 1996
  
  # Applying the Prais-Winsten regression to Brazil with the centered year
  pw_brazil <- prais_winsten(brazil_log ~ year_centered, data = df, index = "year_centered")
  
  # Extracting coefficients from the fitted regression
  beta_brazil <- coef(summary(pw_brazil))["year_centered", "Estimate"]
  intercept_brazil <- coef(summary(pw_brazil))["(Intercept)", "Estimate"]
  
  # Checking the regression equation
  paste0("Brazil: y = ", round(beta_brazil, 4), " * year_centered + ", round(intercept_brazil, 4))
  
  # Updating predicted values after centralization
  df <- df %>%
    mutate(Predicted_brazil = exp(pw_brazil$fitted.values) - 1)
  
  # Updating the graph to reflect the regression adjustment
  df_long <- df %>%
    pivot_longer(cols = c(north, northeast, southeast, south, centralwest, brazil_reg,
                          Predicted_north, Predicted_northeast, Predicted_southeast, Predicted_south, Predicted_centralwest, Predicted_brazil),
                 names_to = "type",
                 values_to = "value") %>%
    mutate(type = factor(type, levels = c("north", "northeast", "southeast", "south", "centralwest", "brazil_reg",
                                          "Predicted_north", "Predicted_northeast", "Predicted_southeast", "Predicted_south", "Predicted_centralwest", "Predicted_brazil"),
                         labels = c("North - Observed", "Northeast - Observed", "Southeast - Observed", "South - Observed", "Central-West - Observed", "Brazil - Observed",
                                    "North - Predicted", "Northeast - Predicted", "Southeast - Predicted", "South - Predicted", "Central-West - Predicted", "Brazil - Predicted")))
  
  # Filtering data to display only observed and predicted from Brazil
  df_long_filtered <- df_long %>%
    filter(grepl("Observed", type) | type == "Brazil - Predicted")
  
  # Creating the updated graph with titles and legends in English
  regional_graph <- ggplot(df_long_filtered, aes(x = year, y = value, color = type, linetype = type)) +
    geom_line(size = 1) +
    geom_line(data = df_long_filtered %>% filter(type == "Brazil - Predicted"), aes(x = year, y = value), linetype = "dashed", size = 1) +
    geom_point(data = df_long_filtered %>% filter(type == "Brazil - Predicted"), size = 2) +
    scale_color_manual(values = colors,
                       labels = c("North - Observed", "Northeast - Observed", "Southeast - Observed", "South - Observed", "Central-West - Observed", "Brazil - Observed", "Brazil - Predicted")) +
    scale_linetype_manual(values = styles) +
    labs(
      x = NULL,  # Removing title from x-axis
      y = "Rates per million",  # Title of y-axis in English
      color = "Legend"
    ) +
    guides(linetype = "none") +
    scale_x_continuous(breaks = seq(1996, 2022, by = 2)) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.background = element_rect(fill = "white"),
      legend.position = "bottom",
      panel.grid.major = element_blank(),  # Removing major grid lines
      panel.grid.minor = element_blank()   # Removing minor grid lines
    )
  
  # Displaying and saving the updated graph
  print(regional_graph)
  ggsave("regional_graph.png", plot = regional_graph, width = 10, height = 6, dpi = 300)
  
  # Displaying the regression equation for Brazil
  equation_brazil <- paste0("Brazil: y = ", round(beta_brazil, 4), " * (year - 1996) + ", round(intercept_brazil, 4))
  predicted_1996 <- exp(intercept_brazil) - 1
  print(equation_brazil)
  
  ================================================================================
    # 7. Package Citation
    
    citation("readxl")
  citation("openxlsx") 
  citation("dplyr")     
  citation("prais")
  citation("ggplot2")
  citation("tidyr")
  citation("scales")
  citation()
  