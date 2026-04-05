library(readxl)
library(dplyr)

# 1. Fixed the working directory path
setwd("/Users/resh/Downloads/DS/DS_TEB2043/Lab 7")

titanic_lab7a <- read.csv("titanic.csv")

# 2. Fixed variable name and made 'fare' lowercase
titanic_sortbyfare = arrange(titanic_lab7a, desc(fare))

write.csv(titanic_sortbyfare, "titanic_sortedbyfare.csv")

titanic_lab7b <- read.csv("titanic_sortedbyfare.csv")

# 3. Made all column names lowercase to match the CSV dataset
titanic_clean <- titanic_lab7b %>%
  select(survived, pclass, sex, age, fare, embarked)

write.csv(titanic_clean, "titanic_clean.csv")

# --- general insights ---
total_pax <- nrow(titanic_clean)
overall_survived <- sum(titanic_clean$survived == 1, na.rm = TRUE)
overall_surv_rate <- round((overall_survived / total_pax) * 100, 1)

# --- age groups ---
children <- filter(titanic_clean, age >= 0 & age <= 12)
teenagers <- filter(titanic_clean, age >= 13 & age <= 17)
adults <- filter(titanic_clean, age >= 18 & age <= 59)
seniors <- filter(titanic_clean, age >= 60)

# --- survival rate by age ---
pct_children <- round((nrow(children) / total_pax) * 100, 1)
child_surv_rate <- round((sum(children$survived == 1, na.rm = TRUE) / nrow(children)) * 100, 1)
adult_surv_rate <- round((sum(adults$survived == 1, na.rm = TRUE) / nrow(adults)) * 100, 1)

# --- largest age group ---
age_counts <- c("Children" = nrow(children), "Teenagers" = nrow(teenagers), 
                "Adults" = nrow(adults), "Seniors" = nrow(seniors))
largest_group <- names(which.max(age_counts))
pct_largest <- round((max(age_counts) / total_pax) * 100, 1)

# --- embarked from Cherbourg ---
cherbourg_pax <- filter(titanic_clean, embarked == "C")
pct_c_surv <- round((sum(cherbourg_pax$survived == 1, na.rm = TRUE) / nrow(cherbourg_pax)) * 100, 1)
pct_c_3rd <- round((sum(cherbourg_pax$pclass == 3, na.rm = TRUE) / nrow(cherbourg_pax)) * 100, 1)

# --- passengers who paid 0 fare ---
zero_fare <- filter(titanic_clean, fare == 0)
pct_zero_1st <- round((sum(zero_fare$pclass == 1, na.rm = TRUE) / nrow(zero_fare)) * 100, 1)
pct_zero_3rd <- round((sum(zero_fare$pclass == 3, na.rm = TRUE) / nrow(zero_fare)) * 100, 1)

# --- youngest passenger ---
youngest_age <- min(titanic_clean$age, na.rm = TRUE)
youngest_pax <- filter(titanic_clean, age == youngest_age)
youngest_status <- ifelse(youngest_pax$survived[1] == 1, "survived", "did not survive")

# --- female First Class Survival ---
female_first <- filter(titanic_clean, sex == "female", pclass == 1)
pct_ff_surv <- round((sum(female_first$survived == 1, na.rm = TRUE) / nrow(female_first)) * 100, 1)

# --- male First Class Survival ---
male_first <- filter(titanic_clean, sex == "male", pclass == 1)
pct_mf_surv <- round((sum(male_first$survived == 1, na.rm = TRUE) / nrow(male_first)) * 100, 1)

cat("--- General Overview ---\n")
cat(sprintf("1. Overall Survival: Out of the %d passengers analyzed, the overall survival rate was %.1f%%.\n\n", total_pax, overall_surv_rate))

cat("--- Age Category Breakdown ---\n")
cat(sprintf("2. The largest age group on board was %s, making up %.1f%% of the passengers.\n", largest_group, pct_largest))
cat(sprintf("3. Children (ages 0-12) made up %.1f%% of the passengers, and %.1f%% of them survived.\n", pct_children, child_surv_rate))
cat(sprintf("4. Comparatively, %.1f%% of Children survived, while only %.1f%% of Adults (ages 18-59) survived.\n\n", child_surv_rate, adult_surv_rate))

cat("--- Specific Demographics and Fares ---\n")
cat(sprintf("5. %.1f%% of female passengers in First Class survived.\n", pct_ff_surv))
cat(sprintf("6. %.1f%% of passengers who embarked from Cherbourg survived, and %.1f%% of them were from Third Class.\n", pct_c_surv, pct_c_3rd))
cat(sprintf("7. %.1f%% of passengers who paid a fare of 0 were in First Class, and %.1f%% were in Third Class.\n", pct_zero_1st, pct_zero_3rd))
cat(sprintf("8. The youngest passenger on board was %.2f years old and they %s.\n", youngest_age, youngest_status))