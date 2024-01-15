#Loading libraries required
library(dplyr)
library(readxl)
library(tidyr)
library(survival)
library(survminer)
library(ggplot2)

#Read in the data 
medical <- read_excel("medical.xlsx")


###Question 1
# (1) Compare frequency distribution
# Numeric variables
summary_table <- medical %>%
  group_by(died) %>%
  summarize(
    median_age = median(age),
    iqr_age = IQR(age),
    median_los = median(los),
    iqr_los = IQR(los)
  )
print(summary_table)
#Comparison test using  Kruskal-wallis test
# Kruskal-Wallis test for numerical variables
kw_test_age <- kruskal.test(age ~ died, data = medical)
kw_test_los <- kruskal.test(los ~ died, data = medical)

# Print the results
print(kw_test_age)
print(kw_test_los)

# Categorical variables
cat_variables <- c("gender", "vacc", "flutype", "com.respir", "com.cardio", "com.renal", "com.immun", "com.metab", "com.neuro", "com.obes", "early")
table_list <- lapply(cat_variables, function(var) {
  table_data <- table(medical[[var]], medical$died)
  chi2_test <- chisq.test(table_data)
  p_value <- chi2_test$p.value
  return(data.frame(table_data, p_value))
})
names(table_list) <- cat_variables
print(table_list)


###Question two
#Exploring association between outcome and exposures
# Odds Ratio and logistic regression
exposures <- c("early", "vacc")
# Create age groups
medical$age_group <- cut(medical$age, breaks = c(0, 60, 79, Inf), labels = c("<60", "60-79", "80+"), right = FALSE)

for (exposure in exposures) {
  # Calculate Odds Ratio
  odds_ratio <- medical %>%
    group_by(died, !!sym(exposure)) %>%
    summarize(count = n()) %>%
    pivot_wider(names_from = !!sym(exposure), values_from = count) %>%
    filter(!is.na(`0`) & !is.na(`1`)) %>%
    transmute(
      OR = (`1` / `0`),
      CI_lower = exp(log(OR) - 1.96 * sqrt(1 / `0` + 1 / `1`)),
      CI_upper = exp(log(OR) + 1.96 * sqrt(1 / `0` + 1 / `1`))
    )
  
  cat(paste0("\n(2) Association between death and ", exposure, "\n"))
  print(odds_ratio)}
#Logistic regression
logistic_model <- glm(died ~ age_group + gender + com.respir + com.cardio + com.renal +
                        com.immun + com.metab + com.neuro + com.obes,
                      data = medical, family = "binomial")

odds_ratios <- exp(coef(logistic_model))
print("The adjusted odds ratio are :")
print(odds_ratios)


###Question 3
# Explore the association using survival analysis
# Create a survival object
surv_obj <- Surv(medical$los, medical$died)

# Kaplan-Meier curves
km_oseltamivir <- survfit(surv_obj ~ early, data = medical)
km_vaccination <- survfit(surv_obj ~ vacc, data = medical)

# Display Kaplan-Meier curves
cat("\n(3) Kaplan-Meier Curves\n")
plot(km_oseltamivir, main = "Kaplan-Meier Curves for Early Oseltamivir", xlab = "Time (Days)", ylab = "Survival Probability", col = c("blue", "red"))
legend("topright", legend = c("Late Oseltamivir", "Early Oseltamivir"), col = c("blue", "red"), lty = 1:1)

plot(km_vaccination, main = "Kaplan-Meier Curves for Seasonal Influenza Vaccination", xlab = "Time (Days)", ylab = "Survival Probability", col = c("blue", "red"))
legend("topright", legend = c("Unvaccinated", "Vaccinated"), col = c("blue", "red"), lty = 1:1)

# Perform Cox regression
cox_model <- coxph(surv_obj ~ early + vacc + age_group + gender + com.respir + com.cardio + com.renal +
                     com.immun + com.metab + com.neuro + com.obes, data = medical)

summary(cox_model)
# Extract Hazard Ratios and 95% Confidence Intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals_cox <- exp(confint(cox_model))

# Print the comparison
print(hazard_ratios)
print(conf_intervals_cox)







