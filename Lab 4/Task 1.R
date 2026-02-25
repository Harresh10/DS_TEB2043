age <- c(55,57,56,52,51,59,58,53,59,55,60,60,60,60,52,55,56,51,60,
         52,54,56,52,57,54,56,58,53,53,50,55,51,57,60,57,55,51,50,57,58)

age_factor <- factor(age)
print("Levels of factor (Staff Ages):")
print(levels(age_factor))

print("Total number of staff per age:")
print(table(age))

age_ranges <- cut(age, breaks = c(50, 52, 54, 56, 58, 60), include.lowest = TRUE, right = FALSE)
range_table <- table(age_ranges)

print("Total number of staff per age range:")
print(range_table)

print("   Insight   ")
print("-------------")
print("Most staff members are retiring at the older end of the spectrum (58-60).") 
print("The age 60 is particularly frequent suggesting it might be the mandatory retirement age.")