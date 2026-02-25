scores <- c(33, 24, 54, 94, 16, 89, 60, 6, 77, 61, 13, 44, 26, 24, 73, 73, 90, 39, 90, 54)

grade_categories <- cut(scores, 
                        breaks = c(-Inf, 49, 59, 69, 79, 89, 100), 
                        labels = c("F", "E", "D", "C", "B", "A"))

grade_counts <- table(grade_categories)
print("Number of students per grade:")
print(grade_counts)

pass_status <- scores > 49
print("Pass status for each student (TRUE = Pass, FALSE = Fail):")
print(pass_status)