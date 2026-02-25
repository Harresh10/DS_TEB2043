student_records$chemistry <- c(59, 71, 83, 68, 65, 57, 62, 92, 92, 59)
student_records$physics   <- c(89, 86, 65, 52, 60, 67, 40, 77, 90, 61)

chem_fails <- sum(student_records$chemistry <= 49)
phys_fails <- sum(student_records$physics <= 49)

cat("Number of students who failed Chemistry:", chem_fails, "\n")
cat("Number of students who failed Physics:", phys_fails, "\n")

combined_score <- student_records$chemistry + student_records$physics
best_index <- which.max(combined_score)
best_student <- student_records$names[best_index]

cat("Student with the best overall performance (Chemistry + Physics):", best_student, "\n")