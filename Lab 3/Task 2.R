student_records <- list(
  names = c("Robert", "Hemsworth", "Scarlett", "Evans", "Pratt", 
            "Larson", "Holland", "Paul", "Simu", "Renner"),
  scores = c(59, 71, 83, 68, 65, 57, 62, 92, 92, 59)
)

highest_score <- max(student_records$scores)
lowest_score  <- min(student_records$scores)
average_score <- mean(student_records$scores)

# Finding specific student names
# which() identifies the index of the min/max values
top_students <- student_records$names[student_records$scores == highest_score]
bottom_students <- student_records$names[student_records$scores == lowest_score]

cat("Highest Score:", highest_score, "\n")
cat("Lowest Score:", lowest_score, "\n")
cat("Average Score:", average_score, "\n")
cat("Student with highest score:", paste(top_students, collapse=", "), "\n")
cat("Student with lowest score:", paste(bottom_students, collapse=", "), "\n")
