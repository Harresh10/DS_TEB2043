print("Task 2: String Comparison")

str1 <- readline(prompt = "Enter string 1: ")
str2 <- readline(prompt = "Enter string 2: ")

is_same <- toupper(str1) == toupper(str2)
cat("This program compare 2 strings. Both inputs are similar:", is_same)