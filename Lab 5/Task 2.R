n <- as.integer(readline(prompt = "Input an integer: "))

for (i in 1:n) {
  cube <- i^3
  cat("Number is:", i, "and cube of the", i, "is :", cube, "\n")
}