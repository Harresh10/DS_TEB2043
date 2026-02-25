cat("Check whether an n digits number is Armstrong or not:\n")
cat("---------------------------------------------------\n")
num <- as.integer(readline(prompt = "Input an integer: "))

digits <- as.numeric(strsplit(as.character(num), "")[[1]])
n_digits <- length(digits)

sum_val <- sum(digits^n_digits)

if (sum_val == num) {
  cat(num, "is an Armstrong number.\n")
} else {
  cat(num, "is not an Armstrong number.\n")
}