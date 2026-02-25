print("--- Task 1: BMI Calculator ---")

weight <- as.numeric(readline(prompt = "Enter Weight (kg): "))
height <- as.numeric(readline(prompt = "Enter Height (m): "))

BMI <- weight / (height ^ 2)

Underweight <- FALSE
Normal      <- FALSE
Overweight  <- FALSE
Obese       <- FALSE

if (BMI <= 18.4) {
  Underweight = TRUE
} else if (BMI >= 18.5 & BMI <= 24.9) {
  Normal = TRUE
} else if (BMI >= 25.0 & BMI <= 39.9) {
  Overweight = TRUE
} else {
  Obese = TRUE
}

print(paste("Underweight:", Underweight))
print(paste("Normal:", Normal))
print(paste("Overweight:", Overweight))
print(paste("Obese:", Obese))
