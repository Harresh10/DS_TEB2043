# --- Task 1: Create the data frame ---
df <- data.frame(
  name = c("Anastasia", "Dima", "Michael", "Matthew", "Laura", "Kevin", "Jonas"),
  score = c(12.5, 9.0, 16.5, 12.0, 9.0, 8.0, 19.0),
  attempts = c(1, 3, 2, 3, 2, 1, 2)
)

# --- Task 2: Add the 'qualify' column ---
df$qualify <- c("yes", "no", "yes", "no", "no", "no", "yes")

# --- Task 3: Add new row for Emily ---
new_row <- data.frame(name="Emily", score=14.5, attempts=1, qualify="yes")
df <- rbind(df, new_row)

# --- Task 4: Display structure, summary, and dimensions ---
# Converting 'qualify' to a factor type enables more useful summary statistics
df$qualify <- as.factor(df$qualify)

print("Updated Data Frame:")
print(df)

print("--- Data Structure ---")
str(df)

print("--- Statistical Summary ---")
summary(df)

print("--- Number of Rows and Columns ---")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")