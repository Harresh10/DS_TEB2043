cat("Task 3: Name & Phone Format")

name_input  <- readline(prompt = "Enter Name: ")
phone_input <- readline(prompt = "Enter Phone Number: ")

name_upper <- toupper(name_input)
first_3 <- substr(phone_input, 1, 3)
last_4  <- substr(phone_input, nchar(phone_input) - 3, nchar(phone_input))
masked_phone <- paste(first_3, "-xxxxx", last_4)

cat("Hi,", name_upper)
cat("A verification code has been sent to", masked_phone)
