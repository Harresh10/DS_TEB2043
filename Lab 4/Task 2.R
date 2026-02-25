V1 <- c(2,3,1,5,4,6,8,7,9)

matrix_1 <- matrix(V1, nrow = 3, ncol = 3)
colnames(matrix_1) <- c("Col1", "Col2", "Col3")
rownames(matrix_1) <- c("Row1", "Row2", "Row3")

matrix_2 <- t(matrix_1)
colnames(matrix_2) <- c("T_Col1", "T_Col2", "T_Col3")
rownames(matrix_2) <- c("T_Row1", "T_Row2", "T_Row3")

add_res <- matrix_1 + matrix_2
sub_res <- matrix_1 - matrix_2
mul_res <- matrix_1 * matrix_2
div_res <- matrix_1 / matrix_2

print("Matrix-1:")
print(matrix_1)
print("Matrix-2:")
print(matrix_2)
print("Addition Result:")
print(add_res)
print("Subtraction Result:")
print(sub_res)
print("Multiplication Result:")
print(mul_res)
print("Division Result:")
print(div_res)