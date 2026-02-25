array1 <- array(1:24, dim = c(2, 4, 3))

array2 <- array(25:54, dim = c(3, 2, 5))

print("Array1:")
print(array1)
print("Array2:")
print(array2)

cat("\nThe second row of the second matrix of the array:\n")
print(array1[2, , 2]) 

cat("\nThe element in the 3rd row and 2nd column of the 1st matrix:\n")
print(array2[3, 2, 1])