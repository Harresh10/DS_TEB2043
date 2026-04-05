data(mtcars)

hist(mtcars$mpg, 
     xlab = "Miles Per Gallon (MPG)", 
     ylab = "Frequency",
     col = "green", 
     border = "red", 
     main = "Distribution of Fleet Fuel Efficiency")

input <- mtcars[,c('wt','mpg')]

plot(x = input$wt, 
     y = input$mpg,
     xlab = "Weight (1000 lbs)",
     ylab = "Mileage (MPG)",
     xlim = c(1.5, 6.0),
     ylim = c(10, 35),
     main = "Vehicle Weight vs. Mileage")

boxplot(mpg ~ cyl, data = mtcars,
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon",
        main = "Mileage by Cylinder Count",
        col = c("green", "yellow", "purple"),
        names = c("4 Cyl", "6 Cyl", "8 Cyl"))