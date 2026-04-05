data(Theoph)

x = Theoph$Wt
y = Theoph$Dose
model1 <- lm(y~x)

print(model1)
print(summary(model1))

plot(y=Theoph$Dose, x=Theoph$Wt, main = "Dose & Weight", abline(lm(Dose~Wt, data=Theoph)), xlab = "Weight", ylab = "Dose")
scatter.smooth(y=Theoph$Dose, x=Theoph$Wt, main="Dose Weight", xlab = "Weight", ylab = "Dose")

newweight <- data.frame(x=c(90, 95, 100))
result <- predict(model1, newweight)
print(result)