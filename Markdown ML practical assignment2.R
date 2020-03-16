mdl <- glm(use ~ wind - 1, binomial, shuttle)
summary(mdl)
log(abs(mdl$coefficients[2]))

mdl <- glm(count ~ factor(spray), poisson, InsectSprays)

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
z <- c(rep(0, 5), rep(1, 6))

fit <- lm(y ~ x + z)
plot(fit)


summary(fit)

z <- (x > 0) * x


fit <- lm(mpg ~ am, data = mtcars)
summary(fit)$coef
confint(fit)
e <- resid(fit)

fit <- lm(mpg ~ factor(am), data = mtcars)
summary(fit)$coef
confint(fit)
e <- resid(fit)

#MPG difference
plot(factor(mtcars$am, labels = c("Automatic","Manual")), 
            mtcars$mpg, 
            ylab = "MPG",
            xlab = "Transmission")

#Residual plot
plot(factor(mtcars$am, labels = c("Automatic","Manual")), e, 
     ylab = "Residuals",
     xlab = "Transmission")

exp(coef(fit)) # volgens mij hoort deze alleen bij glm

table(mtcars$am)

#test for normality
shapiro.test(mtcars$mpg[mtcars$am == 0])
shapiro.test(mtcars$mpg[mtcars$am == 1])
