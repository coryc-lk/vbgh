setwd("/Users/chris/Desktop/R task/Data")

# Load the data
picloram_data <- read.csv("Picloram_Data.csv")

# PART A: create a new variable in the data set that is the observed probability of dying for each dose of picloram
# Calculate the observed probability of death for each dose of picloram
picloram_data$prob_death <- picloram_data$Number.Died / picloram_data$Total.Exposed

# Plot the relationship between observed probability and dose
plot(prob_death ~ Dose, data = picloram_data, xlab = "Dose", ylab = "Observed Probability of Death", main = "Dose-Response Relationship")

# PART B:  fit a logistic regression model to the data set
# Fit a logistic regression model
picloram_model <- glm(cbind(Number.Died, Number.Survived) ~ Dose, data = picloram_data, family = binomial(link = "logit"))

# Print the summary of the model
summary(picloram_model)


# PART C:  Using the fitted model, we can estimate the predicted probability of a larkspur plant dying for a dose of 2.2 units
# Calculate the predicted probability of death for a dose of 2.2 units
dose_new <- data.frame(Dose = 2.2)
predict(picloram_model, newdata = dose_new, type = "response")


# PART D: plot the fitted model on the same graph as the observed data points

# create vector of doses
dose_vector <- seq(from = 0, to = max(picloram_data$Dose) + 0.1, length.out = 1000)

# calculate predicted probability of dying for each dose value
predicted_probs <- plogis(picloram_model$coefficients[1] + picloram_model$coefficients[2] * dose_vector)

# create graph with observed data points and fitted model
library(ggplot2)
ggplot(picloram_data, aes(x = Dose, y = prob_death)) +
  geom_point() +
  geom_line(aes(x = dose_vector, y = predicted_probs), color = "blue") +
  labs(x = "Dose", y = "Probability of Death", title = "Dose-Response Relationship for Picloram on Larkspur")




setwd("/Users/chris/Desktop/R task/Data")

# PART A
# Load the data
picloram <- read.csv("Picloram_Data.csv")


# Create a plot of observed probability of dying versus dose
plot(picloram$dose, picloram$dead/picloram$total, 
     xlab = "Dose (units)", ylab = "Observed probability of dying")


# PART B:

probit_model <- glm(dead/total ~ dose, data = picloram, family = binomial("probit"))
summary(probit_model)

# Part C:
probit_pred <- predict(probit_model, newdata = data.frame(dose = 2.2), type = "response")
probit_pred

# Part D:
doses <- seq(from = 0, to = 5, by = 0.1)

probit_probs <- pnorm((probit_model$coefficients[1] + probit_model$coefficients[2] * doses)/sqrt(sum(probit_model$coefficients[-1]^2)))


library(ggplot2)

ggplot(picloram, aes(x = dose, y = dead/total)) +
  geom_point() +
  geom_line(aes(x = doses, y = probit_probs), color = "red", size = 1) +
  xlab("Dose") +
  ylab("Probability of dying") +
  ggtitle("Probit regression model for picloram and larkspur plants")


# Part E:
library(MASS)

ld50_probit <- dose.p(probit_model, p = 0.5)
ld90_probit <- dose.p(probit_model, p = 0.9)

ld50_probit
ld90_probit


# Load the Sleuth3 package
library(Sleuth3)

# Access the data set
data(ex2225)

# Part (a): Create a scatter plot
plot(ex2225$Size, ex2225$Mates, xlab = "Body Size", ylab = "Number of Mates")

# Part (b): Fit a linear regression model
fit_lm <- lm(Mates ~ Size, data = ex2225)
summary(fit_lm)

# Part (c): Create residual plots
par(mfrow = c(1,2))
qqnorm(fit_lm$residuals)
qqline(fit_lm$residuals)
plot(fit_lm$fitted.values, fit_lm$residuals, xlab = "Fitted Values", ylab = "Residuals")

# Part (d): Superimpose linear regression model onto the scatter plot
plot(ex2225$Size, ex2225$Mates, xlab = "Body Size", ylab = "Number of Mates")
abline(fit_lm, col = "red")

# Part (e): Fit a Poisson regression model
fit_poisson <- glm(Mates ~ Size, data = ex2225, family = "poisson")
summary(fit_poisson)

# Part (f): Superimpose Poisson regression model onto the scatter plot
plot(ex2225$Size, ex2225$Mates, xlab = "Body Size", ylab = "Number of Mates")
lines(ex2225$Size, predict(fit_poisson, type = "response"), col = "blue")

# Part (g): Create a confidence interval for the true effect of body size
conf_int <- confint(fit_poisson, level = 0.95)
exp(conf_int) # exponentiate to get confidence interval on original scale

# Part (h): Predicted mean number of mating pairs for a bullfrog that is 130mm in size
new_data <- data. Frame(Size = 130)
predict(fit_poisson, newdata = new_data, type = "response")
