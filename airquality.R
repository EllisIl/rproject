# Enable pausing between plots
devAskNewPage(TRUE)

# Load airquality dataset
data(airquality)

# Inspect the data’s structure and types
str(airquality)
summary(airquality)
head(airquality)

# Handling missing values
colSums(is.na(airquality))

# Remove rows with missing values
airquality_clean <- na.omit(airquality)

# Fill missing values with column means
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- mean(airquality$Solar.R, na.rm = TRUE)

# Data cleaning
names(airquality) <- c("Ozone", "SolarRadiation", "Wind", "Temperature", "Month", "Day")
airquality$Month <- as.factor(airquality$Month)

# Summary statistics
summary(airquality)

# Create histograms
hist(airquality$Ozone, main="Histogram of Ozone Levels", xlab="Ozone", col="blue", border="black")

# Create bar plot for observations per month
barplot(table(airquality$Month), main="Observations per Month", xlab="Month", ylab="Frequency", col="green")

# Scatter plot for Ozone vs Temperature with trend line and arrow
plot(airquality$Ozone, airquality$Temperature, main="Ozone vs Temperature", xlab="Ozone", ylab="Temperature", col="red", pch=19)
abline(lm(Temperature ~ Ozone, data=airquality), col="blue", lwd=2)
arrows(min(airquality$Ozone), min(airquality$Temperature), max(airquality$Ozone), max(airquality$Temperature), col="blue", length=0.1)

# Scatter plot for Solar Radiation vs Temperature with trend line and arrow
plot(airquality$SolarRadiation, airquality$Temperature, main="Solar Radiation vs Temperature", xlab="Solar Radiation", ylab="Temperature", col="purple", pch=19)
abline(lm(Temperature ~ SolarRadiation, data=airquality), col="blue", lwd=2)
arrows(min(airquality$SolarRadiation), min(airquality$Temperature), max(airquality$SolarRadiation), max(airquality$Temperature), col="blue", length=0.1)

# Scatter plot for Temperature vs Month
plot(airquality$Month, airquality$Temperature, main="Temperature vs Month", xlab="Month", ylab="Temperature", col="darkgreen", pch=19)

# Calculate and analyze correlations
cor_matrix <- cor(airquality[, 1:4], use="complete.obs")
print(cor_matrix)

# Create box plot for Ozone levels by month
boxplot(Ozone ~ Month, data=airquality, main="Ozone Levels by Month", xlab="Month", ylab="Ozone", col="orange")

# Disable pausing
devAskNewPage(FALSE)
