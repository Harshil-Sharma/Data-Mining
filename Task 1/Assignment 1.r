mydata <- read.table(file.choose(), header = TRUE, sep = ",")
data.frame(mydata)

# Q1 Number of patients having heart problem:
length(which(mydata$heart_problem == 1))

# this is a object for patients having heart problem:
have_problem <- mydata[mydata$heart_problem ==1, ]

# this is a object for patients not having heart problem:
no_problem <- mydata[mydata$heart_problem == 0, ]

# Q2.1 average Cholesterol level of people with heart disease:
mean (have_problem$chol, na.rm = FALSE )

# Q2.1 average Cholesterol level of people without heart disease:
mean (no_problem$chol, na.rm = FALSE )

# Q2.2 standard deviation of everyone's cholesterol:
sd(mydata$chol, na.rm = FALSE )
# Q2.2 standard deviation with heart problem:
sd(have_problem$chol, na.rm = FALSE )
# Q2.2 standard deviation without heart problem:
sd(no_problem$chol, na.rm = FALSE )

# object for patients having cholesterol more than 240 but no heart disease:
chol_240np <- no_problem[no_problem$chol > 240, ]

# object for patients having cholesterol more than 240 and heart disease:
chol_240p <- have_problem[have_problem$chol > 240, ]

# object for patients having cholestrol more than 240:
chol_240 <- mydata[mydata$chol > 240, ]

# Q3.1 median and average age of people with cholesterol higher than 240:
median(chol_240$age, na.rm = FALSE)
mean(chol_240$age, na.rm = FALSE)

# Q3.2 median and average age of people with cholesterol higher than 240 and heart disease:
median(chol_240p$age, na.rm = FALSE)
mean(chol_240p$age, na.rm = FALSE)

# Q3.3 median and average age of people with cholesterol higher than 2400 and no heart disease:
median(chol_240np$age, na.rm = FALSE)
mean(chol_240np$age,  na.rm = FALSE)

# Q4 histogram of resting blood pressure:
hist(mydata$trestbps, main = "Histogram of Resting Blood Pressure", xlab = "Resting Blood Pressure", ylab = "No. of Patients", col = "Blue")

# Q5.1 boxplots based on the sex of the patients for cholestrol level:
boxplot(mydata$chol~mydata$sex, main = "Boxplot of Cholestrol based on Sex", xlab = "Gender(Female/Male)", ylab = "Cholestrol")
# Q5.1 boxplots based on the sex of the patients for maximum heart rate:
boxplot(mydata$thalach~mydata$sex, main = "Boxplot of Maximum Blood Pressure based on Sex", xlab = "Gender(Female/Male)", ylab = "Maximum Blood Pressure")

# Object for patients having gender male:
male <- mydata[mydata$sex == 1,]
# Object for patients having gender female:
female <- mydata[mydata$sex == 0,]

# Q6.1 H-Spread (Q3-Q1) of cholesterol level for male and females:
quantile(male$chol, na.rm = FALSE, probs = 0.75) - quantile(male$chol, na.rm = FALSE, probs = 0.25)
quantile(female$chol, na.rm = FALSE, probs = 0.75) - quantile(female$chol, na.rm = FALSE, probs = 0.25)

# Q6.2 Lower Hinge and Upper Hinge values for maximum heart rate for male:
quantile(male$thalach, na.rm = FALSE, probs = 0.25)
quantile(male$thalach, na.rm = FALSE, probs = 0.75)

# Q6.2 Lower Hinge and Upper Hinge values for maximum heart rate for female:
quantile(female$thalach, na.rm = TRUE, probs = 0.25)
quantile(female$thalach, na.rm = TRUE, probs = 0.75)

# Q7.1 scatter plots of age and resting blood pressure for people with heart disease:
plot(have_problem$age, have_problem$trestbps, main = "Scatter Plot for having Heart Problem", xlab = "Age", ylab = "Resting Blood Presssure")

# Q7.1 scatter plots of age and resting blood pressure for people without heart disease:
plot(no_problem$age, no_problem$trestbps, main = "Scatter Plot for Not having Heart Problem", xlab = "Age", ylab = "Resting Blood Presssure")

# Q7.1 The scatter plot of people having heart problems shows a weak positive correlation as the age of the individual increases so the resting blood pressure increases.
# Q7.1 The scatter plot of people not having heart problems show no correlation as the resting blood pressure of the individual do not increase with age.

# Q7.2 Average resting blood pressure for each age with no heart disease:
avg_age1 <- aggregate(no_problem[, 5], by = list(no_problem$age), FUN = mean)
avg_age1
mean (avg_age1$x, na.rm = T)
# Q7.2 average resting blood pressure for each age with heart disease:
avg_age2 <- aggregate(have_problem[, 5], by = list(have_problem$age), FUN = mean)
avg_age2
mean (avg_age2$x, na.rm = T)

# Q7.3 scatter plot of data for patients with no heart disease:
plot(avg_age1$Group.1, avg_age1$x, xlab = "Unique Age", ylab = "Average Resting Blood Pressure", main = "Patients without Heart Disease")
# Q7.3 scatter plot of data for patients without heart disease:
plot(avg_age2$Group.1, avg_age2$x, xlab = "Unique Age", ylab = "Average Resting Blood Pressure", main = "Patients with Heart Disease")

# Q8 Here the comparison of summary of resting blood pressure of patients with and without heart disease:
summary(no_problem$trestbps)
summary(have_problem$trestbps)

