#Loading the necessary libraries
#Please note that you need to install

install.packages("gridExtra")
#install.packages("lme4")
#install.packages("Matrix")

#Note: if the above installation don't work, please use the below installation

#install.packages("lme4", type = "source")
#install.packages("Matrix", type = "source")
#install.packages("lme4", type = "source")

#After the necessary installation, load the necessary libraries
library(lme4)
library('tidyverse')
library('broom')
library(ggplot2)
#library(Matrix)
library(dplyr)
library(gridExtra)
#Task 01. Data Exploration.

#Loading the data
#a. Load the "sleepstudy" dataset.
data("sleepstudy")

# Viewing structure and first six rows
str(sleepstudy)
head(sleepstudy)
count(sleepstudy)

# Filtering out Day 0 and Day 1
sleepstudy <- subset(sleepstudy, Days > 1)
count(sleepstudy)

#b. Explore the structure of the dataset.
str(sleepstudy)

#c. Visualize the data using appropriate plots to understand the distribution and relationships.
plot1 <- ggplot(sleepstudy, aes(x = Days, y = Reaction, color = factor(Subject))) +
  geom_line() +
  labs(title = "Reaction Times Over Days by Subject", 
       x = "Days", y = "Reaction Time") +
  theme_minimal()

# Scatter plot
plot2 <-sleepstudy %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  labs(title = "Reaction Time Over Days",
       x = "Days of Sleep Deprivation", y = "Reaction Time")

# Boxplot
plot3 <- sleepstudy %>%
  ggplot(aes(y = Reaction)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Reaction Time", y = "Reaction Time")

# Density plot
# Showing the densityplot of reaction time by subject
plot4 <-sleepstudy %>%
  ggplot(aes(x = Reaction, fill = as.factor(Subject))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Reaction Time by Subject", x = "Reaction Time") +
  theme_minimal()

# Visualizing the plot that shows the densityplot of reaction time by days
plot5 <- sleepstudy %>%
  ggplot(aes(x = Reaction, fill = as.factor(Days))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Reaction Time by Days", x = "Reaction Time") +
  theme_minimal()

# Arranging the first three plots into a 1x3 grid
grid.arrange(plot1, plot2, plot3, nrow = 1, ncol = 3)

# Arranging the forth and fifth plots into a 1x2 grid
grid.arrange(plot4, plot5, nrow = 1, ncol = 2)

#Task 02. Descriptive Statistics.
#a. Compute and report summary statistics for the key variables.
summary_by_days <- sleepstudy %>%
  group_by(Days) %>%
  summarise(
    Mean_Reaction = mean(Reaction),
    Median_Reaction = median(Reaction),
    SD_Reaction = sd(Reaction),
    Min_Reaction = min(Reaction),
    Max_Reaction = max(Reaction)) %>% 
  mutate_all(~round(., 1))
summary_by_days

write.table(summary_by_days, file = "summary_by_days.txt", sep = ",", quote = FALSE, row.names = F)

summary_by_subject <- sleepstudy %>%
  group_by(Subject) %>%
  summarise(
    Mean_Reaction = mean(Reaction),
    Median_Reaction = median(Reaction),
    SD_Reaction = sd(Reaction),
    Min_Reaction = min(Reaction),
    Max_Reaction = max(Reaction))
summary_by_subject

write.table(summary_by_subject, file = "summary_by_subject.txt", sep = ",", quote = FALSE, row.names = F)

#b. Create visualizations (e.g., histograms, boxplots) to better understand the distribution of reaction times over different days.

# Histogram
plot6 <- sleepstudy %>%
  ggplot(aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Reaction Time", x = "Reaction Time") +
  theme_minimal()

# Boxplot by Days
plot7 <- sleepstudy %>%
  ggplot(aes(x = as.factor(Days), y = Reaction)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Reaction Time by Days", x = "Days of Sleep Deprivation", y = "Reaction Time") +
  theme_minimal()

# Arranging the sixth and seventh plots into a 1x2 grid
grid.arrange(plot6, plot7, nrow = 1, ncol = 2)

# The plot shows the densityplot of reaction time by days
sleepstudy %>%
  ggplot(aes(x = Reaction, fill = as.factor(Days))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Reaction Time by Days", x = "Reaction Time") +
  theme_minimal()

#Task 03. Fit an adequate Model(s).
# Since the dataset involves repeated measurements on the same subjects over multiple days, introducing a level of dependency in the data. 
# A mixed-effects model is well-suited for handling this type of correlated observations within subjects.
model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
model

#Task 04. Interpret the results.
#Interpretations
#The mixed-effects model estimates that the average reaction time across all #subjects starts at 245.10 milliseconds (intercept) on Day 2 and increases by #11.44 milliseconds for each additional day of sleep deprivation (fixed effect #for Days). The random effect for Subject suggests individual baseline reaction #times vary with a standard deviation of 41.80 milliseconds, while the residual #standard deviation (unexplained variability) is 30.22 milliseconds.

#Task 05. Residual Analysis
res <- residuals(model)

#set graphical parameters to generate plot matrix
par(mfrow =c(1,3))

# Plot 1 histogram
hist(res)

# Plot 2 Q-Q Plot
qqnorm(res)
qqline(res)

#Plot 3  Residual plot

plot(fitted(model), res)
