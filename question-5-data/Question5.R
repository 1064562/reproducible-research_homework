#Install/load packages
install.packages("dplyr")
install.packages("gglot2")
library(ggplot2)
library(dplyr)

# PART 1: 
dsDNA_data <- read.csv("question-5-data//Cui_etal2014.csv")
dsDNA_data

# Do natural log of both virion volume and genome length
dsDNA_data$log_Virion.volume <- log(dsDNA_data$Virion.volume..nm.nm.nm.)
dsDNA_data$log_Genome.length..kb. <- log(dsDNA_data$Genome.length..kb.)

# Fit linear model
modelDNA <- lm(log_Virion.volume ~ log_Genome.length..kb., dsDNA_data)
summary(modelDNA)

# Isolate the estimates for cofficicents for slope (alpha) and intercept (beta)
coefficients <- summary(modelDNA)$coefficients

# Isolating the slope
alpha <- coefficients["log_Genome.length..kb.", "Estimate"]

alpha
# Exponentiating the intercept
beta <- exp(coefficients["(Intercept)", "Estimate"])

# Output beta (scaling factor) when exponentiated
beta

# PART 2, replicating the graph:
ggplot(dsDNA_data, aes(x = log_Genome.length..kb., y = log_Virion.volume)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "log[Genome length (kb)]", y = "log[Virion volume (nm3)]")+
  theme_minimal()

# PART 3, estimate volume of 300kb dsDNA virus, using V = B*L^a:
est_vol <- (beta * (300^alpha))
est_vol
