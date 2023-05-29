# Generalized Linear Model for the abundance of Carcharhinus perezi around Fernando de Noronha

# Load packages
library(readxl)
library(carData)
library(jtools)
library(ggplot2)
library(MuMIn)

# Import data
Data_Car_per <- read_excel("Data.xlsx")

#########################################################################################################################################
#########################################################################################################################################
############################################# Whole Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Build GLM with the abundance of Carcharhinus perezi as response variable and all drivers using the poisson family
Whole_GLM_Car_per <- glm(Car_per ~ Depth+ 
                         Substrate+Exposure+MPA+
                         Time+Port+Beach+Fishing+
                         Diving+Turtle+
                         Fish_Abundance+Trophic_Levels, family = poisson,  data = Data_Car_per);  # basic model

# Check for collinearity by computing the generalized variance inflation factor (GVIF)
# Should be below 3.2 
car::vif(Whole_GLM_Car_per)

# Remove parameters one by one according to highest GVIF
Whole_GLM_Car_per <- glm(Car_per ~ Depth+ 
                                Substrate+Exposure+MPA+
                                Time+Beach+Fishing+
                                Diving+Turtle+
                                Trophic_Levels, family = poisson, data = Data_Car_per);
car::vif(Whole_GLM_Car_per)

# Show output of GLM
summary(Whole_GLM_Car_per)

# Show further output of GLM
summ(Whole_GLM_Car_per, confint = TRUE, ci.width = .95)


# Plot the significant variables of the GLM
# Type of substrate
Substrate <- effect_plot(Whole_GLM_Car_per, pred = Substrate, interval = TRUE, plot.points = TRUE, 
                        jitter = 0.05,
                        x.label = "Type of Substrate", y.label = "Abundance of Carcharhinus perezi")
Substrate+theme(text = element_text(size = 18)) 

# Time of sampling
Time <- effect_plot(Whole_GLM_Car_per, pred = Time, interval = TRUE, plot.points = TRUE, 
                                jitter = 0.05,
                                x.label = "Time of Sampling", y.label = "Abundance of Carcharhinus perezi")
Time+theme(text = element_text(size = 18))

# Distance to the beaches
Beach <- effect_plot(Whole_GLM_Car_per, pred = Beach, interval = TRUE, plot.points = TRUE, 
                             jitter = 0.05,
                             x.label = "Distance to Beaches in km", y.label = "Abundance of Carcharhinus perezi")
Beach+theme(text = element_text(size = 18))

#########################################################################################################################################
#########################################################################################################################################
############################################# Whole Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Build GLM with the abundance of Carcharhinus perezi as response variable and all drivers using the poisson family
Best_GLM_Car_per <- glm(Car_per ~ Depth+ 
                       Substrate+Exposure+MPA+
                       Time+Beach+Fishing+
                       Diving+Turtle+
                       Trophic_Levels, family = poisson, data = Data_Car_per);

# Generate a model selection table of all models with combinations variables of the whole model
choose_MuMIn <- dredge(Best_GLM_Car_per , extra = c(alist(AIC),"R^2"))

# Print output of the models with the difference in AIC being smaller than 2 away from the best model
summary(model.avg(choose_MuMIn, subset = delta < 2))

# Generate GLM of the best model
Best_GLM_Car_per <- glm(Car_per ~ Beach+Depth+MPA+Substrate+Time+Turtle, family = poisson, data = Data_Car_per);

# Show output of best GLM
summary(Best_GLM_Car_per)

# Show more outputs of best GLM
summ(Best_GLM_Car_per, confint = TRUE, ci.width = .95)

# Plot the significant variables of the best GLM
# Type of substrate
Substrate <- effect_plot(Best_GLM_Car_per, pred = Substrate, interval = TRUE, plot.points = TRUE, 
                                  jitter = 0.05,
                                  x.label = "Type of Substrate", y.label = "Abundance of Carcharhinus perezi")
Substrate+theme(text = element_text(size = 18)) 

# Distance to the beaches
Beach <- effect_plot(Best_GLM_Car_per, pred = Beach, interval = TRUE, plot.points = TRUE, 
                                  jitter = 0.05,
                                  x.label = "Distance to Beaches in km", y.label = "Abundance of Carcharhinus perezi")
Beach+theme(text = element_text(size = 18)) 

# Time of sampling
Time <- effect_plot(Best_GLM_Car_per, pred = Time, interval = TRUE, plot.points = TRUE, 
                                  jitter = 0.05,
                                  x.label = "Time of Sampling", y.label = "Abundance of Carcharhinus perezi")
Time+theme(text = element_text(size = 18)) 

# Distance to the turtle grounds
Turtle <- effect_plot(Best_GLM_Car_per, pred = Turtle, interval = TRUE, plot.points = TRUE, 
                                  jitter = 0.05,
                                  x.label = "Distance to Turtle Grounds in km", y.label = "Abundance of Carcharhinus perezi")
Turtle+theme(text = element_text(size = 18)) 


