# Generalized Linear Model for the overall shark abundance around Fernando de Noronha

# Load packages
library(readxl)
library(carData)
library(jtools)
library(ggplot2)
library(MuMIn)

# Import data
Data_SharkAbundance <- read_excel("Data.xlsx")

#########################################################################################################################################
#########################################################################################################################################
############################################# Whole Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Build GLM with the shark abundance as response variable and all drivers using the poisson family
Whole_GLM_SharkAbundance <- glm(Shark_Abundance ~ Depth+ 
                      Substrate+Exposure+MPA+
                      Time+Port+Beach+Fishing+
                      Diving+Turtle+
                      Fish_Abundance+Trophic_Levels, family = poisson, data = Data_SharkAbundance);  # basic model

# Check for collinearity by computing the generalized variance inflation factor (GVIF)
# Should be below 3.2 
car::vif(Whole_GLM_SharkAbundance)

# Remove parameters one by one according to highest GVIF
Whole_GLM_SharkAbundance <- glm(Shark_Abundance ~ Depth+ 
                      Substrate+Exposure+MPA+
                      Time+Beach+Fishing+
                      Diving+Turtle+
                      Trophic_Levels, family = poisson, data = Data_SharkAbundance);#
car::vif(Whole_GLM_SharkAbundance)

# Show output of GLM
summary(Whole_GLM_SharkAbundance)

# Show further outputs of GLM
summ(Whole_GLM_SharkAbundance, confint = TRUE, ci.width = .95)


# Plot the significant variables of the GLM
# Type of Substrate
Substrate <- effect_plot(Whole_GLM_SharkAbundance, pred = Substrate, interval = TRUE, plot.points = TRUE, 
                         jitter = 0.05,
                         x.label = "Type of Substrate", y.label = "Shark Abundance")
Substrate+theme(text = element_text(size = 18)) 

# Distance to the Beach
BioAvBeach <- effect_plot(Whole_GLM_SharkAbundance, pred = Beach, interval = TRUE, plot.points = TRUE, 
                              jitter = 0.05,
                              x.label = "Distance to Beaches in km", y.label = "Shark Abundance")
BioAvBeach+theme(text = element_text(size = 18)) 


#########################################################################################################################################
#########################################################################################################################################
############################################# Best Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Change the default "na.omit" to prevent models from being fitted to different datasets in case of missing values
options(na.action = "na.fail") 

# Build GLM with the shark abundance as response variable and all drivers using the poisson family
Best_GLM_SharkAbundance <- glm(Shark_Abundance ~ Depth+ 
                       Substrate+Exposure+MPA+
                       Time+Beach+Fishing+
                       Diving+Turtle+
                       Trophic_Levels, family = poisson, data = Data_SharkAbundance);

# Generate a model selection table of all models with combinations variables of the whole model
choose_MuMIn <- dredge(Best_GLM_SharkAbundance , extra = c(alist(AIC),"R^2"))

# Print output of the models with the difference in AIC being smaller than 2 away from the best model
summary(model.avg(choose_MuMIn, subset = delta < 2))

# Generate GLM of the best model
Best_GLM_SharkAbundance <- glm(Shark_Abundance ~ Beach+Fishing+Substrate+Time, family = poisson, data = Data_SharkAbundance);

# Show output of best GLM
summary(Best_GLM_SharkAbundance)

# Show more outputs of best GLM
summ(Best_GLM_SharkAbundance, confint = TRUE, ci.width = .95)

# Plot the significant variables of the best GLM
# Type of Substrate
BioAvSubstrate <- effect_plot(Best_GLM_SharkAbundance, pred = Substrate, interval = TRUE, plot.points = TRUE, 
                              jitter = 0.05,
                              x.label = "Type of Substrate", y.label = "Shark Abundance")
BioAvSubstrate+theme(text = element_text(size = 18)) 

# Distance to the Beaches
BioAvBeach <- effect_plot(Best_GLM_SharkAbundance, pred = Beach, interval = TRUE, plot.points = TRUE, 
                              jitter = 0.05,
                              x.label = "Distance to Beaches in km", y.label = "Shark Abundance")
BioAvBeach+theme(text = element_text(size = 18)) 

# Distance to the Fishing Grounds
BioAvFishing <- effect_plot(Best_GLM_SharkAbundance, pred = Fishing, interval = TRUE, plot.points = TRUE, 
                              jitter = 0.05,
                              x.label = "Distance to Fishing Grounds in km", y.label = "Shark Abundance")
BioAvFishing+theme(text = element_text(size = 18)) 




