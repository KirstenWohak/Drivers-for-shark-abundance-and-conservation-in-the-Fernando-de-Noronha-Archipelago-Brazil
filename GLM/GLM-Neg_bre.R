# Generalized Linear Model for the abundance of Negaprion brevirostris around Fernando de Noronha

# Load packages
library(readxl)
library(carData)
library(jtools)
library(ggplot2)
library(MuMIn)

# Import data
Data_Neg_bre <- read_excel("Data.xlsx")

#########################################################################################################################################
#########################################################################################################################################
############################################# Whole Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Build GLM with the abundance of Negaprion brevirostris as response variable and all drivers using the poisson family
Whole_GLM_Neg_bre <- glm(Neg_bre ~ Depth+ 
                         Substrate+Exposure+MPA+
                         Time+Port+Beach+Fishing+
                         Diving+Turtle+
                         Fish_Abundance+Trophic_Levels, family = poisson,  data = Data_Neg_bre);  # basic model

# Check for collinearity by computing the generalized variance inflation factor (GVIF)
# Should be below 3.2 
car::vif(Whole_GLM_Neg_bre)

# Remove parameters one by one according to highest GVIF
Whole_GLM_Neg_bre <- glm(Neg_bre ~ Depth+ 
                                Substrate+Exposure+MPA+
                                Time+Fishing+
                                Diving+Turtle+
                                Fish_Abundance, family = poisson, data = Data_Neg_bre);
car::vif(Whole_GLM_Neg_bre)

# Show output of GLM
summary(Whole_GLM_Neg_bre)

# Show further output of GLM
summ(Whole_GLM_Neg_bre, confint = TRUE, ci.width = .95)

# No significant drivers

#########################################################################################################################################
#########################################################################################################################################
############################################# Whole Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Build GLM with the abundance of Negaprion brevirostris as response variable and all drivers using the poisson family
Best_GLM_Neg_bre <- glm(Neg_bre ~ Depth+ 
                       Substrate+Exposure+MPA+
                       Time+Fishing+
                       Diving+Turtle+
                       Fish_Abundance, family = poisson, data = Data_Neg_bre);

# Generate a model selection table of all models with combinations variables of the whole model
choose_MuMIn <- dredge(Best_GLM_Neg_bre , extra = c(alist(AIC),"R^2"))

# Print output of the models with the difference in AIC being smaller than 2 away from the best model
summary(model.avg(choose_MuMIn, subset = delta < 2))

# Generate GLM of the best model
Best_GLM_Neg_bre <- glm(Neg_bre ~ Depth, family = poisson, data = Data_Neg_bre);

# Show output of best GLM
summary(Best_GLM_Neg_bre)

# Show more outputs of best GLM
summ(Best_GLM_Neg_bre, confint = TRUE, ci.width = .95)

# No significant variables
