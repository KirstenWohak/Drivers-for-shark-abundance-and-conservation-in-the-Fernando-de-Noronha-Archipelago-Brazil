# Generalized Linear Model for the abundance of Ginglymostoma cirratum  around Fernando de Noronha

# Load packages
library(readxl)
library(carData)
library(jtools)
library(ggplot2)
library(MuMIn)

# Import data
Data_Gin_cir <- read_excel("Data.xlsx")

#########################################################################################################################################
#########################################################################################################################################
############################################# Whole Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Build GLM with the abundance of Ginglymostoma cirratum as response variable and all drivers using the poisson family
Whole_GLM_Gin_cir <- glm(Gin_cir ~ Depth+ 
                         Substrate+Exposure+MPA+
                         Time+Port+Beach+Fishing+
                         Diving+Turtle+
                         Fish_Abundance+Trophic_Levels, family = poisson,  data = Data_Gin_cir);  # basic model

# Check for collinearity by computing the generalized variance inflation factor (GVIF)
# Should be below 3.2 
car::vif(Whole_GLM_Gin_cir)

# Remove parameters one by one according to highest GVIF
Whole_GLM_Gin_cir <- glm(Gin_cir ~ Depth+ 
                                Substrate+Exposure+MPA+
                                Time+Beach+Fishing+
                                Diving+Turtle+
                                Trophic_Levels, family = poisson, data = Data_Gin_cir);
car::vif(Whole_GLM_Gin_cir)

# Show output of GLM
summary(Whole_GLM_Gin_cir)

# Show further output of GLM
summ(Whole_GLM_Gin_cir, confint = TRUE, ci.width = .95)

# Plot the significant variables of the GLM
# Type of substrate
Substrate <- effect_plot(Whole_GLM_Gin_cir, pred = Substrate, interval = TRUE, plot.points = TRUE, 
                        jitter = 0.05,
                        x.label = "Type of Substrate", y.label = "Abundance of Ginglymostoma cirratum")
Substrate+theme(text = element_text(size = 18)) 


#########################################################################################################################################
#########################################################################################################################################
############################################# Whole Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Build GLM with the abundance of Ginglymostoma cirratum as response variable and all drivers using the poisson family
Best_GLM_Gin_cir <- glm(Gin_cir ~ Depth+ 
                       Substrate+Exposure+MPA+
                       Time+Beach+Fishing+
                       Diving+Turtle+
                       Trophic_Levels, family = poisson, data = Data_Gin_cir);

# Generate a model selection table of all models with combinations variables of the whole model
choose_MuMIn <- dredge(Best_GLM_Gin_cir , extra = c(alist(AIC),"R^2"))

# Print output of the models with the difference in AIC being smaller than 2 away from the best model
summary(model.avg(choose_MuMIn, subset = delta < 2))

# Generate GLM of the best model
Best_GLM_Gin_cir <- glm(Gin_cir ~ Substrate+Exposure, family = poisson, data = Data_Gin_cir);

# Show output of best GLM
summary(Best_GLM_Gin_cir)

# Show more outputs of best GLM
summ(Best_GLM_Gin_cir, confint = TRUE, ci.width = .95)

# Plot the significant variables of the best GLM
# Type of substrate
Substrate <- effect_plot(Best_GLM_Gin_cir, pred = Substrate, interval = TRUE, plot.points = TRUE, 
                                  jitter = 0.05,
                                  x.label = "Type of Substrate", y.label = "Abundance of Ginglymostoma cirratum")
Substrate+theme(text = element_text(size = 18)) 




