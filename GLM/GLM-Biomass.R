# Generalized Linear Model for the overall shark biomass around Fernando de Noronha

# Load packages
library(readxl)
library(carData)
library(jtools)
library(ggplot2)
library(MuMIn)

# Import data
Data_SharkBiomass <- read_excel("Data.xlsx")

#########################################################################################################################################
#########################################################################################################################################
############################################# Whole Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Build GLM with the shark biomass as response variable and all drivers using the gaussian family and Boxcox-Transformation
Whole_GLM_SharkBiomass <- glm((Shark_Biomass)^0.2 ~ Depth+ 
                       Substrate+Exposure+MPA+
                       Time+Port+Beach+Fishing+
                       Diving+Turtle+
                       Fish_Biomass+Trophic_Levels, family = gaussian, data = Data_SharkBiomass);  # basic model

# Check for collinearity by computing the generalized variance inflation factor (GVIF)
# Should be below 3.2 
car::vif(Whole_GLM_SharkBiomass)

# Remove parameters one by one according to highest GVIF
Whole_GLM_SharkBiomass <- glm((Shark_Biomass)^0.2 ~ Depth+ 
                        Substrate+Exposure+MPA+
                        Time+Beach+Fishing+
                        Diving+Turtle+
                        Fish_Biomass+Trophic_Levels, family = gaussian, data = Data_SharkBiomass);#
car::vif(Whole_GLM_SharkBiomass)

# Show output of GLM
summary(Whole_GLM_SharkBiomass)

# Show further output of GLM
summ(Whole_GLM_SharkBiomass, confint = TRUE, ci.width = .95)


# Plot the significant variables of the GLM
# Inside / Outside of MPA
MPA <- effect_plot(Whole_GLM_SharkBiomass, pred = MPA, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05,
            x.label = "Location around FNA", y.label = "Transformed Sum of Shark Biomass in kg")
MPA+theme(text = element_text(size = 18)) 

# Distance to diving sites
Diving <- effect_plot(Whole_GLM_SharkBiomass, pred = Diving, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05,
            x.label = "Distance to Diving Sites in km", y.label = "Transformed Sum of Shark Biomass in kg")
Diving+theme(text = element_text(size = 18))

# Distance to turtle grounds
Turt <- effect_plot(Whole_GLM_SharkBiomass, pred = Turtle, interval = TRUE, plot.points = TRUE, 
                         jitter = 0.05,
                         x.label = "Distance to Turtle Grounds in km", y.label = "Transformed Sum of Shark Biomass in kg")
Turt+theme(text = element_text(size = 18))

# Sum of trophic levels
TL <- effect_plot(Whole_GLM_SharkBiomass, pred = Trophic_Levels, interval = TRUE, plot.points = TRUE, 
                         jitter = 0.05,
                         x.label = "Sum of Trophic Levels", y.label = "Transformed Sum of Shark Biomass in kg")
TL+theme(text = element_text(size = 18))


#########################################################################################################################################
#########################################################################################################################################
############################################# Best Model ################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Change the default "na.omit" to prevent models from being fitted to different datasets in case of missing values
options(na.action = "na.fail") 

# Build GLM with the shark biomass as response variable and all drivers using the gaussian family and Boxcox-Transformation
Best_GLM_SharkBiomass <- glm((Shark_Biomass)^0.2 ~ Depth+ 
                        Substrate+Exposure+MPA+
                        Time+Beach+Fishing+
                        Diving+Turtle+
                        Fish_Biomass+Trophic_Levels, family = gaussian, data = Data_SharkBiomass);

# Generate a model selection table of all models with combinations variables of the whole model
choose_MuMIn <- dredge(Best_GLM_SharkBiomass , extra = c(alist(AIC),"R^2"))

# Print output of the models with the difference in AIC being smaller than 2 away from the best model
summary(model.avg(choose_MuMIn, subset = delta < 2))

# Generate GLM of the best model
Best_GLM_SharkBiomass <- glm((Shark_Biomass)^0.2 ~ Beach+Diving+Exposure+MPA+Trophic_Levels+Turtle, family = gaussian, data = Data_SharkBiomass);

# Show output of best GLM
summary(Best_GLM_SharkBiomass)

# Show more outputs of best GLM
summ(Best_GLM_SharkBiomass, confint = TRUE, ci.width = .95)

# Plot the significant variables of the best GLM
# Distance to the beaches
Beach <- effect_plot(Best_GLM_SharkBiomass, pred = Beach, interval = TRUE, plot.points = TRUE, 
                        jitter = 0.05,
                        x.label = "Distance to Beaches in km", y.label = "Transformed Sum of Shark Biomass in kg")
Beach+theme(text = element_text(size = 18)) 

# Distance to the diving sites
Diving <- effect_plot(Best_GLM_SharkBiomass, pred = Diving, interval = TRUE, plot.points = TRUE, 
                           jitter = 0.05,
                           x.label = "Distance to Diving Sites in km", y.label = "Transformed Sum of Shark Biomass in kg")
Diving+theme(text = element_text(size = 18))

# Inside / Outside of the MPA
MPA <- effect_plot(Best_GLM_SharkBiomass, pred = MPA, interval = TRUE, plot.points = TRUE, 
                           jitter = 0.05,
                           x.label = "Location around FNA", y.label = "Transformed Sum of Shark Biomass in kg")
MPA+theme(text = element_text(size = 18))

# Distance to the turtle grounds
Turt <- effect_plot(Best_GLM_SharkBiomass, pred = Turtle, interval = TRUE, plot.points = TRUE, 
                         jitter = 0.05,
                         x.label = "Distance to Turtle Grounds in km", y.label = "Transformed Sum of Shark Biomass in kg")
Turt+theme(text = element_text(size = 18))

# Sum of trophic levels
TL <- effect_plot(Best_GLM_SharkBiomass, pred = Trophic_Levels, interval = TRUE, plot.points = TRUE, 
                       jitter = 0.05,
                       x.label = "Sum of Trophic Levels", y.label = "Transformed Sum of Shark Biomass in kg")
TL+theme(text = element_text(size = 18))

