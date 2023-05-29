# Species distribution model for tiger sharks around Fernando de Noronha
# adapted from example of https://cran.r-project.org/web/packages/sdm/vignettes/quick_sdm.pdf

# Load packages
library(raster)
library(rgdal)
library(usdm)
library(mapview)
library(sdm)

# Read the species csv-file which contains a column of latitudes, longitudes and presence data
tigers <- read.csv("Data.csv")
class(tigers)

# Define the coordinates of the dataframe which changes the data type and removes the columns containing
# the longitude and latitude from data
coordinates(tigers) <- ~Long + Lat

# Plot data
plot(tigers)


# Import predictor variables, in this case the created raster layers
lst <- list.files("Path", pattern = 'asc$', full.names = T) # list the names of files in the specified path,
lst 

# Created a stacked raster object containing all raster layers
preds <- stack(lst)
preds 

# Check correlation between rasters
cor <- vifstep(preds)
cor

# Remove Port Layer due to correlation
layers <- exclude(preds, cor)
layers

# Plot a layer
plot(layers[[1]]) # plot first layer
points(tigers, cex=0.5, pch=16) # add the species point on the previous plot

# Define the coordinate reference system of the species file
# Done by reading the coordinate reference system of the empty raster which is the one one needs
proj4string(tigers) <- projection(raster())
proj4string(tigers)


# Preparation for SDM
# Create object that holds species and layers as explanatory variables
# bg generates pseudo-absence to not have only-presence data, bg randomly selects over the spatial data to remove bias of sampling effort
d <- sdmData(Species~., train=tigers, predictors=layers, bg = list(n=100, method = 'gRandom'))
d

# Fit the model using several methods and use replication technique to partition 
# them with a specification how many runs this should be carried out for (more than 50 points do 10 reps)
# output gives the average over all the runs if you have several replicates per used method
m1 <- sdm(Species~.,data=d,methods=c('glm', 'svm', 'rf', 'brt','mars'), replication = c('boot'), n=10)
m1
# keep in mind that the accuracy of the model fit will not really be 100% even though it says so in the output
# because here we use pseudo-absence and not real absence to validate the model

# Look at output of the model: can see importance of variables and threshold values that are needed later
gui(m1)

# Get importance of variables averaged over all models
getVarImp(m1, 1:50,wtest='test.dep')

# Evaluate performance of SDM
getEvaluation(m1,wtest='test.dep',stat=c('AUC','TSS'),opt=2)


# Predict data and save it to working directory which has 40 raster files (4 variables with ten repetitions)
# mean = T means the predictor uses the mean of the replications for each method
p <- predict(m1, layers, 'predictions.grd', mean = F, overwrite = T)
plot(p)

# Raster map depicts probability of occurrence for the current time
# predicts using weighted averages giving higher weights to those methods having higher accuracy based
# on TSS using optimization method 2 (see in Gui which one it is) or AUC without option specification
enTSS <- ensemble(m1, layers, 'ensemble.grd', setting = list(method = 'weighted', stat = "TSS", opt = 2), overwrite = T)
enAUC <- ensemble(m1, layers, 'ensemble.grd', setting = list(method = 'weighted', stat = "AUC"), overwrite = T)


# Final distribution of tiger sharks including map of Fernando de Noronha
plot(enTSS)
mapview(enTSS)
rfTSS <- writeRaster(enTSS, filename=file.path("ensemble-gRandom-TSS.tif"), format="GTiff", overwrite=TRUE)

plot(enAUC)
mapview(enAUC)
rfAUC <- writeRaster(enAUC, filename=file.path("ensemble-gRandom-AUC.tif"), format="GTiff", overwrite=TRUE)
