# Principal Coordinate Analysis and CLustering for the marine trophic levels around Fernando de Noronha

# Load packages
library(vegan)
library(readxl)
library(lattice)
library("RVAideMemoire")
library(car)
library(ggplot2)
library(scales)
library(dplyr)
library(ecodist)
library(magrittr)
library(dplyr)
library(ggpubr)
library(NbClust)
library(clValid)
library (cluster)

# Import data
TL <- read_excel("~/Master-Thesis/Data/PCoA-TL-num.xlsx")
TL <- scale(TL)

### Create distance matrix using the environmental parameters based on which clustering will be performed
data <- TL[,-c(8,9,10,11)] # removing trophic levels from dataset
matrix <- vegdist(data, method = "bray") 

# Determine optimal number of clusters
res.nbclust <- NbClust(TL, distance = NULL, diss = matrix,
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")

# Determine optimal cluster method
intern <- clValid(TL, nClust = 2:24, 
                  clMethods = c("hierarchical","kmeans","pam"), validation = "internal")
# Show output
summary(intern)

# Import unscaled data again
TL <- read_excel("~/Master-Thesis/Data/PCoA-TL-num.xlsx")
data <- TL[,-c(8,9,10,11)]
matrix <- vegdist(data, method = "bray")

# Generate clustering
cluster.complete <- hclust (matrix, 'complete') # caluclates clusters

# Determine which BRUVs are in which clustering
groups <- cutree (cluster.complete, k = 5) # get groups
groups
group.order <- groups[cluster.complete$order] # groups ordered
group.order
group.in.cluster <- unique (group.order)
cluster.complete$order # get BRUVs from left to right from cluster

# Plot cluster
plot (cluster.complete)
rect.hclust (cluster.complete, border = group.in.cluster, k = 5) 
legend ('topleft', legend = paste ('Cluster', 1:5), pch = 22, col = 1:5, bty = 'n')


# First, PCoA with Bray-Curtis distances
PCoA <-  cmdscale(matrix, k=6, eig=TRUE)
par (mfrow = c(1,1))

# Define axis labels
labs_tr <- paste0("Dimension ", 1:2, " (",round(PCoA$eig[1:2]/sum(PCoA$eig), 3) * 100, "%)")

# First plot containing clustering and arrows for drivers
ordiplot (PCoA, type = 'n', xlab=labs_tr[1], ylab=labs_tr[2])
points (PCoA$points[,1],PCoA$points[,2], pch = 19, col = groups)
legend ('topright', pch = 19, col = 1:5, legend = paste ('Cluster', 1:5), bty = 'n')

# Define arrows
Sp_zone_d <- as.data.frame(TL[,c(1,2,3,4,5,6,7)])
Zone_sp_abund <- envfit(ord=PCoA, env=Sp_zone_d, permutation=999, na.rm = TRUE)

# Only add important arrows
plot(Zone_sp_abund, p.max = 0.1, col="black", font = 2, cex = 0.8) #p.max is the contribution of each driver


# Second plot containing clustering and arrows for trophic levels
ordiplot (PCoA, type = 'n', xlab=labs_tr[1], ylab=labs_tr[2])
points (x,y, pch = 19, col = groups)
legend ('topright', pch = 19, col = 1:5, legend = paste ('Cluster', 1:5), bty = 'n')

# Define arrows
Sp_zone_e <- as.data.frame(TL[,-c(1,2,3,4,5,6,7)])
Zone_e_abund <- envfit(ord=PCoA, env=Sp_zone_e, permutation=999, na.rm = TRUE)

# Only add important arrows
plot(Zone_e_abund, p.max = 0.1, col="black", font = 2, cex = 0.8) #p.max is the contribution of each species

