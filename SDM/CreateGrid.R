# Code to create grid for Species Distribution Model
# Load packages
library(readxl)
library("writexl")

#########################################################################################################################################
#########################################################################################################################################
########################################## Create Grid ##################################################################################
#########################################################################################################################################
#########################################################################################################################################

# Top left corner of grid
x_start = -3.7643756
y_start = -32.5137678
delta = 0.001 # step size

# Number of grid cells per longitude and latitude
num_lat = 171
num_long = 171

# Empty file which will contain grid
library(readxl)
Coord <- read_excel("Helper1.xlsx")

# Determine coordinates of all grid cells of the grid
x = x_start
for (j in 1:num_lat) # number of latitudes
{
  y = y_start
  for (i in 1:num_long) # number of longitudes
  {
    Coord[(j-1)*num_lat+i,1] <- x
    Coord[(j-1)*num_lat+i,2] <- y
    
    y = y+delta
  }
  x = x-delta
}

# Save to Excelfile
write_xlsx(Coord,"Coordinates.xlsx")

#########################################################################################################################################
#########################################################################################################################################
########################################## Calculate distances ##########################################################################
#########################################################################################################################################
#########################################################################################################################################

# Code to calculate distances from the Coordinates to the port, beaches, fishing and turtle grounds

# Import needed longitudes and latitudes of locations
Others <- read_excel("OtherLocations.xlsx")

# Load file which will contain the distances
Distances <- read_excel("Helper2.xlsx")

# Divide Fernando de Noronha into three parts according to the overall slope of the land. This is needed to determine which way to
# go around the island when the BRUV and the location are on different sides of the island or even on the same side, but land is in 
# the way
# Coodinates of the chosen points to separate the island are in the OtherLocations file

m1 = (Others$Lat[47]-Others$Lat[46])/(Others$Long[47]-Others$Long[46])
m2 = (Others$Lat[48]-Others$Lat[47])/(Others$Long[48]-Others$Long[47])
m3 = (Others$Lat[49]-Others$Lat[48])/(Others$Long[49]-Others$Long[48])

for (j in 1:40) # number of other locations
{for (i in 1:nrow(Coord)) # number of Coord
  {
  # values of needed slopes
  mp = (Coord$Lat[i]-Others$Lat[46])/(Coord$Long[i]-Others$Long[46])
  mq = (Others$Lat[j]-Others$Lat[46])/(Others$Long[j]-Others$Long[46])
  mp47 = (Coord$Lat[i]-Others$Lat[47])/(Coord$Long[i]-Others$Long[47])
  mq47 = (Others$Lat[j]-Others$Lat[47])/(Others$Long[j]-Others$Long[47])
  mp48 = (Coord$Lat[i]-Others$Lat[48])/(Coord$Long[i]-Others$Long[48])
  mq48 = (Others$Lat[j]-Others$Lat[48])/(Others$Long[j]-Others$Long[48])
  mp49 = (Coord$Lat[i]-Others$Lat[49])/(Coord$Long[i]-Others$Long[49])
  mq49 = (Others$Lat[j]-Others$Lat[49])/(Others$Long[j]-Others$Long[49])
  mp50 = (Coord$Lat[i]-Others$Lat[50])/(Coord$Long[i]-Others$Long[50])
  mq50 = (Others$Lat[j]-Others$Lat[50])/(Others$Long[j]-Others$Long[50])
  m4648 = (Others$Lat[48]-Others$Lat[46])/(Others$Long[48]-Others$Long[46])
  m4649 = (Others$Lat[49]-Others$Lat[46])/(Others$Long[49]-Others$Long[46])
  m4650 = (Others$Lat[50]-Others$Lat[46])/(Others$Long[50]-Others$Long[46])
  m4750 = (Others$Lat[50]-Others$Lat[47])/(Others$Long[50]-Others$Long[47])
  m4950 = (Others$Lat[50]-Others$Lat[49])/(Others$Long[50]-Others$Long[49])
  
###########################################################################################################################################################
###################### First part: BRUV left of S ########################################################################################################
###########################################################################################################################################################
  if (Coord$Long[i] <= Others$Long[46])
    {
    # if longitude of location (beach, port, fishing,... is smaller than long of Point S
      if (Others$Long[j] <= Others$Long[46]) 
      {
        distance = c()
        distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
        Distances[i,j+1] <- distance
        
    # if longitude of location (beach, port, fishing,... is in between S and MS
      } else if ((Others$Long[j] >=Others$Long[46]) & (Others$Long[j] <= Others$Long[47]))
      {
        if ((Coord$Lat[i] <= Others$Lat[46]) & (Coord$Lat[i] >= Others$Lat[46]))
        {
          if (mp <= mq)
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else 
          {
            distance = c()
            distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          }
        } else if ((Coord$Lat[i] >=Others$Lat[46]) & (Coord$Lat[i] <=Others$Lat[46]))
        {
          if (mp >= mq)
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else 
          {
            distance = c()
            distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          }
        } else if ((Coord$Lat[i] <=Others$Lat[46]) & (Others$Lat[j] <=Others$Lat[46]) & mq >=m1)
        {
          distance = c()
          distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          Distances[i,j+1] <- distance
        } else
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        }

    # if longitude of location (beach, port, fishing,... is in between MS and MN
      } else if ((Others$Long[j] >=Others$Long[47]) & (Others$Long[j] <= Others$Long[48]))
      {
        if (((mp <= m4648) & ( mq48 <= m4648)) || ((mp >= m1) & (mq <= m1)) || ((mq47 >= m2) &  (mp <= mq)) || ((mp48 >= m2) & (mq48 >= m2)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else
        {
          distance = c()
          distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          Distances[i,j+1] <- distance
        }
        
    # if longitude of location (beach, port, fishing,... is in between MN and N
      } else if ((Others$Long[j] >=Others$Long[48]) & (Others$Long[j] <= Others$Long[49]))
      {
        if ((mq48 >= m3) & (mp <= mq))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if (mq48 >= m3)
        {
          distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          Distances[i,j+1] <- distance
        } else if (mq50 <=mp50)
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if ((mq50 >=m4650) & (mp >= m1) & (mq50 <= m4950))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        } else if ((mq50 >=m4650) & (mp <=m1))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[50])*110.574)^2+((Others$Long[46]-Others$Long[50])*111.320*cos(Others$Lat[46]-Others$Lat[50]))^2)+
            sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        } else
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        }
        
        
    # if longitude of location (beach, port, fishing,... is right of N
      } else if (Others$Long[j] >=Others$Long[49])
      {
        if (((mp <= m4649) & (mq >= m4649)) || ((mp >= m4650) & (mq <=m4650)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if ((mp <= m1) & (mq50 <= m4650))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_south, distance_north)
          Distances[i,j+1] <- distance
        } else if ((mp47 <=mq47)) # & (Coord$Lat[i] <= Others$Lat[46])) HERE
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
          distance <- min(distance_south, distance_north)
          Distances[i,j+1] <- distance
        } else if (mp <= m1)
        {
          distance = c()
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[46])*110.574)^2+((Others$Long[50]-Others$Long[46])*111.320*cos(Others$Lat[50]-Others$Lat[46]))^2) +
            sqrt(((Others$Lat[j]-Others$Lat[50])*110.574)^2+((Others$Long[j]-Others$Long[50])*111.320*cos(Others$Lat[j]-Others$Lat[50]))^2)
          distance <- min(distance_south, distance_north)
          Distances[i,j+1] <- distance
        } else
        {
          distance = c()
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[j]-Others$Lat[46])*110.574)^2+((Others$Long[j]-Others$Long[46])*111.320*cos(Others$Lat[j]-Others$Lat[46]))^2)
          distance <- min(distance_south, distance_north)
          Distances[i,j+1] <- distance
        }
      }
    
    
###########################################################################################################################################################
###################### Second part: BRUV in between S and MS ########################################################################################################
###########################################################################################################################################################
    # if longitude of location of BRUV, tiger shark, grid is smaller than long of Point MS
    } else if ((Coord$Long[i] <= Others$Long[47]) & (Coord$Long[i] >=Others$Long[46]))
    {
      # if longitude of location (beach, port, fishing,... is smaller than long of Point S
      if (Others$Long[j] <= Others$Long[46])
      {
        if (((mp >= mq) & (mp >= m1)) || ((mp <= mq ) & (mp <= m1)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if((mp <= m1) & (mq <= m1) || (mp >=m1) & (mq >=m1))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        } else
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        }
      # else longitude of location (beach, port, fishing,... is in between MS and S
      } else if ((Others$Long[j] <= Others$Long[47]) & (Others$Long[j] >=Others$Long[46]))
      {
        if ((Coord$Lat[i] >= Others$Lat[46]) & (Coord$Lat[i] <= Others$Lat[46]))
        {
          if (mp >= mq)
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else 
          {
            distance = c()
            distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          }
        } else if ((Coord$Lat[i] <=Others$Lat[46]) & (Coord$Lat[i] >=Others$Lat[46]))
        {
          if (mp <= mq)
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else 
          {
            distance = c()
            distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          }
        } else if ((Others$Lat[j] <=Others$Lat[46]) & (Coord$Lat[i] <=Others$Lat[46]) & mp >=m1)
        {
          distance = c()
          distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          Distances[i,j+1] <- distance
        } else
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        }
      
        
      # else longitude of location (beach, port, fishing,... is in between MS and MN
      } else if ((Others$Long[j] <= Others$Long[48]) & (Others$Long[j] >=Others$Long[47]))
      {
        if(mp >=m1 & mq47 <=m2)
          {distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
            sqrt(((Others$Lat[j]-Others$Lat[50])*110.574)^2+((Others$Long[j]-Others$Long[50])*111.320*cos(Others$Lat[j]-Others$Lat[50]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north,distance_south)
          Distances[i,j+1] <- distance
          } else if(mp <=m1 & mq47 >=m2)
          {
            distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
              sqrt(((Others$Lat[j]-Others$Lat[49])*110.574)^2+((Others$Long[j]-Others$Long[49])*111.320*cos(Others$Lat[j]-Others$Lat[49]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north,distance_south)
            Distances[i,j+1] <- distance
          } else
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          }
        
        
      # else longitude of location (beach, port, fishing,... is in between MN and N
      } else if ((Others$Long[j] <= Others$Long[49]) & (Others$Long[j] >=Others$Long[48]))
      {
        if ((m4650 >= mq) & (mp >= m1))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        } else if (((mp >= m1) & (mq48 >= m3)) || ((mq50 <= mp50) & (mp <= m1)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if(mp <= m1 & mq48 >= m3)
        {
          distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
            sqrt(((Others$Lat[j]-Others$Lat[49])*110.574)^2+((Others$Long[j]-Others$Long[49])*111.320*cos(Others$Lat[j]-Others$Lat[49]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north,distance_south)
          Distances[i,j+1] <- distance
        } else if((mp >= m1) & (mq48 <= m3) & (mq50 >= m4750))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[j]-Others$Lat[49])*110.574)^2+((Others$Long[j]-Others$Long[49])*111.320*cos(Others$Lat[j]-Others$Lat[49]))^2)
          distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[50])*110.574)^2+((Others$Long[46]-Others$Long[50])*111.320*cos(Others$Lat[46]-Others$Lat[50]))^2) +
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north,distance_south)
          Distances[i,j+1] <- distance
        } else 
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        }

          
      # else longitude of location (beach, port, fishing,... is in right of N
      } else if (Others$Long[j] >=Others$Long[49])
      {   
        if ((mp >= m1) & (mq50 <= m4650))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_south, distance_north)
          Distances[i,j+1] <- distance
        } else if (((mp50 >= mq50) & (mp <= m1)) || ((mp >= m1) & (mp49 <= mq49)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if ((mq50 <= m4950) & (mp <= m1))
        {
          distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        } else if ((mq50 >= m4950) & (mp <= m1))
        {
          distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
            sqrt(((Others$Lat[j]-Others$Lat[49])*110.574)^2+((Others$Long[j]-Others$Long[49])*111.320*cos(Others$Lat[j]-Others$Lat[49]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north,distance_south)
          Distances[i,j+1] <- distance
        } else if ((mq50 >=m4650) & (mp >=m1))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[46])*110.574)^2+((Others$Long[50]-Others$Long[46])*111.320*cos(Others$Lat[50]-Others$Lat[46]))^2) +
            sqrt(((Others$Lat[j]-Others$Lat[50])*110.574)^2+((Others$Long[j]-Others$Long[50])*111.320*cos(Others$Lat[j]-Others$Lat[50]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        } else
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        }
      
        
        
        }
      
      
      
##########################################################################################################################################################
##################### Third part: BRUV between MS and MN ##################################################################################################
##########################################################################################################################################################
      # if longitude of location of BRUV, tiger shark, grid is in between MS and MN
      } else if ((Coord$Long[i] >=Others$Long[47]) & (Coord$Long[i] <= Others$Long[48]))      
      {
        # if longitude of location (beach, port, fishing,... is smaller than long of Point S
        if (Others$Long[j] <=Others$Long[46])
        {
          if (((mq48 <= m4648) & ( mp >= m4648)) || ((mq >= m1) & (mp <= m1)) || ((mp47 >= m2) &  (mq <= mp)) || ((mq48 >= m2) & (mp48 >= m2)))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else 
          {
            distance = c()
            distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          }
      
        # if longitude of location (beach, port, fishing,... is between S and MS
        } else if ((Others$Long[j] <=Others$Long[47]) & (Others$Long[j] >=Others$Long[46]))
          {
            if(mq >=m1 & mp47 <=m2)
            {
              distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
                sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
                sqrt(((Others$Lat[j]-Others$Lat[49])*110.574)^2+((Others$Long[j]-Others$Long[49])*111.320*cos(Others$Lat[j]-Others$Lat[49]))^2)
              distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
                sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
              distance <- min(distance_north >= distance_south)
              Distances[i,j+1] <- distance
            } else if(mq <=m1 & mp47 >=m2)
            {
              distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
                sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
                sqrt(((Others$Lat[j]-Others$Lat[50])*110.574)^2+((Others$Long[j]-Others$Long[50])*111.320*cos(Others$Lat[j]-Others$Lat[50]))^2)
              distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
                sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
              distance <- min(distance_north,distance_south)
              Distances[i,j+1] <- distance
            } else
            {
              distance = c()
              distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
              Distances[i,j+1] <- distance
            }
      
        # if longitude of location (beach, port, fishing,... is between MS and MN
        } else if ((Others$Long[j] <=Others$Long[48]) & (Others$Long[j] >=Others$Long[47]))
        {
          if((mp47 <= m4750) & (mq47 >= m2))
          {
            distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[50])*110.574)^2+((Others$Long[49]-Others$Long[50])*111.320*cos(Others$Lat[49]-Others$Lat[50]))^2) +
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else if (((mp47 <=m2) & (mp47 >=m4750) & (mq47 >= m2)) || ((mp47 >=m2) & (mq47 >=m4750) & (mq47 <=m2)))
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else if ((mp47 >= m2) & (mq47 <= m4750))
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
              sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          }
        
          
          # if longitude of location (beach, port, fishing,... is between MN and N
        } else if ((Others$Long[j] <=Others$Long[49]) & (Others$Long[j] >=Others$Long[48]))
        {
          if ((mq49 >= m3) & (mp47 >= m4750) & (mp47 <= m2))
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else if ((mq49 >= m3) & (mp47 <= m2))
          {
            distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[50])*110.574)^2+((Others$Long[49]-Others$Long[50])*111.320*cos(Others$Lat[49]-Others$Lat[50]))^2) +
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else if ((mp50 >= mq50) & (mp50 <=m4750))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else if (mp47 <= m2)
          {
            distance = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          } else if ((mp47 >= m2) & (mp49 <= mq49))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          }
          
        
        # if longitude of location (beach, port, fishing,... is right of N
        } else if (Others$Long[j] >= Others$Long[49])
        {  
          if (((mp47 <= m2) & (mp50 >= mq50)) || ((mp47 >= m2) & (mp49 <= mq49)))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else if ((mp50 >= m4750) & (mq49 <= m4950))
          {
            distance = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          } else if (mp50 >= m4750)
          {
            distance = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
              sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          } else 
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          }
        }
        
    
        
##########################################################################################################################################################
##################### Fourth part: BRUV between MN and N ##################################################################################################
##########################################################################################################################################################
        # if longitude of location of BRUV, tiger shark, grid is in between MN and N
      } else if ((Coord$Long[i] >=Others$Long[48]) & (Coord$Long[i] <= Others$Long[49]))      
      {
        # if longitude of location (beach, port, fishing,... is smaller than long of Point S
        if (Others$Long[j] <=Others$Long[46])
        {
          if ((mp47 <= m4750) & (mq47 >= m1))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else if ((mp48 >= m3) & (mq <= mp))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else if (mp48 >= m3)
          {
            distance = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          } else if (mp50 <=mq50)
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else if ((mp50 >=m4650) & (mq >= m1))
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else if ((mp50 >=m4650) & (mq <=m1))
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[50])*110.574)^2+((Others$Long[46]-Others$Long[50])*111.320*cos(Others$Lat[46]-Others$Lat[50]))^2)+
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          }
        
        # if longitude of location (beach, port, fishing,... is in between S and MS
        } else if ((Others$Long[j] <= Others$Long[47]) & (Others$Long[j] >=Others$Long[46]))
        {
          if (((mq >= m1) & (mp48 >= m3)) || ((mp50 <= mq50) & (mq <= m1)))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else if(mq <= m1 & mp48 >= m3)
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
              sqrt(((Others$Lat[j]-Others$Lat[50])*110.574)^2+((Others$Long[j]-Others$Long[50])*111.320*cos(Others$Lat[j]-Others$Lat[50]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north,distance_south)
            Distances[i,j+1] <- distance
          } else if((mq >= m1) & (mp48 <= m3) & (mp50 >= m4750))
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[j]-Others$Lat[49])*110.574)^2+((Others$Long[j]-Others$Long[49])*111.320*cos(Others$Lat[j]-Others$Lat[49]))^2)
            distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[50])*110.574)^2+((Others$Long[46]-Others$Long[50])*111.320*cos(Others$Lat[46]-Others$Lat[50]))^2) +
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north,distance_south)
            Distances[i,j+1] <- distance
          } else 
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          }
          
          
        # if longitude of location (beach, port, fishing,... is in between MS and MN
        } else if ((Others$Long[j] <= Others$Long[48]) & (Others$Long[j] >=Others$Long[47]))
        {  
          if ((mp <= m3) & (mp >= m4650) & (mq47 <= m2) & (mq50 <= mp50)) # added
          {
            distance = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          } else if (((mp48 >= m3) & (mq47 >= m2)) || ((mp50 <= mq50) & (mp48 <= m3) & (mq47 <= m2)) || ((mp47 <= m4750) & (mq47 <= m2)))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else if ((mp <= m4650) & (mp50 <= m4950) & (mq47 >= m2))
          {
            distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[50])*110.574)^2+((Others$Long[49]-Others$Long[50])*111.320*cos(Others$Lat[49]-Others$Lat[50]))^2) +
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else if ((mq50 <= m4950) & (mp48  >= m3))
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[50])*110.574)^2+((Others$Long[49]-Others$Long[50])*111.320*cos(Others$Lat[49]-Others$Lat[50]))^2) +
              sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } else
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          }
        
          
      # if longitude of location (beach, port, fishing,... is in between MN and N
        } else if ((Others$Long[j] <= Others$Long[49]) & (Others$Long[j] >=Others$Long[48]))
        { 
          if (((mp48 <= m3) & (mq48 >= m3)) || ((mp48 >= m3) & (mq48 <= m3))) 
          {
            distance = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            Distances[i,j+1] <- distance
          } else 
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          }
        
          
      # if longitude of location (beach, port, fishing,... is right of N
        } else if (Others$Long[j] >=Others$Long[49])
        {
          if (((mp49 <= mq49) & (mp48 >= m3)) || ((mp49 >= mq49) & (mp48 <= m3)))
          {
            distance = c()
            distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
            Distances[i,j+1] <- distance
          } else
          {
            distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
            distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
              sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
            distance <- min(distance_north, distance_south)
            Distances[i,j+1] <- distance
          } 
        }
        
        
###########################################################################################################################################################
###################### Fifth part: BRUV right of N ########################################################################################################
###########################################################################################################################################################
      # if longitude of location of BRUV, tiger shark, grid is smaller than long of Point S
      } else if (Coord$Long[i] >= Others$Long[49])
      {
      # if longitude of location (beach, port, fishing,... is left of S
      if (Others$Long[j] <= Others$Long[46])
      {
        if ((mp47 <= m4750) & (mq47 >= m1))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if (((mq <= m4649) & (mp >= m4649)) || ((mq >= m4650) & (mp <=m4650)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if ((mq50 <=mp50) & (Others$Lat[j] <= Others$Lat[46]))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
          distance <- min(distance_south, distance_north)
          Distances[i,j+1] <- distance
        } else if (mq <= m1)
        {
          distance = c()
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[46])*110.574)^2+((Others$Long[50]-Others$Long[46])*111.320*cos(Others$Lat[50]-Others$Lat[46]))^2) +
            sqrt(((Others$Lat[j]-Others$Lat[46])*110.574)^2+((Others$Long[j]-Others$Long[46])*111.320*cos(Others$Lat[j]-Others$Lat[46]))^2)
          distance <- min(distance_south, distance_north)
          Distances[i,j+1] <- distance
        } else
        {
          distance = c()
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[j]-Others$Lat[49])*110.574)^2+((Others$Long[j]-Others$Long[49])*111.320*cos(Others$Lat[j]-Others$Lat[49]))^2)
          distance <- min(distance_south, distance_north)
          Distances[i,j+1] <- distance
        }
        
        
      # if longitude of location (beach, port, fishing,... is in between S and MS
      } else if ((Others$Long[j] <= Others$Long[47]) & (Others$Long[j] >=Others$Long[46]))
      {
        if (((mq50 >= mp50) & (mq <= m1)) || ((mq >= m1) & (mq49 <= mp49)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if ((mp50 <= m4950) & (mq <= m1))
        {
          distance_north = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
          Distances[i,j+1] <- distance
        } else if ((mp50 >= m4950) & (mq <= m1))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
            sqrt(((Others$Lat[j]-Others$Lat[50])*110.574)^2+((Others$Long[j]-Others$Long[50])*111.320*cos(Others$Lat[j]-Others$Lat[50]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north,distance_south)
          Distances[i,j+1] <- distance
        } else if ((mp50 >= m4650) & (mq >= m1))
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[46])*110.574)^2+((Others$Long[50]-Others$Long[46])*111.320*cos(Others$Lat[50]-Others$Lat[46]))^2) +
            sqrt(((Others$Lat[j]-Others$Lat[46])*110.574)^2+((Others$Long[j]-Others$Long[46])*111.320*cos(Others$Lat[j]-Others$Lat[46]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        } else
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        }
      
        
      # if longitude of location (beach, port, fishing,... is in between MS and MN
      } else if ((Others$Long[j] <= Others$Long[48]) & (Others$Long[j] >=Others$Long[47]))
      {
        if (((mq47 <= m2) & (mq50 >= mp50)) || ((mq47 >= m2) & (mq49 <= mp49)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else if ((mq50 >= m4750) & (mp49 <= m4950))
        {
          distance = sqrt(((Others$Lat[50]-Coord$Lat[i])*110.574)^2+((Others$Long[50]-Coord$Long[i])*111.320*cos(Others$Lat[50]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
          Distances[i,j+1] <- distance
        } else if (mq50 >= m4750)
        {
          distance = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[50]-Others$Lat[49])*110.574)^2+((Others$Long[50]-Others$Long[49])*111.320*cos(Others$Lat[50]-Others$Lat[49]))^2) +
            sqrt(((Others$Lat[50]-Others$Lat[j])*110.574)^2+((Others$Long[50]-Others$Long[j])*111.320*cos(Others$Lat[50]-Others$Lat[j]))^2)
          Distances[i,j+1] <- distance
        } else 
        {
          distance = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          Distances[i,j+1] <- distance
        }
       
        
      # if longitude of location (beach, port, fishing,... is in between MN and N
      } else if ((Others$Long[j] <= Others$Long[49]) & (Others$Long[j] >=Others$Long[48]))
      {
        if (((mq49 <= mp49) & (mq48 >= m3)) || ((mq49 >= mp49) & (mq48 <= m3)))
        {
          distance = c()
          distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
          Distances[i,j+1] <- distance
        } else
        {
          distance_north = sqrt(((Others$Lat[49]-Coord$Lat[i])*110.574)^2+((Others$Long[49]-Coord$Long[i])*111.320*cos(Others$Lat[49]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[49]-Others$Lat[j])*110.574)^2+((Others$Long[49]-Others$Long[j])*111.320*cos(Others$Lat[49]-Others$Lat[j]))^2)
          distance_south = sqrt(((Others$Lat[46]-Coord$Lat[i])*110.574)^2+((Others$Long[46]-Coord$Long[i])*111.320*cos(Others$Lat[46]-Coord$Lat[i]))^2) + 
            sqrt(((Others$Lat[46]-Others$Lat[j])*110.574)^2+((Others$Long[46]-Others$Long[j])*111.320*cos(Others$Lat[46]-Others$Lat[j]))^2)
          distance <- min(distance_north, distance_south)
          Distances[i,j+1] <- distance
        } 
      
      # if longitude of location (beach, port, fishing,... is left of N
      } else if (Others$Long[j] >= Others$Long[49])
      {
        distance = c()
        distance = sqrt(((Others$Lat[j]-Coord$Lat[i])*110.574)^2+((Others$Long[j]-Coord$Long[i])*111.320*cos(Others$Lat[j]-Coord$Lat[i]))^2)
        Distances[i,j+1] <- distance
      }
    }
  }
}


# Save to Excelfile
write_xlsx(Distances,"DistancesGrid.xlsx")

#########################################################################################################################################
#########################################################################################################################################
########################################## Calculate weighted average distances #########################################################
#########################################################################################################################################
#########################################################################################################################################

# Code to calculate weighted average distances and closest location

# Import percentages to according to which the distances will be weighed
Perc <- read_excel("PercentagesLocations.xlsx")
AvCl_Distances <- read_excel("Helper3.xlsx")

for (i in 1:nrow(Coord)) # number of Coord
  { 
    Beach_Av <- Distances[i,3]*Perc[2,8]+ Distances[i,4]*Perc[3,8] + Distances[i,5]*Perc[4,8] +
                Distances[i,6]*Perc[5,8]+ Distances[i,7]*Perc[6,8]
    
    Fish_Av<- Distances[i,8]*Perc[7,6] + Distances[i,9]*Perc[8,6] + Distances[i,10]*Perc[9,6] +
              Distances[i,11]*Perc[10,6] + Distances[i,12]*Perc[11,6] + Distances[i,13]*Perc[12,6] +
              Distances[i,14]*Perc[13,6] + Distances[i,15]*Perc[14,6] + Distances[i,16]*Perc[15,6]
    
    Dive_Av <- Distances[i,17]*Perc[16,10] + Distances[i,18]*Perc[17,10] + Distances[i,19]*Perc[18,10] +
                Distances[i,20]*Perc[19,10] + Distances[i,21]*Perc[20,10] + Distances[i,22]*Perc[21,10] +
                Distances[i,23]*Perc[22,10] + Distances[i,24]*Perc[23,10] + Distances[i,25]*Perc[24,10] +
                Distances[i,26]*Perc[25,10] + Distances[i,27]*Perc[26,10] + Distances[i,28]*Perc[27,10] + 
                Distances[i,29]*Perc[28,10] + Distances[i,30]*Perc[29,10] + Distances[i,31]*Perc[30,10] + 
                Distances[i,32]*Perc[31,10] + Distances[i,33]*Perc[32,10]
    
    Turtle_Av <- Distances[i,34]*Perc[33,11] + Distances[i,35]*Perc[34,11] + Distances[i,36]*Perc[35,11] +
                  Distances[i,37]*Perc[36,11] + Distances[i,38]*Perc[37,11] + Distances[i,39]*Perc[38,11] +
                  Distances[i,40]*Perc[39,11] + Distances[i,41]*Perc[40,11] 
    
    AvCl_Distances[i,1] <- Beach_Av
    AvCl_Distances[i,2] <- Fish_Av
    AvCl_Distances[i,3] <- Dive_Av
    AvCl_Distances[i,4] <- Turtle_Av
    
  }

# Combine the coordinates of the grid cells to the weighted average distances
AvCl_Grid <- cbind(Coord, Distances[c(1:29241),2], AvCl_Distances)

# Save to Excelfile
write_xlsx(AvCl_Grid,"GridAvCl.xlsx")
