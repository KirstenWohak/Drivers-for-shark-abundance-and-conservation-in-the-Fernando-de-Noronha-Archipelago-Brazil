# Tiger Sharks Species Distribution Model
Code of the species distribution model (SDM) for tiger sharks around the Fernando de Noronha Archipelago, Brazil. 

Values for the drivers (weighted average of distances to several locations) are calculated through code called "CreateGrid". This is used to generate raster layers needed for the SDM with the help of QGIS (version 3.20.1) following these steps:
- Save weighted average distances as a csv file and open it as a delimited layer in QGIS.
- Export the layer as a .shp file.
- Create a tiff file for each driver from this .shp file through the command rasterize (Raster -> Conversion -> Rasterize).
	- Setting:
	- Input layer is the .shp file.
	- Field to use for a burn-in value is the driver.
	- Select Georeferenced units as Output raster size units.
	- Resolution hoizontal and vertical is 0.001.
	- Output extent is the one of the shape file.
	- Give the rasterized file a name to save it.
- Repeat this for all drivers (after the first run one can select change parameters to execute the same for other drivers).
- Convert the tiff files to .asc files (Raster -> Conversion -> Translate).
- These .asc files are the files to use in the SDM code.

Code enquiries can be directed to: kirsten.wohak@imbrsea.eu.

No data or code can be taken out of this work without prior approval of the thesis promoter (Hudson Tercio Pinheiro, htpinheiro@usp.br).