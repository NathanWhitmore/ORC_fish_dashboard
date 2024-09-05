# ORC fish dashboard

## Purpose 
The purpose of this R dashboard is to allow users to identify potential barriers for freshwater fish movement and migration as well as calculating distance between points on river networks

## Folder structure and data requirements
Because of the large size of the data it has not been uploaded to GitHub. If you are downloading this R project you will have to manually create and populate 6 additional folders within the project folder named (exact capitalisation required):
* Fish Barriers (containing one CSV file labelled "FishPassageSearchResults.csv") from the Fish Passage Assessment Tool avilable at https://niwa.co.nz/fish-passage/fish-passage-assessment-tool
* Catchments (containing one set of SHP files labelled "Catchments.shp" etc.) that represents the catchments of interest
* REC2_geodata_version_5 (containing a folder of GeodataBase files). This is available from https://niwa.co.nz/static/web/REC2_12Feb2014/nzRec2_v5/REC2_geodata_version_5.zip
* Regional Boundaries (containing one set of SHP files curently labelled "regional-council-2022-generalised.shp" etc.)
* Sightings (as many csv files as you like containing the columns "Species", "Lat", "Long"). This has been set up to receive file outputs from the ORC_eDNA dashboard.
* Your Non-Migratory Freshwater Fish Distribution (containing one set of SHP files labelled "Non_migratory_Freshwater_Fish_Distribution.shp" etc.). Downloadable from the New Zealand Freshwater Fish Database available at https://niwa.co.nz/freshwater/nz-freshwater-fish-database
  
## Support
Dashboard development was funded by Otago Regional Council and has been developed specifically for their needs.

## Licensing 
This code has been released under an MIT license.
