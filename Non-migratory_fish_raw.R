
library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(flexdashboard)
library(plotly)
library(viridis)
library(DT)
library(readxl)
library(factoextra)
library(leaflet.extras)
library(geojsonsf)
library(jsonify)
library(stringr)
library(janitor)
library(lwgeom) # for st_split
library(tidygraph)
library(sfnetworks)
library(igraph)


# read in fish data
fish <- read.csv("Galaxias depressiceps.csv")

# change to sf
my.fish <- st_as_sf(fish,                         
               coords = c("Long", "Lat"),
               crs = 4326)

my.fish <- my.fish %>% st_transform(crs = 2193)

# read in province data
# select provincial boundaries
boundaries <- st_read("Regional boundaries\\regional-council-2022-generalised.shp", quiet = TRUE)
otago <- boundaries %>% filter(boundaries$REGC2022_1 == "Otago Region") %>%
  st_transform(2193)

# Reads in massive REC files (gbd format)
rec <- st_read("C:\\Users\\Nathan\\Downloads\\REC2_geodata_version_5\\nzRec2_v5.gdb")

# get known species data
non.mig <-  st_read(
  "Your Non-Migratory Freshwater Fish Distribution/Non_migratory_Freshwater_Fish_Distribution.shp",
  quiet = TRUE
)

my.species <- "Galaxias depressiceps"
known <- non.mig %>% filter(Species == my.species)

# compile max reasonable extent
box.known <-  
  st_as_sfc(st_bbox(known)) %>% 
  st_as_sf() %>%
  st_buffer(10000)

box.my <-  
  st_as_sfc(st_bbox(my.fish)) %>% 
  st_as_sf() %>% 
  st_buffer(10000)

# join and find extent for cropping
combined <- rbind(box.known, box.my)
combined <- st_as_sfc(st_bbox(combined)) %>% 
  st_as_sf() 

# crop rec
rec.species <- st_crop(rec, combined) 


# buffer in meters
my.buffer <- 20
known.buffered <- st_buffer(known, my.buffer )

# intersects with known distribution
#my.buffered.fish %>% st_filter(geometry, .predicates = st_intersects(known))

# color on basis of expectation
my.fish$Sighting <-
  lengths(st_intersects(my.fish, known.buffered )) %>% as.factor()
my.fish <- my.fish %>% mutate(Sighting =fct_recode(Sighting,
                                                   "Expected" = "1",
                                                   "New" = "0"))
         
# get catchments
catchments <- st_read("ORC_Catchments\\ORC_Catchments.shp") 
catchments$show <-
  ifelse(lengths(st_intersects(catchments,known.buffered)) > 0 |
                   lengths(st_intersects(catchments,my.fish)) >0,
                           "yes",
                           "no")

catchments.filtered <- catchments %>% filter(show == "yes")

ggplot()+
  theme_bw()+
  #geom_sf(data = box.obs, fill = NA, colour = "red")+
  #geom_sf(data = otago, fill = NA, colour = "forestgreen", lwd =2)+
  #geom_sf(data = combined , fill = NA, colour = "forestgreen")+
  #geom_sf(data = box.non.mig, fill = NA, colour = "purple", alpha = 0.2)+
  geom_sf(data = catchments.filtered, aes(fill = Catchment), alpha = 0.2)+
  geom_sf(data = rec.species) +
  #geom_sf(data = catchments, colour = "forestgreen", fill = NA)+
  geom_sf(data = known, colour = "red",  alpha = 0.2,lwd = 1)+
  geom_sf(data = my.fish, aes(colour = Sighting), fill = NA, size = 5)+
  scale_colour_manual(values = c("black", "red"))
 
  


# reduce 
masked.rec.species <- st_intersection(rec.species, catchments.filtered)

ggplot()+
  geom_sf(data = masked.rec.species)+
  geom_sf(data = catchments.filtered, aes(fill = Catchment), alpha = 0.1)+
  geom_sf(data = known, colour = "red", lwd = 1, alpha = 0.2)+
  geom_sf(data = my.fish, aes(colour = Sighting), fill = NA, size = 5)+
  scale_colour_manual(values = c("black", "red"))


# filter for single catchment
rec.catchment <- masked.rec.species %>% filter(Catchment == "Akatore Creek")
known.catchment <- st_intersection(known, rec.catchment)
my.fish.catchment <- st_crop(my.fish, rec.catchment)

# bounding box
bb <- st_bbox(rec.catchment)

# geometry of record
obs <- my.fish.catchment[1,]

# this may become obsolete
nearest <- st_nearest_feature(obs, rec.catchment)

ggplot()+
  #coord_sf(xlim =bb[c(1,3)],
  #         ylim =bb[c(2,4)])+
  geom_sf(data = rec.catchment)+
  geom_sf(data = known.catchment, colour = "red", lwd = 2) +
  geom_sf(data = rec.catchment[nearest,] , colour="forestgreen", 
          fill = NA, size = 5) +
  #geom_sf(data = known, colour = "purple", lwd = 3)+
  geom_sf(data = obs, size = 5, colour = "purple" )+
  geom_sf(data = rec.catchment[nearest,] , colour = "purple", fill = NA, lwd = 2)+
  ggtitle(paste0(unique(rec.catchment$Catchment),
                ": ",
                my.species))
  
ggsave("Example reach vs point.png", scale = 2)  

#############


########## Network analysis
# Step 1:change river from shape file to network (make nodes and edges)

# make edges
edges <- rec.catchment  %>%
  mutate(edgeID = c(1:n()))

# make nodes from edges
nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

# Step 3: Give each node a unique index

# make moniker
nodes.index  <- nodes

nodes.index$xy <-  paste(nodes.index$X, nodes.index$Y)
nodes.index$xy <-  factor(nodes.index$xy, levels = unique(nodes.index$xy))
nodes.index$nodeID <- as.numeric(nodes.index$xy)
nodes.index$xy <- NULL

nodes.index

# Step 5: Combine the node indices with the edges
source_nodes <- nodes.index %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes.index %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges <-  edges %>%
  mutate(from = source_nodes, to = target_nodes)

edges

# Step 4: Remove duplicate nodes
nodes <- nodes.index %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

nodes

# Step 5: Convert to tbl_graph
graph <-  tbl_graph(nodes = nodes, edges = as_tibble(edges), 
                    directed = FALSE) 


# this might be a version 4.2.1 solution only
graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(st_geometry(edges)))



# alternative
# mutate(length = st_length(geometry)) works in 4.3.1



# Step 6: Get coordinates of all nodes in the network
all_nodes <- graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf()

all_coords <- all_nodes %>%
  st_coordinates()

# Step 7: Get origin and destination edges

# semi-random destination
set.seed(487)
des <- st_sample(edges, size = 1, type = "random") %>% st_cast("POINT")


# find nearest point on line point will be nearest edge(need to snap to line)
feature_o <- edges[st_nearest_feature(obs, edges), ]
feature_d <- edges[st_nearest_feature(des, edges), ]

# sample feature every 10 m
ten.m.lengths <-round(as.numeric(st_length(feature_o))/ 10  , 0)
origin.points <- st_sample(feature_o, size = ten.m.lengths, type = "regular") %>%
  st_as_sf() %>% st_cast("POINT")

destination.points <- st_sample(feature_d, size = ten.m.lengths, type = "regular") %>%
  st_as_sf() %>% st_cast("POINT")

# nearest point on edge (feature)
nearest_to_origin <- st_nearest_feature(obs, origin.points)
nearest_to_destination <- st_nearest_feature(des, destination.points)

  
# random destination destination will be 


ggplot()+
  geom_sf(data = feature_o) +
  geom_sf(data = edges) +
  geom_sf(data = obs) +
  geom_sf(data = origin.points) +
  geom_sf(data = origin.points[nearest_to_origin,], colour = "red",
          size = 5)+
  geom_sf(data = des, colour = "orange", size = 5)+
  geom_sf(data = destination.points[nearest_to_destination,], colour = "red",
        size = 5)

# find shortest path
node_o <- all_nodes[origin.node, ]
node_d <- all_nodes[destination.node, ]

path1 <- shortest_paths(
  graph = graph,
  from = origin.node[1] , # new origin
  to = destination.node[1],   # new destination
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path_graph1 <- graph %>%
  subgraph.edges(eids = path1$epath %>% unlist()) %>%
  as_tbl_graph()

  
  
  


  

