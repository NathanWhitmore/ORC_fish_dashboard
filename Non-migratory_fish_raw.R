
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
library(scales)


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


### leaflet

leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>%
  
  # add base maps
  addProviderTiles("Esri.WorldImagery",
                   # give the layer a name
                   group = "World") %>%
  # set zoom and position
  setView(lng = 169,
          lat = -45.9,
          zoom = 8) %>%
  addPolylines(data = masked.rec.species %>% st_transform(crs =4326),
               color = "blue",
               fillOpacity = 0.001,
               weight = 2) %>% 
  addPolylines(data = known %>% st_transform(crs =4326),
              color = "red",
              fillOpacity = 0.001,
              weight = 3) %>%
  addMarkers(data = my.fish %>% st_transform(crs =4326)) 
  
  





#### 
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

# nearest point spatial feature
nearest_to_origin <- st_nearest_feature(obs, origin.points)
nearest_to_destination <- st_nearest_feature(des, destination.points)

# nearest edge on network
nearest_edge_origin <- st_nearest_feature(origin.points[nearest_to_origin,],
                                          edges)
nearest_edge_destination <- st_nearest_feature(destination.points[nearest_to_destination,],
                                               edges)

# check graph
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

# find nodes corresponding to edges
my.origin <- nodes.index %>% filter(edgeID == nearest_edge_origin )
origin.node <- my.origin$nodeID[1:2]
origin.node

my.destination <- nodes.index %>% filter(edgeID == nearest_edge_destination)
destination.node <- my.destination$nodeID[1:2]
destination.node



# find shortest path
node_o <- all_nodes[origin.node, ]
node_d <- all_nodes[destination.node, ]


# calculate paths (there are 4 variants)


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

path2 <- shortest_paths(
  graph = graph,
  from = origin.node[2] , # new origin
  to = destination.node[2],   # new destination
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path_graph2 <- graph %>%
  subgraph.edges(eids = path2$epath %>% unlist()) %>%
  as_tbl_graph()

path3 <- shortest_paths(
  graph = graph,
  from = origin.node[1] , # new origin
  to = destination.node[2],   # new destination
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path_graph3 <- graph %>%
  subgraph.edges(eids = path3$epath %>% unlist()) %>%
  as_tbl_graph()

path4 <- shortest_paths(
  graph = graph,
  from = origin.node[2] , # new origin
  to = destination.node[1],   # new destination
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path_graph4 <- graph %>%
  subgraph.edges(eids = path4$epath %>% unlist()) %>%
  as_tbl_graph()

### making the multilines singular
my.path1 <- path_graph1 %>% activate(edges) %>% as_tibble() %>% st_as_sf() 
my.path2 <- path_graph2 %>% activate(edges) %>% as_tibble() %>% st_as_sf() 
my.path3 <- path_graph3 %>% activate(edges) %>% as_tibble() %>% st_as_sf() 
my.path4 <- path_graph4 %>% activate(edges) %>% as_tibble() %>% st_as_sf()

ggplot()+
  geom_sf(data = my.path1)+
  geom_sf(data = my.path2)+
  geom_sf(data = my.path3)+
  geom_sf(data = my.path4)

### critical step  if 

if (nrow(my.path1) == 0 & nrow(my.path2) == 0 & nrow(my.path3) == 0 & nrow(my.path4) == 0) {
  
  ggplot() +
    theme_void()+
    # geom_sf(data = nodes, colour = "blue")+
    geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
    #  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'blue', size = 0.5) +
    # geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'yellow') +
    geom_sf(data = origin.points[nearest_to_origin,], colour = "red", size =5) +
    geom_sf(data = destination.points[nearest_to_destination,], colour = "red", size = 5)+
    ggtitle("No connecting path")+
    labs(subtitle = nrow(parts))
  
  
} else {
  
  # make a union (combine with shared geometry)
  my.path <- rbind(my.path1, my.path2, my.path3, my.path4) 
  
  # note the bit before is different in 4.2.1
  my.path <- my.path  %>%  
    summarize(geometry = st_union(st_geometry(my.path))) %>% 
    st_line_merge() 
  
  merged.line <- my.path  %>% 
    summarize(geometry = st_combine(st_geometry(my.path))) %>% 
    st_line_merge() 
  
  
  ### the blade 
  # cutting the line
  # make the blade for cutting
  blade <- rbind(origin.points[nearest_to_origin,], 
                 destination.points[nearest_to_destination,])  %>% st_as_sf()
  
  
  # snapping
  # turn reach into 10 m lengths
  ten.m.lengths <-round(as.numeric(st_length(merged.line))/ 10  , 0)
  merged.line.points <- st_sample(merged.line, size = ten.m.lengths, type = "regular") %>% st_as_sf()
  
  # find the nearest nodes which relate to the blade (observations)
  nrst <- st_nearest_points(blade, merged.line.points) %>%
    st_cast("POINT") %>% st_as_sf()
  
  # keep only the end nodes 
  p_snapped <- nrst[seq(from = 2, to = nrow(nrst), by = 2),]
  
  # merge the lines to produce an emulation of the reach
  merged.lines <- merged.line.points %>% st_cast("LINESTRING")
  
  # split the emulation by the blade
  parts <- st_collection_extract(st_split(merged.lines$x,p_snapped$x ),"LINESTRING") %>% 
    st_as_sf() 
  
  
  
  # find the middle point of a line 
  # if there are 3 lines it  will touch both other lines
  # if there are two lines it will intersect with the blade
  middle <- if(nrow(parts) == 3){
    parts[lengths(st_touches(parts)) > 1,]
  } else if(nrow(parts) == 2) {
    # parts %>% st_filter(st_buffer(blade, 10), .predicates = st_intersects) # buffering because can miss
    # this is where there is an ongoing issue
    touchy <- st_intersects(parts, st_buffer(blade, 200))
    parts[lengths(touchy) >= 2,]
  } else  {
    parts}
  
  
  
  
  # graph
  ggplot() +
    theme_void()+
    # geom_sf(data = nodes, colour = "blue")+
    geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
    #  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'blue', size = 0.5) +
    # geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'yellow') +
    geom_sf(data = origin.points[nearest_to_origin,], colour = "red", size =5) +
    geom_sf(data = destination.points[nearest_to_destination,], colour = "red", size = 5)+
    #geom_sf(data = parts[1,], colour = "yellow", lwd =1)+
    # geom_sf(data = parts[2,], colour = "red", lwd =1)+
    geom_sf(data = middle, colour = "firebrick", lwd =1)+
    ggtitle(paste0("Reach length = ", comma(st_length(middle) %>% round(0) %>% as.numeric()), " m"))+
    labs(subtitle = nrow(parts))
  
}

  
  
river.zone <- graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()
my.origin <- origin.points[nearest_to_origin,]
my.destination <- destination.points[nearest_to_destination,]


icon.home <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')
icon.star <- makeAwesomeIcon(icon = 'star', markerColor = 'green', library='ion')
icon.bridge <- makeAwesomeIcon(icon = 'road', markerColor = 'green', library='glyphicon')
icon.culvert <- makeAwesomeIcon(icon = 'refresh', markerColor = 'blue')
icon.unknown<- makeAwesomeIcon(icon = 'magnet', markerColor = 'red')
icon.natural <- makeAwesomeIcon(icon = 'leaf', markerColor = 'green')
icon.dam <- makeAwesomeIcon(icon = 'stop', markerColor = 'blue', library='ion')
icon.flap.no.culvert <- makeAwesomeIcon(icon = 'plus', markerColor = 'purple', library='ion')
icon.flap.culvert <- makeAwesomeIcon(icon = 'plus-circled', markerColor = 'purple', library='ion')
icon.ford.no.culvert <- makeAwesomeIcon(icon = 'android-arrow-dropright', markerColor = 'red', library='ion')
icon.ford.culvert <- makeAwesomeIcon(icon = 'android-arrow-dropright-circle', markerColor = 'red', library='ion')
icon.weir <- makeAwesomeIcon(icon = 'android-funnel', markerColor = 'blue', library='ion')
icon.other <- makeAwesomeIcon(icon = 'android-search', markerColor = 'blue', library='ion')
icon.bridge <- makeAwesomeIcon(icon = 'android-car', markerColor = 'blue', library='ion')


leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>%
  
  # add base maps
  addProviderTiles("Esri.WorldImagery",
                   # give the layer a name
                   group = "World") %>%
  # set zoom and position
  setView(lng = 169,
          lat = -45.9,
          zoom = 8) %>%
  addPolylines(data = river.zone  %>% st_transform(crs =4326),
               color = "blue",
               #fillOpacity = 0.001,
               weight = 2) %>% 
  addPolylines(data = middle %>% st_transform(crs =4326),
               color = "yellow",
               #fillOpacity = 0.001,
               weight = 5) %>%
  addAwesomeMarkers(data = my.origin  %>% st_transform(crs =4326),
                    icon = icon.bridge) %>%
  addAwesomeMarkers(data = my.destination  %>% st_transform(crs =4326),
             icon = icon.star)





  

