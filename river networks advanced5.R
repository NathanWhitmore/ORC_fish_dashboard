library(sf)
library(tidyverse)
library(gridExtra)
library(SpatialKDE)
library(tmap)
library(lwgeom) # for st_split
library(tidygraph)
library(sfnetworks)
library(nabor)
library(igraph)
library(ggrepel)
library(scales)




# read river
river <- st_read("river-environment-classification-new-zealand-2010.shp") %>%
  st_cast("LINESTRING")


# set.seed(Sys.time()) # random
# set.seed(2) # close in space but far away
# set.seed(17) # fail


# set.seed(8)

# sample river
samples <- st_sample(river, size = 5)  %>% st_as_sf() 
samples <-  samples %>% filter(!st_is_empty(.))  %>% st_as_sf() 


########## Network analysis
# Step 1:change river from shape file to network (make nodes and edges)

# make edges
edges <- river  %>%
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
graph <-  tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

# 
# Step 6:Get origin and destination
coords_o <- samples[1,] %>%
  st_coordinates()
coords_d <- samples[2,]  %>%
  st_coordinates() 

# Step 7: Get coordinates of all nodes in the network
all_nodes <- graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf()

all_coords <- all_nodes %>%
  st_coordinates()

# Step 8: Calculate nearest points on the network. 
# buffer to ensure is on line then extract first value
o <- st_intersects(st_buffer(samples[1,],  2), edges) 
o <- unlist(o)[1]
edge_index_o <- edges$edgeID[o[1]]
edge_index_o 

d <- st_intersects(st_buffer(samples[2,],  2), edges) %>% as.numeric()
d <- unlist(d)[1]
edge_index_d <- edges$edgeID[d]
edge_index_d 



# Step 9:  must relate edges to nodes

##### There are always 2 nodes connected with any line
# because we don't know where the sample is exactly at this stage 
# the easiest way forward is to assess the line across its maximum length
# this means using all combination of nodes

my.origin <- nodes.index %>% filter(edgeID == edge_index_o )
origin.node <- my.origin$nodeID[1:2]
origin.node

my.destination <- nodes.index %>% filter(edgeID == edge_index_d )
destination.node <- my.destination$nodeID[1:2]
destination.node

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

### critical step  if 

if (nrow(my.path1) == 0 & nrow(my.path2) == 0 & nrow(my.path3) == 0 & nrow(my.path4) == 0) {
  
  ggplot() +
    theme_void()+
    # geom_sf(data = nodes, colour = "blue")+
    geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
    #  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'blue', size = 0.5) +
    # geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'yellow') +
    geom_sf(data = samples[1,], colour = "forestgreen", size = 4) +
    geom_sf(data = samples[2,], colour = "purple", size = 4) +
    geom_label_repel(
      data = samples[1,],
      aes(geometry = x),
      label = "location A", 
      stat = "sf_coordinates",
      box.padding =1, 
      min.segment.length = 0
    ) +
    geom_label_repel(
      data = samples[2,],
      aes(geometry = x),
      label = "location B", 
      stat = "sf_coordinates",
      box.padding =1, 
      min.segment.length = 0
    ) +
    ggtitle("No connecting path")+
    labs(subtitle = nrow(parts))
  
  
} else {
  
  # make a union (combine with shared geometry)
  my.path <- rbind(my.path1, my.path2, my.path3, my.path4) 
  
  my.path <- my.path  %>%  
    summarize(geometry = st_union(geometry)) %>% 
    st_line_merge() 

  merged.line <- my.path  %>% 
    summarize(geometry = st_combine(geometry)) %>% 
    st_line_merge() 


### the blade 
# cutting the line
# make the blade for cutting
blade <- rbind(samples[1,], samples[2,])  %>% st_as_sf()


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
  geom_sf(data = samples[1,], colour = "forestgreen", size = 4) +
  geom_sf(data = samples[2,], colour = "purple", size = 4) +
 #geom_sf(data = parts[1,], colour = "yellow", lwd =1)+
# geom_sf(data = parts[2,], colour = "red", lwd =1)+
  geom_sf(data = middle, colour = "firebrick", lwd =1)+
  geom_label_repel(
    data = samples[1,],
    aes(geometry = x),
    label = "location A", 
    stat = "sf_coordinates",
    box.padding =1, 
    min.segment.length = 0
  ) +
  geom_label_repel(
    data = samples[2,],
    aes(geometry = x),
    label = "location B", 
    stat = "sf_coordinates",
    box.padding =1, 
    min.segment.length = 0
  ) +
  ggtitle(paste0("Reach length = ", comma(st_length(middle) %>% round(0) %>% as.numeric()), " m"))+
  labs(subtitle = nrow(parts))

}

#save.image("all.RData")
# load("all.RData")


# st_intersects(st_buffer(samples[1,],  0.1), edges)
# st_intersects(samples[1,], edges)

#ggplot()+
#  geom_sf(data = st_buffer(samples[1,],  2), size=5)+
#  geom_sf(data = edges, aes(colour = as.factor(edgeID)))+
#  coord_sf(xlim = c(2279315 -10, 2279315 + 10),
#           ylim = c(5556463 -10, 5556463 + 10))+
#  theme(legend.position = "none")


