packages <- c("tidyverse", "magrittr", "ggiraph")
install.packages(packages[!which(packages %in% installed.packages()$Package)])
dummy <- sapply(packages, library, character.only = T)

# download.file is not working.. files are broken it seems
#download.file(url = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IND_2_sf.rds", destfile = "gadm36_IND_2_sf.rds")
#download.file(url = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IND_1_sf.rds", destfile = "gadm36_IND_1_sf.rds")

### ggiraph attempt ----
state_dat <- readRDS("gadm36_IND_1_sf.rds")
dist_dat <- readRDS("gadm36_IND_2_sf.rds")

cleaner_for_ggplot <- function(dat){
    lengths <- sapply(dat$geometry, function(x){length(unlist(x))/2})
    pols <- dat$geometry %>% lapply(function(x){
        x <- x %>% lapply(function(u){u[[1]]})
        if(length(x) > 1) 
        {
            x <- x %>% reduce(rbind.data.frame) %>% as.list
        }
        else 
        {
            x <- x %>% data.frame %>% as.list
        }
        return(x)
    })
    
    pols_x <- lapply(pols, function(x){x[[1]]})
    pols_y <- lapply(pols, function(y){y[[2]]})
    
    df <- dat
    df <- df[, which(colnames(df) != "geometry")]
    df$X <- pols_x
    df$Y <- pols_y
    df$population <- 123
    df <- unnest(df)
    return(df)
    
}

state_df <- cleaner_for_ggplot(state_dat)
dist_df <- cleaner_for_ggplot(dist_dat)

p <- ggplot() + 
    geom_polygon_interactive(data = dist_df, mapping = aes(x = X, y = Y, fill = NAME_2, 
                                                           tooltip = paste(NAME_2, "\n", NAME_1, "\n", population), data_id =NAME_2)) + 
    geom_polygon(data = state_df, mapping = aes(x = X, y = Y, color = NAME_1), fill = NA) +
    scale_color_manual(values = rep("black", length(unique(state_df$NAME_1)))) +
    theme(legend.position = "none")
girafe(code = print(p))
    
    ## Census
    
    dat <- read.csv("./Kaggle_data/elementary_2015_16.csv")
    
    state_pops <- dat %>% select(STATE.NAME, TOTAL.POULATION) %>%
        group_by(STATE.NAME) %>%
        summarise(Pop = sum(TOTAL.POULATION, na.rm = T))
    colnames(state_pops)[1] <- "NAME_1"
    state_pops$NAME_1 <- as.character(state_pops$NAME_1)
    
    state_pops$NAME_1[which(str_detect(state_pops$NAME_1, "DELHI"))] <- "NCT OF DELHI"
    state_pops <- state_pops[(order(state_pops$NAME_1)), ]


### LEAFLET ----

#install.packages("leaflet")
library(leaflet)
library(tidyverse)


d2 <- readRDS("gadm36_IND_2_sp.rds")
d1 <- readRDS("gadm36_IND_1_sp.rds")

d1@data$NAME_1 <- toupper(d1@data$NAME_1)
d1@data$Pop <- state_pops$Pop
d1@data$Pop_lab <- state_pops$Pop/1e06

d2@data$NAME_2 <- toupper(d2@data$NAME_2)

hc_data <- read.csv("./Health_centers.csv", stringsAsFactors = F)
hc_data$Latitude <- as.numeric(hc_data$Latitude)
hc_data$Longitude <- as.numeric(hc_data$Longitude)
hc_data <- hc_data[complete.cases(hc_data$Latitude),]
hc_data <- hc_data[complete.cases(hc_data$Longitude),]
hc_data <- hc_data[-which(hc_data$Longitude > 100 | hc_data$Longitude < 65 | hc_data$Latitude > 38, hc_data$Latitude < 6 ), ]

x <- sapply(hc_data$Facility.Type, function(y){
         if(y == "chc") return("red")
         else if (y == "dis_h") return("green")
         else if (y == "phc") return("blue")
         else if (y == "sub_cen") return("purple")
         else return("orange")
     })
hc_data$colors <- x

pal <- colorBin("YlOrRd", domain = d1@data$Pop, bins = 8)

icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion', 
    markerColor = hc_data$colors
)


m1 <- leaflet() %>%
    addTiles(group = "HealthCenters") %>%
    setView(lng = 77, lat = 13, zoom = 6) %>%
    addPolygons(data = d1,
                fillColor = ~pal(d1@data$Pop),
                color = "black", 
                group = "States",
                weight = 2,
                opacity = 1,
                dashArray = "3",
                fillOpacity = 0.8,
                highlight = highlightOptions(weight = 5),
                label = d1@data$NAME_1
                ) %>%
    addAwesomeMarkers(data = hc_data,
               lat = ~Latitude, lng = ~Longitude, icon = icons,
               clusterOptions = markerClusterOptions(),
               group = "HealthCenters", label = paste0(hc_data$Facility.Type, "\n", hc_data$District.Name)
            ) %>%
    addPolygons(data = d2, color = "blue", group = "Districts", weight = 1, 
                highlightOptions = highlightOptions(weight = 6),
                label = paste0(d2@data$NAME_2, "\n", d2@data$NAME_1)) %>% 
    addMiniMap()%>%
    addLayersControl(overlayGroups = c("HealthCenters", "States", "Districts")) %>%
    addLegend(pal = pal, values = d1@data$Pop_lab, opacity = 0.7, title = "Population (millions)",
              position = "bottomright")
m1

m <- leaflet() %>% addTiles(group = "Markers", layerId = "World") %>%
                    addMarkers(lat = hc_data$Latitude, lng = hc_data$Longitude, 
                               clusterOptions = markerClusterOptions(), 
                               group = "Markers") %>%
    addPolygons(data = d1,
                fillColor = ~pal(d1@data$Pop),
                color = "black", 
                group = "States",
                layerId = "India",
                weight = 1,
                opacity = 1,
                dashArray = "3",
                fillOpacity = 0.8,
                highlight = highlightOptions(weight = 5)) %>%
    addLayersControl(overlayGroups = c("Markers", "States"),baseGroups = c("World","India"))


library(htmlwidgets)
saveWidget(m1, "map_full1.html", selfcontained = F)
saveWidget(m, "map3.html", selfcontained = F)
library(mapview)
mapshot(m1, url = "map1.html", remove_url = F)
