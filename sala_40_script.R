#install.packages(toponym)
library(toponym)
#install.packages(ggplot2)
library(ggplot2)
#install.packages("magick")
library(magick)
#install.packages("geodata")
library(geodata)
#install.packages("sf")
library(sf)


#geonames data downloaded on 3. September 2026
#osm data downloaded on 4. September 2026

´
#osm data extraction function
osm_data <- function(country = "IN"){
  ### download Open Street map (OSM) data
  osm_stored <- osm(country, "places", path = getwd())
  ### loads the data; requires sf package
  osm_stored <- sf::st_as_sf(osm_stored)
  ### sf's geom column into a dataframe
  coords <- as.data.frame(st_coordinates(osm_stored))
  colnames(coords) <- c("longitude", "latitude")
  ### sf into df
  osm_stored <- as.data.frame(osm_stored)
  # remove geom column
  osm_stored <- osm_stored[,-4]
  # add long and lat column
  osm_stored$latitude <- coords$latitude
  osm_stored$longitude <- coords$longitude
  osm_stored$'country code' <- country
  rm(coords)
  return(osm_stored)
}


osm_IN <- osm_data()

### function to search OSM data and add matched strings to 'group' column
### The function returns a data frame where every row is a match. There are rows with the full name and coordinates.
search_osm <- function(name_pattern, osm_stored = osm_IN){
  i <- grep(name_pattern, osm_stored$name)
  
  i <- osm_stored[i,]
  
  m_strings <- rep(NA,nrow(i)) # vector with NA values of gn length
  m <- regexpr(paste(name_pattern, collapse = "|"), i[, "name"], perl = TRUE) #pos of match
  m[is.na(m)] <- -1 # replace NA with -1
  m_strings[m!=-1] <- regmatches(i[, "name"], m) # gets matched strings or NA
  
  i[, "group"] <- m_strings # adds matches to "group" column
  return(i)
}

#polygon Karnataka
poly_KN <- createPolygon("IN", region_name = "Karnataka", retrieve = TRUE)

### The following functions custom_freq_end() and custom_freq_start are not used in this script. 
# I have used them in the console. The purpose is to search frequent strings in custom data sets, e.g. in select Indian states.
custom_freq_end <- function(data, len = 2, limit = 5){
  toponyms <- paste(
    # creates a reg expr looking for strings of length "len"
    regmatches(
      data$name,
      regexpr(paste0(paste(replicate(len, "."), collapse = ""), "$"), data$name))
  )
  freq_top <- table(toponyms)[order(table(toponyms), decreasing = TRUE)][1:limit] # only a selection of the most frequent toponyms
  return(freq_top)
}

custom_freq_start <- function(data, len = 2, limit = 5){
  toponyms <- paste(
    # creates a reg expr looking for strings of length "len"
    regmatches(
      data$name,
      regexpr(paste0("^", paste(replicate(len, "."), collapse = "")), data$name))
  )
  freq_top <- table(toponyms)[order(table(toponyms), decreasing = TRUE)][1:limit] # only a selection of the most frequent toponyms
  return(freq_top)
}

#############################################################################
#############################################################################
#############################################################################
##### Slavic example
##### Slavic example
##### Slavic example

data_itz <- top(strings = "itz$", countries = "DE")
map <- mapper(data_itz, title = "Traces of Slavic presence")
plot_name <- "itz_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)

View(data_itz)

# Basic Commands
country(c("Italy", "Switzerland"))
data_sopra <- top(strings = "Sopra$", countries = c("IT", "CH"))

# Frequent toponyms
topFreq(countries = "BR", len = 6, limit = 10)
grande <- top(strings = "Grande$", countries = "BR")
map <- mapper(grande, regions = 1)
plot_name <- "grande_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)

# Further orientation
# ?top



#############################################################################
#############################################################################
#############################################################################
#### Examples from India
#### Examples from India
#### Examples from India
#### Examples from India

#halli & palli
halli_palli <- search_osm(c("[ph]alli$"))
halli_palli$group[halli_palli$group == "palli"] <- "-palli"
halli_palli$group[halli_palli$group == "halli"] <- "-halli"
map <- mapper(halli_palli, plot_size = 0, regions = 1, title = "-paḷḷi / -haḷḷi (OSM data)", color = c("red", "navyblue"))
plot_name <- "halli_palli_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)


#restrict search to halli
halli <- search_osm(c("halli$"))
halli$group[halli$group == "halli"] <- "-halli"
map <- mapper(halli, plot_size = 0, regions = 1, title = "-haḷḷi (OSM data)", frame = poly_KN)
plot_name <- "halli_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)



nrow(search_osm(c("halli")))
#5883 matches

#as suffix
nrow(search_osm(c("halli$")))
#5788 matches



################################### Coastal
################################### Coastal
################################### Coastal


######################### MANE
######################### MANE
######################### MANE

#mani
nrow(top(c("mani$"), "IN"))
#128 matches, many in Tamil Nadu

mani <- top("mani$", "IN")
mani$group[mani$group == "mani"] <- "-mani"
map <- mapper(mani, plot_size = 0, regions = 1, title = "places ending in -mani")
plot_name <- "mani_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)


#mane
nrow(top(c("mane"), "IN"))
#413 matches

#as suffix
nrow(top(c("mane$"), "IN"))
#361 matches


mane <- top(c("mane$"), "IN")

#latitudinal outliers
length(mane[mane$latitude > 15, "name"])
#4 matches
mane[mane$latitude > 15, "name"]
#"Satmane", "Dhamane", "Sātmane", "Makhdumane"

#longitudinal outliers
length(mane[mane$longitude > 76.3, "name"])
#4 matches
mane[mane$longitude > 76.3, "name"]
#"Karemane", "Makhdumane", "Totadamane", "Nelamane"

#remove outliers
mane_sel <- mane[!mane$latitude > 15,]
mane_sel <- mane_sel[!mane_sel$longitude > 76.3,]

mane_sel$group[mane_sel$group == "mane"] <- "-mane"
map <- mapper(mane_sel, plot_size = 0, regions = 1, title = "-mane", frame = poly_KN)
plot_name <- "mane_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)

######################### GADDE
######################### GADDE
######################### GADDE
#gadde, gādde and gaddu
nrow(top(c("g[aā]ddu"), "IN"))
#no gaddu
nrow(top(c("g[aā]dde"), "IN"))
#302 matches
nrow(top(c("ganna$"), "IN"))
#2 matches
nrow(top(c("garde$"), "IN"))
#0 matches

#as suffix
gadde <- top(c("g[aā]dde$"), "IN")
nrow(gadde)
#296 matches
table(gadde$group)
#295 gadde 1 gādde

#return entry with gādde
gadde[gadde$group == "gādde", "name"]

#merge both groups 'gadde' and 'gādde'
gadde$group <- "gadde"

#latitudinal outliers
length(gadde[gadde$latitude < 12.2, "name"])
#4 matches
gadde[gadde$latitude < 12.2, "name"]
#"Sujjalugadde", "Hagalgeregadde", "Bhairigadde", "Ajarigadde"   

#longitudinal outliers
length(gadde[gadde$longitude > 77, "name"])
#7 matches
gadde[gadde$longitude > 77, "name"]
#"Chatlupalligadde", "Chintamānigadde", "Guntūrugadde"
#"Kadaroligadde", "Mākalavārigadde", "Kurapalligadde", "Palligadde"

#remove outliers
gadde_sel <- gadde[!gadde$latitude < 12.2,]
gadde_sel <- gadde_sel[!gadde_sel$longitude > 76.5,]
gadde_sel$group[gadde_sel$group == "gadde"] <- "-gadde"
map <- mapper(gadde_sel, plot_size = 0, regions = 1, title = "-gadde")
plot_name <- "gadde_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)

######################### BAIL
######################### BAIL
######################### BAIL


place_data <- top(c("vayalu?$"), "IN")
mapper(place_data, plot_size = 0, regions = 1)
#vayal at the northern border of Kerala and Tamil Nadu
#vayal at coast of Tamil Nadu
#1 match for vayalu$

place_data <- top(c("vailu?$"), "IN")
mapper(place_data, plot_size = 0, regions = 1)
#4 matches for vail around Kerala Karnataka border

place_data <- top(c("bayalu?$"), "IN")
mapper(place_data, plot_size = 0, regions = 1)
#a few matches in Andhra Pradesh

place_data <- top(c("bayilu?$"), "IN")
mapper(place_data, plot_size = 20, regions = 1)
#2 matches in Kerala for bayil

nrow(top(c("baylu?$"), "IN"))
#0 matches
nrow(top(c("baialu?$"), "IN"))
#0 matches

place_data <- top(c("^Bail[auāū]"), "IN")
mapper(place_data, plot_size = 0, regions = 1)
#25 matches for forms of Bail in the beginning



nrow(top(c("bailu"), "IN"))
#48 matches
nrow(top(c("bail"), "IN"))
#276 matches

#as suffix
nrow(top(c("bailu$"), "IN"))
#47 matches
nrow(top(c("bail$"), "IN"))
#209 matches

bail <- top(c("bailu?$"), "IN")
mapper(bail)

#latitudinal outliers
length(bail[bail$latitude > 15.2, "name"])
#8 matches
bail[bail$latitude > 15.2, "name"]
# [1] "Tangarbail"     "Bābail"         "Purwa Mahābail" "Khabail"        "Debail"        
# [6] "Yalebail"       "Bihābail"       "Chhabail" 

#longitudinal outliers
length(bail[bail$longitude > 75.8, "name"])
#22 matches
bail[bail$longitude > 75.8, "name"]
# [1] "Tangarbail"     "Bābail"         "Purwa Mahābail" "Khabail"        "Mājarabailu"    "Garginbailu"    "Kārabailu"     
# [8] "Debail"         "Manibailu"      "Marlabailu"     "Kollabailu"     "Donabailu"      "Donabailu"      "Kottubailu"    
# [15] "Velugulabailu"  "Tummalabailu"   "Marlabailu"     "Gundalabailu"   "Pidatalabailu"  "Attubail"       "Bihābail"      
# [22] "Chhabail"

#remove outliers
bail_sel <- bail[!bail$latitude > 15.2,]
bail_sel <- bail_sel[!bail_sel$longitude > 75.8,]
bail_sel$group[bail_sel$group == "bail"] <- "-bail"
bail_sel$group[bail_sel$group == "bailu"] <- "-bailu"
map <- mapper(bail_sel, plot_size = 0, regions = 1, title = "-bail(u)", color = c("red", "navyblue"))
plot_name <- "bailu_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)




######################### BETTA & MALE
######################### BETTA & MALE
######################### BETTA & MALE

place_data <- top(c("bett[au]$"), "IN")
mapper(place_data, plot_size = 0, regions = 1)

nrow(top(c("betta"), "IN"))
#48 matches
nrow(top(c("bettu"), "IN"))
#80 matches
nrow(top(c("male"), "IN"))
#40 matches


#as suffix
nrow(top(c("betta$"), "IN"))
#29 matches
nrow(top(c("bettu$"), "IN"))
#80 matches
nrow(top(c("male$"), "IN"))
#29 matches


bettu <- top(c("bettu$"), "IN")
mapper(bettu)
male <- top(c("male$"), "IN")
mapper(male, plot_size = 1)

#latitudinal outliers
length(bettu[bettu$latitude < 12, "name"])
#1 match
bettu[bettu$latitude < 12, "name"]
# [1] "Kattēbettu"

#remove outliers
bettu_sel <- bettu[!bettu$latitude < 12,]
bettu_sel$group[bettu_sel$group == "bettu"] <- "-bettu"

map <- mapper(bettu_sel, plot_size = 0, regions = 1, title = "-bettu")
plot_name <- "bettu_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)


#longitudinal outliers
length(male[male$longitude > 76, "name"])
#8 matches
male[male$longitude > 76, "name"]
# [1] "Muragamale"  "Chetamale"   "Kadatanmale" "Penumale"    "Chenchimale"
# [6] "Chikmale"    "Dodmale"     "Sūlāmale"   

#remove outliers
male_sel <- male[!male$longitude > 76,]

mapper(male_sel, plot_size = 0, regions = 1, title = "-male")

bettu_male <- rbind(male_sel, bettu_sel)
mapper(bettu_male, plot_size = 0, regions = 1, title = "-bettu & -male")


####################### COMBINE COASTAL
####################### COMBINE COASTAL
####################### COMBINE COASTAL

combined <- rbind(bail_sel, mane_sel, gadde_sel, bettu_sel)
#merge bailu and bail to bail --> all entries will be bail
combined$group[combined$group == "-bailu"] <- "-bail"
map <- mapper(combined, plot_size = 0, regions = 1, title = "combined", color = c("red", "navy", "yellow", "darkgreen"))
plot_name <- "combined_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)


####################### COASTAL AND HALLI
####################### COASTAL AND HALLI
####################### COASTAL AND HALLI
####################### COASTAL AND HALLI

#merge all non-halli for comparison with halli places
combined$group <- "not -halli"

combined[setdiff(names(halli), names(combined))] <- NA
halli[setdiff(names(combined), names(halli))] <- NA
combined <- rbind(combined, halli)

map <- mapper(combined, plot_size = 0, regions = 1, title = "", frame = poly_KN, color = c("red", "navyblue"))
plot_name <- "halli_not_halli_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)

#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
##### EXTRA SLIDES
##### NORTH EAST INDIA
##### NORTH EAST INDIA

gaon_data <- top(c("[Gg][āa]on$"), c("IN", "BD"))
gaon_data$group <- "-gaon"
gram_data <- top(c("[Gg]r[āa]m$"), c("IN", "BD"))
gram_data$group <- "-gram"
gram_gaon_data <- rbind(gram_data, gaon_data)
map <- mapper(gram_gaon_data, plot_size = 0, regions = 1, color = c("red", "navyblue"))
plot_name <- "gram_gaon_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)



ng_data <- top(c("ng$"), c("IN", "BD"))
ng_data$group <- "-ng"
map <- mapper(ng_data, plot_size = 0, regions = 1, color = "red")
plot_name <- "ng_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)


combined_data <- rbind(ng_data, gram_gaon_data)
combined_data$group[combined_data$group != "-ng"] <- "-gram / -gaon"
map <- mapper(combined_data, plot_size = 0, regions = 1, color = c("red", "navyblue"))
plot_name <- "north_east_combined_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)


ch_ng <- top(c("[Cc]h.ng$"), c("IN", "BD"))
map <- mapper(ch_ng, plot_size = 0, regions = 1)
plot_name <- "ch_ng_cropped.png"
ggsave(plot_name, map, width=6, height=6, units="in")
image_write(
  image_trim(
    image_read(
      plot_name)), plot_name)