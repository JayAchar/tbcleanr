library(dplyr)

## Generate mission look up table
# list of all key value sets for missions
raw_missions <- list(
  Afghanistan = "Afghanistan", 
  Bangladesh = "Bangladesh", 
  CAR = "CAR", 
  Chad = c("Chad", "Chad 2"), 
  Colombia = "Colombia", 
  Cote_dIvoire = "Côte d'Ivoire", 
  DRC_Katanga = c("DRC Katanga",
                  "DRC", 
                  "DRC CONGO- KATANGA", 
                  "DRC Katanga Measles"), 
  DRC_North_Kivu = "DRC North Kivu", 
  DRC_South_Kivu = "DRC South Kivu", 
  Ethiopia = c("Ethiopia", "Ethiopia Liben"), 
  Haiti = "Haiti", 
  HAT_mobile = "HAT mobile", 
  India = "India", 
  Iraq = c("Iraq", "Iraq Kalar"), 
  Jordan = "Jordan", 
  Liberia = "Liberia", 
  Libya = c("Libya", "Mediterranean boat"), 
  Malaysia = "Malaysia", 
  Mobile_HAT = c("Mobile HAT", "Mobile HAT Team"), 
  Myanmar = "Myanmar", 
  Nigeria = "Nigeria", 
  Pakistan = "Pakistan", 
  Philippines = "Philippines", 
  PNG = "PNG", 
  Russia = "Russia", 
  Sierra_Leone = "Sierra Leone", 
  Somalia = c("Somaliland", "Somalia"),
  South_Sudan = c("South Sudan", "South Sudan Maban", "Sudan South"), 
  Sri_Lanka = "Sri Lanka", 
  Swaziland = "Swaziland", 
  Syria = "Syria", 
  Tajikistan = "Tajikistan", 
  Turkey = "Turkey", 
  Uganda = "Uganda", 
  Uzbekistan = c("Uzbekistan Tajikistan", "Uzbekistan"), 
  Yemen = "Yemen", 
  Zimbabwe = "Zimbabwe"
  )


# list all key value sets for projects
raw_projects <- list(
  Nukus = c("NUKUS", "Nuqus")
)


# function to generate look up table from list

generate_look_up <- function(list) {
  # generate original oca_mission names from list
  raw_names <- unlist(list, use.names=FALSE)

  # generate correct new names for all original names
  new_names <- names(list)  

  new_names <- purrr::map_int(new_names, .f = ~ length(list[[.x]])) %>% 
    purrr::map2(.y = seq(new_names), .f = ~rep(new_names[[.y]], .x)) %>% 
    unlist(use.names = FALSE)  
  
  names(new_names) <- raw_names
  
  new_names
}


# generate look up tables
oca_missions <- generate_look_up(raw_missions)
oca_projects <- generate_look_up(raw_projects)

# save to internal package data
usethis::use_data(oca_missions, oca_projects, 
                   internal = TRUE,
                   overwrite = TRUE)
g
