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
  Abdurafi = "Abdurafi",
  Abeche = "Abeche",
  Abou_Deia = "Abou Deia",
  Ackakale = "Ackakale",
  Ad_Dhale = c("Ad Dhale", "Ad Dhale (ER + PHC)",
                 "Addhale"),
  Al_Qaim = "Al Qaim Syria refugees response",
  Amtiman = c("Am Timan", "AMTIMAN", "Amtiman", "Am-timan"),
  Aquarius = "Aquarius",
  Arauca = "Arauca",
  Assam = "Assam Violence Intervention",
  Atrato = "ATRATO",
  Bandarban = "Bandarban Emergency Malaria Intervention",
  Baraka = c("Baraka", "CS002 - Baraka"),
  Batil = c("Batil Kaya Refugee Camp Combined",
            "Batil Refugee Camp"),
  Bendera = "Bendera",
  Bentiu = c(
    "Bentiu",
    "Bentiu Community",
    "Bentiu POC",
    "BENTIU PROJECT",
    "Bentiu Town"
  ),
  Biltine = "Biltine",
  Bir_Nahal = "Bir Nahal",
  BKL = "BKL",
  Boguila = c("Boguila", "Boguilla"),
  Bokoro = c("Bokoro",
             "BOKORO"),
  Bolivar = "Bolivar",
  Borno = c("Borno", "Maiduguri"),
  Bossangoa = "Bossangoa",
  Bukama = c("BUKAMA", "BUKAMA PALUDISME",
             "EPREP Bukama-Butumba"),
  Bukavu = c("Cholera Bukavu", "Cholera Uvira-Bukavu Jan 12"),
  Buramino = "Buramino",
  Ceerigabo = c("CEERIGABO", "Ceerigabo"),
  CERU = "CERU",
  Chaman = c("CHAMAN", "Chaman"),
  Chechnya = c(
    "Chechnya",
    "Chechnya Essential Health Care",
    "Chechnya TB and Mental Health Project",
    "Chehnya TB & MH Project"
  ),
  Chhattisgarh = c("CG", "Chhattisgarh"),
  Cholera = "Oral Cholera vaccination",
  Chuil = "Chuil",
  CRUO = c("CRUO", "CRUO project"),
  Dagastan = "Dagestan",
  Daloa = "Daloa",
  Damara = "Damara",
  Damboa = "Damboa",
  Danot = c("Danod",
            "Danot"),
  Delhi = "Delhi",
  Dhaka = "Dhaka Election Violence MH",
  DMJ = "DMJ",
  Dollo = c("Dollo Somalia", "Doolo Ado"),
  Dubie = c("DUBIE",
            "Dubie projet", "VIH DUBIE"),
  Dushanbe = c("Dushanbe", "Dushanbe Pediatric TB"),
  East_Chad = c("East Chad nutrition", "East Chad vaccination"),
  Eastern_Balochistan = c(
    "East balochistan",
    "East Balochistan",
    "Eastern Balochistan",
    "Eastern Balochistan (DMJ)"
  ),
  Emergency = "Emergency intervention",
  Epworth = c("Epworth", "Epworth Project"),
  Fallujah = c("Fallujah",
               "Fallujah MHCU"),
  Faya = "E-Response Faya returnees from Lybia",
  Fizi = "Fizi Vaccination",
  Flood_Emergency = "Flood Emergency",
  Fulbaria = c("_FUL", "Fulbaria", "Fulbaria KA", "Fulbaria PKDL Research"),
  Gabmella = "Gambela",
  Galadi = "Galadi",
  Galcayo = c(
    "Galcayo North",
    "Galcayo South",
    "Galcayosouth",
    "Galkayo North"
  ),
  Gambella = "Gambella",
  Gendrassa = "Gendrassa Refugee Camp",
  Gentil = "Gentil Combined Refugee Camp",
  Gokwe_North = c("Gokwe Combined", "Gokwe North", "Gokwe North Combined"),
  Goma = c("CTC Goma", "Goma cholera", "Goma Cholera"),
  Goronyo = c("Goronyo", "Goronyo MCH and Nutrition Project"),
  Gweru = "Gweru",
  HAR = "South Sudan",
  Harare = c(
    "COH - COMBINED",
    "Harare City Health",
    "Harare Psychiatric",
    "HPP",
    "HPU"
  ),
  Hargeisa = "Hargeisa",
  HAT = c(
    "Bili MHAT",
    "CAR",
    "Chad",
    "Doruma HAT team",
    "Doruma MHAT",
    "DRC & CAR",
    "HAT",
    "SS MHAT"
  ),
  Hayan = "Hayan emergency response",
  HCERU = c("Cholera",
            "Cholera emergency", "Haiti Cholera", "HCERU"),
  Helmand = c("Helmand",
              "Lashkargah"),
  Hiloweyn = "Hiloweyn",
  Honiara = "Honiara floods response",
  Imam_Ali = c("Imam Ali", "Imam Ali MHCU"),
  Ingushetia = c("Ingushetia",
                 "Ingushetia Essential Health Care"),
  Irbid = "Irbid",
  Isaie_Jeanty = "Isaïe Jeanty",
  Jamaam = "Jamaam Refugee Camp",
  Jamam = "Jamam Refugee Camp",
  Jikmir = "Jikmir SUN",
  Juba = "Juba Mobile Clinic",
  Kachin = "Kachin",
  Kafabu = "EPREP Kafubu",
  Kalar = "Kalar",
  Kalole = "Kalole Measles Vaccination",
  Kamrangirchar = c(
    "Kamrangirchar",
    "Kamrangirchar_",
    "Kamrangirchar_OEH",
    "Kamrangirchar_SRH"
  ),
  Karakalpakstan = c(
    "Comprehensive TB Care for All - Karakalpakstan",
    "Karakalpakstan",
    "Nukus",
    "Nukus Karakalpakstan",
    "Nukus_TB",
    "Tuberculosis"
  ),
  Kashmir = "Kashmir",
  Kaya = "KAYA Refugee Camp",
  Kebbe = "Kebbe",
  Kerfi = "Kerfi",
  KERU = c("Keru", "KERU"),
  Khanaqin = "Khanaqin",
  Kiambi = "KIAMBI",
  Kilinochchi = "Kilinochchi",
  Kilwa = c("KILWA", "Kilwa", "VIH Kilwa"),
  Kimbi = c(
    "Kimbi",
    "Kimbi - Lulenge",
    "Kimbi Lulenge",
    "Kimbi project",
    "Kimbi-Lulenge",
    "Lulimba"
  ),
  Kinkondja = c("Kinkondja", "Kitchanga"),
  Kirkuk = "Kirkuk",
  Kismayo = "Kismayo",
  Kitchanga = "Kitchanga",
  Kitgum = "Kitgum",
  Kitshanga = "Kitshanga",
  KM18 = "KM 18 and KM 43 Emergency Response",
  Kobane = "Kobane",
  Kongolo = "Kongolo",
  KTP = c("_Cox's Bazar",
          "Cox's_Bazar_", "KTP", "KTP BHC", "Kutupalong"),
  Kuchlak = "Kuchlak",
  Kule = "Kule",
  Lae = "Lae",
  Lankien = c(
    "Lankien Hospital",
    "Lankien PHCC",
    "LANKIEN PHCC",
    "LANKIEN PHCC(YUAI,PIERI PHCU)",
    "Lankien Yuai",
    "YUAI PHCU"
  ),
  LBB = "LBB",
  Leer = c("Leer Hopsital",
           "Leer Hospital", "LEER HOSPITAL"),
  Liben = "Liben",
  Lubumbashi = c("Cholera Lubumbashi",
                 "EPREP Lubumbashi"),
  Lwanza = "CHOLERA LWANZA LUKONZOLWA",
  Maitikoulou = "Maitikoulou",
  Malakal = c(
    "Malakal",
    "Malakal(Pagil,Adong,Atar,Khorfulus,Rom)",
    "Malakal,Rom,Pagil,Atar,Adong,Khorfulus"
  ),
  Mandelia = "Mandelia MenAfriVac Vaccination campaign",
  Manipur = "Manipur",
  Mankayane = c(
    "Mankayane",
    "Mankayane (DR) TB HIV",
    "Mankayane Manzini",
    "Manzini Mankayane",
    "Moneni Mankayane"
  ),
  Marere = "Marere",
  Matsapha = c(
    "Manzini",
    "Manzini Matsapha",
    "Matsapha",
    "Matsapha Clinic",
    "Matsapha Manzini"
  ),
  Maungdaw = "Maungdaw",
  Mbabane = "Mbabane NRL",
  Melfi = "Melfi Meningitis Intervention",
  Menbij = "Menbij",
  Minsk = c("Belarus prison TB", "Belarus TB project"),
  Mitwaba = "Mitwaba",
  Moba = "Moba Vaccination",
  Moneni = "Moneni NTBH",
  Monrovia = "0",
  Moscow = c("MoscowMigrant", "MoscowMigrants"),
  Mrauk_U = "Mrauk U",
  Mugunga = "KCR Mugunga",
  Mulamba = "Mulamba",
  Mundri = "Mundri",
  Mweso = c("MWESO", "Mweso"),
  Nasir = c("Nasir",
            "Nasir Hospital", "NASIR HOSPITAL"),
  NDS = "NDS",
  NERU = c(
    "NERU",
    "Nigeria Emergency Response Unit",
    "Nigeria Emergency Response Unit (NERU)"
  ),
  NHMP = "NHMP",
  Nokou = "Nokou",
  NOMA = "NOMA",
  North_Syria = "Northern Syria Emergency",
  NRS = "NRS",
  Ouaka = c("OUAKA", "Ouaka"),
  Oum_Hadjer = "Oum Hadjer",
  Palorinya = "Palorinya",
  PapWest = "PaPWest",
  Penang = c("Penang BCH",
             "Penang BHC"),
  Pinga = c("Pinga", "PINGA"),
  Prison = "Prison",
  Pugnido = "Pugnido",
  Quetta = c("QPH", "QPH Quetta", "Quetta",
             "QUETTA", "Quetta Paeds Hospital"),
  Rakhine = c("ERS", "Rakhine",
              "Sittwe"),
  Ramtha = c("Ramtha", "Ramtha Project"),
  Raqqa = "Raqqa",
  Rhino = "Rhino",
  RTT = "RTT",
  Ruwayshed = "Ruwayshed",
  Santander = "Norte de Santander",
  Savar = "Savar Emergency MH",
  SGBV = "SGBV",
  Shamwana = c("Shamwana",
               "SHAMWANA"),
  Shan = c("Shan", "SHAN"),
  Shire = "Shire",
  Sibut = "Sibut",
  SKERU = c("Skeru", "SKERU"),
  South_Cholera = "West south cholera",
  Southern_Unity = "Southern Unity",
  Southern_Upper_Nile = "Southern Upper Nile",
  Sulaymaniyah = c("Sulaymaniyah", "SULAYMANIYAH"),
  Suruc = "Suruc",
  Tabou = "Tabou",
  Tai = "Tai",
  Taiz = "Taiz",
  Tal_Abyad = "Tal Abyad",
  Tari = c("TARI", "Tari"),
  Tashkent = c("Tashkent HIV", "Tashkent_HIV"),
  Tenke = "TENKE",
  Tigray = "Tigray",
  Tissi = c("CombinedTissiAbGadam",
            "TISSI", "Tissi"),
  Tonkolili = "Tonkolili",
  TR002 = "TR002 and TR003",
  Tripoli = "Tripoli",
  Ulvira = "Vaccination Uvira-Nundu",
  Uraba = "Uraba",
  Urfa = "Urfa",
  Vavuniya = "Vavuniya",
  Walikale = "Walikale",
  Wardher = c("Warder", "Wardher"),
  Warhdher = "Wardher",
  West_Ayod = "West Ayod",
  West_Chad = "West Chad Nutrition Intervention",
  Yangon = c("Yangon",
             "Yangon Project"),
  Yarmouk = c("Yarmouk", "Yarmouk MHCU"),
  Zaatari = c("Zaatari", "Za'atari"),
  Zemio = c("ZEMIO", "Zemio",
            "Zémio"),
  ZHMP = "ZHMP",
  Zim_Prison = c("Prison", "Prison Project"),
  Zwedru = "Zwedru"
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


drug_adhere_vars <- data.frame(var_name = c("XC", "XD", "XE", "XF", "XG", "XH", "XI"),
                               drug_abb = c("MFX", "PTO", "LFX", "LZD", "BDQ", "IMP", "DLM"),
                               stringsAsFactors = FALSE)

# save to internal package data
usethis::use_data(oca_missions, oca_projects, drug_adhere_vars,
                   internal = TRUE,
                   overwrite = TRUE)

