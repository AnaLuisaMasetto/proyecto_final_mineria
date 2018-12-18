load <- function(){
  if(!file.exists('../Data/walmart.rds')){
    walmart_data <- read_csv('../Data/train.csv',
                              col_names=TRUE)#,
#                              na = "?")
    saveRDS(walmart_data, "../Data/walmart.rds")
    print('walmart.rds se guardó')
  }
  else{
    warning('walmart.rds ya existe')
    walmart_data <- readRDS("../Data/walmart.rds")
  }
  
  return(walmart_data)
}


#walmart_clean_colnames <- function(x){
#  str_replace_all(x,'-','_')
#}

walmart_clean_data <- function(x){
  str_replace_all(x,c(','='', '-' = '_', '__'='_', '___'='_'))#, '\\?'='(other)'))
}

#imports_85_clean_factor_data <- function(x){
#  str_replace_all(x,c('\\?'='<unk>', 'mercedes-bens'='mercedes-benz'))
#}
save.rds.wal <- function(num){
    saveRDS(walmart_data, paste("../Data/walmart_",num,".rds",sep=""))
    print(paste("walmart_",num,".rds"," se guardó",sep=""))
}

add.variables <- function(df){
source("groups.R")
df %>%
  group_by(TripType, VisitNumber, Weekday, Upc, DepartmentDescription, FinelineNumber) %>%
  summarise(ScanCount = sum(ScanCount)) %>%
  ungroup() %>%
  mutate(ScanType = factor(ifelse(ScanCount > 0, 'Purchase', 
                            ifelse(ScanCount < 0,'Return','Check'))),
#         Weekday  = factor(Weekday, levels = c('Monday', 'Tuesday','Wednesday', 
#                                              'Thursday', 'Friday', 'Saturday', 
#                                              'Sunday')),
         Department = factor(ifelse(DepartmentDescription %in% electronics_office, 'ELECTRONICS_AND_OFFICE',
                       ifelse(DepartmentDescription %in% movies_music_books, 'MUSIC_MOVIES_AND_BOOKS',
                        ifelse(DepartmentDescription %in% home_forniture_appliances, 'HOME_AND_APPLIANCES',
                         ifelse(DepartmentDescription %in% improvement_patio, 'HOME_IMPROVEMENT_AND_PATIO',
                          ifelse(DepartmentDescription %in% clothing, 'CLOTHING',
                           ifelse(DepartmentDescription %in% shoes_accessories, 'SHOES_AND_ACCESSORIES',
                           ifelse(DepartmentDescription %in% baby_toddler, 'BABY_AND_TODDLER',
                            ifelse(DepartmentDescription %in% toys_videogames, 'TOYS_AND_VIDEOGAMES',
                             ifelse(DepartmentDescription %in% food, 'FOOD',
                              ifelse(DepartmentDescription %in% household, 'HOUSEHOLD',
                               ifelse(DepartmentDescription %in% pharma_optical, 'PHARMACY_AND_OPTICAL',
                                ifelse(DepartmentDescription %in% beauty, 'HEALTH_AND_BEAUTY',
                                ifelse(DepartmentDescription %in% sports_fit_outdoors, 'SPORTS_FITNESS_AND_OUTDOORS',
                                 ifelse(DepartmentDescription %in% auto_tires_industrial, 'AUTOMOTIVE_AND_TIRES',
                                  ifelse(DepartmentDescription %in% photo_personalized, 'PHOTO_AND_PERSONALIZED',
                                   ifelse(DepartmentDescription %in% art_craft_party, 'ART_CRAFT_AND_PARTY', as.character(DepartmentDescription)))))))))))))))))),
#         TripType = factor(TripType),
         DepartmentDescription = str_replace(DepartmentDescription, 'MENSWEAR', 'MENS_WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'GIRLS_WEAR_4_6X_AND_7_14|BOYS_WEAR', 'KIDS_WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'LADIES_SOCKS|BRAS_&_SHAPEWEAR|SHEER_HOSIERY|SLEEPWEAR_FOUNDATIONS|PLUS_AND_MATERNITY', 'LADIESWEAR')) %>%
  mutate(Upc = as.character(format(Upc, scientific = FALSE)),
         ManufacturerCode = ifelse(grepl("[0-9]{12}", Upc),substr(Upc, 2, 6),
                              ifelse(grepl("[ ][0-9]{11}", Upc),substr(Upc, 2, 6), 
                                ifelse(grepl("[ ]{2}[0-9]{10}", Upc),substr(Upc, 3, 7),
                                  ifelse(grepl("[ ]{3}[0-9]{9}", Upc),substr(Upc, 4, 7),
                                    ifelse(grepl("[ ]{4}[0-9]{8}", Upc),substr(Upc, 5, 8),
                                      ifelse(grepl("[ ]{7}[0-9]{5}", Upc),"wwwww",
                                        ifelse(grepl("[ ]{8}[0-9]{4}", Upc),"wwwww",
                                          ifelse(grepl("[ ]{10}NA", Upc) & DepartmentDescription %in% c("PHARMACY_RX"), "unk_ph",
                                            ifelse(grepl("[ ]{10}NA", Upc) & DepartmentDescription %in% c("NULL"), "unk_null",
                                              "unk")))))))))) %>%
  mutate(ProductCode = ifelse(grepl("[0-9]{12}", Upc),substr(Upc, 7, 11),
                         ifelse(grepl("[ ][0-9]{11}", Upc),substr(Upc, 7, 11), 
                           ifelse(grepl("[ ]{2}[0-9]{10}", Upc),substr(Upc, 8, 12),
                             ifelse(grepl("[ ]{3}[0-9]{9}", Upc),substr(Upc, 8, 11),
                               ifelse(grepl("[ ]{4}[0-9]{8}", Upc),substr(Upc, 9, 12),
                                 ifelse(grepl("[ ]{7}[0-9]{5}", Upc),substr(Upc, 8, 11),
                                   ifelse(grepl("[ ]{8}[0-9]{4}", Upc),substr(Upc, 9, 12),
                                     ifelse(grepl("[ ]{10}NA", Upc) & DepartmentDescription %in% c("PHARMACY_RX"), "unk_ph",
                                       ifelse(grepl("[ ]{10}NA", Upc) & DepartmentDescription %in% c("NULL"), "unk_null",
                                         "unk"))))))))),
         DepartmentDescription = factor(DepartmentDescription),
         ManufacturerCode = factor(ManufacturerCode),
         ProductCode = factor(ProductCode))
}

#extract(all, into=c("NO3", "NH4", "resto"), regex="([0-9]*.[0-9]{5})([0-9]*.[0-9]*)/(.*)/NA", remove=TRUE)
