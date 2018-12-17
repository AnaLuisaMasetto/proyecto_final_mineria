# Directorio de Trabajo

setwd("C:/Users/rener/Desktop/Mineria_Final")

# Librerías
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(GGally)
library(ggcorrplot)

# Train Set

#TripType(TARGET VARIABLE) - a categorical id representing the type of shopping trip the customer made. 
#                            TripType_999 is an "other" category.
#VisitNumber - an id corresponding to a single trip by a single customer
#Weekday - the weekday of the trip
#Upc - the UPC number of the product purchased
#ScanCount - the number of the given item that was purchased. A negative value indicates a product return.
#DepartmentDescription - a high-level description of the item's department
#FinelineNumber - a more refined category for each of the products, created by Walmart


train = read.csv('train.csv')

# Departamentos de Walmart US

electronics_office         = c('ACCESSORIES', 'ELECTRONICS', 'HARDWARE', 'OFFICE SUPPLIES', 
                               'WIRELESS', 'CAMERAS AND SUPPLIES')
movies_music_books         = c('BOOKS AND MAGAZINES')
home_forniture_appliances  = c('BEDDING', 'CONCEPT STORES', 'COOK AND DINE', 'FURNITURE', 
                               'HOME DECOR', 'HOME MANAGEMENT')
improvement_patio          = c('HORTICULTURE AND ACCESS', 'LAWN AND GARDEN', 'PAINT AND ACCESSORIES')
clothing_shoes_accessories = c('BOYS WEAR', 'BRAS & SHAPEWEAR', 'GIRLS WEAR, 4-6X  AND 7-14', 'INFANT APPAREL', 
                               'JEWELRY AND SUNGLASSES', 'LADIES SOCKS', 'LADIESWEAR', 'MENSWEAR', 'MENS WEAR',
                               'PLUS AND MATERNITY', 'SHEER HOSIERY', 'SHOES', 'SLEEPWEAR/FOUNDATIONS')
baby_toddler               = c('INFANT CONSUMABLE HARDLINES')
toys_videogames            = c('MEDIA AND GAMING', 'PLAYERS AND ELECTRONICS','TOYS')
food_household_pets        = c('BAKERY', 'CANDY, TOBACCO, COOKIES', 'COMM BREAD', 'DAIRY', 'DSD GROCERY', 
                               'FROZEN FOODS', 'GROCERY DRY GOODS', 'HOUSEHOLD CHEMICALS/SUPP', 'HOUSEHOLD PAPER GOODS', 
                               'LARGE HOUSEHOLD GOODS', 'LIQUOR,WINE,BEER', 'MEAT - FRESH & FROZEN', 'PETS AND SUPPLIES', 
                               'PRE PACKED DELI','PRODUCE', 'SEAFOOD', 'SERVICE DELI')
pharma_health_beauty       = c('BATH AND SHOWER', 'BEAUTY', 'HEALTH AND BEAUTY AIDS', 'OPTICAL - FRAMES', 'OPTICAL - LENSES',
                               'PERSONAL CARE', 'PHARMACY OTC', 'PHARMACY RX')
sports_fit_outdoors        = c('SPORTING GOODS', 'SWIMWEAR/OUTERWEAR')
auto_tires_industrial      = c('AUTOMOTIVE')
photo_personalized         = c('1-HR PHOTO')
art_craft_party            = c('CELEBRATION', 'FABRICS AND CRAFTS', 'SEASONAL')

# Ajustamos
train = train %>%
  group_by(TripType, VisitNumber, Weekday, Upc, DepartmentDescription, FinelineNumber) %>%
  summarise(ScanCount = sum(ScanCount)) %>%
  ungroup() %>%
  filter(ScanCount != 0) %>%
  mutate(ScanType = factor(ifelse(ScanCount > 0, 'Purchase', 'Return')),
         Weekday  = factor(Weekday, levels = c('Monday', 'Tuesday','Wednesday', 
                                              'Thursday', 'Friday', 'Saturday', 
                                              'Sunday')),
         Department = factor(ifelse(DepartmentDescription %in% electronics_office, 'ELECTRONICS AND OFFICE',
                       ifelse(DepartmentDescription %in% movies_music_books, 'MUSIC, MOVIES AND BOOKS',
                        ifelse(DepartmentDescription %in% home_forniture_appliances, 'HOME AND APPLIANCES',
                         ifelse(DepartmentDescription %in% improvement_patio, 'HOME IMPROVEMENT AND PATIO',
                          ifelse(DepartmentDescription %in% clothing_shoes_accessories, 'CLOTHING, SHOES AND ACCESORIES',
                           ifelse(DepartmentDescription %in% baby_toddler, 'BABY AND TODDLER',
                            ifelse(DepartmentDescription %in% toys_videogames, 'TOYS AND VIDEOGAMES',
                             ifelse(DepartmentDescription %in% food_household_pets, 'FOOD, HOUSEHOLD AND PETS',
                              ifelse(DepartmentDescription %in% pharma_health_beauty, 'PHARMACY, HEALTH AND BEAUTY',
                               ifelse(DepartmentDescription %in% sports_fit_outdoors, 'SPORTS, FITNESS AND OUTDOORS',
                                ifelse(DepartmentDescription %in% auto_tires_industrial, 'AUTOMOTIVE AND TIRES',
                                 ifelse(DepartmentDescription %in% photo_personalized, 'PHOTO AND PERSONALIZED',
                                  ifelse(DepartmentDescription %in% art_craft_party, 'ART, CRAFT AND PARTY', as.character(DepartmentDescription))))))))))))))),
         TripType = factor(TripType),
         DepartmentDescription = str_replace(DepartmentDescription, 'MENSWEAR', 'MENS WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'GIRLS WEAR, 4-6X  AND 7-14|BOYS WEAR', 'KIDS WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'LADIES SOCKS|BRAS & SHAPEWEAR|SHEER HOSIERY', 'KIDS WEAR'),
         DepartmentDescription = factor(DepartmentDescription))

train %>%
  group_by(Weekday, ScanType, Department) %>%
  summarise(Items = sum(abs(ScanCount))) %>%
  ggplot(aes(x = Weekday, y = Items, fill = Department)) + 
  geom_bar(stat = 'Identity') + 
  facet_wrap(~ ScanType, scales = 'free') +
  theme_bw() 

train %>%
  group_by(Department, TripType, ScanType) %>%
  summarise(Items = sum(abs(ScanCount))) %>%
  ggplot(aes(y = reorder(Department, Items), x = reorder(TripType, Items), fill = log(Items))) + 
  geom_raster() + 
  facet_grid(ScanType~., scales = 'free') + 
  scale_fill_gradient2_tableau()

train %>%
  filter(ScanType == 'Purchase') %>%
  group_by(Department, TripType) %>%
  summarise(Items = sum(abs(ScanCount)))  %>%
  spread(Department, Items) %>%
  mutate_all(function(y) ifelse(is.na(y), 0, y)) %>%
  select(-TripType) %>% 
  cor() %>%
  ggcorrplot(method = 'circle', type = 'upper', lab = TRUE) + 
  scale_fill_gradient2_tableau() +  labs(fill = 'Correl')

train %>%
  filter(ScanType == 'Purchase') %>%
  group_by(DepartmentDescription, TripType) %>%
  summarise(Items = sum(abs(ScanCount)))  %>%
  spread(DepartmentDescription, Items) %>%
  mutate_all(function(y) ifelse(is.na(y), 0, y)) %>%
  select(-TripType) %>% 
  cor() %>%
  ggcorrplot(type = 'upper') + 
  scale_fill_gradient2_tableau() +  labs(fill = 'Correl') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

x[is.na(x)] = 0

library(corrplot)

train %>%
  group_by(Weekday, DepartmentDescription, ScanType) %>%
  summarise(Items = sum(abs(ScanCount))) %>%
  ggplot(aes(x = Weekday, y = DepartmentDescription, fill = log(Items))) + 
  geom_raster() + 
  facet_wrap(~ScanType) +
  scale_fill_gradient2_tableau()

train %>%
  group_by(DepartmentDescription, ScanType) %>%
  mutate(Dtotal = sum(abs(ScanCount))) %>%
  group_by(TripType, DepartmentDescription, ScanType) %>%
  summarise(Dtotal = unique(Dtotal),
            Items = sum(abs(ScanCount)),
            Percent = 100*Items/Dtotal) %>%
  ggplot(aes(x = reorder(TripType, Items), 
             y = reorder(DepartmentDescription,Items), 
             fill = log(Items))) + 
  geom_raster() +  facet_wrap(~ScanType) +
  scale_fill_gradient2_tableau()

