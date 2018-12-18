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


train = readr::read_csv('train.csv')

# Departamentos de Walmart US

electronics_office         = c('ACCESSORIES', 'ELECTRONICS', 'HARDWARE', 'OFFICE SUPPLIES', 
                               'WIRELESS', 'CAMERAS AND SUPPLIES')
movies_music_books         = c('BOOKS AND MAGAZINES')
home_forniture_appliances  = c('BEDDING', 'CONCEPT STORES', 'COOK AND DINE', 'FURNITURE', 
                               'HOME DECOR', 'HOME MANAGEMENT', 'LARGE HOUSEHOLD GOODS')
improvement_patio          = c('HORTICULTURE AND ACCESS', 'LAWN AND GARDEN', 'PAINT AND ACCESSORIES')
clothing                   = c('BOYS WEAR', 'BRAS & SHAPEWEAR', 'GIRLS WEAR, 4-6X  AND 7-14', 
                               'JEWELRY AND SUNGLASSES', 'LADIES SOCKS', 'LADIESWEAR', 'MENSWEAR', 'MENS WEAR',
                               'PLUS AND MATERNITY', 'SHEER HOSIERY', 'SHOES', 'SLEEPWEAR/FOUNDATIONS')
shoes_accessories          = c('JEWELRY AND SUNGLASSES','SHOES')
baby_toddler               = c('INFANT CONSUMABLE HARDLINES', 'INFANT APPAREL')
toys_videogames            = c('MEDIA AND GAMING', 'PLAYERS AND ELECTRONICS','TOYS')
food                       = c('BAKERY', 'CANDY, TOBACCO, COOKIES', 'COMM BREAD', 'DAIRY', 'DSD GROCERY', 
                               'FROZEN FOODS', 'GROCERY DRY GOODS', 'MEAT - FRESH & FROZEN',
                               'PRE PACKED DELI','PRODUCE', 'SEAFOOD', 'SERVICE DELI')
household                  = c('HOUSEHOLD CHEMICALS/SUPP', 'HOUSEHOLD PAPER GOODS')
pharma_optical             = c('PHARMACY OTC', 'PHARMACY RX','OPTICAL - FRAMES', 'OPTICAL - LENSES')
beauty                     = c('BEAUTY', 'HEALTH AND BEAUTY AIDS','PERSONAL CARE')
sports_fit_outdoors        = c('SPORTING GOODS', 'SWIMWEAR/OUTERWEAR')
auto_tires_industrial      = c('AUTOMOTIVE')
photo_personalized         = c('1-HR PHOTO')
art_craft_party            = c('CELEBRATION', 'FABRICS AND CRAFTS', 'SEASONAL')

# Ajustamos
train = train %>%
  group_by(TripType, VisitNumber, Weekday, Upc, DepartmentDescription, FinelineNumber) %>%
  summarise(ScanCount = sum(ScanCount)) %>%
  ungroup() %>%
  mutate(ScanType = factor(ifelse(ScanCount > 0, 'Purchase', 
                            ifelse(ScanCount < 0,'Return','Check'))),
         Weekday  = factor(Weekday, levels = c('Monday', 'Tuesday','Wednesday', 
                                              'Thursday', 'Friday', 'Saturday', 
                                              'Sunday')),
         Department = factor(ifelse(DepartmentDescription %in% electronics_office, 'ELECTRONICS AND OFFICE',
                       ifelse(DepartmentDescription %in% movies_music_books, 'MUSIC, MOVIES AND BOOKS',
                        ifelse(DepartmentDescription %in% home_forniture_appliances, 'HOME AND APPLIANCES',
                         ifelse(DepartmentDescription %in% improvement_patio, 'HOME IMPROVEMENT AND PATIO',
                          ifelse(DepartmentDescription %in% clothing, 'CLOTHING',
                           ifelse(DepartmentDescription %in% shoes_accessories, 'SHOES AND ACCESSORIES',
                           ifelse(DepartmentDescription %in% baby_toddler, 'BABY AND TODDLER',
                            ifelse(DepartmentDescription %in% toys_videogames, 'TOYS AND VIDEOGAMES',
                             ifelse(DepartmentDescription %in% food, 'FOOD',
                              ifelse(DepartmentDescription %in% household, 'HOUSEHOLD',
                               ifelse(DepartmentDescription %in% pharma_optical, 'PHARMACY AND OPTICAL',
                                ifelse(DepartmentDescription %in% beauty, 'HEALTH AND BEAUTY',
                                ifelse(DepartmentDescription %in% sports_fit_outdoors, 'SPORTS, FITNESS AND OUTDOORS',
                                 ifelse(DepartmentDescription %in% auto_tires_industrial, 'AUTOMOTIVE AND TIRES',
                                  ifelse(DepartmentDescription %in% photo_personalized, 'PHOTO AND PERSONALIZED',
                                   ifelse(DepartmentDescription %in% art_craft_party, 'ART, CRAFT AND PARTY', as.character(DepartmentDescription)))))))))))))))))),
         TripType = factor(TripType),
         DepartmentDescription = str_replace(DepartmentDescription, 'MENSWEAR', 'MENS WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'GIRLS WEAR, 4-6X  AND 7-14|BOYS WEAR', 'KIDS WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'LADIES SOCKS|BRAS & SHAPEWEAR|SHEER HOSIERY|SLEEPWEAR/FOUNDATIONS|PLUS AND MATERNITY', 'LADIESWEAR'),
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
  group_by(DepartmentDescription, TripType, ScanType) %>%
  summarise(Items = sum(abs(ScanCount))) %>%
  filter(ScanType == 'Purchase') %>%
  ggplot(aes(y = reorder(DepartmentDescription, Items), 
             x = reorder(TripType, Items), 
             fill = log(Items))) + 
  geom_raster() + 
  facet_grid(ScanType~., scales = 'free') + 
  scale_fill_gradient2_tableau()

train %>%
  group_by(Department, TripType, ScanType, Weekday) %>%
  summarise(Items = sum(abs(ScanCount))) %>%
  filter(ScanType == 'Purchase') %>%
  ggplot(aes(y = reorder(Department, Items), x = reorder(TripType, Items), fill = log(Items))) + 
  geom_raster() + 
  facet_grid(.~Weekday) + 
  scale_fill_gradient2_tableau() +   
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5, size = 9),
        axis.text.y = element_text(size = 9)) + coord_flip()

train %>%
  group_by(Weekday, TripType, ScanType, VisitNumber) %>%
  summarise(Items = unique(ScanCount)/sum(abs(ScanCount))) %>%
  ggplot(aes(y = reorder(Weekday, Items), x = reorder(TripType, Items), fill = log(Items))) + 
  geom_raster() + 
  facet_grid(ScanType~., scales = 'free') + 
  scale_fill_viridis_c()

train %>%
  group_by(Weekday, TripType, ScanType) %>%
  summarise(Items = sum(abs(ScanCount))) %>%
  ggplot(aes(x = (Items), fill = Weekday, alpha = 0.5)) + 
  geom_density() + facet_grid(Weekday~ScanType, scales = 'free_y')+
  scale_fill_colorblind()

#Cambiar NULL == OTHER DEPARTMENT

train %>%
  filter(ScanType == 'Purchase') %>%
  group_by(Department, TripType) %>%
  summarise(Items = sum(abs(ScanCount)))  %>%
  spread(Department, Items) %>%
  mutate_all(function(y) ifelse(is.na(y), 0, y)) %>%
  select(-TripType) %>% 
  cor() %>%
  ggcorrplot(lab = TRUE) + 
  scale_fill_gradient2_tableau() +  labs(fill = 'Correl') +
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5, size = 9),
        axis.text.y = element_text(size = 9))

train %>%
  filter(ScanType == 'Purchase') %>%
  arrange(Department) %>%
  group_by(Department, DepartmentDescription, TripType) %>%
  summarise(Items = sum(abs(ScanCount)))  %>%
  spread(DepartmentDescription, Items) %>%
  mutate_all(function(y) ifelse(is.na(y), 0, y)) %>%
  ungroup()%>%
  select(-TripType, -Department) %>% 
  scale() %>%
  cor() %>%
  ggcorrplot() + 
  scale_fill_gradient2_tableau() +  labs(fill = 'Correl') +
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5, size = 9),
        axis.text.y = element_text(size = 9))


y = train %>% spread(DepartmentDescription, ScanCount, -TripType)

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

