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

# Ajustamos
train = train %>%
  group_by(VisitNumber, TripType, Weekday, Upc, DepartmentDescription, FinelineNumber) %>%
  summarise(ScanCount = sum(ScanCount)) %>%
  ungroup() %>%
  mutate(ScanType = factor(ifelse(ScanCount > 0, 'Purchase', 
                            ifelse(ScanCount < 0,'Return','Check'))),
         Weekday  = factor(Weekday, levels = c('Monday', 'Tuesday','Wednesday', 
                                              'Thursday', 'Friday', 'Saturday', 
                                              'Sunday')),
         DepartmentDescription = str_replace(DepartmentDescription, 'MENSWEAR', 'MENS WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'HEALTH AND BEAUTY AIDS', 'BEAUTY'),
         DepartmentDescription = str_replace(DepartmentDescription, 'GIRLS WEAR, 4-6X  AND 7-14|BOYS WEAR', 'KIDS WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'LADIES SOCKS|BRAS & SHAPEWEAR|SHEER HOSIERY|SLEEPWEAR/FOUNDATIONS|PLUS AND MATERNITY', 'LADIESWEAR'),
         DepartmentDescription = factor(DepartmentDescription),Upc = as.character(format(Upc, scientific = FALSE)))


train = train %>% 
  spread(DepartmentDescription, ScanCount, convert = TRUE) %>%
  group_by(TripType, VisitNumber, Weekday) %>%
  summarise_at(7:67,sum,na.rm = T)


write.csv(train,'train_clean.csv')

#TEst
test = readr::read_csv('test.csv')

# Ajustamos
test = test %>%
  group_by(VisitNumber, Weekday, Upc, DepartmentDescription, FinelineNumber) %>%
  summarise(ScanCount = sum(ScanCount)) %>%
  ungroup() %>%
  mutate(ScanType = factor(ifelse(ScanCount > 0, 'Purchase', 
                                  ifelse(ScanCount < 0,'Return','Check'))),
         Weekday  = factor(Weekday, levels = c('Monday', 'Tuesday','Wednesday', 
                                               'Thursday', 'Friday', 'Saturday', 
                                               'Sunday')),
         DepartmentDescription = str_replace(DepartmentDescription, 'MENSWEAR', 'MENS WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'HEALTH AND BEAUTY AIDS', 'BEAUTY'),
         DepartmentDescription = str_replace(DepartmentDescription, 'GIRLS WEAR, 4-6X  AND 7-14|BOYS WEAR', 'KIDS WEAR'),
         DepartmentDescription = str_replace(DepartmentDescription, 'LADIES SOCKS|BRAS & SHAPEWEAR|SHEER HOSIERY|SLEEPWEAR/FOUNDATIONS|PLUS AND MATERNITY', 'LADIESWEAR'),
         DepartmentDescription = factor(DepartmentDescription),Upc = as.character(format(Upc, scientific = FALSE)))


test = test %>% 
  spread(DepartmentDescription, ScanCount, convert = TRUE) %>%
  group_by(VisitNumber, Weekday) %>%
  summarise_at(6:66,sum,na.rm = T)


write.csv(test,'test_clean.csv')