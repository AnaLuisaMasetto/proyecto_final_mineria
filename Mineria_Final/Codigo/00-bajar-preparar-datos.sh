#!/bin/bash

# baja datos de kaggle y descomprime
cd ../Data
kaggle competitions download -c walmart-recruiting-trip-type-classification
for i in $(ls *.zip); do unzip $i; done
rm *.zip

# comienza a limpiar datos
cat train.csv | sed -r -E ':a;s/([0-9A-Z&\/_]*) ([0-9A-Z&\/_]*.*$)/\1_\2/g;ta' | sed 's/ //g' | sed -r -E 's/_+|\//_/g' > tmp
mv tmp train.csv
cat test.csv | sed -r -E ':a;s/([0-9A-Z&\/_]*) ([0-9A-Z&\/_]*.*$)/\1_\2/g;ta' | sed 's/ //g' | sed -r -E 's/_+|\//_/g' > tmp
mv tmp test.csv

cd ../Codigo

