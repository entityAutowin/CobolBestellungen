cobc -free main.cob read-data.cob check-kunde.cob check-produkt.cob check-sperre.cob verarbeitung.cob
echo
cobcrun MAIN
rm -f *.dylib