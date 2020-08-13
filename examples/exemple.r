library(GeoestatisticaR)

DIRECTORY = "/Users/reinaldo/Documents/GeoestatisticaR/exemples/"
setdir(DIRECTORY)
data = load_data("Dados_geo.txt", h = T, separator = "\t",decimal = ".")
geod = load_geodata(data, 1:2, 3)
test_norm = statistical_analysis(data, 3)
