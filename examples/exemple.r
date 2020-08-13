library(GeoestatisticaR)

DIRECTORY = "/Users/reinaldo/Documents/GeoestatisticaR/examples/"
setdir(DIRECTORY)
data = load_data("Dados_geo.txt", h = T, separator = "\t",decimal = ".")
geod = load_geodata(data, 1:2, 3)
test_norm = statistical_analysis(data, 3)
freq_dist(data[,3], show_preview = T)
