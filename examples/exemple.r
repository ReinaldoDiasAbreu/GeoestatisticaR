library(GeoestatisticaR)

# Pré configuração do script
DIRECTORY = "/Users/reinaldo/Documents/GeoestatisticaR/examples/"
DATA_FILE = "Dados_geo.txt"
COL_COORS = 1:2
COL_DATA = 3

# Sequência de exemplo de uso
setdir(DIRECTORY)
data = load_data(DATA_FILE, h = T, separator = "\t",decimal = ".")
geod = load_geodata(data, COL_COORS, COL_DATA)
test_norm = statistical_analysis(data, COL_DATA)
normality_test_2 = normality_test(data, COL_DATA)
table_freq = freq_dist(data, COL_DATA, k=NULL)
print(table_freq)
basic(data, COL_DATA)
