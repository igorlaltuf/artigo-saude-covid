# COVID E RESERVAS INDIGENAS

# Não precisa fazer um mapa do zero
# ver esse mapa https://covid19.socioambiental.org/
# e essa nota técnica https://drive.google.com/file/d/1H596_oDmOGf4mOTziHGIrbYM17PdycVj/view


library(mapview)
i <- geobr::read_indigenous_land()
mapview::mapview(i)