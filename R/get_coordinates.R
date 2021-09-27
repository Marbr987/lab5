#' getLocation
#' @param city_name The name of a city as a string
#' @return The latitude and longitude coordinates of the city
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export


get_coordinates <- function(city_name){
if(!is.character(city_name)){stop(paste("city_name must be a string, city_name is", toString(typeof(city_name))))}
else if (length(city_name) != 1){stop(paste("lenght of city_name must be 1, but lenght of city_name is", length(city_name)))}
res = httr::GET(paste0("https://nominatim.openstreetmap.org/search?q=/",city_name,"/&format=json"))
status = res$status_code
if(status!=200){stop(paste("Request not successful. Status code", toString(status)))}
content = res$content
data = jsonlite::fromJSON(rawToChar(content))
data = data.frame(data)
coordinates = c(longitude=round(as.numeric(data$lon[1]),5), latitude=round(as.numeric(data$lat[1]),5))
if(is.null(coordinates)){print(paste0("The city '", city_name, "' can not be located."))}
else if (coordinates[["longitude"]] < 10 || coordinates[["longitude"]] > 25 ||coordinates[["latitude"]] < 55 || coordinates[["latitude"]] > 70){print(paste(city_name, "is not in Sweden."))}
return(coordinates)
}

