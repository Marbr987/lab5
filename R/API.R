install.packages(c("httr", "jsonlite"))

getlocation = function(city){
res = httr::GET(paste0("https://nominatim.openstreetmap.org/search?q=/",city,"/&format=json"))
res

status = res$status_code
status
content = res$content
content
data = jsonlite::fromJSON(rawToChar(content))
data = data.frame(data)
cordinates = cbind(data$lat, data$lon)
return(cordinates)
}

getlocation("Linkoping")




