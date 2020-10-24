street_names <- c("Lombard Street, San Francisco", 
                  "Santa Monica Boulevard", 
                  "Bourbon Street, New Orleans", 
                  "Fifth Avenue, New York", 
                  "Richards Street, Vancouver")
remDr <- remoteDriver(browserName = 'chrome')
remDr$open()
get_lat_lon <- function(street_names) {
  remDr$navigate("https://www.latlong.net/convert-address-to-lat-long.html")
  final <- c()
  for(i in 1:length(street_names)) {
    
    remDr$refresh()
    Sys.sleep(1)
    
    address_element <- remDr$findElement(using = 'class', value = 'width70')
    
    address_element$sendKeysToElement(list(street_names[i]))
    button_element <- remDr$findElement(using = 'class', value = "button")
    
    button_element$clickElement()
    Sys.sleep(3)
    
    out <- remDr$findElement(using = "class", value = "coordinatetxt")
    output <- out$getElementText()
    final <- c(final, output)
    
  }
  
  return(final)
}


vector_out <- get_lat_lon(street_names)
