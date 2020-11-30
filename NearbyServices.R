library(googleway)
google_places <- function(search_string = NULL,
                          location = NULL,
                          radar = FALSE,
                          radius = NULL,
                          rankby = NULL,
                          keyword = NULL,
                          language = NULL,
                          name = NULL,
                          place_type = NULL,
                          price_range = NULL,
                          open_now = NULL,
                          page_token = NULL,
                          simplify = TRUE,
                          curl_proxy = NULL,
                          key = get_api_key("places")
){
  
  if(is.null(search_string) & is.null(location))
    stop("One of 'search_string' or 'location' must be specified")
  
  if(!is.null(location)){
    location <- validateGeocodeLocation(location)
  }
  
  logicalCheck(radar)
  logicalCheck(simplify)
  logicalCheck(open_now)
  
  radar <- validateRadar(radar, search_string, keyword, name, place_type, location, radius)
  location <- validateLocationSearch(location, search_string, radius, rankby, keyword, name, place_type)
  radius <- validateRadius(radius)
  rankby <- validateRankBy(rankby, location, search_string)
  radius <- validateRadiusRankBy(rankby, radius, location)
  
  language <- validateLanguage(language)
  name <- validateName(name, search_string)
  price_range <- validatePriceRange(price_range)
  place_type <- validatePlaceType(place_type)
  page_token <- validatePageToken(page_token)
  
  if(isTRUE(radar)){
    map_url <- "https://maps.googleapis.com/maps/api/place/radarsearch/json?"
  }else{
    if(!is.null(search_string)){
      search_string <- gsub(" ", "+", search_string)
      map_url <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=", search_string)
    }else{
      map_url <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?")
    }
  }
  
  map_url <- constructURL(map_url, c("location" = location,
                                     "radius" = radius,
                                     "rankby" = rankby,
                                     "keyword" = keyword,
                                     "language" = language,
                                     "name" = name,
                                     "type" = place_type,
                                     "minprice" = price_range[1],
                                     "maxprice" = price_range[2],
                                     "opennow" = open_now,
                                     "pagetoken" = page_token,
                                     "key" = AIzaSyB2IJ76FvjC3K368pO-clXbL-YnL6Vwq5w))
  
  return(downloadData(map_url, simplify, curl_proxy))
  
}
View(map_url)