library(data.table)
library(XML)
medikoe <- fread("C:\\Users\\Cat\\Desktop\\medikoe.csv")
XY.unq <- unique(medikoe[, .(lat, lon)])
API_key <- "AIzaSyBFOkBl3ueqE3HpeuacE55_UWaEdgAkZRQ"
XY.unq[, link := paste0("https://maps.googleapis.com/maps/api/geocode/xml?latlng=", lat, ",", lon, "&key=", API_key)]
XY.unq[, XY_XML := lapply(link, readLines)]
Get_from_XML <- function(xml, type){
 
  idx <- grep(pattern = type, x = xml, fixed = TRUE)[1]
  my.gsub <- function(x) gsub(pattern = ' *<.*?> *', replacement = '', x)
  switch(type,
         status            = my.gsub(x = xml[idx]),
         formatted_address = my.gsub(x = xml[idx]),
         administrative_area_level_1 = ,
         administrative_area_level_2 = ,
         administrative_area_level_3 = ,
         administrative_area_level_4 = ,
         administrative_area_level_5 = ,
         country = my.gsub(x = xml[idx-2]),
         stop("Provide a type!")
  )
}
XY.unq[, ":=" 
       (Status  = sapply(XY_XML, function(i) Get_from_XML(xml = i, type = 'status')),
         Country = sapply(XY_XML, function(i) Get_from_XML(xml = i, type = 'country')),
         Admin1  = sapply(XY_XML, function(i) Get_from_XML(xml = i, type = 'administrative_area_level_1')),
         Admin2  = sapply(XY_XML, function(i) Get_from_XML(xml = i, type = 'administrative_area_level_2')),
         Admin3  = sapply(XY_XML, function(i) Get_from_XML(xml = i, type = 'administrative_area_level_3')),
         Admin4  = sapply(XY_XML, function(i) Get_from_XML(xml = i, type = 'administrative_area_level_4')),
         Admin5  = sapply(XY_XML, function(i) Get_from_XML(xml = i, type = 'administrative_area_level_5')),
         Address = sapply(XY_XML, function(i) Get_from_XML(xml = i, type = 'formatted_address'))
        
       )]
colnames(XY.unq)
XY.unq[, c("link", "XY_XML") := NULL]
RevGeo <- merge(x = medikoe, y = XY.unq, by = c("lat", "lon"), all.x = TRUE)
RevGeo <- XY.unq[medikoe, on = c("lat", "lon")]

#Save/Load object
save(RevGeo, file = "RevGeo.rda")
load(file = "RevGeo.rda")
