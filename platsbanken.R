# Annonsdata från Platsbanken
# Created by Gisela Jönsson, 2014-11-09

# install.packages('XML')
# install.packages('httr')

library('httr')
library('XML')

# Sätt en URL för lista över yrkesområden
URL <- "http://api.arbetsformedlingen.se/platsannons/soklista/yrkesomraden"

# Hämta och formattera XML-data från URL
yrkesomradenXML <- GET(URL, add_headers("Accept-Language" = "se-sv,sv"), accept_xml())
yrkesomradenDOM = xmlRoot(xmlTreeParse(yrkesomradenXML))

# En funktion som ger lista över yrkesområden
getYrkesomraden <- function(doc) {
  matrix <- data.frame(yrkesomradeid=NA, yrkesomrade=NA, antal_platsannonser=NA)
  for(i in 4:xmlSize(doc)) {
    row <- c(doc[[i]]$child$id$child$text$value, doc[[i]]$child$namn$child$text$value, doc[[i]]$child$antal_platsannonser$child$text$value)
    matrix <- rbind(matrix, row)
  }
  return(na.omit(matrix))
}

# Anropa funktionen
listaYrkesomraden <- getYrkesomraden(yrkesomradenDOM)

yrkesrange <- as.list(listaYrkesomraden$yrkesomradeid)

# Sätt yrkesområdesID att jobba med
readinteger <- function()
{
  n <- readline(prompt="Ange yrkesområdesID du vill jobba med: ")
  if(n %in% yrkesrange) {
    return(as.integer(n))
  }
  else {
    print("Det ID:t finns inte.")
    return()
  }
}


# Kör för att inputta siffran.
# VIKTIG PARAMETER SOM ANVÄNDS I HELA SCRIPTET
yrkesomradeid <- readinteger()

# KONTROLLERA VAD DU VALT:
kontroll <- function(yrkesomradeid=yrkesomradeid) {
  yrkesomrade <- listaYrkesomraden[listaYrkesomraden$yrkesomradeid == yrkesomradeid,]$yrkesomrade
  antal_platsannonser <- listaYrkesomraden[listaYrkesomraden$yrkesomradeid == yrkesomradeid,]$antal_platsannonser
  print(paste0("Du valde yrkesområde: ", yrkesomradeid, " - ", yrkesomrade, ". Antal annonser: ", antal_platsannonser))
}

# Kör kontrollen
kontroll(yrkesomradeid)


# Gå vidare med att borra ner i yrken om man vill, har inte utforskat helt
# yrkesomradeid <- 19 # 19=transport, sätt manuellt om man vill.
URL2 = paste0("http://api.arbetsformedlingen.se/platsannons/soklista/yrkesgrupper?yrkesomradeid=", yrkesomradeid)

yrkeslistaXML <- GET(URL2, add_headers("Accept-Language" = "se-sv,sv"), accept_xml())

yrkeslistaDOM = xmlRoot(xmlTreeParse(yrkeslistaXML))

getYrkeslista <- function(doc) {
  yrkeslista <- c()
  for (i in 4:xmlSize(doc)) {
    yrkeslista <- append(yrkeslista, as.character(doc[[i]][1][1]$id[1]$text$value))
  }
  return(yrkeslista)
}

yrkeslistaTransport <- getYrkeslista(yrkeslistaDOM)


# Matcha annonser på Yrkesområde ID

URL3 <- paste0("http://api.arbetsformedlingen.se/platsannons/matchning?yrkesomradeid=", yrkesomradeid)

annonsIDXML <- GET(URL3, add_headers("Accept-Language" = "se-sv,sv"), accept_xml())
annonsIDDOM = xmlRoot(xmlTreeParse(annonsIDXML))

# Hämta antalet sidor
pages <- c(1:as.integer(annonsIDDOM["antal_sidor"]$antal_sidor[1]$text$value))

getAnnonsIDlista <- function(doc) {
  annonslista <- c()
  # 5 är där annonserna börjar, ej hittat bättre sätt att bestämma denna ännu
  pages <- as.integer(doc["antal_sidor"]$antal_sidor[1]$text$value)
  for (i in 5:xmlSize(doc)) {
    annonslista <- append(annonslista, as.character(doc[[i]][1]$annonsid[1]$text$value))
  }
  return(annonslista)
}

#annonsIDlistaTransport <- getAnnonsIDlista(annonsIDDOM)

# Initiera en tom lista
totalAnnonsIDlista <- c()

# När detta körs, fylls listan på
for(page in pages) {
  pageURL <- paste0("http://api.arbetsformedlingen.se/platsannons/matchning?yrkesomradeid=", yrkesomradeid, "&sida=", page)
  # print(pageURL)
  annonsIDXML <- GET(pageURL, add_headers("Accept-Language" = "se-sv,sv"), accept_xml())
  annonsIDDOM = xmlRoot(xmlTreeParse(annonsIDXML))
  annonsIDlistasida <- getAnnonsIDlista(annonsIDDOM)
  totalAnnonsIDlista <- append(totalAnnonsIDlista, annonsIDlistasida)
}

# Hämta annonserna från id listan

# URL4 <- paste0("http://api.arbetsformedlingen.se/platsannons/", annonsid)

getAnnonser <- function(doc) {
  matrix <- data.frame(annonsid=NA, annonsrubrik=NA, yrkesbenamning=NA, arbetsplatsnamn=NA, annonstext=NA)
  for(i in doc) {
    print(i)
    URLannons <- paste0("http://api.arbetsformedlingen.se/platsannons/", i)
    print(URLannons)
    annonsXML <- GET(URLannons, add_headers("Accept-Language" = "se-sv,sv"), accept_xml())
    annonsDOM = xmlRoot(xmlTreeParse(annonsXML))
    row <- c(annonsDOM[[1]][1]$annonsid[1]$text$value, annonsDOM[[1]][2]$annonsrubrik[1]$text$value, annonsDOM[[1]][4]$yrkesbenamning[1]$text$value, annonsDOM[[4]][1]$arbetsplatsnamn[1]$text$value, annonsDOM[[1]][3]$annonstext[1]$text$value )
    matrix <- rbind(matrix, row)
  }
  return(matrix)
}

# Stoppa in lista med ID i denna funktion, t.ex. ovan totalAnnonsIDlista
annonser <- getAnnonser(totalAnnonsIDlista)

# Exportera annonser till CSV
## Skapa ett filnamn
filename <- paste0(yrkesomradeid, "-", yrkesomrade, "-", antal_platsannonser, ".csv")

## Output to file
write.csv(na.omit(annonser), filename, row.names=TRUE, na="")
print(paste("Du har skrivit filen:", filename))
