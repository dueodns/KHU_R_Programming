# Installing the packages
install.packages("httr")
install.packages("glue")
install.packages("jsonlite")

# Loading packages
library(httr)
library(glue)
library(jsonlite) 

# Call API from DATA.GO.KR
base_URL <- "https://apis.data.go.kr/B552584/ArpltnInforInqireSvc/getMsrstnAcctoRltmMesureDnsty?"
serviceKey <- "ENTER YOUR API KEY HERE"
stationName <- URLencode("종로구")
dataTerm <- "DAILY"

# paste0 allows to append characters without any space. 
request_URL <- paste0(base_URL, 
                      "serviceKey=", serviceKey, "&",
                      "stationName=", stationName, "&",
                      "dataTerm=", dataTerm, "&",
                      "returnType=json", "&",
                      "numOfRows=100"
)

# glue is equivalent to f-string in Python. 
request_URL <- glue("{base_URL}",
                    "serviceKey={serviceKey}&",
                    "stationName={stationName}&",
                    "dataTerm={dataTerm}&",
                    "returnType=json&",
                    "numOfRows=100"
                    )

request_URL

# Call API
response <- GET(request_URL)

# Return is in the form of ASCII Hex code
response$content

# Extract the return and change the format to UTF-8
response_txt <- content(response, 
                        as = "text", 
                        encoding = "UTF-8")

# Convert the text to JSON
response_json <- fromJSON(response_txt)

# Check the json-formatted response
response_json$response

response_df <- data.frame(response_json$response)
response_df

# Make a plot for pm10 values
plot(response_df$body.items.pm10Value)

# Same plot but in ggplot2
library(ggplot2)
ggplot(data=response_df, mapping=aes(x=body.items.dataTime, y=body.items.pm10Value)) +
  geom_point()


# Exercise
# Check the website and find any data set which appropriate to your project
# Try to retrieve the data set through API 




## Kakao Geocoding API
base_URL <- "https://dapi.kakao.com/v2/local/search/address?"
base_URL

API_KEY <- "ENTER YOUR API KEY HERE"
Authorization <- paste0("KakaoAK ", API_KEY)

# Requesting parameters
params <- list(
  query = "서울특별시 동대문구 경희대로 26"  # 검색할 장소 키워드
)

# API 요청 보내기
response <- GET(
  url = base_URL,
  query = params,
  add_headers(Authorization = Authorization)
)

# Check the return
response$content

res_chr <- content(response, as= 'text')
res_json <- fromJSON(res_chr)

res_json$meta

# Check the Geocoding results
res_json$documents$x
res_json$documents$y


# install and importing leaflet package
install.packages("leaflet")
library(leaflet)

map_korea <- leaflet()
map_korea <- addTiles(map=map_korea) # add default OpenStreetMap map tiles
map_korea <- setView(map=map_korea, lng=126.978125, lat=37.565518, zoom = 10) # Seoul
map_korea

# The steps above can be simplified with the "%>%" symbol.  
map_korea_2 <- leaflet() %>% 
  addTiles() %>% # add default OpenStreetMap map tiles
  setView(lng=126.978125, lat=37.565518, zoom = 10) # Seoul
map_korea_2

map_korea_2 <- addMarkers(map=map_korea, 
                          lng=c(as.numeric(res_json$documents$x)),
                          lat=c(as.numeric(res_json$documents$y)),
                          popup='경희대학교'
                          )
map_korea_2


# Near Search API (Kakao)
base_URL <- "https://dapi.kakao.com/v2/local/search/keyword?"
base_URL

API_KEY <- "ENTER YOUR API KEY HERE"
Authorization <- paste0("KakaoAK ", API_KEY)

# 요청 파라미터 설정
params <- list(
  query = "편의점",  # 검색할 장소 키워드
  x='127.054890960564',
  y='37.5939491407769',
  radius='500'
)

# API 요청 보내기
response <- GET(
  url = base_URL,
  query = params,
  add_headers(Authorization = Authorization)
)

response_chr <- content(response, as='text')
response_json <- fromJSON(response_chr)

stores <- data.frame(response_json$documents)
stores

# Plot the store locations
khu <- leaflet() %>% 
  addTiles() %>% # add default OpenStreetMap map tiles
  setView(lng=as.numeric(res_json$documents$x), 
          lat=as.numeric(res_json$documents$y), zoom = 15) # Seoul
khu

for (i in 1:dim(stores)[1]){
  print(i)
  temp_x <- stores[i, 'x']
  temp_y <- stores[i, 'y']
  
  khu<- addMarkers(map=khu, 
             lng=c(as.numeric(temp_x)),
             lat=c(as.numeric(temp_y)),
             popup=stores[i, 'place_name']
  )
}

khu


