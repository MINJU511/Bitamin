install.packages("rvest")
library(rvest)

url_base <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=70254&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="

#주소설정
url <- paste(url_base,1,sep='')
#html 읽어오기
htxt <- read_html(url,encoding="UTF-8")
#node 읽기
table <- html_nodes(htxt,".score_result")
content <- html_nodes(table,".score_reple")
content2 <- html_nodes(content,paste("#_filtered_ment_",1,sep='')) #이렇게 하면 큰따옴표 안에 들어감. 
#text 읽기
reviews <- html_text(content2)
reviews

#for문을 활용한 댓글 크롤링
all.reviews <- c()
for(page in 1:10){
  for(num in 1:9){
    url <- paste(url_base,page,sep='')
    htxt <- read_html(url,encoding = "UTF-8")
    table <- html_nodes(htxt,".score_result")
    content <- html_nodes(table,".score_reple")
    content2 <- html_nodes(content,paste("#_filtered_ment_",num,sep='')) #<>과 </>사이에 있는 text를 받아옴.
    reviews <- html_text(content2)
    if(length(reviews)==0){break}
    all.reviews<-c(all.reviews,reviews)
    print(page)
  }
}
head(all.reviews)

data <- gsub("[[:cntrl:]]","",all.reviews)
head(data)

#for문을 이용한 평점 크롤링
all.score <- c()
for (page in 1:10){
  url <- paste(url_base,page,sep='')
  htxt <- read_html(url,encoding = "UTF-8")
  table <- html_nodes(htxt,".score_result")
  content <- html_nodes(table,".star_score") 
  content2 <- html_nodes(content,"em") #em은 그냥 <em> </em>이니까! 그리고 em은 st_off하위항목이 아님. star_score밑에 st_off과 em이 존재. 
  score<-html_text(content2)
  all.score <- c(all.score,score)
  print(page)
}
as.numeric(all.score)
hist(as.numeric(all.score))

#dplyr을 활용한 코드 간소화
all.score2 <- list()
for(page in 1:10){
  all.score2[[page]] <- read_html(paste(url_base,page,sep=''),encoding = "UTF-8") %>%
    html_nodes(".score_result") %>% html_nodes(".star_score") %>% html_nodes("em") %>% html_text()
}
all.score2
unlist(all.score2)
hist(as.numeric(unlist(all.score2)))


##Selenium
install.packages("RSelenium")
library(RSelenium)

remDr <- remoteDriver(port=4445L,browserName="chrome")
remDr$open()
remDr$navigate("http://www.naver.com")
blogButton <-remDr$findElement(using="xpath",value='//*[@id="PM_ID_ct"]/div[1]/div[2]/div[1]/ul[1]/li[3]/a/span[1]')
blogButton$clickElement()
webElemButton <- remDr$findElement(using="css selector",value='#header > div.header_common > div > div.area_search > form > fieldset > div > input')
webElemButton$sendKeysToElement(list(key='shift',key='home',key='delete'))
webElemButton$sendKeysToElement(list('김민주'))
click_Button <- remDr$findElement(using="css selector", value = '#header > div.header_common > div > div.area_search > form > fieldset > a.button.button_blog > i')
click_Button$clickElement()(); Sys.sleep(2)
click_period <- remDr$findElement(using="css selector", value = '#content > section > div.category_search > div.search_information > div > div > a' )
click_period$clickElement(); Sys.sleep(2)
click_inaweek <- remDr$findElement(using="css selector",value='#content > section > div.category_search > div.search_information > div > div > div > a:nth-child(2)')
click_inaweek$clickElement(); Sys.sleep(2)

#검색건수
html <- read_html(remDr$getPageSource()[[1]]);Sys.sleep(1)
content <- html_nodes(html,".search_number")
num<-html_text(content)
num
content2 <- html_nodes(html,".title")
title <- html_text(content2)
title
