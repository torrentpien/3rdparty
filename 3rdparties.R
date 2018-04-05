library(httr)
library(rjson)
library(httpuv)
library(tm)
library(Rfacebook)
library(jiebaR)
library(wordcloud)
library(ggplot2)

setwd("C:/Users/Torrent/Google 雲端硬碟/stat/R/3rdparties")
setwd("/media/torrent/Database/Users/Torrent/Google 雲端硬碟/stat/R/prtr") #for ubuntu's R
load("fb_oauth") #讀入Rfacebook製作之oauth檔案

#-----------------------
#   社會民主黨 Social Democratic Party
#-----------------------

sd_page <- getPage(page = "sdparty.tw", n = 10000, token = fb_oauth) #撈取社民黨粉專內容
sd_message <- sd_page$message[2:423] #將文字內容存成sd_message，由於第一筆為綠社合併，予以剔除。
#sd_test <- iconv(sd_message, to = "utf-8")
#Encoding(sd_message) <- 'UTF-8'

#sd_message <- (sd_message[!is.na(sd_message)])

#code <- grep('ed', sd_message)

#一些社民黨粉專內容文末有募款訊息，要挑出刪除以免關鍵字計算有偏差

donateinfo <- grep('社會民主黨政治獻金專戶', sd_message) #確定具有募款說明的文件位置
y <- regexpr('社會民主黨政治獻金專戶', sd_message) #確定募款說明在文件中的字符位置

for (i in 1:length(donateinfo)){ #以迴圈逐筆刪除募款說明
  donate = donateinfo[i]
  sd_message[donate] <- substr(sd_message[donate], 1, y[donate]-1)
}

sd_corpus <- Corpus(DataframeSource(as.data.frame(sd_message), encoding = "utf-8")) #將粉專內容轉成tm向量

#數據清理

sd_corpus <- tm_map(sd_corpus, removePunctuation) 
sd_corpus <- tm_map(sd_corpus, stripWhitespace)
sd_corpus <- tm_map(sd_corpus, PlainTextDocument)

sd_corpus <- tm_map(sd_corpus, function(x) gsub("[0-9a-zA-Z]+?", "", x)) #清除英文及數字
sd_corpus <- tm_map(sd_corpus, function(x) gsub("攼|㹤|愼|㸰|戼|攼|㸱|㤼|㸹|㠼|㹣", "", x)) #有些社民黨的粉專內容有放小花，這些都是亂碼，事先予以清除
sd_corpus <- tm_map(sd_corpus, function(x) gsub("社民黨|社會民主黨", "", x)) #刪除社民黨、社會民主黨
#sd_corpus <- tm_map(sd_corpus, removeWords, stopwords("english"))

#進行中文分詞

mixseg <- worker(user = "sociology.txt") #載入自訂詞庫
sd_keyword <- tm_map(sd_corpus, function(x) segment(code = x, jieba = mixseg)) #執行分詞
sd_vector <- Corpus(VectorSource(sd_keyword, encoding = "utf-8")) #轉為tm向量

stopword <- readLines("stopword.txt", encoding = "utf-8") #載入停用詞庫
stopword <- enc2utf8(stopword) #將停用詞庫轉碼為utf-8


sd_dtmxi <- TermDocumentMatrix(sd_vector, control = list(wordLengths = c(2, Inf), minWordLength = 3, stopwords = stopword)) #製作tm矩陣

#製作文字雲cloud

m <- as.matrix(sd_dtmxi)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

windowsFonts(myFont = windowsFont("思源黑體 Regular"))
wordcloud(d$word, d$freq,   
          col = brewer.pal(8, "Dark2"), min.freq = 20, , random.color = T,   
          max.words = 50, random.order = F, rot.per=.15,  scale = c(6, .1), family = "myFont")  

#製作關聯字網路term association

#存為node/edge list給Gephi

asssdtw <- as.data.frame(findAssocs(sd_dtmxi, "台灣", 0.4)) #提出關聯度為0.4以上的關鍵字關聯
asssdtw$target <- rownames(asssdtw) #將列名copy為變項
source <- c("台灣") #製作source
asssdtw <- cbind(source, asssdtw) #將source合併進原向量asssdtw
colnames(asssdtw) <- c('source', 'weight', 'target') #欄位更名
write.csv(asssdtw, "asssdtw.csv") #存為csv檔案給Gephi繪圖

asssd1 <- as.data.frame(findAssocs(sd_dtmxi, "民進黨", 0.4))
asssd1$target <- rownames(asssd1)
source <- c("民進黨")
asssd1 <- cbind(source, asssd1)
colnames(asssd1) <- c('source', 'weight', 'target')
write.csv(asssd1, "asssd1.csv")



#-----------------------
#   時代力量 New Power Party
#-----------------------

npp_page <- getPage(page = "newpowerparty", n = 1000, token = fb_oauth)
npp_message <- npp_page$message

npp_corpus <- Corpus(DataframeSource(as.data.frame(npp_message), encoding = "utf-8"))

npp_corpus <- tm_map(npp_corpus, removePunctuation)
npp_corpus <- tm_map(npp_corpus, stripWhitespace)
npp_corpus <- tm_map(npp_corpus, PlainTextDocument)

npp_corpus <- tm_map(npp_corpus, function(x) gsub("[0-9a-zA-Z]+?", "", x))
npp_corpus <- tm_map(npp_corpus, function(x) gsub("時代力量", "", x))

mixseg <- worker(user = "sociology.txt")
npp_keyword <- tm_map(npp_corpus, function(x) segment(code = x, jieba = mixseg))

npp_vector <- Corpus(VectorSource(npp_keyword, encoding = "utf-8"))

stopword <- readLines("stopword.txt", encoding = "utf-8")
stopword <- enc2utf8(stopword)

npp_dtmxi <- TermDocumentMatrix(npp_vector, control = list(wordLengths = c(2, Inf), minWordLength = 3, stopwords = stopword))

findFreqTerms(npp_dtmxi, 100)
findAssocs(npp_dtmxi, "台灣", 0.5)

m <- as.matrix(npp_dtmxi)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq = v)

windowsFonts(myFont = windowsFont("思源黑體 Regular"))
wordcloud(d$word, d$freq,   
          col = brewer.pal(8, "Dark2"), min.freq = 20, , random.color = T,   
          max.words = 50, random.order = F, rot.per=.15,  scale = c(6, .1), family = "myFont") 

assnpp1 <- as.data.frame(findAssocs(npp_dtmxi, "民進黨", 0.5))
write.csv(assnpp1, "assnpp1.csv")

#---------------------
#     綠黨 Green Party
#---------------------

tgp_page <- getPage(page = "TaiwanGreenParty", n = 1000, token = fb_oauth)

tgp_message <- tgp_page$message[2:372]

tgp_corpus <- Corpus(DataframeSource(as.data.frame(tgp_message), encoding = "utf-8"))

tgp_corpus <- tm_map(tgp_corpus, removePunctuation)
tgp_corpus <- tm_map(tgp_corpus, stripWhitespace)
tgp_corpus <- tm_map(tgp_corpus, PlainTextDocument)

tgp_corpus <- tm_map(tgp_corpus, function(x) gsub("[0-9a-zA-Z]+?", "", x))
tgp_corpus <- tm_map(tgp_corpus, function(x) gsub("綠黨", "", x))

mixseg <- worker(user = "sociology.txt")
tgp_keyword <- tm_map(tgp_corpus, function(x) segment(code = x, jieba = mixseg))

tgp_vector <- Corpus(VectorSource(tgp_keyword, encoding = "utf-8"))

stopword <- readLines("stopword.txt", encoding = "utf-8")
stopword <- enc2utf8(stopword)

tgp_dtmxi <- TermDocumentMatrix(tgp_vector, control = list(wordLengths = c(2, Inf), minWordLength = 3, stopwords = stopword))

findFreqTerms(tgp_dtmxi, 100)
findAssocs(tgp_dtmxi, "中國", 0.2)

m <- as.matrix(tgp_dtmxi)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq = v)

wordcloud(d$word, d$freq,   
          col = brewer.pal(8, "Dark2"), min.freq = 20, , random.color = T,   
          max.words = 50, random.order = F, rot.per=.15,  scale = c(6, .1), family = "myFont")


#---------------------
#     民國黨 National Party
#---------------------

mkt_page <- getPage(page = "MinKuoTang", n = 1000, token = fb_oauth)

mkt_message <- mkt_page$message

mkt_corpus <- Corpus(DataframeSource(as.data.frame(mkt_message), encoding = "utf-8"))

mkt_corpus <- tm_map(mkt_corpus, removePunctuation)
mkt_corpus <- tm_map(mkt_corpus, stripWhitespace)
mkt_corpus <- tm_map(mkt_corpus, PlainTextDocument)

mkt_corpus <- tm_map(mkt_corpus, function(x) gsub("[0-9a-zA-Z]+?", "", x))
mkt_corpus <- tm_map(mkt_corpus, function(x) gsub("民國黨", "", x))

mixseg <- worker(user = "sociology.txt")
mkt_keyword <- tm_map(mkt_corpus, function(x) segment(code = x, jieba = mixseg))

mkt_vector <- Corpus(VectorSource(mkt_keyword, encoding = "utf-8"))

stopword <- readLines("stopword.txt", encoding = "utf-8")
stopword <- enc2utf8(stopword)

mkt_dtmxi <- TermDocumentMatrix(mkt_vector, control = list(wordLengths = c(2, Inf), minWordLength = 3, stopwords = stopword))

findFreqTerms(mkt_dtmxi, 30)
findAssocs(tgp_dtmxi, "國民黨", 0.2)

m <- as.matrix(mkt_dtmxi)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq = v)

wordcloud(d$word, d$freq,   
          col = brewer.pal(8, "Dark2"), min.freq = 20, , random.color = T,   
          max.words = 50, random.order = F, rot.per=.15,  scale = c(6, .1), family = "myFont")


#--------------------
#   自由台灣黨 Free Taiwan Party
#--------------------

ftp_page <- getPage(page = "TaiwanGreenParty", n = 1000, token = fb_oauth)

ftp_message <- ftp_page$message

ftp_corpus <- Corpus(DataframeSource(as.data.frame(ftp_message), encoding = "utf-8"))

ftp_corpus <- tm_map(ftp_corpus, removePunctuation)
ftp_corpus <- tm_map(ftp_corpus, stripWhitespace)
ftp_corpus <- tm_map(ftp_corpus, PlainTextDocument)

ftp_corpus <- tm_map(ftp_corpus, function(x) gsub("[0-9a-zA-Z]+?", "", x))
ftp_corpus <- tm_map(ftp_corpus, function(x) gsub("自由台灣黨", "", x))

mixseg <- worker(user = "sociology.txt")
ftp_keyword <- tm_map(ftp_corpus, function(x) segment(code = x, jieba = mixseg))

ftp_vector <- Corpus(VectorSource(ftp_keyword, encoding = "utf-8"))

stopword <- readLines("stopword.txt", encoding = "utf-8")
stopword <- enc2utf8(stopword)

ftp_dtmxi <- TermDocumentMatrix(ftp_vector, control = list(wordLengths = c(2, Inf), minWordLength = 3, stopwords = stopword))

findFreqTerms(ftp_dtmxi, 100)
findAssocs(ftp_dtmxi, "台灣", 0.2)

m <- as.matrix(ftp_dtmxi)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq = v)

wordcloud(d$word, d$freq,   
          col = brewer.pal(8, "Dark2"), min.freq = 20, , random.color = T,   
          max.words = 50, random.order = F, rot.per=.15,  scale = c(6, .1), family = "myFont")

