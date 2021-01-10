library(tm);
library(memoise);
library(tidyRSS);
library(httr);
library(XML);
library(wordcloud2);
library(stringi);
library(solrium);

lematyzacja<-function(tekst)
{
  parametry<-list(lpmn="any2txt|wcrft2", text=tekst, user="nowakowskaewa.ka@gmail.com");
  odpowiedz<-POST("http://ws.clarin-pl.eu/nlprest2/base/process", body=parametry, encode = "json", verbose());
  zawartosc<-content(odpowiedz, "text", encoding = "UTF-8");
  xml<-xmlParse(zawartosc, encoding = "UTF-8");
  slowa<-xpathSApply(xml, '//chunkList/chunk/sentence/tok/lex/base', xmlValue, encoding = "UTF-8");
  return(paste(slowa, collapse = " "));
}

function(input, output){
  
  polaczenie <- SolrClient$new(host="127.0.0.1", port = 8983, path="/solr/rdzen3/select")
  
  #aktualnosci
  strona<-read_html(x="https://www.ur.edu.pl/kolegia/kolegium-nauk-spolecznych/", encoding="UTF-8");
  aktualnosci2<-xml_find_all(x=strona, xpath="//div[@class='news-content']/a");
  teksty<-xml_text(x=aktualnosci2);
  stop<-as.vector(unlist(read.csv("stop_words_pl.txt", header = FALSE, sep = ",", fileEncoding = "UTF-8")));
  
  dane <- data.frame(matrix(ncol=2, nrow=20));
  colnames(dane)[1]<-"id";
  colnames(dane)[2]<-"content";
  dane$id<-c(1:20);
  dane$content<-c(teksty);
  
  solrium::add(x=dane, conn=polaczenie, name="rdzen3");
  
  baza<-solr_search(conn=polaczenie, params=list(q="*:*", rows=-1));
  
  dokumenty<-Corpus(VectorSource(stri_enc_toutf8(baza)));
  
  dokumenty<-tm_map(dokumenty, removePunctuation);
  dokumenty<-tm_map(dokumenty, removeNumbers);
  dokumenty<-tm_map(dokumenty, removeWords, stop);
  dokumenty<-tm_map(dokumenty, removeWords, "więcejn")
  
  for (d in 1:length(dokumenty)) {
    
    dokumenty[[d]]$content<-lematyzacja(dokumenty[[d]]$content);
    dokumenty[[d]]$content<-stri_enc_toutf8(dokumenty[[d]]$content);
    
  }
  
  
  tdml<-TermDocumentMatrix(dokumenty);
  m1<-as.matrix(tdml);
  v<-sort(rowSums(m1), decreasing = TRUE);
  d<-data.frame(words=names(v), freq=v);
  
  output$cloud<- renderWordcloud2({
    
    wordcloud2(d, size=input$size, shape = input$selection, minSize = input$freq);
  })
  
  
  
}