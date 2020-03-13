library(KoNLP)
library(wordcloud2)
useSejongDic()

anthem <- readLines("D:/R/kakaotalk.txt")
anthem

noun_anthem <- sapply(anthem, extractNoun, USE.NAMES = F)
noun_anthem

undata <- unlist(noun_anthem)
undata

word_table <- table(undata)
word_table

undata2 <- Filter(function(x) { nchar(x) >= 2 }, undata)
undata2 <- gsub("\\[????»\\]", "", undata2)
undata2 <- gsub("\\[?̵???\\]", "", undata2)
undata2 <- gsub("[0-9]","", undata2)
undata2 <- gsub("?̸?", "", undata2)
undata2 <- gsub("Ƽ", "", undata2)
undata2 <- gsub("??", "", undata2)
undata2 <- gsub("????", "", undata2)
undata2 <- gsub("????", "", undata2)
undata2 <- gsub("[??????????????]", "", undata2)
undata2 <- gsub("??????","",undata2)
undata2 <- gsub("ȭ????","",undata2)
undata2 <- gsub("??????","",undata2)
undata2 <- gsub("??????","",undata2)
undata2 <- gsub("?ݿ???","",undata2)
undata2 <- gsub("??????","",undata2)
undata2 <- gsub("?Ͽ???","",undata2)
undata2 <- gsub("[\\^]", "", undata2)

word_table2 <- table(undata2)
word_table2

sort(word_table2, decreasing = T)

wordcloud2(word_table2)

wordcloud2(word_table2, fontFamily="맑은고딕" size = 45,
           color = "random-light", backgroundColor = "black", shape = "star")