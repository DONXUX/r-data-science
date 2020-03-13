library(KoNLP)
library(wordcloud2)
useNIADic()

anthem <- readLines("D:/source/R/¾Ö±¹°¡.txt")
anthem

noun_anthem <- sapply(anthem, extractNoun, USE.NAMES = F)
noun_anthem

add_words <- c("¹éµÎ»ê","³²»ê","Ã¶°©","°¡À»","ÇÏ´Ã","´Þ")
add_words

buildDictionar?(user_dic = data.frame(add_words, rep("ncn", length(add_words))), replace_usr_dic = T)
get_dictionary('user_dic')

noun_anthem <- sapply(anthem, extractNoun, USE.NAMES = F)
noun_anthem

undata <- unlist(noun_anthem)
undata

undata2 <- gsub("°øÈ°ÇÑµ¥","",un?ata2)
undata2

word_table <- table(undata)
word_table

undata2 <- Filter(function(x){ nchar(x) >=2 }, undata)
word_table2 <- table(undata2)
word_table2

sort(word_table2, decreasing = T)

wordcloud2(word_table2)
wordcloud2(word_table2, fontFamily="¸¼Àº°íµñ?, size=1.2, color = "random-light", backgroundColor="black", shape="star")