library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

###################### LOAD DATA ###################### 
heart_data <- read.csv("./input/heart.csv")
name <- c("age", "sex", "chest_pain", "rest_bp", "chol", "fasting_bloodsugar", "rest_ecg", "max_heartrate", "excercise_angina", "slope", "n_major_vasel","thal","target")
heart_data2 <- heart_data
head(heart_data)

###################### DATA MUNGING ###################### 
name <- c("age","sex","chest_pain","rest_bp","chol","fasting_bloodsugar",
          "rest_ecg","max_heartrate","excercise_angina","ST_depression","slope","n_major_vasel","thal","target")
names(heart_data) <- name
heart_data$sex <- as.character(heart_data$sex)
heart_data$sex <- ifelse(heart_data$sex=="0",'female','male')
heart_data$chest_pain <- as.factor(heart_data$chest_pain)

title.center <- theme(plot.title = element_text(hjust = 0.5))

heart_data_target = heart_data %>% filter(target==1)

head(heart_data)
str(heart_data)
summary(heart_data_target)

###################### EDA ###################### 
### 나이 시각화 ### 
# 히스토그램
ggplot(heart_data_target, aes(x = age)) + geom_histogram(bins = 30, fill = "dodgerblue4") + 
  theme_bw() + theme_classic() + ggtitle("age hist") + ylab("number of people")
# 곡선 그래프
ggplot(heart_data_target, aes(x = age)) + geom_density(fill ="dodgerblue4") + 
  theme_bw() + theme_classic() + ggtitle("age dist") + ylab("number of people")
# 박스플롯
boxplot(heart_data_target$age, main = "age boxplot", col ="dodgerblue4", notch = T)
# Q-Q 플롯
qqPlot(heart_data_target$age, main = "age qqplot", grid = F)

### 성별 시각화 ### 
# 막대 그래프
ggplot(heart_data_target, aes(x = sex)) + geom_bar(width = 0.2, fill = "green") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + theme_bw() + 
  theme_classic() + ylab("number of count") + ggtitle("sex") + title.center
table(heart_data_target$chest_pain)

### 가슴통증 시각화 ### 
# 막대 그래프
ggplot(heart_data_target, aes(x = chest_pain)) + geom_bar(width = 0.2, fill = "red") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  theme_bw() + theme_classic() + ggtitle("chest pain type") + title.center

### 휴식 중 혈압 시각화 ### 
# 박스플롯
boxplot(heart_data_target$rest_bp, col = "purple", main = "boxplot of rest_bp", col.main="dodgerblue4")
# 히스토그램
ggplot(heart_data_target, aes(rest_bp)) + geom_histogram(bins = 20, fill = "purple") + 
  theme_bw() + theme_classic() + ggtitle("rest_bp") + title.center
# 곡선 그래프
ggplot(heart_data_target, aes(rest_bp)) + geom_density(fill = "purple") + 
  theme_bw() + theme_classic() + ggtitle("density plot of resp_bp") + title.center

### 콜레스트롤 시각화 ###
# 박스플롯
boxplot(heart_data_target$chol,col = "brown",main ="boxplot of chol",col.main="dodgerblue4")
# 히스토그램
ggplot(heart_data_target,aes(chol)) + geom_histogram(bins =20,fill ="brown") + theme_bw() + 
  theme_classic() +ggtitle("chol") +title.center
# 곡선 그래프
ggplot(heart_data_target,aes(chol)) + geom_density(fill ="brown") + theme_bw() + 
  theme_classic()+ggtitle("density plot of chol") +title.center
table(heart_data_target$fasting_bloodsugar)

### 혈당 시각화 ###
# 막대 그래프
ggplot(heart_data_target, aes(x = factor(fasting_bloodsugar))) + 
  geom_bar(width = 0.1,fill ="green") + geom_text(stat = 'count',aes(label =..count..),vjust =-0.5) + 
  theme_bw() + theme_classic() +ylab("number of count") + ggtitle("blood sugar") + title.center

### 휴식 중 심전도 시각화 ###
# 막대 그래프
ggplot(heart_data_target, aes(factor(rest_ecg))) + geom_bar(width = 0.2,fill ="orange") + theme_bw() + 
  theme_classic()+geom_text(stat ='count',aes(label = ..count..),vjust =-0.2)+ggtitle("barplot of rest_ecg") + title.center

### 최대 심장 박동 수 ###
# 히스토그램
ggplot(heart_data_target, aes(max_heartrate)) + geom_histogram(fill = "lightblue",alpha =0.5) + theme_bw()+theme_classic()
# 곡선 그래프
ggplot(heart_data_target, aes(max_heartrate)) + geom_density(fill = "lightblue",alpha =0.5) + theme_bw()+theme_classic()
# 박스플롯
boxplot(heart_data_target$max_heartrate,col ="lightblue",notch = T,main ="boxplot of the maximum heart rate")

### 운동 중 협심증 경험 시각화 ###
# 막대그래프
ggplot(heart_data_target, aes(factor(excercise_angina))) + geom_bar(width = 0.2,fill ="black") + 
  theme_bw() + theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2) + 
  ggtitle("barplot of exercise angina") +title.center

### ST 분절(ST Segment Depression) 형태 시각화 ###
# 막대그래프
ggplot(heart_data_target, aes(factor(slope))) + 
  geom_bar(width = 0.2,fill ="dodgerblue4") + theme_bw() + 
  theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2) + 
  ggtitle("barplot of slope") +title.center

### 지중해빈혈 여부 시각화 ###
# 막대그래프
ggplot(heart_data_target, aes(factor(thal))) + geom_bar(width = 0.2,fill ="red") + theme_bw() + 
  theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2)+ggtitle("barplot of thal") + title.center