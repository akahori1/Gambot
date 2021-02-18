library(dplyr)
library(ggplot2)
library(causalTree)

#下記のコードはファイルを置いている場所によって変化します
data1 <-read.csv('~/Desktop/R/D_prymary_analysis0106.csv',fileEncoding="CP932")
columnList <- c("X","allocation","BL_GA","BL_age","BL_income","BL_gamble_acuse","BL_gamble_amount","BL_gamble_frequency","BL_gamble_type","BL_marital_status","BL_gamble_first_age","BL_gamble_knowledge","BL_LINE_num_messages","d28_gamble_amount","d28_gamble_frequency","PGSI_change")

data2 <- data1[,columnList]

#欠損値がある行を削除する
data3 <- na.omit(data2)
df <- data3

dim(df)

#比較したい変数を取り出す
df_Y <- df[c('PGSI_change')]

#介入因子を取り出す
df_W <- df[c('allocation')]

#念のためdfをコピーしておく
df_reserve <- df

#df_Yとdf_Wで使った列を削除する
df <- df[-1]
df <- df[-15]


