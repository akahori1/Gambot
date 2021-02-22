#最初から１９０個のデータを訓練データとする
#残りのテストデータでのestimate_CATEをcsv形式で出力する

library(dplyr)
library(ggplot2)
library(causalTree)
library(openxlsx)

#下記のコードはファイルを置いている場所によって変化します
data1 <-read.csv('~/Desktop/R/D_prymary_analysis0106.csv',fileEncoding="CP932")

columnList <- c('BL_gamble_amount','BL_income','BL_education','BL_GA','allocation','PGSI_change')

data2 <- data1[,columnList]

#欠損値がある行を削除する
data3 <- na.omit(data2)
df <- data3

print(dim(df))

# allocationの列の０と１を変更する
df %>% mutate(allocation=ifelse(allocation==0,1,0)) -> df

# BL_GAがあるの人は１に、それ以外は０に変換する
df %>% mutate(BL_GA=ifelse(BL_GA=="ある",1,0)) -> df

#BL_educationを変換する
mapping1 <- c('4年生大学'=0,'6年生大学または大学院'=0,'高校'=0,'専門学校'=1,'中学校'=1)
df$BL_education <- mapping1[df$BL_education]

#BL_incomeを変換する
mapping2 <- c('収入なし'=0,'100万未満'=100,'100-300万未満'=300,'300-500万未満'=500,'500-700万未満'=700,'700-900万未満'=900,'900-1100万未満'=1100,'1100万以上'=1300)
df$BL_income <- mapping2[df$BL_income]

#訓練データとテストデータに分割する
df_train <- df[1:190,]
df_test <- df[191:227,]
print(str(df_train))
forest <- causal_forest(df_train[c('BL_gamble_amount','BL_income','BL_education','BL_GA'
                            )]
                            ,df_train$PGSI_change,df_train$allocation,seed=123)

print(test_calibration(forest))

est_test <- predict(forest,df_test[c('BL_gamble_amount','BL_income','BL_education','BL_GA')])
df_test$estimate_CATE <- est_test
#print(str(df_test$estimate_CATE))
print(typeof(df_test$estimate_CATE))
print(dim(df_test))
#df_test$estimete_CATE_char <- as.character(df_test$estimate_CATE)
#print(df_test$estimate_CATE_char)

df_test_estimate_order <- df_test[order(df_test$estimate_CATE,decreasing=FALSE),]
print(df_test_estimate_order)

df_data <- data.frame(df_test_estimate_order)
print(str(df_data))
print(df_data$estimate_CATE$predictions)

df_data$estimate <- df_data$estimate_CATE$predictions

readr::write_excel_csv(df_data[c('BL_gamble_amount','BL_income','BL_education','BL_GA','estimate')],'~/Desktop/R/df_test_causalforest_0222_new.csv')
