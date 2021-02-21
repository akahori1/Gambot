library(dplyr)
library(ggplot2)
library(causalTree)

#下記のコードはファイルを置いている場所によって変化します
data1 <-read.csv('~/Desktop/R/D_prymary_analysis0106.csv',fileEncoding="CP932")

columnList <- c("X","allocation","BL_GA","BL_age","BL_education","BL_income","BL_gamble_acuse",
                "BL_gamble_amount","BL_gamble_frequency","BL_gamble_type","BL_marital_status",
                "BL_gamble_first_age","BL_gamble_knowledge","BL_LINE_num_check","BL_LINE_num_messages","BL_LINE_year","BL_medical","d28_gamble_amount",
                "d28_gamble_frequency","BL_PGSI_total","PGSI_change")

data2 <- data1[,columnList]

#欠損値がある行を削除する
data3 <- na.omit(data2)
df <- data3

#比較したい変数を取り出す
df_Y <- df[c('PGSI_change')]

#allocationの列の０と１を変更する
df %>% mutate(allocation=ifelse(allocation==0,1,0)) -> df

#介入因子を取り出す
df_W <- df[c('allocation')]

#BL_GAがあるの人は１に、それ以外は０に変換する
df %>% mutate(BL_GA=ifelse(BL_GA=="ある",1,0)) -> df

#BL_gamble_acuseであるを１に、それ以外を０にする
df %>% mutate(BL_gamble_acuse=ifelse(BL_gamble_acuse=="ある",1,0)) ->df

#BL_gamble_frequencyを変換する
mapping_fre <- c('毎日'=3,'週2-3回'=3,'週1回'=1,'月に1回'=1,'月に2-3回'=1)
df$BL_gamble_frequency <- mapping_fre[df$BL_gamble_frequency]

#BL_marital_statusを変換する
mapping_mari <- c('離婚'=1,'未婚'=2,'既婚'=0)
df$BL_marital_status <- mapping_mari[df$BL_marital_status]

#BL_gamble_typeを変換する
mapping_type <- c('パチンコ'=3,'パチスロ'=2,'競馬'=1,'競艇'=0,'その他'=0,'ゲーム'=0,'宝くじ'=0,'先物取引'=0,'競輪'=0)
df$BL_gamble_type <- mapping_type[df$BL_gamble_type]

#BL_gamble_knowledgeを変換する
mapping_know <- c('良く知っている'=3,'少しは知っている'=2,'聞いたことはある'=1,'全く知らない'=0)
df$BL_gamble_knowledge <- mapping_know[df$BL_gamble_knowledge]

mapping_num_mes <- c('1日21回以上'=6,'1日16-20回'=5,'1日11-15回'=4,'1日6-10回'=3,'1日1-5回'=2,'週に1-2回'=1,'週に1回未満'=0)
df$BL_LINE_num_messages <- mapping_num_mes[df$BL_LINE_num_messages]

#d28_gamble_frequencyを変換する
mapping_d28 <- c('毎日'=4,'週2，3回'=3,'週1回'=2,'月に2，3回'=1,'月に1回'=0)
df$d28_gamble_frequency <- mapping_d28[df$d28_gamble_frequency]

#BL_educationを変換する
mapping1 <- c('4年生大学'=0,'6年生大学または大学院'=0,'高校'=0,'専門学校'=1,'中学校'=1)
df$BL_education <- mapping1[df$BL_education]

#BL_incomeを変換する
mapping2 <- c('収入なし'=0,'100万未満'=100,'100-300万未満'=300,'300-500万未満'=500,'500-700万未満'=700,'700-900万未満'=900,'900-1100万未満'=1100,'1100万以上'=1300)
df$BL_income <- mapping2[df$BL_income]

#BL_LINE_num_checkを変換する
mapping3 <- c('1日1-5回'=0,'1日11-15回'=1,'1日16-20回'=2,'1日21回以上'=3,'1日6-10回'=4,'週に1-2回'=5,'週に1回未満'=5)
df$BL_LINE_num_check <- mapping3[df$BL_LINE_num_check]

#BL_medicalを変換する
mapping4 <- c('今は通院していない'=0,'相談したことがない'=0,'相談できる事を知らない'=1)
df$BL_medical <- mapping4[df$BL_medical]

#収入/ギャンブルを計算する
df$BL_income_2 <- factor(df$BL_income, levels=c('収入なし','100万未満','100-300万未満','300-500万未満','500-700万未満',
                            '700-900万未満','900-1100万未満','1100万以上'),labels=c('0','0.5','2','4','6','8','10','12'))
df$income_gamble_rate <- as.numeric(as.character(df$BL_income_2)) * 100000 / df$BL_gamble_amount

df$amount_prop <- df$BL_gamble_amount/df$d28_gamble_amount

forest <- causal_forest(df[c('BL_gamble_amount','BL_income','BL_education','BL_gamble_frequency','BL_GA')]
                            ,df$PGSI_change,df$allocation,seed=90)

print(test_calibration(forest))
