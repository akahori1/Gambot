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

# allocationの列の０と１を変更する
df %>% mutate(allocation=ifelse(allocation==0,1,0)) -> df

#介入因子を取り出す
df_W <- df[c('allocation')]

# BL_GAがあるの人は１に、それ以外は０に変換する
df %>% mutate(BL_GA=ifelse(BL_GA=="ある",1,0)) -> df

# BL_gamble_acuseであるを１に、それ以外を０にする
df %>% mutate(BL_gamble_acuse=ifelse(BL_gamble_acuse=="ある",1,0)) ->df

# BL_gamble_frequencyを変換する
df$BL_gamble_frequency[df$BL_gamble_frequency=="毎日"]<-3
df$BL_gamble_frequency[df$BL_gamble_frequency=="週2-3回"]<-2
df$BL_gamble_frequency[df$BL_gamble_frequency=="週1回"]<-1
df$BL_gamble_frequency[df$BL_gamble_frequency=="月に1回"]<-4
df$BL_gamble_frequency[df$BL_gamble_frequency=="月に2-3回"]<-0

# BL_marital_statusを変換する
df$BL_marital_status[df$BL_marital_status=="離婚"]<-1
df$BL_marital_status[df$BL_marital_status=="未婚"]<-2
df$BL_marital_status[df$BL_marital_status=="既婚"]<-0

# BL_gamble_typeを変換する
df$BL_gamble_type[df$BL_gamble_type=="パチンコ"]<-3
df$BL_gamble_type[df$BL_gamble_type=="パチスロ"]<-2
df$BL_gamble_type[df$BL_gamble_type=="競馬"]<-1
df$BL_gamble_type[df$BL_gamble_type=="競艇"]<-0
df$BL_gamble_type[df$BL_gamble_type=="その他"]<-0
df$BL_gamble_type[df$BL_gamble_type=="ゲーム"]<-0
df$BL_gamble_type[df$BL_gamble_type=="宝くじ"]<-0
df$BL_gamble_type[df$BL_gamble_type=="先物取引"]<-0
df$BL_gamble_type[df$BL_gamble_type=="競輪"]<-0

#BL_gamble_knowledgeを変換する
df$BL_gamble_knowledge[df$BL_gamble_knowledge=="良く知っている"]<-3
df$BL_gamble_knowledge[df$BL_gamble_knowledge=="少しは知っている"]<-2
df$BL_gamble_knowledge[df$BL_gamble_knowledge=="聞いたことはある"]<-1
df$BL_gamble_knowledge[df$BL_gamble_knowledge=="全く知らない"]<-0
df$BL_LINE_num_messages[df$BL_LINE_num_messages=="1日21回以上"]<-6
df$BL_LINE_num_messages[df$BL_LINE_num_messages=="1日16-20回"]<-5
df$BL_LINE_num_messages[df$BL_LINE_num_messages=="1日11-15回"]<-4
df$BL_LINE_num_messages[df$BL_LINE_num_messages=="1日6-10回"]<-3
df$BL_LINE_num_messages[df$BL_LINE_num_messages=="1日1-5回"]<-2
df$BL_LINE_num_messages[df$BL_LINE_num_messages=="週に1-2回"]<-1
df$BL_LINE_num_messages[df$BL_LINE_num_messages=="週に1回未満"]<-0

#d28_gamble_frequencyを変換する
df$d28_gamble_frequency[df$d28_gamble_frequency=="毎日"]<-4
df$d28_gamble_frequency[df$d28_gamble_frequency=="週2，3回"]<-3
df$d28_gamble_frequency[df$d28_gamble_frequency=="週1回"]<-2
df$d28_gamble_frequency[df$d28_gamble_frequency=="月に2，3回"]<-1
df$d28_gamble_frequency[df$d28_gamble_frequency=="月に1回"]<-0

#念のためdfをコピーしておく
df_reserve <- df

#df_Yとdf_Wで使った列を削除する
df <- df[-1]
df <- df[-15]
df <- df[-1]

#収入/ギャンブル総額という新しい特徴量を作成する
#まずはBL_incomeのカテゴリ値を数値に変更する
#収入なし→0、100万未満→0.5、100-300未満→2、300-500→4、500-700→6、700-900→8、900-1100→10、1100~→12と変換する
df$BL_income_2 <- factor(df$BL_income, levels=c('収入なし','100万未満','100-300万未満','300-500万未満','500-700万未満','700-900万未満','900-1100万未満','1100万以上'),labels=c('0','0.5','2','4','6','8','10','12'))
#収入/ギャンブルを計算する
df$income_gamble_rate <- as.numeric(as.character(df$BL_income_2)) * 100000 / df$BL_gamble_amount

df$BL_gamble_frequency <- as.numeric(df$BL_gamble_frequency)
df$BL_gamble_type <- as.numeric(df$BL_gamble_type)
df$BL_marital_status <- as.numeric(df$BL_marital_status)
df$BL_gamble_knowledge <- as.numeric(df$BL_gamble_knowledge)
df$BL_LINE_num_messages <- as.numeric(df$BL_LINE_num_messages)
df$d28_gamble_frequency <- as.numeric(df$d28_gamble_frequency)

forest <- causal_forest(df[c('BL_age','BL_gamble_acuse','BL_gamble_frequency','BL_gamble_type','BL_marital_status','BL_gamble_first_age','BL_gamble_knowledge','BL_LINE_num_messages','d28_gamble_frequency','income_gamble_rate')],df_reserve$PGSI_change,df_reserve$allocation)

print(test_calibration(forest))
