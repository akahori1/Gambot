data2<-read.cvs('~/Desktop/R/D_prymary_analysis0106.csv',fileEncoding="CP932")
data4 <- na.omit(data2)
df <- data4
# allocationの列の０と１を変更する
df %>% mutate(allocation=ifelse(allocation==0,1,0)) -> df
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

library(ggplot2)
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

df$d28_gamble_frequency[df$d28_gamble_frequency=="毎日"]<-4
df$d28_gamble_frequency[df$d28_gamble_frequency=="週2，3回"]<-3
df$d28_gamble_frequency[df$d28_gamble_frequency=="週1回"]<-2
df$d28_gamble_frequency[df$d28_gamble_frequency=="月に2，3回"]<-1
df$d28_gamble_frequency[df$d28_gamble_frequency=="月に1回"]<-0

tree <- causalTree(PGSI_change ~ BL_age + BL_gamble_acuse + BL_gamble_amount + BL_gamble_frequency + BL_gamble_type + BL_marital_status + BL_gamble_first_age + BL_gamble_knowledge + BL_LINE_num_messages + d28_gamble_amount + d28_gamble_frequency, data=df, treatment = df$allocation, split.Rule = "CT",cv.option = "CT",split.Honest = T,cv.Honest = T,split.Bucket = F,xval=5,cp=0,minsize = 20)

rpart.plot(tree)
