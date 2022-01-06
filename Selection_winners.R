library(tidyverse)

perticipants <- read.csv("event_231958_participants.csv", fileEncoding = "cp932")

# 無作為に当選者（30名）を選ぶ、上位20名が図書券、下位10名が書籍
set.seed(20220108)
winners <- participants %>%
     filter(参加枠名 == "学生（高校生、学部生・大学院生）") %>%
     sample_n(30) %>%
     select("ユーザー名", "PayPal取引ID", "受付番号")

# テーブルの書き出し
write.table(winners, "winners.txt", row.names = F, col.names = F)

# 全員「学生」枠であることを確認
winners_name <- winners$ユーザー名
participants %>%
     filter(ユーザー名 %in% winners_name)