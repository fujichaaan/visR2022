# Data preparation
library(tidyverse); library(openxlsx)

# Page.8 ##########
source("http://janhove.github.io/RCode/plot_r.R")
plot_r(r = 0.7, n = 30)

# Page.13（散布図）##########
ggplot(data = health) +
     aes(x = sbp, y = dbp, fill = sex) +
     geom_point(shape = 21,
                color = "Black",
                size = 5,
                alpha = 0.6) +
     theme_bw() +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) + 
     theme(axis.title = element_text(size = 24),    
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           plot.title = element_text(size = 28),
           plot.caption.position = "panel",
           plot.caption = element_text(size = 12)) +   
     labs(x = "systolic blood pressure (mmHg)", y = "diastolic blood pressure (mmHg)",
          title =  "Figure 1. Scatter plot of SBP and DBP",
          caption = "Stratified by sex (man & woman)")

# Page.15（散布図）##########
ggplot(data = health) +      # 使用する data を決める（data = は省略可能）
     aes(x = sbp, y = dbp) +  # 描画する変数を決める
     geom_point()             # 描画するグラフの種類を決める

# Page.18（散布図）##########
ggplot(data = health) +
     aes(x = sbp, y = dbp) +
     geom_point(shape = 21,      # 形の指定
                fill = "Red",   # 塗りつぶし色の指定
                color = "Black", # 周囲の色の指定
                size = 5,        # 点のサイズ変更
                alpha = 0.6)     # 透過度（0: 見えない、1: 見える）

# Page.20（散布図）##########
ggplot(data = health) +
     aes(x = sbp, y = dbp) +
     geom_point(shape = 21,
                fill = "Red",
                color = "Black",
                size = 5,
                alpha = 0.6) +
     theme_bw() +                                   # 背景の設定を変更する
     theme(axis.title = element_text(size = 24),    # 軸のタイトルのサイズを24にする
           axis.text = element_text(size = 20)) +   # 軸の数字や文字のサイズを20にする
     labs(x = "systolic blood pressure", y = "diastolic blood pressure")  # 軸ラベル