# Data preparation
library(tidyverse); library(openxlsx)

# Page.52（棒グラフ：使用しません）##########
sbp_plot <- health %>%
     group_by(sex) %>%
     summarise(mean_sbp = mean(sbp),
               upper = mean(sbp) + sd(sbp),
               lower = mean(sbp) - sd(sbp))

ggplot(sbp_plot) +
     aes(x = sex, y = mean_sbp, fill = sex) +
     geom_bar(stat = "identity", alpha = 0.5, color = "black") +
     geom_errorbar(aes(ymin = lower, ymax = upper), color = "black", width = 0.2) +
     scale_fill_manual(values = c("white", "grey")) +
     theme_classic() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.53 
set.seed(123)
data_different <- tibble(a = rnorm(100, 50, 3),
                         b = c(rnorm(99, 50, 3), 90),
                         c = c(rnorm(50, 80, 3), rnorm(50, 20, 3)))

summary <- data_different %>%
     summarise(mean_a = mean(a),
               mean_b = mean(b),
               mean_c = mean(c))

# Page.53（棒グラフ左）
ggplot(summary) +
        aes(x = "", y = mean_a) +
        geom_bar(stat = "identity", color = "black", fill = "#D95F02") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.53（棒グラフ中央）
ggplot(summary) +
        aes(x = "", y = mean_b) +
        geom_bar(stat = "identity", color = "black", fill = "#1B9E77") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.53（棒グラフ右）
ggplot(summary) +
        aes(x = "", y = mean_c) +
        geom_bar(stat = "identity", color = "black", fill = "#7570B3") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.54（箱ひげ図左）
ggplot(data_different) +
        aes(x = "", y = a) +
        geom_boxplot(color = "black", fill = "#D95F02") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.54（箱ひげ図中央）
ggplot(data_different) +
        aes(x = "", y = b) +
        geom_boxplot(color = "black", fill = "#1B9E77") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.54（箱ひげ図右）
ggplot(data_different) +
        aes(x = "", y = c) +
        geom_boxplot(color = "black", fill = "#7570B3") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.55（ドットプロット左）
ggplot(data_different) +
        aes(x = "", y = a) +
        geom_point(size = 3, alpha = 0.8, color = "#D95F02") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.55（ドットプロット中央）
ggplot(data_different) +
        aes(x = "", y = b) +
        geom_point(size = 3, alpha = 0.8, color = "#1B9E77") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.55（ドットプロット右）
ggplot(data_different) +
        aes(x = "", y = c) +
        geom_point(size = 3, alpha = 0.8, color = "#7570B3") +
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.ticks.x = element_blank(),
              axis.title = element_blank()) +
        scale_y_continuous(limits = c(0, 100))

# Page.56（ドットプロット）
ggplot(health) +
     aes(x = sex, y = sbp, color = sex) +
     geom_point(alpha = 0.5, size = 3) +
     scale_color_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.57, 1. Ineffective dot
ggplot(health) +
        aes(x = sex, y = sbp, fill = sex) +
        geom_point(size = 6, shape = 21) +
        scale_fill_manual(values = c("#2166AC", "#B2182B")) +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18),
              legend.position = "top")

# Page.57, 2. Reducing point sizes
ggplot(health) +
        aes(x = sex, y = sbp, fill = sex) +
        geom_point(size = 3, shape = 21) +
        scale_fill_manual(values = c("#2166AC", "#B2182B")) +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18),
              legend.position = "top")

# Page.57, 3. Increasse transparency
ggplot(health) +
        aes(x = sex, y = sbp, fill = sex) +
        geom_point(size = 3, alpha = 0.5, shape = 21) +
        scale_fill_manual(values = c("#2166AC", "#B2182B")) +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18),
              legend.position = "top")

# Page.57, 4. Jittering
ggplot(health) +
        aes(x = sex, y = sbp, fill = sex) +
        geom_jitter(size = 3, alpha = 0.5, width = 0.1, shape = 21) +
        scale_fill_manual(values = c("#2166AC", "#B2182B")) +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18),
              legend.position = "top")

# Page.57, 5. Symmetric jittering
library(ggbeeswarm)
ggplot(health) +
        aes(x = sex, y = sbp, fill = sex) +
        geom_beeswarm(size = 3, alpha = 0.5, cex = 1.5, shape = 21) +
        scale_fill_manual(values = c("#2166AC", "#B2182B")) +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18),
              legend.position = "top")

# Page.58（箱ひげ図）
ggplot(health) +
     aes(x = sex, y = sbp, fill = sex) +
     geom_boxplot(alpha = 0.5) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.59（ヒストグラム）
ggplot(health) +
     aes(x = sbp, fill = sex) +
     geom_histogram(alpha = 0.5) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.60, histogram with bin n = 5
ggplot(health) +
     aes(x = sbp, fill = sex) +
     geom_histogram(alpha = 0.5, bins = 5) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.60, histogram with bin n = 10
ggplot(health) +
     aes(x = sbp, fill = sex) +
     geom_histogram(alpha = 0.5, bins = 10) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.60, histogram with bin n = 50
ggplot(health) +
     aes(x = sbp, fill = sex) +
     geom_histogram(alpha = 0.5, bins = 50) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.60, histogram with bin n = 100
ggplot(health) +
     aes(x = sbp, fill = sex) +
     geom_histogram(alpha = 0.5, bins = 100) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.61（密度プロット）
ggplot(health) +
     aes(x = sbp, fill = sex) +
     geom_density(alpha = 0.5) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.62（バイオリン・プロット）
ggplot(health) +
     aes(x = sex, y = sbp, fill = sex) +
     geom_violin(alpha = 0.5) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.61（リッジライン・プロット）
library(ggridges)
ggplot(health) +
     aes(x = sbp, y = sex, fill = sex) +
     geom_density_ridges(alpha = 0.5) +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     guides(fill = guide_legend(reverse = TRUE)) +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18))

# Page.63（箱ひげ図＋ジッタープロット）
ggplot(health) +
     aes(x = sex, y = sbp, fill = sex) +
     geom_boxplot(alpha = 0.5) +
     geom_jitter(alpha = 0.5, width = 0.2, color = "black") +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18),
           legend.position = "top")

# Page.63（レインクラウド・プロット）
remotes::install_github('jorvlan/raincloudplots')
library(raincloudplots)

sbp_m <- health %>%
     filter(sex == "man") %>%

sbp_f <- health %>%
     filter(sex == "woman") %>%

df_1x1 <- data_1x1(
     array_1 = sbp_m$sbp,
     array_2 = sbp_f$sbp,
     jit_distance = .09,
     jit_seed = 321)

raincloud_1x1(
     data = df_1x1, 
     colors = (c("black", "black")), 
     fills = (c("#2166AC", "#B2182B")), 
     size = 1, 
     alpha = .6, 
     ort = 'h') +
     scale_x_continuous(breaks=c(1.2, 2.2),
                        label = c("Man", "Woman"), 
                        limits=c(0.7, 3)) +
     xlab("Sex") + 
     ylab("Systolic blood pressure") +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20))

# Page.71（散布図＋密度プロット）
library(ggExtra)
p <- ggplot(data = health) +
     aes(x = sbp, y = dbp, fill = sex, color = sex) +
     geom_point(shape = 21, size = 5, alpha = 0.6) +
     theme_bw() +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     scale_color_manual(values = c("#2166AC", "#B2182B")) +
     theme(axis.title = element_text(size = 24),    
           axis.text = element_text(size = 20)) +   
     labs(x = "systolic blood pressure", y = "diastolic blood pressure")
ggMarginal(p, groupFill = TRUE, alpha = 0.5)

# Page.75（折れ線グラフ+gghighlight）
library(gghighlight)
ggplot(cancer) +
     aes(x = Year, y = AAM, group = type, color = type) +
     geom_line(size = 1.5) +
     geom_point(size = 4) +
     gghighlight(type == "liver", label_params = list(size = 10)) +
     theme_bw() +
     scale_color_manual(values = c("#2166AC")) +
     scale_size_manual(values = c(2, 0.7)) +
     theme(legend.position = "none") +
     theme(axis.text = element_text(size = 20),
           axis.title = element_text(size = 24)) +
     labs(y = "Age-adjusted mortality rate (per 100K)") +
     scale_y_continuous(limits = c(0, 80)) +
     scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))

# Page.76（折れ線グラフ+facet_wrap）
ggplot(cancer) +
     aes(x = Year, y = AAM, color = type) +
     geom_line(size = 1.5) +
     geom_point(size = 4) +
     theme_minimal() +
     theme(axis.text = element_text(size = 14),
           axis.title = element_text(size = 20),
           strip.text = element_text(size = 20)) +
     scale_color_brewer(palette = "Dark2") +
     theme(legend.position = "none") +
     labs(y = "Age-adjusted mortality rate (per 100K)") +
     scale_y_continuous(limits = c(0, 80)) +
     scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015)) +
     facet_wrap(~type)

# Page.77（折れ線グラフ+plotly）
library(plotly)

cancer_w <- pivot_wider(cancer, names_from = type, values_from = AAM)

plot_ly(cancer_w, x = ~Year, y = ~stomach, name = "stomach", type = 'scatter', mode = 'lines+markers') %>%
     add_trace(y = ~esophagus, name = "esophagus", mode = 'lines+markers') %>%
     add_trace(y = ~liver, name = "liver", mode = 'lines+markers') %>%
     add_trace(y = ~pancreas, name = "pancreas", mode = 'lines+markers') %>%
     add_trace(y = ~Lung, name = "Lung", mode = 'lines+markers') %>%
     add_trace(y = ~prostate, name = "prostate", mode = 'lines+markers') %>%
     add_trace(y = ~bladder, name = "bladder", mode = 'lines+markers') %>%
     add_trace(y = ~colorectal, name = "colorectal", mode = 'lines+markers') %>%
     layout(xaxis = list(title = "Year"),
            yaxis = list (title = "Mortality rate"))

# Page.80（ヒートマップ）##########
set.seed(123)
library(gplots)

df_heatmap <- health %>%
        select(ID, Gene_A:Gene_E) %>%
        mutate_at(vars(Gene_A:Gene_E), funs(scale(.))) %>%
        column_to_rownames("ID") %>%
        as.matrix()

heatmap.2(df_heatmap, cexCol = 1, key = TRUE, keysize = 1, key.xlab = "Z-value", key.title = NA, 
          density.info = "none", colsep = c(1:4), sepcolor = "Grey", sepwidth = c(0.001, 0.05), 
          trace = "none", col = bluered(100))

# Page.81（ヒートマップ:失敗例）##########
set.seed(123)
library(gplots)

df_heatmap <- health %>%
        select(ID, Gene_A:Gene_E) %>%
        mutate_at(vars(Gene_A:Gene_E), funs(scale(.))) %>%
        column_to_rownames("ID") %>%
        as.matrix()

heatmap.2(df_heatmap, cexCol = 1, key = TRUE, keysize = 1, key.xlab = "Z-value", key.title = NA, 
          density.info = "none", colsep = c(1:4), sepcolor = "Grey", sepwidth = c(0.001, 0.05), 
          trace = "none", col = bluered(100), dendrogram = "none", Rowv = FALSE, Colv = FALSE)

# Page.82（ヒートマップ：失敗例）##########
set.seed(123)
library(gplots)

df_heatmap <- health %>%
        select(ID, Gene_A:Gene_E) %>%
        column_to_rownames("ID") %>%
        as.matrix()

heatmap.2(df_heatmap, cexCol = 1, key = TRUE, keysize = 1, key.xlab = "Z-value", key.title = NA, 
          density.info = "none", colsep = c(1:4), sepcolor = "Grey", sepwidth = c(0.001, 0.05), 
          trace = "none", col = bluered(100))