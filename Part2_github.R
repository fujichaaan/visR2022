# Data preparation
library(tidyverse); library(openxlsx)

# Page.24（棒グラフ左）##########
ggplot(health) +
     aes(x = sex, fill = sex) +
     geom_bar(width = 0.8) +
     theme_bw() +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18))

# Page.24（棒グラフ右）##########
ggplot(health) +
     aes(x = smoking, fill = smoking) +
     geom_bar(width = 0.8) +
     theme_bw() +
     scale_fill_manual(values = c("#F8766D", "#53B400", "#00B6EB")) +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18))

# Page.25（棒グラフ左）##########
ggplot(health) +
     aes(x = area, fill = area) +
     geom_bar(width = 0.8) +
     theme_bw() +
     scale_fill_brewer(palette = "Dark2") +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18))

# Page.25（棒グラフ右）##########
ggplot(health) +
     aes(x = area, fill = area) +
     geom_bar(width = 0.8) +
     theme_bw() +
     coord_cartesian(ylim = c(80, 125)) +
     scale_fill_brewer(palette = "Dark2") +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18))

# Page.26（棒グラフ左）##########
ggplot(health) +
     aes(x = birth, fill = birth) +
     geom_bar(width = 0.8) +
     theme_bw() +
     scale_fill_brewer(palette = "Set3") + 
     theme(panel.border = element_blank(),
           panel.grid.major.y = element_line(linetype = "dashed"),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.text = element_text(size = 20),
           axis.title = element_text(size = 24),
           legend.position = "none") +
     labs(x = "birth", y = "count")

# Page.26（棒グラフ中央）##########
health %>%
     mutate(birth_order = fct_relevel(birth, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
        ggplot() +
        aes(x = birth_order, fill = birth_order) +
        geom_bar(width = 0.8) +
        theme_bw() +
        scale_fill_brewer(palette = "Set3") + 
        theme(panel.border = element_blank(),
              panel.grid.major.y = element_line(linetype = "dashed"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.text = element_text(size = 20),
              axis.title = element_text(size = 24),
              legend.position = "none") +
        labs(x = "birth", y = "count")

# Page.26（棒グラフ右）##########
health %>%
        mutate(birth_order = fct_infreq(birth)) %>%
        ggplot() +
        aes(x = birth_order, fill = birth_order) +
        geom_bar(width = 0.8) +
        theme_bw() +
        scale_fill_brewer(palette = "Set3") + 
        theme(panel.border = element_blank(),
              panel.grid.major.y = element_line(linetype = "dashed"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.text = element_text(size = 20),
              axis.title = element_text(size = 24),
              legend.position = "none") +
        labs(x = "birth", y = "count")

# Page.27（棒グラフ）##########
health %>%
     mutate(sex_miscode = rep(c("man", "woman", "Man", "Woman"), times = c(147, 350, 1, 2))) %>%
     ggplot() +
     aes(x = sex_miscode) +
     geom_bar(width = 0.8, fill = "black") +
     theme_bw() +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18))

# Page.28（棒グラフ）##########
ggplot(covid) +
        aes(x = location, y = people_vaccinated_per_hundred) +
        geom_bar(stat = "identity", width = 0.8, fill = "black") +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Contry", y = "People Vaccinated (%)")

# Page.28（クリーブランドのドットプロット）##########
ggplot(covid) +
        aes(x = people_vaccinated_per_hundred, y = reorder(location, people_vaccinated_per_hundred)) +
        geom_point(size = 6) +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18)) +
        labs(x = "People Vaccinated (%)", y = "Contry")

# Page.29（クリーブランドのドットプロット）##########
ggplot(covid) +
        aes(x = people_vaccinated_per_hundred, y = reorder(location, people_vaccinated_per_hundred), color = continent) +
        geom_point(size = 6) +
        scale_color_brewer(palette = "Dark2") +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18),
              legend.position = "top") +
        labs(x = "People Vaccinated (%)", y = "Contry")

# Page.29（ダンベルプロット）##########
library(ggalt)
blue <- "#0171CE"
red <- "#DE4433"

ggplot(covid) +
        geom_segment(aes(y = reorder(location, people_vaccinated_per_hundred), yend = reorder(location, people_vaccinated_per_hundred), x = 0, xend = 100), color = "#b2b2b2", size = 0.15) +
        geom_dumbbell(aes(y = reorder(location, people_vaccinated_per_hundred), x = people_fully_vaccinated_per_hundred, xend = people_vaccinated_per_hundred),
                      size = 2, color = "#b2b2b2", size_x = 4, size_xend = 4, colour_x = red, colour_xend = blue) +
        geom_text(data = filter(covid, location == "South Korea"),
                  aes(x = people_vaccinated_per_hundred + 8, y = reorder(location, people_vaccinated_per_hundred), label = "Vaccinated"),
                  color = blue, size = 3, vjust = -1.5, fontface = "bold") +
        geom_text(data = filter(covid, location == "South Korea"),
                  aes(x = people_fully_vaccinated_per_hundred - 8, y = reorder(location, people_vaccinated_per_hundred), label = "Fully vacccinated"),
                  color = red, size = 3, vjust = -1.5, fontface = "bold") +
        theme_bw() +
        theme(axis.title = element_text(size = 24),
              axis.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18)) +
        labs(x = "People Vaccinated (%)", y = "Contry")

# Page.31（棒グラフ左）##########
health %>%
     count(smoking, sex) %>%
     mutate(smoking = fct_relevel(smoking, c("never", "ever", "current")),
            sex = fct_relevel(sex, c("man", "woman"))) %>%
     ggplot() +
     aes(x = smoking, y = n, fill = sex) +
     geom_bar(stat = "identity", position = "dodge") +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme_bw() +
     labs(y = "count") +
     theme(axis.text = element_text(size = 20),
           axis.title = element_text(size = 24),
           legend.title = element_text(size = 24),
           legend.text = element_text(size = 20),
           legend.position = "top")

# Page.34（棒グラフ左）##########
health %>%
     count(smoking, drink) %>%
     mutate(smoking = fct_relevel(smoking, c("never", "ever", "current")),
            drink = fct_relevel(drink, c("current", "ever", "never"))) %>%
     ggplot() +
     aes(x = smoking, y = n, fill = drink) +
     geom_bar(stat = "identity") +
     scale_fill_brewer(palette = "Greens", direction = -1) +
     theme_bw() +
     labs(y = "count") +
     theme(axis.text = element_text(size = 20),
           axis.title = element_text(size = 24),
           legend.title = element_text(size = 24),
           legend.text = element_text(size = 20))

# Page.34（棒グラフ右）##########
health %>%
     count(smoking, drink) %>%
     mutate(smoking = fct_relevel(smoking, c("never", "ever", "current")),
            drink = fct_relevel(drink, c("current", "ever", "never"))) %>%
     ggplot() +
     aes(x = smoking, y = n, fill = drink) +
     geom_bar(stat = "identity", position = "fill") +
     scale_fill_brewer(palette = "Greens", direction = -1) +
     theme_bw() +
     scale_y_continuous(labels = scales::percent) +
     labs(y = "percent") +
     theme(axis.text = element_text(size = 20),
           axis.title = element_text(size = 24),
           legend.title = element_text(size = 24),
           legend.text = element_text(size = 20))

# Page.38（円グラフ左）##########
health %>%
     count(sex) %>%
     mutate(sex = fct_reorder(sex, n)) %>%
     ggplot() +
     aes(x = "", y = n, fill = sex) +
     geom_bar(stat = "identity", width = 1) +
     coord_polar("y") +
     theme_void() +
     scale_fill_manual(values = c("#2166AC", "#B2182B")) +
     theme(legend.title = element_text(size = 24),
           legend.text = element_text(size = 20)) +
     guides(fill = guide_legend(reverse = TRUE)) +
     labs(fill = "sex")

# Page.38（円グラフ右）##########
health %>%
     count(smoking) %>%
     mutate(smoking = fct_reorder(smoking, n)) %>%
     ggplot() +
     aes(x = "", y = n, fill = smoking) +
     geom_bar(stat = "identity", width = 1) +
     coord_polar("y") +
     theme_void() +
     scale_fill_manual(values = c("#F8766D", "#53B400", "#00B6EB")) +
     theme(legend.title = element_text(size = 24),
           legend.text = element_text(size = 20)) +
     guides(fill = guide_legend(reverse = TRUE)) +
     labs(fill = "smoking")

# Page.39, 40（円グラフ左, 円グラフ右）##########
health %>%
     count(area) %>%
     mutate(area = fct_reorder(area, n)) %>%
     ggplot() +
     aes(x = "", y = n, fill = area) +
     geom_bar(stat = "identity", width = 1) +
     coord_polar("y") +
     theme_void() +
     scale_fill_brewer(palette = "Set2", direction = -1) +
     theme(legend.title = element_text(size = 24),
           legend.text = element_text(size = 20)) +
     guides(fill = guide_legend(reverse = TRUE)) +
     labs(fill = "area")

# Page.39（棒グラフ）##########
health %>%
     mutate(area = fct_relevel(area, "higashi", "kita", "chuo", "nishi", "minami")) %>%
     ggplot() +
     aes(x = area, fill = area) +
     geom_bar() +
     theme_bw() +
     scale_fill_brewer(palette = "Set2") +
     theme(axis.title = element_text(size = 24),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 20),
           legend.text = element_text(size = 18)) +
     labs(fill = "area")

# Page.41（円グラフ左）##########
health %>%
        count(birth) %>%
        mutate(area = fct_reorder(birth, n)) %>%
        ggplot() +
        aes(x = "", y = n, fill = birth) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        theme_void() +
        scale_fill_brewer(palette = "Set3", direction = -1) +
        theme(legend.title = element_text(size = 24),
              legend.text = element_text(size = 20)) +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(fill = "month")

# Page.41（円グラフ右）##########
health %>%
        count(season) %>%
        mutate(area = fct_reorder(season, n)) %>%
        ggplot() +
        aes(x = "", y = n, fill = season) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        theme_void() +
        scale_fill_brewer(palette = "Set3", direction = -1) +
        theme(legend.title = element_text(size = 24),
              legend.text = element_text(size = 20)) +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(fill = "month")

# Page.43（ベン図）##########
library(ggvenn)

list_drug <- list(
        drug_DM = which(health$drug_DM == "yes"),
        drug_HT = which(health$drug_HT == "yes")
        )

ggvenn(list_drug,
       fill_color = c("#0073C2FF", "#CD534CFF"),
       text_size = 5)

# Page.46（UpSet）##########
library(UpSetR)

list_risk <- list(
        smoking = which(health$smoking == "current"),
        drinking = which(health$drink == "current"),
        HT = which(health$HT == "yes"),
        Obesity = which(health$bmi >= 25)
)

upset(fromList(list_risk), order.by = "freq", text.scale = 3.5)