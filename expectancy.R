library(ggplot2)
library(tidyr)
library(dplyr)


df <- read.csv("~/R/10 life expectancy/lifeexpectancy.csv", stringsAsFactors = F)

df = df %>% 
  select(-total) %>% 
  gather(sex, expectancy, -age)

df_ribbon = df %>% 
  group_by(age) %>% 
  summarise(expectancy = max(expectancy))

segments = data.frame(x = 15, 
                      y = c(0, 17.5), 
                      xend = 15, 
                      yend = c(12.5, 65))

annotation = data.frame(x = 17.5, 
                        y = c(5, 37.5), 
                        label = c('вік', 'очікувана тривалість життя'))

legend = data.frame(x = 17.5, y = c(68.5, 78.5), 
                    sex = c('male', 'female'), 
                    label = c('чоловіки', 'жінки'))

png(filename = 'expectancy.png', width = 1200, height = 1000)
ggplot(df)+
  geom_ribbon(data = df_ribbon, aes(x = age, ymin = age, ymax = age + expectancy), 
              alpha = 0.25, fill = '#B0B1AC')+
  geom_line(aes(x = age, y = age), size = 1, color = '#3A3F4A')+
  geom_segment(data = segments, aes(x = x, y = y, xend = xend, yend = yend), 
               size = 0.5, color = '#5D646F',
               arrow = arrow(angle = 20, length = unit(0.55, 'lines')))+
  geom_line(aes(x = age, y = age+expectancy, group = sex, color = sex), 
            size = 1, show.legend = F)+
  geom_text(data = annotation, aes(x = x, y = y, label = label, family = 'Ubuntu Condensed'), 
            color = '#3A3F4A', hjust = 0, size = 5)+
  geom_text(data = legend, aes(x = x, y = y, label = label, 
                               color = sex, family = 'Ubuntu Condensed'),
            hjust = 0, size = 5, show.legend = F)+
  theme_minimal(base_family = 'Ubuntu Condensed')+
  scale_x_reverse(expand = c(0, 0), breaks = seq(0, 100, 10))+
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10), position = 'top')+
  scale_color_manual(values = c('#B2534F', '#268CA0'))+
  coord_flip()+
  labs(title = 'Очікувана тривалість життя',
       subtitle = 'За статтю і віком, у 2015 році',
       caption = 'Дані: Державна служба статистики | Візуалізація: Textura.in.ua')+
  theme(
    text = element_text(color = '#3A3F4A'), 
    axis.title = element_blank(),
    axis.text = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(0.5, 'lines'),
    panel.grid.major = element_line(linetype = "dotted", size = 0.3, color = '#5D646F'),
    panel.spacing.x = unit(5, 'lines'),
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    legend.title = element_text(hjust = 0.5),
    legend.margin = margin(b = -10, t = 0),
    strip.text = element_text(size = 16),
    plot.title = element_text(face = "bold", size = 40, margin = margin(b = 20)),
    plot.subtitle = element_text(size = 22, margin = margin(b = 20)),
    plot.caption = element_text(size = 14, margin = margin(b = 10, t = 50), color = "#5D646F"),
    plot.background = element_rect(fill = "#EFF2F4"),
    plot.margin = unit(c(2, 3, 2, 3), "cm")
  )
dev.off()