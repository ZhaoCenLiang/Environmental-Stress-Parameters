library(ggplot2)
library(ggsci)
library(plotbiomes)
library(ggpmisc)
library(patchwork)
library(viridis)

theme_zg <- function(..., bg='white'){
  require(grid)
  theme_classic(...) +
    theme(rect=element_rect(fill=bg),
          plot.margin=unit(rep(0.5,4), 'lines'),
          panel.background=element_rect(fill='transparent', color='black'),
          panel.border=element_rect(fill='transparent', color='transparent'),
          panel.grid=element_blank())
}

windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),
             Times=windowsFont("Times New Roman"),
             ARL=windowsFont("Arial"))

#
gc_plot = read.csv('./data/climate_space.csv')

## Ws CV
p_a_w <- ggplot(data = gc_plot, aes(x = MAT, y =MAP)) +
  geom_polygon(data = Whittaker_biomes, aes(x=temp_c, y = precp_cm*10, fill = biome),
               colour = 'darkorange2',linewidth=0.618, alpha = 0) + 
  geom_point(aes(color = rW_unique*100, size = CV_GPP/CV_W ),alpha=0.4) +
  scale_size_continuous(range = c(0.1, 5), name= expression(paste('CV'[GPP],' / ','CV'[W[s]]))) +
  theme_zg() + 
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=14),
        legend.title = element_text(family="Times",colour="black",size=14),
        legend.position = 'top',
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 20,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10)) +
  scale_y_continuous(breaks = seq(0,4000,1000),limits = c(-150, 5000),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-50,50,10),limits = c(-22.5, 32.5),expand = c(0, 0)) + 
  ylab(expression('Yearly total precipation (mm)')) +
  xlab(expression(paste("Yearly mean air temperature (",degree,"C)"))) + 
  scale_color_viridis(direction = -1, name = expression(paste('V'[unique],' of ','W'[s],' (%)'))) +
  guides(fill = FALSE, size = guide_legend(label.position = 'bottom', order = 1)) +
  annotate("text", x = -19, y = 250, label = "(a)", family = 'Times',size = 8)



p_b_w = ggplot(data = gc_plot, aes(y = CV_GPP/CV_W, x = log10(100*rW_unique) )) + theme_zg() +
  geom_point(size=1.5,alpha=0.5,color="#6baed6") +
  geom_smooth(method = "lm",color = "red", fill = "#cbc9e2",
              size=0.75,level = 0.95,formula = y ~ x) +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label..,..p.value.label.., sep = '~~')),
    formula = y ~ x,  parse = TRUE,
    size = 3, #??ʽ??????С
    label.x = 0.025,  #λ?? ??0-1֮???ı???
    label.y = 0.975,
    family = "Times") +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=10),
        axis.title.x = element_text(family="Times",colour="black",size=10),
        axis.text.x = element_text(family="Times",colour="black",size=7),
        axis.text.y = element_text(family="Times",colour="black",size=7),
        title = element_text(family="Times",face = "bold",colour="black",size=7),
        legend.text= element_text(family="Times",colour="black",size=7),
        legend.title = element_text(family="Times",colour="black",size=7),
        legend.position = 'right') +
  scale_y_continuous(breaks = seq(0,10,2.5),limits = c(0, 11),expand = c(0, 0)) +
  ylab(expression(paste('CV'[GPP],' / ','CV'[W[s]]) )) +
  xlab(expression(paste('The unique contribution of ','W'[s],' [log10]')))

p_w = p_a_w + inset_element(p_b_w, left = 0.01, bottom = 0.515, right = 0.6, top = 0.975)


## Ts CV 
p_a_t <- ggplot(data = gc_plot, aes(x = MAT, y =MAP)) +
  geom_polygon(data = Whittaker_biomes, aes(x=temp_c, y = precp_cm*10, fill = biome),
               colour = 'darkorange2',linewidth=0.618, alpha = 0) + 
  geom_point(aes(color = rT_unique*100, size = CV_GPP/CV_T ),alpha=0.4) +
  scale_size_continuous(range = c(0.1, 5), name= expression(paste('CV'[GPP],' / ','CV'[T[s]]))) +
  theme_zg() + 
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=14),
        legend.title = element_text(family="Times",colour="black",size=14),
        legend.position = 'top',
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 20,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10)) +
  scale_y_continuous(breaks = seq(0,4000,1000),limits = c(-150, 5000),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-50,50,10),limits = c(-22.5, 32.5),expand = c(0, 0)) + 
  ylab(expression('Yearly total precipation (mm)')) +
  xlab(expression(paste("Yearly mean air temperature (",degree,"C)"))) + 
  scale_color_viridis(direction = -1,limits = c(-10,10), name = expression(paste('V'[unique],' of ','T'[s],' (%)'))) +
  guides(fill = FALSE, size = guide_legend(label.position = 'bottom', order = 1)) +
  annotate("text", x = -19, y = 250, label = "(b)", family = 'Times',size = 8)



p_b_t = ggplot(data = gc_plot, aes(y = CV_GPP/CV_T, x = log10(100*rT_unique) )) + theme_zg() +
  geom_point(size=1.5,alpha=0.5,color="#6baed6") +
  geom_smooth(method = "lm",color = "red", fill = "#cbc9e2",
              size=0.75,level = 0.95,formula = y ~ x) +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label..,..p.value.label.., sep = '~~')),
    formula = y ~ x,  parse = TRUE,
    size = 3, #??ʽ??????С
    label.x = 0.025,  #λ?? ??0-1֮???ı???
    label.y = 0.975,
    family = "Times") +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=10),
        axis.title.x = element_text(family="Times",colour="black",size=10),
        axis.text.x = element_text(family="Times",colour="black",size=7),
        axis.text.y = element_text(family="Times",colour="black",size=7),
        title = element_text(family="Times",face = "bold",colour="black",size=7),
        legend.text= element_text(family="Times",colour="black",size=7),
        legend.title = element_text(family="Times",colour="black",size=7),
        legend.position = 'right') +
  ylab(expression(paste('CV'[GPP],' / ','CV'[T[s]]) )) +
  ylim(c(0,10)) + 
  xlab(expression(paste('The unique contribution of ','T'[s],' [log10]')))

p_t = p_a_t + inset_element(p_b_t, left = 0.01, bottom = 0.515, right = 0.6, top = 0.975)


layout <-"
AB
"

fig8 = p_w + p_t + plot_layout(design = layout)

ggsave(fig8, path = './plot/',
       file='fig_8.pdf', width=13, height=6, dpi = 450)
ggsave(fig8, path = './plot/',
       file='fig_8.png', width=13, height=6, dpi = 600)