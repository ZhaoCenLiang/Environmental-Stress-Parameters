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

### biomes plot
biomes_plot = whittaker_base_plot() + theme_zg() +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=12),
        legend.title = element_text(family="Times",colour="black",size=12))

ggsave(biomes_plot, path = './plot/',
       file='figS1_BIOME.pdf', width=8, height=4.5, dpi = 450)
ggsave(biomes_plot, path = './plot/',
       file='figS1_BIOME.png', width=8, height=4.5, dpi = 600)

## fig7 a tw r2
p_a_tw_r2 <- ggplot(data = gc_plot, aes(x = MAT, y =MAP)) +
  geom_polygon(data = Whittaker_biomes, aes(x=temp_c, y = precp_cm*10, fill = biome),
               colour = 'darkorange2',linewidth=0.618, alpha = 0) + 
  geom_point(aes(color = rTW_total*100, size = R2 ),alpha=0.4) +
  scale_size_continuous(range = c(0.1, 5), name= expression(paste('R'^2))) +
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
  scale_color_viridis(direction = -1,limits = c(0,100), name = expression(paste('V'[total],' of ','T'[s],'+','W'[s]))) +
  guides(fill = FALSE, size = guide_legend(label.position = 'bottom', order = 1)) +
  annotate("text", x = -19, y = 250, label = "(a)", family = 'Times',size = 8)

p_b_tw_r2 = ggplot(data = gc_plot, aes(y = R2, x = log10(100*rTW_total) )) + theme_zg() +
  geom_point(size=1.5,alpha=0.5,color="#6baed6") +
  geom_smooth(method = "lm",color = "red", fill = "#cbc9e2",
              size=0.75,level = 0.95,formula = y ~ x) +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label..,..p.value.label.., sep = '~~')),
    formula = y ~ x,  parse = TRUE,
    size = 3, #??ʽ??????С
    label.x = 0.025,  #λ?? ??0-1֮???ı???
    label.y = 0.025,
    family = "Times") +
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=10),
        axis.title.x = element_text(family="Times",colour="black",size=10),
        axis.text.x = element_text(family="Times",colour="black",size=7),
        axis.text.y = element_text(family="Times",colour="black",size=7),
        title = element_text(family="Times",face = "bold",colour="black",size=7),
        legend.text= element_text(family="Times",colour="black",size=7),
        legend.title = element_text(family="Times",colour="black",size=7),
        legend.position = 'right') +
  ylab(expression(paste('R'^2))) + 
  xlab(expression(paste('The total contribution of ','T'[s],'+','W'[s],' [log10]')))

p_tw_r2 = p_a_tw_r2 + inset_element(p_b_tw_r2, left = 0.01, bottom = 0.515, right = 0.6, top = 0.975)

## fig7 b tw r2
p_a_tw_rMAE <- ggplot(data = gc_plot, aes(x = MAT, y =MAP)) +
  geom_polygon(data = Whittaker_biomes, aes(x=temp_c, y = precp_cm*10, fill = biome),
               colour = 'darkorange2',linewidth=0.618, alpha = 0) + 
  geom_point(aes(color = rTW_total*100, size = rMAE*100 ),alpha=0.4) +
  scale_size_continuous(range = c(0.1, 5), name= expression(paste('rMAE'))) +
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
  scale_color_viridis(direction = -1,limits = c(0,100), name = expression(paste('V'[total],' of ','T'[s],'+','W'[s]))) +
  guides(fill = FALSE, size = guide_legend(label.position = 'bottom', order = 1)) +
  annotate("text", x = -19, y = 250, label = "(b)", family = 'Times',size = 8)



p_b_tw_rMAE = ggplot(data = gc_plot, aes(y = rMAE*100, x = log10(100*rTW_total) )) + theme_zg() +
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
  ylab(expression(paste('rMAE (%)'))) + 
  xlab(expression(paste('The total contribution of ','T'[s],'+','W'[s],' [log10]')))

p_tw_rMAE = p_a_tw_rMAE + inset_element(p_b_tw_rMAE, left = 0.01, bottom = 0.515, right = 0.6, top = 0.975)

## fig7 b tw CV
p_a_tw_cv <- ggplot(data = gc_plot, aes(x = MAT, y =MAP)) +
  geom_polygon(data = Whittaker_biomes, aes(x=temp_c, y = precp_cm*10, fill = biome),
               colour = 'darkorange2',linewidth=0.618, alpha = 0) + 
  geom_point(aes(color = rTW_total*100, size = CV_GPP/CV_TW ),alpha=0.4) +
  scale_size_continuous(range = c(0.1, 5), name= expression(paste('CV'[GPP],' / ','CV'[T[s]+W[s]]))) +
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
  scale_color_viridis(direction = -1,limits = c(0,100), name = expression(paste('V'[total],' of ','T'[s],'+','W'[s]))) +
  guides(fill = FALSE, size = guide_legend(label.position = 'bottom', order = 1)) +
  annotate("text", x = -19, y = 250, label = "(c)", family = 'Times',size = 8)


p_b_tw_cv = ggplot(data = gc_plot, aes(y = CV_GPP/CV_TW, x = 100*rTW_total )) + theme_zg() +
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
  ylab(expression(paste('CV'[GPP],' / ','CV'[T[s]+W[s]]))) + 
  xlab(expression(paste('The total contribution of ','T'[s],'+','W'[s]))) +
  xlim(c(0,100)) +
  ylim(c(0,10))

p_tw_cv = p_a_tw_cv + inset_element(p_b_tw_cv, left = 0.01, bottom = 0.515, right = 0.6, top = 0.975)

## fig7 b tw Residuals
p_a_tw_resi <- ggplot(data = gc_plot, aes(x = MAT, y =MAP)) +
  geom_polygon(data = Whittaker_biomes, aes(x=temp_c, y = precp_cm*10, fill = biome),
               colour = 'darkorange2',linewidth=0.618, alpha = 0) + 
  geom_point(aes(color = rTW_total*100, size = Residuals ),alpha=0.4) +
  scale_size_continuous(range = c(0.1, 5), name= expression(paste('Residuals'))) +
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
  scale_color_viridis(direction = -1,limits = c(0,100), name = expression(paste('V'[total],' of ','T'[s],'+','W'[s]))) +
  guides(fill = FALSE, size = guide_legend(label.position = 'bottom', order = 1)) +
  annotate("text", x = -19, y = 250, label = "(d)", family = 'Times',size = 8)


p_b_tw_resi = ggplot(data = gc_plot, aes(y = Residuals, x = 100*rTW_total )) + theme_zg() +
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
  ylab(expression(paste('Residuals'))) + 
  xlab(expression(paste('The total contribution of ','T'[s],'+','W'[s]))) +
  xlim(c(0,100)) 

p_tw_resi = p_a_tw_resi + inset_element(p_b_tw_resi, left = 0.01, bottom = 0.515, right = 0.6, top = 0.975)

## fig 7
layout <-"
AB
CD
"

P_TW_TOTAL = p_tw_r2 + p_tw_rMAE + p_tw_cv + p_tw_resi + plot_layout(design = layout)

ggsave(P_TW_TOTAL, path = './plot/',
       file='fig_7.pdf', width=12, height=12, dpi = 450)
ggsave(P_TW_TOTAL, path = './plot/',
       file='fig_7.png', width=12.5, height=12, dpi = 600)