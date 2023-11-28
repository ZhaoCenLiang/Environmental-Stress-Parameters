library(ggplot2)
library(patchwork)
library(ggsci)

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


setwd("D:/JuypterNotebook/Github/Environmental-Stress-Parameters")

# temperature stress error propagation
temp = read.csv('./data/error_analysis_temp.csv')
temp_plot = cbind(rbind(temp[,1:6],
                        temp[,1:6],
                        temp[,1:6],
                        temp[,1:6]),
                  data.frame(error = c(temp$T1_ERROR,temp$T2_ERROR,temp$T3_ERROR,temp$T4_ERROR),
                             group = rep(c('T1','T2','T3','T4'),each = nrow(temp))))

north_t = subset(temp_plot,temp_plot$lat > 0 & temp_plot$error<100)
fig5_t_n = ggplot(data = north_t, aes(x = month,y =error, group = group, color = group)) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width=0.6,
               position = position_dodge(0.35)) +
  stat_summary(fun="mean", geom="line",
               position = position_dodge(0.35)) + 
  stat_summary(fun="mean", geom="point",size = 1.5, 
               position = position_dodge(0.35))+
  theme_zg()+
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=11),
        legend.position = c(0.75,0.875),
        legend.box = 'vertical' ,
        legend.title=element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'black'),
        legend.direction = "horizontal") +
  scale_fill_npg(alpha = 1-0.618) + 
  scale_color_npg() +
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,5,1),limits = c(-0.25,2.5),expand = c(0, 0)) +
  geom_hline(yintercept=1, color = 'red', lty = 2) +
  ggtitle("Northern hemisphere") +
  ylab('Relative error (%)') +
  xlab('Month')
fig5_t_n

south_t = subset(temp_plot,temp_plot$lat <0 & temp_plot$error<100)
fig5_t_s = ggplot(data = south_t, aes(x = month,y =error, group = group, color = group)) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width=0.6,
               position = position_dodge(0.35)) +
  stat_summary(fun="mean", geom="line",
               position = position_dodge(0.35)) + 
  stat_summary(fun="mean", geom="point",size = 1.5, 
               position = position_dodge(0.35))+
  theme_zg()+
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=11),
        legend.position = c(0.75,0.875),
        legend.box = 'vertical' ,
        legend.title=element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'black'),
        legend.direction = "horizontal") +
  scale_fill_npg(alpha = 1-0.618) + 
  scale_color_npg() +
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,5,1),limits = c(-0.25,2.5),expand = c(0, 0)) +
  geom_hline(yintercept=1, color = 'red', lty = 2) +
  ggtitle("Southern hemisphere") +
  ylab('Relative error (%)') +
  xlab('Month')
fig5_t_s

## water stress error propagation 
water = read.csv('./data/error_analysis_water.csv')
water_plot = cbind(rbind(water[,1:6],
                         water[,1:6],
                         water[,1:6],
                         water[,1:6]),
                   data.frame(error = c(water$W1_ERROR,water$W2_ERROR,water$W3_ERROR,water$W4_ERROR),
                              group = rep(c('W1','W2','W3','W4'),each = nrow(water))))

north_w = subset(water_plot,water_plot$lat > 0 & water_plot$error<100)
fig5_w_n = ggplot(data = north_w, aes(x = month,y =error, group = group, color = group)) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width=0.6,
               position = position_dodge(0.35)) +
  stat_summary(fun="mean", geom="line",
               position = position_dodge(0.35)) + 
  stat_summary(fun="mean", geom="point",size = 1.5, 
               position = position_dodge(0.35))+
  theme_zg()+
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=11),
        legend.position = c(0.725,0.875),
        legend.box = 'vertical' ,
        legend.title=element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'black'),
        legend.direction = "horizontal") +
  scale_fill_npg(alpha = 1-0.618) + 
  scale_color_npg() +
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,5,1),limits = c(-0.25,2.5),expand = c(0, 0)) +
  geom_hline(yintercept=1, color = 'red', lty = 2) +
  ggtitle("Northern hemisphere") +
  ylab('Relative error (%)') +
  xlab('Month')
fig5_w_n

south_w = subset(water_plot,water_plot$lat <0 & water_plot$error<100)
fig5_w_s = ggplot(data = south_w, aes(x = month,y =error, group = group, color = group)) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width=0.6,
               position = position_dodge(0.35)) +
  stat_summary(fun="mean", geom="line",
               position = position_dodge(0.35)) + 
  stat_summary(fun="mean", geom="point",size = 1.5, 
               position = position_dodge(0.35))+
  theme_zg()+
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=11),
        legend.position = c(0.725,0.875),
        legend.box = 'vertical' ,
        legend.title=element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'black'),
        legend.direction = "horizontal") +
  scale_fill_npg(alpha = 1-0.618) + 
  scale_color_npg() +
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,5,1),limits = c(-0.25,2.5),expand = c(0, 0)) +
  geom_hline(yintercept=1, color = 'red', lty = 2) +
  ggtitle("Southern hemisphere") +
  ylab('Relative error (%)') +
  xlab('Month')
fig5_w_s
### fig 5
layout <-"
AB
CD
"
fig5 <- fig5_t_n + fig5_w_n + 
  fig5_t_s + fig5_w_s  + plot_layout(design = layout) + plot_annotation(tag_levels = 'a',
                                                                    tag_prefix = '(',
                                                                    tag_suffix = ')') &
  theme(plot.tag.position = c(0.1, 0.8),
        plot.tag = element_text(size = 20, hjust = 0, vjust = 0))

ggsave(fig5, path = './plot/'
       ,file='fig_5.pdf', width=12, height=8, dpi = 450)
ggsave(fig5, path = './plot/'
       ,file='fig_5.png', width=12, height=8, dpi = 600)