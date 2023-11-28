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

### fig 6
comb = c('T1 × W1','T1 × W2','T1 × W3','T1 × W4',
         'T2 × W1','T2 × W2','T2 × W3','T2 × W4',
         'T3 × W1','T3 × W2','T3 × W3','T3 × W4',
         'T4 × W1','T4 × W2','T4 × W3','T4 × W4')
joint = read.csv('./data/error_analysis_joint.csv')
tw_plot = cbind(rbind(joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6],
                      joint[,1:6]),
                data.frame(error = c(joint[,7],joint[,8],joint[,9],joint[,10],
                                     joint[,11],joint[,12],joint[,13],joint[,14],
                                     joint[,15],joint[,16],joint[,17],joint[,18],
                                     joint[,19],joint[,20],joint[,21],joint[,22]),
                           group = rep(comb,each = nrow(joint))))

### plot 
north_tw = subset(tw_plot,tw_plot$lat > 0 & tw_plot$error<100)
fig6_tw_n = ggplot(data = north_tw, aes(x = month,y =error, group = group, color = group)) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width=1.6,
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
        legend.text= element_text(family="Times",colour="black",size=8),
        legend.position = c(0.5,0.875),
        legend.box = 'vertical' ,
        legend.title=element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'black'),
        legend.direction = "horizontal") +
  scale_fill_d3("category20") + 
  scale_color_d3("category20") +
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,5,1),limits = c(-0.25,2.5),expand = c(0, 0)) +
  geom_hline(yintercept=1, color = 'red', lty = 2) +
  ggtitle("Northern hemisphere") +
  ylab('Relative error (%)') +
  xlab('Month')+
  guides( color = guide_legend(ncol = 1))
fig6_tw_n

south_tw = subset(tw_plot,tw_plot$lat <0 & tw_plot$error<100)
fig6_tw_s = ggplot(data = south_tw, aes(x = month,y =error, group = group, color = group)) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width=1.6,
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
        legend.text= element_text(family="Times",colour="black",size=8),
        legend.position = c(0.5,0.875),
        legend.box = 'vertical' ,
        legend.title=element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'black'),
        legend.direction = "horizontal") +
  scale_fill_d3("category20") + 
  scale_color_d3("category20") +
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,5,1),limits = c(-0.25,2.5),expand = c(0, 0)) +
  geom_hline(yintercept=1, color = 'red', lty = 2) +
  ggtitle("Southern hemisphere") +
  ylab('Relative error (%)') +
  xlab('Month') +
  guides( color = guide_legend(ncol = 1))
fig6_tw_s

## export
fig6 <- fig6_tw_n + fig6_tw_s + plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'a',
                                                                            tag_prefix = '(',
                                                                            tag_suffix = ')') &
  theme(plot.tag.position = c(0.1, 0.85),
        plot.tag = element_text(size = 20, hjust = 0, vjust = 0))
fig6

ggsave(fig6, path = './plot/'
       ,file='fig6.pdf', width=12, height=5, dpi = 450)
ggsave(fig6, path = './plot/'
       ,file='fig6.png', width=12, height=5, dpi = 600)