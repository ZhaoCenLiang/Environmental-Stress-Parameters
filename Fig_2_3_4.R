#Fig 4
library(rdacca.hp)
library(ggplot2)
library(ggsci)
library(patchwork)

setwd('D:/JuypterNotebook/Github/Environmental-Stress-Parameters') # change the workspace
meta = read.csv('./data/readyforCA.csv')

# CA Ts
(spe.hp  <- rdacca.hp(meta$GPP_VUT_Night,meta[,11:14],method="RDA", type="adjR2",var.part=T))
# Venn
plot(spe.hp)
# export CA results
write.csv(spe.hp$Hier.part,file = 'CA_Temp_hier.csv')
write.csv(spe.hp$Var.part,file = 'CA_Temp_VPA.csv')
# significant level test
permu.hp(meta$GPP_VUT_Night,meta[,11:14], method="RDA", type = "adjR2")

# CA Ws
(spe.hp  <- rdacca.hp(meta$GPP_VUT_Night,meta[,15:18],method="RDA", type="adjR2",var.part=T))
# Venn
plot(spe.hp)
# export CA results
write.csv(spe.hp$Hier.part,file = 'CA_Water_hier.csv')
write.csv(spe.hp$Var.part,file = 'CA_Water_VPA.csv')
# significant level test
permu.hp(meta$GPP_VUT_Night,meta[,15:18], method="RDA", type = "adjR2")

# fig 4 violin plot
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

# temeprature stress
box = data.frame(Ts = c(meta$T1,meta$T2,meta$T3,meta$T4), group = rep(c('T1','T2','T3','T4'),each = nrow(meta)))
box$Ts[box$Ts > 1] = 1

mean = data.frame(value = c(mean(meta$T1),mean(meta$T2),mean(meta$T3),mean(meta$T4)),
                  group = c('T1','T2','T3','T4'))

cl=pal_npg("nrc",alpha = 0.5)(4)
cf=pal_npg("nrc")(4)

fig4_t <- ggplot(box, aes(x = group, y = Ts, fill = group)) +
  geom_violin() +
  geom_boxplot(width=0.075, color='black',fill = cf) +
  geom_point(data=mean, aes(x = group, y = value), shape = 18, color =" white",
             fill = 'black',size = 2) +
  theme_zg() + 
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=11),
        legend.position = 'top',
        legend.justification = c(1, 0),
        legend.box = 'vertical' ,
        legend.title=element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'black'),
        legend.direction = "horizontal") +
  scale_fill_npg(alpha = 1-0.618) + 
  scale_color_npg() +
  scale_y_continuous(breaks = seq(0,1,0.2),limits = c(-0.1,1.1),expand = c(0, 0)) +
  ylab('Temperature stress') +
  xlab('')
fig4_t

# water stress
cl=pal_npg("nrc",alpha = 0.5)(4)
cf=pal_npg("nrc")(4)

box2 = data.frame(Ws = c(meta$W1,meta$W2,meta$W3,meta$W4), group = rep(c('W1','W2','W3','W4'),each = nrow(meta)))
box2$Ws[box2$Ws > 1] = 1
mean2 = data.frame(value = c(mean(meta$W1),mean(meta$W2),mean(meta$W3),mean(meta$W4)),
                   group = c('W1','W2','W3','W4'))
fig4_w <- ggplot(box2, aes(x = group, y = Ws, fill = group)) +
  geom_violin() +
  geom_boxplot(width=0.075, color='black',fill = cf) +
  geom_point(data=mean2, aes(x = group, y = value), shape = 18, color =" white",
             fill = 'black',size = 2) +
  theme_zg() + 
  theme(axis.title.y=element_text(family="Times",angle=90, colour="black",size=14),
        axis.title.x = element_text(family="Times",colour="black",size=14),
        axis.text.x = element_text(family="Times",colour="black",size=14),
        axis.text.y = element_text(family="Times",colour="black",size=14),
        title = element_text(family="Times",face = "bold",colour="black",size=14),
        legend.text= element_text(family="Times",colour="black",size=11),
        legend.position = 'top',
        legend.justification = c(1, 0),
        legend.box = 'vertical' ,
        legend.title=element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'black'),
        legend.direction = "horizontal") +
  scale_fill_npg(alpha = 0.25) + 
  scale_color_npg() +
  scale_y_continuous(breaks = seq(0,1,0.2),limits = c(-0.1,1.1),expand = c(0, 0)) +
  ylab('Water stress') +
  xlab('')
fig4_w


# fig4 export
layout <-"
AA
BB
"

fig4 <- fig4_t + fig4_w + plot_layout(design = layout) + plot_annotation(tag_levels = 'a',
                                                                   tag_prefix = '(',
                                                                   tag_suffix = ')') &
  theme(plot.tag.position = c(0.075, 0.925),
        plot.tag = element_text(size = 20, hjust = 0, vjust = 0))

fig4

ggsave(fig4, path = './plot/'
       ,file='fig_4.pdf', width=6, height=8, dpi = 450)
ggsave(fig4, path = './plot/'
       ,file='fig_4.png', width=6, height=8, dpi = 600)