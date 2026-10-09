library(ggplot2)
library(ggh4x)
library(ggpubr)
library(cowplot)

#### Figure 1 ####
dataall <- read.csv("vietnam_serosurvey.csv")
dataall <- as.data.table(dataall)
colnames(dataall)

figuredata1 <- melt(dataall[,c(1,13:15,29,30,19:22)],measure.vars = c("DV1","DV2","DV3","DV4"))
xaxis_list <- dataall[,c(1,13:15)]
xaxis_list <- xaxis_list[order(xaxis_list[,DAY]),]
xaxis_list <- xaxis_list[order(xaxis_list[,MONTH]),]
xaxis_list <- xaxis_list[order(xaxis_list[,YEAR]),]

xaxis_list[,self_id_index:=1:dim(xaxis_list)[1]]
figuredata1 <- merge(figuredata1,xaxis_list[,c(1,5)],by="sampleID")
figuredata1[,serotype2:=factor(variable,levels=c("DV4","DV3","DV2","DV1"),labels=c("D4","D3","D2","D1"))]
figuredata1[,city1:=factor(id1,levels = c("HC","KH"),
                           labels = c("Ho Chi Minh City","Khanh Hoa"))]
figuredata1[,age_group1:=factor(age_group,levels = c("(0-5]","(5-10]","(10-15]","(15-20]","(20-25]","(25-30]"))]

p <- ggplot(figuredata1)+
  geom_tile(aes(x=factor(self_id_index),y=serotype2,fill=value))+
  scale_fill_gradientn(colours = c("#E3F2FDFF", "#BADEFAFF","#64B4F6FF","#1E87E5FF","#0C46A0FF"),
                       values = scales::rescale(c(3.322,5,5+1.774,5+1.774*2,10.322),
                                                to = c(0, 1), from = c(3.322, 10.322)),
                       breaks = c(3,5,7,9))+
  facet_grid2(city1~age_group1,scales = "free_x",independent="x")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  labs(fill="Titre")+
  theme_bw()+
  theme(legend.position = "bottom",legend.justification = "left",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.ticks.length.x = unit(0, "pt"))

ggsave("Figure1.png",p,width = 12,height = 4,bg="white")

#### Figure 2 ####
##### foi #####
result1 <- readRDS("foi_est_vn_StatusModel.rds")
result2 <- readRDS("foi_est_vn_TitreModel.rds")


result1[,type1:="cross-reactive catalytic model"]
result2[,type1:="cross-reactive catalytic-titre model"]

figuredata <- rbind(result1,result2)
figuredata[,city1:=factor(city,levels = c("HCMC","KH"),
                          labels = c("Ho Chi Minh City","Khanh Hoa"))]

pfoi <- ggplot(subset(figuredata,serotype!="non-serotype-specific"))+
  geom_point(aes(x=time_group,y=foi_median,color=type1,group=type1),position=position_dodge(width = 0.6),size=1)+
  geom_errorbar(aes(x=time_group,ymin=foi_L,ymax=foi_U,color=type1,group=type1),width=0.2,position=position_dodge(width = 0.6))+
  facet_grid(city1~serotype,scales = "free_y")+
  scale_x_discrete(breaks=seq(1980,2017,by=5))+
  coord_cartesian(ylim = c(0,0.4))+
  scale_color_manual(breaks = c("cross-reactive catalytic model","cross-reactive catalytic-titre model"),
                     values = c("#2A2A6B","#A40000"),name="",
                     labels = c("serostatus-based model","titre-incorporated model"))+
  labs(y="Annual force of infection (FOI)",x="Year")+
  theme_bw()+
  theme(legend.position = c(0.12,0.9),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        legend.background = element_rect(fill='transparent'),
        axis.title.y = element_text(size=10))


##### cross reactivity #####
cross_result1 <- readRDS("cross_est_vn_StatusModel.rds")
cross_result2 <- readRDS("cross_est_vn_TitreModel.rds")

cross_result1[,label_index:=1:dim(cross_result1)[1]]
cross_result2[,label_index:=1:dim(cross_result2)[1]]
cross_result1[,type1:="cross-reactive catalytic model"]
cross_result2[,type1:="cross-reactive catalytic-titre model"]

cross_figuredata <- rbind(cross_result1,cross_result2)
cross_figuredata[,label1:=factor(infected_virus1,levels = c("D1","D2","D3","D4",
                                                            "D1,D2","D1,D3","D1,D4","D2,D3","D2,D4","D3,D4",
                                                            "D1,D2,D3","D1,D2,D4","D1,D3,D4","D2,D3,D4"))]

set(cross_figuredata,which(cross_figuredata[,infec_time==1]),"facet1","Primary infections")
set(cross_figuredata,which(cross_figuredata[,infec_time==2]),"facet1","Two prior infections")
set(cross_figuredata,which(cross_figuredata[,infec_time==3]),"facet1","Three prior infections")

exp_figure <- expand.grid(serotype=c("D1","D2","D3","D4"),exposure_history=c("D1","D2","D3","D4",
                                                                             "D1,D2","D1,D3","D1,D4","D2,D3","D2,D4","D3,D4",
                                                                             "D1,D2,D3","D1,D2,D4","D1,D3,D4","D2,D3,D4"))
exp_figure <- as.data.table(exp_figure)
for (i in 1:dim(exp_figure)[1]){
  infec_serotype=strsplit(as.character(exp_figure[i,]$exposure_history),split=",")[[1]]
  if (exp_figure[i,]$serotype %in% infec_serotype){
    set(exp_figure,i,"infec_serotype",exp_figure[i,]$serotype)
  }
}
rev(exp_figure$exposure_history)
exp_figure[,rev_exposure_history:=factor(exposure_history,levels = rev(unique(exp_figure$exposure_history)))]
exp_figure[,rev_serotype:=factor(serotype,levels = rev(unique(exp_figure$serotype)))]

cross_figuredata[,rev_label1:=factor(label1,levels = rev(unique(cross_figuredata$label1)))]

p1 <- ggplot(exp_figure)+
  geom_tile(aes(y=serotype,x=exposure_history,fill=infec_serotype),alpha=0.9,color="white")+
  geom_text(aes(y=serotype,x=exposure_history,label=infec_serotype),color="white")+
  scale_fill_manual(name="",na.value = "grey82",
                    breaks=c("D1","D2","D3","D4"),
                    values = c("black","black","black","black"))+
  ggh4x::facet_grid2(serotype~exposure_history,scales = "free",independent="all")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  theme_bw()+
  theme(
    panel.spacing.y = unit(0, "pt"),
    panel.spacing.x = unit(2, "pt"),
    panel.border = element_rect(color = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    strip.text.x = element_blank(),
    strip.background.y = element_rect(fill = "white",color = "grey"),
    plot.margin = margin(t = 0.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"),
    legend.position = ""
  )

p2 <- ggplot(cross_figuredata)+
  geom_point(aes(x=cross_virus1 ,y=beta_est,color=type1,group=type1),position=position_dodge(width = 0.5),size=2)+
  geom_errorbar(aes(x=cross_virus1 ,ymin=beta_CI_L,ymax=beta_CI_U,color=type1,group=type1),width=0.3,position=position_dodge(width = 0.5))+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,by=0.2))+
  scale_color_manual(breaks = c("cross-reactive catalytic model","cross-reactive catalytic-titre model"),
                     values = c("#2A2A6B","#A40000"),
                     name="")+
  ggh4x::facet_grid2(cross_virus1~label1,scales="free_x",independent="x")+
  theme_bw()+
  theme(
    panel.spacing.y = unit(0, "pt"),
    panel.spacing.x = unit(2, "pt"),
    panel.border = element_rect(color = "grey"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.x = unit(0, "pt"),
    strip.text.x = element_blank(),
    strip.background.y = element_rect(fill = "white",color = "grey"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.margin = margin(t = 5.5, r = 5.5, b = 0.5, l = 25.5, unit = "pt"),
    legend.position = ""
  )

pc1 <- ggarrange(p2,p1,nrow=2,heights = c(4,0.7),align = "v")

ads=0.064
ads_s=0.00225

k1=1:13
pcross=ggdraw(pc1) +
  draw_line(x = c(0.049, 0.049), y = c(0.012, 0.99), linewidth = 0.5)

for (k in 1:length(k1)){
  pcross=pcross+
    draw_line(x = c(0.049+k1[k]*ads+(k1[k]-1L)*ads_s, 0.049+k1[k]*ads+(k1[k]-1L)*ads_s), y = c(0.012, 0.99), linewidth = 0.3)+
    draw_line(x = c(0.049+k1[k]*ads+k1[k]*ads_s, 0.049+k1[k]*ads+k1[k]*ads_s), y = c(0.012, 0.99), linewidth = 0.3)
}
pcross=pcross+
  draw_line(x = c(0.049+14*ads+13*ads_s, 0.049+14*ads+13*ads_s), y = c(0.012, 0.99), linewidth = 0.5)+
  draw_line(x = c(0.049+14*ads+13*ads_s+0.019, 0.049+14*ads+13*ads_s+0.019), y = c(0.012, 0.99), linewidth = 0.5)

pcross=pcross+
  draw_line(x = c(0.049+4*ads+3.5*ads_s, 0.049+4*ads+3.5*ads_s), y = c(0.012, 0.99), linewidth = 0.7)+
  draw_line(x = c(0.049+10*ads+9.5*ads_s, 0.049+10*ads+9.5*ads_s), y = c(0.012, 0.99), linewidth = 0.7)

for (k in 1:length(k1)){
  pcross=pcross+
    draw_line(x = c(0.049+k1[k]*ads+k1[k]*ads_s, 0.049+(k1[k]+1L)*ads+k1[k]*ads_s), y = c(0.012, 0.012), linewidth = 0.3)+
    draw_line(x = c(0.049+k1[k]*ads+k1[k]*ads_s, 0.049+(k1[k]+1L)*ads+k1[k]*ads_s), y = c(0.99, 0.99), linewidth = 0.3)
}
pcross=pcross+
  draw_line(x = c(0.049, 0.049+1L*ads), y = c(0.012, 0.012), linewidth = 0.3)+
  draw_line(x = c(0.049, 0.049+1L*ads), y = c(0.99, 0.99), linewidth = 0.3)
pcross=pcross+
  draw_line(x = c(0.049+14*ads+13*ads_s, 0.049+14*ads+13*ads_s+0.019), y = c(0.012, 0.012), linewidth = 0.3)+
  draw_line(x = c(0.049+14*ads+13*ads_s, 0.049+14*ads+13*ads_s+0.019), y = c(0.99, 0.99), linewidth = 0.3)

pcross=pcross+
  draw_text("Probability of generating cross-reactive antibodies against heterotypic serotypes",y = mean(c(0.157, 0.99)), angle = 90,x = 0.015,size=10)+
  draw_text("Exposure\nprofiles",y = mean(c(0.01, 0.157)), angle = 90,x = 0.025,size=10)

pcross2 <- ggarrange(pcross,NULL,nrow=2,heights = c(1,0.06))
curdis = 0.01
bottom_y=0.04
curve1 <- curveGrob(
  x1 = unit(0.049, "npc"),
  y1 = unit(0.08, "npc"),
  x2 = unit(0.049+curdis, "npc"),
  y2 = unit(bottom_y, "npc"),
  curvature = 0.5
)
curve2 <- curveGrob(
  x1 = unit(0.049-curdis+4L*ads+3L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+4L*ads+3L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = 0.5
)
curve3 <- curveGrob(
  x1 = unit(0.049+curdis+4L*ads+4L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+4L*ads+4L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = -0.5
)
curve4 <- curveGrob(
  x1 = unit(0.049-curdis+10L*ads+9L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+10L*ads+9L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = 0.5
)
curve5 <- curveGrob(
  x1 = unit(0.049+curdis+10L*ads+10L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+10L*ads+10L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = -0.5
)
curve6 <- curveGrob(
  x1 = unit(0.049-curdis+14L*ads+13L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+14L*ads+13L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = 0.5
)
pcross2=pcross2+
  draw_line(x = c(0.049+curdis, 0.049-curdis+4L*ads+3L*ads_s), y = c(bottom_y, bottom_y), linewidth = 0.3)+
  draw_line(x = c(0.049+curdis+4L*ads+4L*ads_s, 0.049-curdis+10L*ads+9L*ads_s), y = c(bottom_y, bottom_y), linewidth = 0.3)+
  draw_line(x = c(0.049+curdis+10L*ads+10L*ads_s, 0.049-curdis+14L*ads+13L*ads_s), y = c(bottom_y, bottom_y), linewidth = 0.3)

pcross2=ggdraw(pcross2) +
  draw_grob(curve1)+
  draw_grob(curve2)+
  draw_grob(curve3)+
  draw_grob(curve4)+
  draw_grob(curve5)+
  draw_grob(curve6)

pcross3=pcross2+
  draw_text("Primary infection",x=mean(c(0.049+curdis, 0.049-curdis+4L*ads+3L*ads_s)),y = 0.025,size=10)+
  draw_text("Secondary infection",x=mean(c(0.049+curdis+4L*ads+4L*ads_s, 0.049-curdis+10L*ads+9L*ads_s)),y = 0.025,size=10)+
  draw_text("Third infection",x=mean(c(0.049+curdis+10L*ads+10L*ads_s, 0.049-curdis+14L*ads+13L*ads_s)),y = 0.025,size=10)

legend <- cowplot::get_legend(pfoi)
legend_figure <- as_ggplot(legend)
legend_figure <- legend_figure+theme(legend.margin=margin(c(t=0,r=0,b=0,l=0)))

pcross4=ggdraw(pcross3)+
  draw_plot(legend_figure, 0.88,0.3,0.05,0.05)

pall <- ggarrange(pfoi,pcross4,ncol=1,labels = c("A","B"),heights = c(1,1.2))
ggsave("Figure2.png",pall,width = 12,height = 13,bg="white")

#### Figure 3 ####
sero_age <- readRDS("prevalence_est_TitreModel.rds")
sus_age <- readRDS("susceptibility_est_TitreModel.rds")

sero_age <- subset(sero_age,year==2015)
sero_age[,location1:=factor(location,levels = c("HC","KH"),
                            labels = c("Ho Chi Minh City","Khanh Hoa"))]

sus_age <- subset(sus_age,year==2015)
sus_age[,location1:=factor(location,levels = c("HC","KH"),
                           labels = c("Ho Chi Minh City","Khanh Hoa"))]

set(sero_age,NULL,"serotype",sero_age[,factor(serotype,levels = c("non-serotype","D1","D2","D3","D4"),labels = c("non-serotype","DENV-1","DENV-2","DENV-3","DENV-4"))])
set(sus_age,NULL,"serotype",sus_age[,factor(serotype,levels = c("non-serotype","D1","D2","D3","D4"),labels = c("non-serotype","DENV-1","DENV-2","DENV-3","DENV-4"))])

p_sus0_age <- ggplot()+
  geom_point(data=subset(sus_age,serotype=="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,y=sus_all_primary,group=location1))+
  geom_errorbar(data=subset(sus_age,serotype=="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,ymin=sus_all_primary_l,ymax=sus_all_primary_u,group=location1),width=0.4)+
  facet_wrap(.~location1)+
  scale_x_continuous(breaks=c(1,seq(10,80,by=10)))+
  scale_y_continuous(limits = c(-0.01,1),breaks=seq(0,1,by=0.2),labels=scales::percent,expand = c(0.002,0.002))+
  labs(title="Susceptibility to primary dengue infection",x="Age (years)",y="Proportion")+
  theme_bw()+
  theme(legend.position = "",
        plot.title = element_text(size=11),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(0.3, "cm"),
        legend.direction = "vertical", legend.box = "horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_immune1st_age <- ggplot()+
  geom_point(data=subset(sero_age,serotype!="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,y=sero_est_primary,group=location1,color=serotype))+
  geom_errorbar(data=subset(sero_age,serotype!="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,ymin=sero_est_primary_l,ymax=sero_est_primary_u,group=location1,color=serotype),width=0.4)+
  geom_line(data=subset(sero_age,serotype!="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,y=sero_mono_est,group=location1,color=serotype))+
  geom_ribbon(data=subset(sero_age,serotype!="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,ymin=sero_mono_est_l,ymax=sero_mono_est_u,group=location1,fill=serotype),alpha=0.5)+
  scale_color_manual(breaks = c("DENV-1","DENV-2","DENV-3","DENV-4"),
                     values = c("#BB3200","#EBA000","#238E8F","#085F8D"))+
  scale_fill_manual(breaks = c("DENV-1","DENV-2","DENV-3","DENV-4"),
                    values = c("#BB3200","#EBA000","#238E8F","#085F8D"))+
  facet_grid(location1~serotype)+
  scale_x_continuous(breaks=c(1,seq(10,80,by=10)))+
  scale_y_continuous(limits = c(-0.01,1),breaks=seq(0,1,by=0.2),labels=scales::percent,expand = c(0.002,0.002))+
  labs(title="Monotypic immunity and monotypic seroprevalence by target serotype",x="Age (years)",y="Proportion")+
  theme_bw()+
  theme(legend.position = "",
        plot.title = element_text(size=11),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(0.3, "cm"),
        legend.direction = "vertical", legend.box = "horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p_immune2nd_age <- ggplot()+
  geom_point(data=subset(sero_age,serotype!="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,y=sero_est_multi,group=location1,color=serotype))+
  geom_errorbar(data=subset(sero_age,serotype!="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,ymin=sero_est_multi_l,ymax=sero_est_multi_u,group=location1,color=serotype),width=0.4)+
  geom_line(data=subset(sero_age,serotype!="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,y=sero_multi_est,group=location1,color=serotype))+
  geom_ribbon(data=subset(sero_age,serotype!="non-serotype"&age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,ymin=sero_multi_est_l,ymax=sero_multi_est_u,group=location1,fill=serotype),alpha=0.5)+
  scale_color_manual(breaks = c("DENV-1","DENV-2","DENV-3","DENV-4"),
                     values = c("#BB3200","#EBA000","#238E8F","#085F8D"))+
  scale_fill_manual(breaks = c("DENV-1","DENV-2","DENV-3","DENV-4"),
                    values = c("#BB3200","#EBA000","#238E8F","#085F8D"))+
  facet_grid(location1~serotype)+
  scale_x_continuous(breaks=c(1,seq(10,80,by=10)))+
  scale_y_continuous(limits = c(-0.01,1),breaks=seq(0,1,by=0.2),labels=scales::percent,expand = c(0.002,0.002))+
  labs(title="Multitypic immunity and multitypic seroprevalence by target serotype",x="Age (years)",y="Proportion")+
  theme_bw()+
  theme(legend.position = "",
        plot.title = element_text(size=11),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(0.3, "cm"),
        legend.direction = "vertical", legend.box = "horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p_sus1st_age <- ggplot()+
  geom_point(data=subset(sus_age,age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,y=sus_all_secondary,group=serotype,color=serotype),position = position_dodge(width = 3))+
  geom_errorbar(data=subset(sus_age,age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,ymin=sus_all_secondary_l,ymax=sus_all_secondary_u,group=serotype,color=serotype),width=0.4,position = position_dodge(width = 3))+
  scale_color_manual(breaks = c("non-serotype","DENV-1","DENV-2","DENV-3","DENV-4"),
                     values = c("black","#BB3200","#EBA000","#238E8F","#085F8D"))+
  scale_fill_manual(breaks = c("non-serotype","DENV-1","DENV-2","DENV-3","DENV-4"),
                    values = c("black","#BB3200","#EBA000","#238E8F","#085F8D"))+
  #facet_wrap(.~location1,ncol=1, strip.position = "right")+
  facet_grid(location1~1)+
  scale_x_continuous(breaks=c(1,seq(10,80,by=10)))+
  scale_y_continuous(limits = c(-0.01,1),breaks=seq(0,1,by=0.2),labels=scales::percent,expand = c(0.002,0.002))+
  labs(title="Susceptibility to secondary infection by the target serotype",x="Age (years)",y="Proportion")+
  theme_bw()+
  theme(legend.position = "",
        plot.title = element_text(size=11),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background.y = element_rect(fill = "white"),
        strip.text.x = element_text(colour = NA),
        strip.background.x  = element_blank(),
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(0.3, "cm"),
        legend.direction = "vertical", legend.box = "horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_sus2nd_age <- ggplot()+
  geom_point(data=subset(sus_age,age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,y=sus_all_multi,group=serotype,color=serotype),position = position_dodge(width = 3))+
  geom_errorbar(data=subset(sus_age,age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,ymin=sus_all_multi_l,ymax=sus_all_multi_u,group=serotype,color=serotype),width=0.4,position = position_dodge(width = 3))+
  scale_color_manual(breaks = c("non-serotype","DENV-1","DENV-2","DENV-3","DENV-4"),
                     values = c("black","#BB3200","#EBA000","#238E8F","#085F8D"))+
  scale_fill_manual(breaks = c("non-serotype","DENV-1","DENV-2","DENV-3","DENV-4"),
                    values = c("black","#BB3200","#EBA000","#238E8F","#085F8D"))+
  facet_grid(location1~1)+
  scale_x_continuous(breaks=c(1,seq(10,80,by=10)))+
  scale_y_continuous(limits = c(-0.01,1),breaks=seq(0,1,by=0.2),labels=scales::percent,expand = c(0.002,0.002))+
  labs(title="Susceptibility to third or later infection by the target serotype",x="Age (years)",y="Proportion")+
  theme_bw()+
  theme(legend.position = "",
        plot.title = element_text(size=11),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background.y = element_rect(fill = "white"),
        strip.text.x = element_text(colour = NA),
        strip.background.x  = element_blank(),
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(0.3, "cm"),
        legend.direction = "vertical", legend.box = "horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##### legend #####
p_forlegend1 <- ggplot()+
  geom_point(data=subset(sero_age,age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,y=sero_est_primary,group=location1,color=serotype))+
  geom_errorbar(data=subset(sero_age,age_est%in%c(1,seq(5,80,by=5))),aes(x=age_est,ymin=sero_est_primary_l,ymax=sero_est_primary_u,group=location1,color=serotype),width=0.4)+
  scale_color_manual(name="Reconstructed immunity (B.,D.)\nor susceptibility (A.,C.,E.)",breaks = c("non-serotype","DENV-1","DENV-2","DENV-3","DENV-4"),
                     values = c("black","#BB3200","#EBA000","#238E8F","#085F8D"),
                     labels = c("Any dengue serotype","DENV-1","DENV-2","DENV-3","DENV-4")  )+
  facet_grid(location1~serotype)+
  scale_x_continuous(breaks=c(1,seq(10,80,by=10)))+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,by=0.2),labels=scales::percent,expand = c(0.002,0.002))+
  labs(title="Monotypic immunity to the target serotype",x="Age (years)",y="Proportion")+
  theme_bw()+
  theme(plot.title = element_text(size=11),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 8),
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(color = guide_legend(override.aes = list(linetype = 0)))

p_forlegend2 <- ggplot()+
  geom_line(data=subset(sero_age,serotype!="non-serotype"),aes(x=year,y=sero_multi_est,group=location1,color=serotype))+
  geom_ribbon(data=subset(sero_age,serotype!="non-serotype"),aes(x=year,ymin=sero_multi_est_l,ymax=sero_multi_est_u,group=location1,fill=serotype),alpha=0.5)+
  scale_color_manual(name="Reconstructed seroprevalence (B.,D.)",breaks = c("DENV-1","DENV-2","DENV-3","DENV-4"),
                     values = c("#BB3200","#EBA000","#238E8F","#085F8D"))+
  scale_fill_manual(name="Reconstructed seroprevalence (B.,D.)",breaks = c("DENV-1","DENV-2","DENV-3","DENV-4"),
                    values = c("#BB3200","#EBA000","#238E8F","#085F8D"))+
  facet_grid(location1~serotype)+
  scale_x_continuous(breaks=c(seq(2000,2017,by=4)))+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,by=0.2),labels=scales::percent,expand = c(0.002,0.002))+
  labs(title="Multitypic immunity to the target serotype",x="Year",y="Proportion")+
  theme_bw()+
  theme(plot.title = element_text(size=11),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 8),
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##### combine figure #####
leg1 <- get_legend(p_forlegend1)
legend_figure1 <- as_ggplot(leg1)
legend_figure1 <- legend_figure1+theme(legend.margin=margin(c(t=0,r=0,b=0,l=0)))

leg2 <- get_legend(p_forlegend2)
legend_figure2 <- as_ggplot(leg2)
legend_figure2 <- legend_figure2+theme(legend.margin=margin(c(t=0,r=0,b=0,l=0)))

p_sus0_age_1 <- ggarrange(p_sus0_age,labels = c("A"))
pall1 <- ggarrange(legend_figure1,legend_figure2,p_sus0_age_1,ncol=3,widths = c(0.5,0.5,1))
pall3 <- ggarrange(p_immune1st_age,p_sus1st_age,ncol=2,labels = c("B","C"),widths = c(1,1))
pall5 <- ggarrange(p_immune2nd_age,p_sus2nd_age,ncol=2,labels = c("D","E"),widths = c(1,1))
pall <- ggarrange(pall1,pall3,pall5,ncol = 1,heights = c(1.25,2,2))

ggsave("Figure3.png",pall,width = 12,height = 10,bg="white")

#### Figure 4 ####
dataall <- readRDS("infec_history_est_TitreModel.rds")
est_data_combine <- readRDS("titre_distribution_est_TitreModel.rds")

est1 <- dataall[,list(count=.N),by=c("infec_status1","infec_status2","infec_status3","infec_status4")]
est1 <- est1[order(est1[,count],decreasing = TRUE),]

figuredata1 <- melt(dataall,measure.vars = c("DV1","DV2","DV3","DV4","ZIKV"))

dataall[,infec_time:=as.integer(infec_status1!=0)+as.integer(infec_status2!=0)+as.integer(infec_status3!=0)+as.integer(infec_status4!=0) ]
figuredata1[,infec_time:=as.integer(infec_status1!=0)+as.integer(infec_status2!=0)+as.integer(infec_status3!=0)+as.integer(infec_status4!=0) ]

ymax = 2.081

##### data preprocess for figure #####
titre_value_list <- runif(1e3,min(dataall$DV1),max(dataall$DV1))

i=1
titre_value_list1 <- log(titre_value_list[i]-min(dataall$DV1)+1.01)
for (i in 2:length(titre_value_list)){
  titre_value_list1 <- c(titre_value_list1,log(titre_value_list[i]-min(dataall$DV1)+1.01))
}

k=1
if (est_data_combine[k,]$distri=="normal"){
  density1 = dnorm(titre_value_list1,est_data_combine[k,]$mean1,est_data_combine[k,]$sd1)
}else if (est_data_combine[k,]$distri=="gamma"){
  density1 = dgamma(titre_value_list1,shape=est_data_combine[k,]$mean1,rate=est_data_combine[k,]$sd1)
}else if (est_data_combine[k,]$distri=="up gamma"){
  density1 = dgamma((ymax-titre_value_list1),shape=est_data_combine[k,]$mean1,rate=est_data_combine[k,]$sd1)
}

est_data_density <- data.frame(titrevalue=titre_value_list,titrevalue1=titre_value_list1,pdf=density1,
                               prop=est_data_combine[k,]$prop, 
                               type1=est_data_combine[k,]$type1,type2=est_data_combine[k,]$type2,variable=est_data_combine[k,]$variable,
                               infec_status1=est_data_combine[k,]$infec_status1,infec_status2=est_data_combine[k,]$infec_status2,infec_status3=est_data_combine[k,]$infec_status3,infec_status4=est_data_combine[k,]$infec_status4)

for (k in 2:dim(est_data_combine)[1]){
  if (est_data_combine[k,]$distri=="normal"){
    density1 = dnorm(titre_value_list1,est_data_combine[k,]$mean1,est_data_combine[k,]$sd1)
  }else if (est_data_combine[k,]$distri=="gamma"){
    density1 = dgamma(titre_value_list1,shape=est_data_combine[k,]$mean1,rate=est_data_combine[k,]$sd1)
  }else if (est_data_combine[k,]$distri=="up gamma"){
    density1 = dgamma((ymax-titre_value_list1),shape=est_data_combine[k,]$mean1,rate=est_data_combine[k,]$sd1)
  }
  
  est_data_density_this <- data.frame(titrevalue=titre_value_list,titrevalue1=titre_value_list1,pdf=density1,
                                      prop=est_data_combine[k,]$prop, 
                                      type1=est_data_combine[k,]$type1,type2=est_data_combine[k,]$type2,variable=est_data_combine[k,]$variable,
                                      infec_status1=est_data_combine[k,]$infec_status1,infec_status2=est_data_combine[k,]$infec_status2,infec_status3=est_data_combine[k,]$infec_status3,infec_status4=est_data_combine[k,]$infec_status4)
  est_data_density <- rbind(est_data_density,est_data_density_this)
}
est_data_density <- as.data.table(est_data_density)
est_data_density <- est_data_density[order(est_data_density[,titrevalue])]


figuredata1 <- subset(figuredata1,variable!="ZIKV")

set(figuredata1,which(figuredata1[,variable=="DV1"]),"serotype",1)
set(figuredata1,which(figuredata1[,variable=="DV2"]),"serotype",2)
set(figuredata1,which(figuredata1[,variable=="DV3"]),"serotype",3)
set(figuredata1,which(figuredata1[,variable=="DV4"]),"serotype",4)

set(est_data_density,which(est_data_density[,variable=="DV1"]),"serotype",1)
set(est_data_density,which(est_data_density[,variable=="DV2"]),"serotype",2)
set(est_data_density,which(est_data_density[,variable=="DV3"]),"serotype",3)
set(est_data_density,which(est_data_density[,variable=="DV4"]),"serotype",4)

est_data_density[,type3:=paste(type1,type2)]

##### x axis legend for infection history #####
exp_figure <- expand.grid(serotype=c("D1","D2","D3","D4"),exposure_history=c("D0","D1","D2","D3","D4",
                                                                             "D1,D2","D1,D3","D1,D4","D2,D3","D2,D4","D3,D4",
                                                                             "D1,D2,D3","D1,D2,D4","D1,D3,D4","D2,D3,D4",
                                                                             "D1,D2,D3,D4"))
exp_figure <- as.data.table(exp_figure)
for (i in 1:dim(exp_figure)[1]){
  infec_serotype=strsplit(as.character(exp_figure[i,]$exposure_history),split=",")[[1]]
  if (exp_figure[i,]$serotype %in% infec_serotype){
    set(exp_figure,i,"infec_serotype",exp_figure[i,]$serotype)
  }
}
rev(exp_figure$exposure_history)
exp_figure[,rev_exposure_history:=factor(exposure_history,levels = rev(unique(exp_figure$exposure_history)))]
exp_figure[,rev_serotype:=factor(serotype,levels = rev(unique(exp_figure$serotype)))]

theme_bw()$plot.margin
p1 <- ggplot(exp_figure)+
  geom_tile(aes(y=serotype,x=exposure_history,fill=infec_serotype),alpha=0.9,color="white")+
  geom_text(aes(y=serotype,x=exposure_history,label=infec_serotype),color="white")+
  scale_fill_manual(name="",na.value = "grey82",
                    breaks=c("D1","D2","D3","D4"),
                    values = c("black","black","black","black"))+
  #values = c("#00796BFF","#00796BFF","#00796BFF","#00796BFF"))+
  #                   values = c("#BB3200","#EBA000","#238E8F","#085F8D"))+
  ggh4x::facet_grid2(serotype~exposure_history,scales = "free",independent="all")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  theme_bw()+
  theme(
    panel.spacing.y = unit(0, "pt"),
    panel.spacing.x = unit(2, "pt"),
    panel.border = element_rect(color = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    strip.text.x = element_blank(),
    strip.background.y = element_rect(fill = "white",color = "grey"),
    plot.margin = margin(t = 0.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"),
    legend.position = ""
  )

exp_figure_list <- unique(figuredata1[,c(33:36,39)])
exp_figure_list <- exp_figure_list[order(exp_figure_list[,infec_status4])]
exp_figure_list <- exp_figure_list[order(exp_figure_list[,infec_status3])]
exp_figure_list <- exp_figure_list[order(exp_figure_list[,infec_status2])]
exp_figure_list <- exp_figure_list[order(exp_figure_list[,infec_status1])]
exp_figure_list <- exp_figure_list[order(exp_figure_list[,infec_time])]
exp_figure_list[,exposure_history:=c("D0","D1","D2","D3","D4",
                                     "D1,D2","D1,D3","D1,D4","D2,D3","D2,D4","D3,D4",
                                     "D1,D2,D3","D1,D2,D4","D1,D3,D4","D2,D3,D4",
                                     "D1,D2,D3,D4")]
figuredata1 <- merge(figuredata1,exp_figure_list,by=c("infec_status1","infec_status2","infec_status3","infec_status4"))
set(figuredata1,which(figuredata1[,variable=="DV1"]),"serotype1","D1")
set(figuredata1,which(figuredata1[,variable=="DV2"]),"serotype1","D2")
set(figuredata1,which(figuredata1[,variable=="DV3"]),"serotype1","D3")
set(figuredata1,which(figuredata1[,variable=="DV4"]),"serotype1","D4")

est_data_density <- merge(est_data_density,exp_figure_list,by=c("infec_status1","infec_status2","infec_status3","infec_status4"))
set(est_data_density,which(est_data_density[,variable=="DV1"]),"serotype1","D1")
set(est_data_density,which(est_data_density[,variable=="DV2"]),"serotype1","D2")
set(est_data_density,which(est_data_density[,variable=="DV3"]),"serotype1","D3")
set(est_data_density,which(est_data_density[,variable=="DV4"]),"serotype1","D4")

figuredata1 <- as.data.table(figuredata1)
set(figuredata1,NULL,"exposure_history",figuredata1[,factor(exposure_history,levels=c("D0","D1","D2","D3","D4",
                                                                                      "D1,D2","D1,D3","D1,D4","D2,D3","D2,D4","D3,D4",
                                                                                      "D1,D2,D3","D1,D2,D4","D1,D3,D4","D2,D3,D4",
                                                                                      "D1,D2,D3,D4"))])

set(est_data_density,NULL,"exposure_history",est_data_density[,factor(exposure_history,levels=c("D0","D1","D2","D3","D4",
                                                                                                "D1,D2","D1,D3","D1,D4","D2,D3","D2,D4","D3,D4",
                                                                                                "D1,D2,D3","D1,D2,D4","D1,D3,D4","D2,D3,D4",
                                                                                                "D1,D2,D3,D4"))])
##### figure code #####
p_titre <- ggplot()+
  geom_point(data=subset(figuredata1,(infec_status1==2&infec_status2==4&infec_status3==0&infec_status4==0)|(infec_status1==1&infec_status2==2&infec_status3==4&infec_status4==0)),aes(y=value,x=3.5,group=factor(serotype)),size=0.4)+
  geom_violin(data=subset(figuredata1,!((infec_status1==2&infec_status2==4&infec_status3==0&infec_status4==0)|(infec_status1==1&infec_status2==2&infec_status3==4&infec_status4==0))),aes(y=value,x=3.5,group=factor(serotype)),width=2)+
  geom_boxplot(data=subset(figuredata1,!((infec_status1==2&infec_status2==4&infec_status3==0&infec_status4==0)|(infec_status1==1&infec_status2==2&infec_status3==4&infec_status4==0))),aes(y=value,x=3.5,group=factor(serotype)),width=0.3,outlier.size=0.4)+
  geom_path( data=subset(est_data_density,type1=="neg"&type2=="no other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,type1=="neg"&type2=="other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,type1=="infec"&type2=="no other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,type1=="infec"&type2=="other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity" ,linewidth = 0.8)+
  geom_path( data=subset(est_data_density,type1=="cross"&type2=="other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,type1=="cross"&type2=="no other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,type1=="neg"&type2=="no cross neg"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,type1=="neg"&type2=="cross neg"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,infec_time>=2&type1=="infec"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,infec_time>=2&type1=="cross"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,infec_time>=2&type1=="neg"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  ggh4x::facet_grid2(serotype1~exposure_history)+
  scale_x_continuous(limits = c(0,4.8),breaks=seq(0,2.4,by=1),expand=c(0,0))+
  scale_color_manual(breaks = c("infec other","infec no other","infec ",
                                "cross other","cross no other","cross ",
                                "neg cross neg","neg other",
                                "neg no cross neg","neg ",
                                "neg no other"),
                     values = c("#004C3FFF","#00796BFF","#00796BFF",
                                "#631879FF","#8D24AAFF","#8D24AAFF",
                                "#CD92D8FF","#C16622FF",
                                "#F47B00FF","#F47B00FF",
                                "#F47B00FF"))+
  scale_y_continuous(limits = c(3,11),breaks = seq(3,11,by=2))+
  theme_bw()+
  theme(legend.position = "",
        strip.text.x = element_blank(),
        strip.background.y = element_rect(fill = "white",color = "grey"),
        plot.margin = margin(t = 0.5, r = 5.5, b = 0.5, l = 25.5, unit = "pt"),
        panel.spacing.y = unit(0, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length.x = unit(0, "pt"),
        panel.spacing.x = unit(2, "pt"),
        panel.border = element_rect(color = "grey"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'))


figuredata1[,serotype2:=factor(serotype1,levels = c("D4","D3","D2","D1"))]
pheat <- ggplot(figuredata1)+
  geom_tile(aes(x=factor(self_index),y=serotype1,fill=value))+
  scale_fill_gradientn(colours = c("#E3F2FDFF", "#BADEFAFF","#64B4F6FF","#1E87E5FF","#0C46A0FF"),
                       values = scales::rescale(c(3.322,5,5+1.774,5+1.774*2,10.322),
                                                to = c(0, 1), from = c(3.322, 10.322)),
                       breaks = c(3,5,7,9))+
  #scale_fill_material("blue")+
  facet_grid2(serotype1~exposure_history,scales = "free",independent="all")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  #scale_x_discrete(breaks=c("DV1","DV2","DV3","DV4"),labels=c("DENV-1","DENV-2","DENV-3","DENV-4"))+
  labs(fill="titre")+
  theme_bw()+
  theme(legend.position = " ",
        panel.spacing.y = unit(0, "pt"),
        panel.spacing.x = unit(2, "pt"),
        panel.border = element_rect(color = "grey"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        strip.text.x = element_blank(),
        strip.background.y = element_rect(fill = "white",color = "grey"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(t = 5.5, r = 5.5, b = 0.5, l = 5.5, unit = "pt"))


pbox1 <- ggarrange(pheat,p_titre,p1,nrow=3,heights = c(1,4,0.7),align = "v")

ads=0.055
ads_s=0.0029

k1=1:15
pbox1=ggdraw(pbox1) +
  draw_line(x = c(0.049, 0.049), y = c(0.013, 0.99), linewidth = 0.5)

for (k in 1:length(k1)){
  pbox1=pbox1+
    draw_line(x = c(0.049+k1[k]*ads+(k1[k]-1L)*ads_s, 0.049+k1[k]*ads+(k1[k]-1L)*ads_s), y = c(0.013, 0.99), linewidth = 0.3)+
    draw_line(x = c(0.049+k1[k]*ads+k1[k]*ads_s, 0.049+k1[k]*ads+k1[k]*ads_s), y = c(0.013, 0.99), linewidth = 0.3)
}
pbox1=pbox1+
  draw_line(x = c(0.049+16*ads+15*ads_s, 0.049+16*ads+15*ads_s), y = c(0.013, 0.99), linewidth = 0.5)+
  draw_line(x = c(0.049+16*ads+15*ads_s+0.019, 0.049+16*ads+15*ads_s+0.0195), y = c(0.013, 0.99), linewidth = 0.5)

pbox1=pbox1+
  draw_line(x = c(0.049+1*ads+0.5*ads_s, 0.049+1*ads+0.5*ads_s), y = c(0.013, 0.99), linewidth = 0.7)+
  draw_line(x = c(0.049+5*ads+4.5*ads_s, 0.049+5*ads+4.5*ads_s), y = c(0.013, 0.99), linewidth = 0.7)+
  draw_line(x = c(0.049+11*ads+10.5*ads_s, 0.049+11*ads+10.5*ads_s), y = c(0.013, 0.99), linewidth = 0.7)+
  draw_line(x = c(0.049+15*ads+14.5*ads_s, 0.049+15*ads+14.5*ads_s), y = c(0.013, 0.99), linewidth = 0.7)

for (k in 1:length(k1)){
  pbox1=pbox1+
    draw_line(x = c(0.049+k1[k]*ads+k1[k]*ads_s, 0.049+(k1[k]+1L)*ads+k1[k]*ads_s), y = c(0.013, 0.013), linewidth = 0.3)+
    draw_line(x = c(0.049+k1[k]*ads+k1[k]*ads_s, 0.049+(k1[k]+1L)*ads+k1[k]*ads_s), y = c(0.99, 0.99), linewidth = 0.3)
}
pbox1=pbox1+
  draw_line(x = c(0.049, 0.049+1L*ads), y = c(0.013, 0.013), linewidth = 0.3)+
  draw_line(x = c(0.049, 0.049+1L*ads), y = c(0.99, 0.99), linewidth = 0.3)
pbox1=pbox1+
  draw_line(x = c(0.049+16*ads+15*ads_s, 0.049+16*ads+15*ads_s+0.0195), y = c(0.013, 0.013), linewidth = 0.3)+
  draw_line(x = c(0.049+16*ads+15*ads_s, 0.049+16*ads+15*ads_s+0.0195), y = c(0.99, 0.99), linewidth = 0.3)

#c(1,4,0.7)
pbox1=pbox1+
  draw_text("Observed\nindividual titres",y = mean(c(0.814, 0.99)), angle = 90,x = 0.018,size=9.5)+
  draw_text("Titre distributions:\nestimated density and observed pattern",y = mean(c(0.132, 0.814)), angle = 90,x = 0.018,size=9.5)+
  draw_text("Exposure\nprofiles",y = mean(c(0.001, 0.13)), angle = 90,x = 0.018,size=9.5)

pbox2 <- ggarrange(pbox1,NULL,nrow=2,heights = c(1,0.06))
curdis = 0.01
bottom_y=0.04
curve1 <- curveGrob(
  x1 = unit(0.049, "npc"),
  y1 = unit(0.08, "npc"),
  x2 = unit(0.049+curdis, "npc"),
  y2 = unit(bottom_y, "npc"),
  curvature = 0.5
)
curve2 <- curveGrob(
  x1 = unit(0.049-curdis+ads, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+ads, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = 0.5
)
curve3 <- curveGrob(
  x1 = unit(0.049+curdis+ads+ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+ads+ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = -0.5
)
curve4 <- curveGrob(
  x1 = unit(0.049-curdis+5*ads+4*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+5*ads+4.5*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = 0.5
)
curve5 <- curveGrob(
  x1 = unit(0.049+curdis+5L*ads+5L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+5L*ads+5L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = -0.5
)
curve6 <- curveGrob(
  x1 = unit(0.049-curdis+11L*ads+10L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+11L*ads+10L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = 0.5
)
curve7 <- curveGrob(
  x1 = unit(0.049+curdis+11L*ads+11L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+11L*ads+11L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = -0.5
)
curve8 <- curveGrob(
  x1 = unit(0.049-curdis+15L*ads+14L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+15L*ads+14L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = 0.5
)
curve9 <- curveGrob(
  x1 = unit(0.049+curdis+15L*ads+15L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+15L*ads+15L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = -0.5
)
curve10 <- curveGrob(
  x1 = unit(0.049-curdis+16L*ads+15L*ads_s, "npc"),
  y2 = unit(0.08, "npc"),
  x2 = unit(0.049+16L*ads+15L*ads_s, "npc"),
  y1 = unit(bottom_y, "npc"),
  curvature = 0.5
)
pbox2=pbox2+
  draw_line(x = c(0.049+curdis, 0.049-curdis+ads), y = c(bottom_y, bottom_y), linewidth = 0.3)+
  draw_line(x = c(0.049+curdis+ads+ads_s, 0.049-curdis+5L*ads+4L*ads_s), y = c(bottom_y, bottom_y), linewidth = 0.3)+
  draw_line(x = c(0.049+curdis+5L*ads+5L*ads_s, 0.049-curdis+11L*ads+10L*ads_s), y = c(bottom_y, bottom_y), linewidth = 0.3)+
  draw_line(x = c(0.049+curdis+11L*ads+11L*ads_s, 0.049-curdis+15L*ads+14L*ads_s), y = c(bottom_y, bottom_y), linewidth = 0.3)+
  draw_line(x = c(0.049+curdis+15L*ads+15L*ads_s, 0.049-curdis+16L*ads+15L*ads_s), y = c(bottom_y, bottom_y), linewidth = 0.3)


pbox2=ggdraw(pbox2) +
  draw_grob(curve1)+
  draw_grob(curve2)+
  draw_grob(curve3)+
  draw_grob(curve4)+
  draw_grob(curve5)+
  draw_grob(curve6)+
  draw_grob(curve7)+
  draw_grob(curve8)+
  draw_grob(curve9)+
  draw_grob(curve10)

pbox3=pbox2+
  draw_text("Dengue-naïve",x=mean(c(0.049+curdis, 0.049-curdis+ads)),y = 0.025,size=10)+
  draw_text("Primary infection",x=mean(c(0.049+curdis+ads+ads_s, 0.049-curdis+5L*ads+4L*ads_s)),y = 0.025,size=10)+
  draw_text("Secondary infection",x=mean(c(0.049+curdis+5L*ads+5L*ads_s, 0.049-curdis+11L*ads+10L*ads_s)),y = 0.025,size=10)+
  draw_text("Third infection",x=mean(c(0.049+curdis+11L*ads+11L*ads_s, 0.049-curdis+15L*ads+14L*ads_s)),y = 0.025,size=10)+
  draw_text("Fourth infection",x=mean(c(0.049+curdis+15L*ads+15L*ads_s, 0.049-curdis+16L*ads+15L*ads_s)),y = 0.025,size=10)

##### legend #####

pheat_legend <- ggplot(figuredata1)+
  geom_tile(aes(x=factor(self_index),y=serotype1,fill=value))+
  scale_fill_gradientn(colours = c("#E3F2FDFF", "#BADEFAFF","#64B4F6FF","#1E87E5FF","#0C46A0FF"),
                       values = scales::rescale(c(3.322,5,5+1.774,5+1.774*2,10.322),
                                                to = c(0, 1), from = c(3.322, 10.322)),
                       breaks = c(3,5,7,9))+
  facet_grid2(serotype1~exposure_history,scales = "free",independent="all")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  labs(x="",y="Individuals",fill="Titre",title="Observed titres for each individual by estimated infection history")+
  theme_bw()+
  theme(legend.background = element_rect(fill='transparent'),
    legend.position = "bottom",
    panel.spacing.y = unit(0, "pt"),
    panel.spacing.x = unit(2, "pt"),
    panel.border = element_rect(color = "grey"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    legend.spacing.y = unit(0.01, 'cm'),
    legend.text = element_text(size=8),
    legend.title = element_text(size=9),
    legend.key.size = unit(0.5, "cm"),
    strip.text.x = element_blank(),
    strip.background.y = element_rect(fill = "white",color = "grey"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.margin = margin(t = 5.5, r = 5.5, b = 0.5, l = 35.5, unit = "pt"))

legend_heat <- cowplot::get_legend(pheat_legend)
legend_heat <- as_ggplot(legend_heat)
legend_heat <- legend_heat+theme(legend.margin=margin(c(t=0,r=0,b=0,l=0)))

plegend_infec <- ggplot()+
  geom_violin(data=subset(figuredata1,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0),aes(y=value,x=3.5,group=factor(serotype)),width=2)+
  geom_boxplot(data=subset(figuredata1,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0),aes(y=value,x=3.5,group=factor(serotype)),width=0.3,outlier.size=0.4)+
  geom_path( data=subset(est_data_density,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0&type1=="infec"&type2=="no other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0&type1=="infec"&type2=="other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity" ,linewidth = 0.8)+
  facet_wrap(.~variable,scales="free_x",ncol=4)+
  facetted_pos_scales(x=list(
    variable == "DV1" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-1 titre")),
    variable == "DV2" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-2 titre")),
    variable == "DV3" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-3 titre")),
    variable == "DV4" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-4 titre"))
  ))+
  scale_color_manual(breaks = c("infec other","infec no other","infec ",
                                "cross other","cross no other","cross ",
                                "neg cross neg","neg other",
                                "neg no cross neg","neg "),
                     values = c("#004C3FFF","#00796BFF","#00796BFF",
                                "#631879FF","#8D24AAFF","#8D24AAFF",
                                "#CD92D8FF","#C16622FF",
                                "#F47B00FF","#F47B00FF"),
                     name="Estimated titre distribution",
                     labels = c("infecting-serotype positive titres (with additional external boost)","infecting-serotype positive titres","infec ",
                                "cross-reactive positive titres (with additional external boost)","cross-reactive positive titres","cross ",
                                "cross-reactive negative titres","negative titres (with additional external boost)",
                                "negative titres (no external and cross-reactivity boost)","neg "))+
  scale_y_continuous(limits = c(3,11),breaks = seq(3,11,by=2))+
  labs(x="",y="titre",title="Infecting serotypes: DENV-2 (N=42)")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),panel.spacing = unit(0,'lines'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        legend.spacing.y = unit(0.01, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9),
        legend.key.size = unit(0.5, "cm"))

plegend_cross <- ggplot()+
  geom_violin(data=subset(figuredata1,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0),aes(y=value,x=3.5,group=factor(serotype)),width=2)+
  geom_boxplot(data=subset(figuredata1,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0),aes(y=value,x=3.5,group=factor(serotype)),width=0.3,outlier.size=0.4)+
  geom_path( data=subset(est_data_density,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0&type1=="cross"&type2=="other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0&type1=="cross"&type2=="no other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0&type1=="neg"&type2=="cross neg"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  facet_wrap(.~variable,scales="free_x",ncol=4)+
  facetted_pos_scales(x=list(
    variable == "DV1" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-1 titre")),
    variable == "DV2" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-2 titre")),
    variable == "DV3" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-3 titre")),
    variable == "DV4" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-4 titre"))
  ))+
  scale_color_manual(breaks = c("infec other","infec no other","infec ",
                                "cross other","cross no other","cross ",
                                "neg cross neg","neg other",
                                "neg no cross neg","neg "),
                     values = c("#004C3FFF","#00796BFF","#00796BFF",
                                "#631879FF","#8D24AAFF","#8D24AAFF",
                                "#CD92D8FF","#C16622FF",
                                "#F47B00FF","#F47B00FF"),
                     name=" ",
                     labels = c("infecting-serotype positive titres (with additional external boost)","infecting-serotype positive titres","infec ",
                                "cross-reactive positive titres (with additional external boost)","cross-reactive positive titres","cross ",
                                "cross-reactive negative titres","negative titres (with additional external boost)",
                                "negative titres (no external and cross-reactivity boost)","neg "))+
  scale_y_continuous(limits = c(3,11),breaks = seq(3,11,by=2))+
  labs(x="",y="titre",title="Infecting serotypes: DENV-2 (N=42)")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),panel.spacing = unit(0,'lines'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        legend.spacing.y = unit(0.01, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9),
        legend.key.size = unit(0.5, "cm"))

plegend_neg <- ggplot()+
  geom_violin(data=subset(figuredata1,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0),aes(y=value,x=3.5,group=factor(serotype)),width=2)+
  geom_boxplot(data=subset(figuredata1,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0),aes(y=value,x=3.5,group=factor(serotype)),width=0.3,outlier.size=0.4)+
  geom_path( data=subset(est_data_density,infec_status1==2&infec_status2==0&infec_status3==0&infec_status4==0&type1=="neg"&type2=="no cross neg"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  geom_path( data=subset(est_data_density,infec_status1==0&infec_status2==0&infec_status3==0&infec_status4==0&type1=="neg"&type2=="other"),aes(x=pdf*prop,y=titrevalue,group=factor(serotype),color=type3),position="identity",linewidth = 0.8)+
  facet_wrap(.~variable,scales="free_x",ncol=4)+
  facetted_pos_scales(x=list(
    variable == "DV1" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-1 titre")),
    variable == "DV2" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-2 titre")),
    variable == "DV3" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-3 titre")),
    variable == "DV4" ~ scale_x_continuous(limits = c(0,4.8),expand=c(0,0), breaks = c(1,3.5),labels = c("estimated\ntitre distribution","observed\nDENV-4 titre"))
  ))+
  scale_color_manual(breaks = c("infec other","infec no other","infec ",
                                "cross other","cross no other","cross ",
                                "neg cross neg","neg other",
                                "neg no cross neg","neg "),
                     values = c("#004C3FFF","#00796BFF","#00796BFF",
                                "#631879FF","#8D24AAFF","#8D24AAFF",
                                "#CD92D8FF","#C16622FF",
                                "#F47B00FF","#F47B00FF"),
                     name=" ",
                     labels = c("infecting-serotype positive titres (with additional external boost)","infecting-serotype positive titres","infec ",
                                "cross-reactive positive titres (with additional external boost)","cross-reactive positive titres","cross ",
                                "cross-reactive negative titres","negative titres (with additional external boost)",
                                "negative titres (no external and cross-reactivity boost)","neg "))+
  scale_y_continuous(limits = c(3,11),breaks = seq(3,11,by=2))+
  labs(x="",y="titre",title="Infecting serotypes: DENV-2 (N=42)")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),panel.spacing = unit(0,'lines'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        legend.spacing.y = unit(0.01, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9),
        legend.key.size = unit(0.5, "cm"))

legend_infec <- cowplot::get_legend(plegend_infec )
legend_cross <- cowplot::get_legend(plegend_cross )
legend_neg <- cowplot::get_legend(plegend_neg )
legend <- ggarrange(legend_heat,NULL,legend_infec,legend_cross,legend_neg,ncol=5,widths = c(1.2,0.13,2.15,2,2))

##### figure combine #####
pbox4 <- ggarrange(pbox3,legend,ncol=1,heights = c(5.7,0.5))

ggsave("Figure4.png",pbox4,width = 11.8,height = 9,bg="white")

#### Figure 5 ####
##### foi #####
foi_par <- readRDS("simulate_value_foi_highendemic.rds")
figuredata1_1 <- readRDS("simulation_foiest_highendemic_periodic_complete.rds")
figuredata1_2 <- readRDS("simulation_foiest_highendemic_periodic_subsettested.rds")

figuredata1_1[,datatype:="complete data"]
figuredata1_2[,datatype:="subset-tested data"]

figuredata1 <- rbind(figuredata1_1,figuredata1_2)


time_index_data <- data.table(time_group=c(rep("1952-1961",10),rep("1962-1971",10),rep("1972-1981",10),rep("1982-1991",10),rep("1992-2001",10),
                                           rep("2002-2005",4),rep("2006-2009",4),rep("2010-2013",4),rep("2014-2017",4),rep("2018-2021",4)),
                              year=1952:2021,time_index=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),
                                                          rep(6,4),rep(7,4),rep(8,4),rep(9,4),rep(10,4)))

time_index_data1 <- time_index_data[,list(count=.N),by=c("time_group","time_index")]
time_index_data1 <- time_index_data1[,c(1,2)]

foi_par2 <- merge(foi_par[,2:6],time_index_data,by="year")
foi_par3 <- foi_par2[,list(d1=mean(d1),d2=mean(d2),d3=mean(d3),d4=mean(d4)),by="time_group"]
foi_par4 <- merge(time_index_data,foi_par3,by="time_group")
foi_par5 <- melt(foi_par4,id.vars = c("time_group","year","time_index"))
colnames(foi_par5)[4] = "serotype"

foi_par5 <- as.data.table(foi_par5)
set(foi_par5,which(foi_par5[,serotype=="d1"]),"serotype","D1")
set(foi_par5,which(foi_par5[,serotype=="d2"]),"serotype","D2")
set(foi_par5,which(foi_par5[,serotype=="d3"]),"serotype","D3")
set(foi_par5,which(foi_par5[,serotype=="d4"]),"serotype","D4")

foi_par5_tmp <- subset(foi_par5,year==2021)
foi_par5_tmp$year = 2022
foi_par5 <- rbind(foi_par5,foi_par5_tmp)
foi_par5_before <- subset(foi_par5,year<2002)
foi_par5_after <- subset(foi_par5,year>=2002)

figuredata1_before <- subset(figuredata1,year_start<2002)
figuredata1_after <- subset(figuredata1,year_start>=2002)

foi_par5_after$type2 <- "After 2000"
figuredata1_after$type2 <- "After 2000"

foi_par5_before$type2 <- "Before 2000"
figuredata1_before$type2 <- "Before 2000"

foi_par5 <- rbind(foi_par5_before,foi_par5_after)
figuredata1 <- rbind(figuredata1_before,figuredata1_after)

set(foi_par5,NULL,"type2",foi_par5[,factor(type2,levels = c("Before 2000","After 2000"))])
set(figuredata1,NULL,"type2",figuredata1[,factor(type2,levels = c("Before 2000","After 2000"))])

wid1 <- (10/2-3)/3
wid2 <- (2/2-0.4)/3

figuredata1[,year_figure:=year_middle]

figureorder <- data.frame(order1=1:5,type1=c("age below 15","age below 30","age above 15","age above 5","all samples"))
set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[1,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[1,]$type1&type2=="Before 2000"]),year_middle-4.5*wid1])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[1,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[1,]$type1&type2=="Before 2000"]),year_middle-3.5*wid1])

set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[2,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[2,]$type1&type2=="Before 2000"]),year_middle-2.5*wid1])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[2,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[2,]$type1&type2=="Before 2000"]),year_middle-1.5*wid1])

set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[3,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[3,]$type1&type2=="Before 2000"]),year_middle-0.5*wid1])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[3,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[3,]$type1&type2=="Before 2000"]),year_middle+0.5*wid1])

set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[4,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[4,]$type1&type2=="Before 2000"]),year_middle+1.5*wid1])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[4,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[4,]$type1&type2=="Before 2000"]),year_middle+2.5*wid1])

set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[5,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[5,]$type1&type2=="Before 2000"]),year_middle+3.5*wid1])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[5,]$type1&type2=="Before 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[5,]$type1&type2=="Before 2000"]),year_middle+4.5*wid1])




set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[1,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[1,]$type1&type2=="After 2000"]),year_middle-4.5*wid2])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[1,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[1,]$type1&type2=="After 2000"]),year_middle-3.5*wid2])

set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[2,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[2,]$type1&type2=="After 2000"]),year_middle-2.5*wid2])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[2,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[2,]$type1&type2=="After 2000"]),year_middle-1.5*wid2])

set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[3,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[3,]$type1&type2=="After 2000"]),year_middle-0.5*wid2])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[3,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[3,]$type1&type2=="After 2000"]),year_middle+0.5*wid2])

set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[4,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[4,]$type1&type2=="After 2000"]),year_middle+1.5*wid2])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[4,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[4,]$type1&type2=="After 2000"]),year_middle+2.5*wid2])

set(figuredata1,which(figuredata1[,datatype=="complete data"&type1==figureorder[5,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="complete data"&type1==figureorder[5,]$type1&type2=="After 2000"]),year_middle+3.5*wid2])
set(figuredata1,which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[5,]$type1&type2=="After 2000"]),"year_figure",figuredata1[which(figuredata1[,datatype=="subset-tested data"&type1==figureorder[5,]$type1&type2=="After 2000"]),year_middle+4.5*wid2])

set(figuredata1,NULL,"datatype",figuredata1[,factor(datatype,levels = c("complete data","subset-tested data"),
                                                    labels = c("complete data","subset-tested data"))])

pfoi1 <- ggplot(NULL)+
  geom_step(data=subset(foi_par5,type2=="After 2000"),aes(x=year,y=value))+
  facet_wrap(.~serotype,ncol=1,scales="free_y")+
  geom_point(data=subset(figuredata1,type2=="After 2000"),aes(x=year_figure,y=foi_median,color = type1,shape = datatype),size=1.2)+
  geom_errorbar(data=subset(figuredata1,type2=="After 2000"),aes(x=year_figure,ymin=foi_L,ymax=foi_U,color = type1),width=0.05)+
  scale_x_continuous(breaks = c(2004,2008,2012,2016,2019.5),
                     labels = c("71-74","75-78","79-82","83-86","87-90"),
                     expand = c(0.02,0.02))+
  scale_shape_manual(breaks = c("complete data","subset-tested data"),values=c(1,2),name="")+
  scale_color_manual(name="",breaks = c("age below 15","age below 30","age above 15","age above 5","all samples"),
                     values=c("#F39B7FB2","#E64B35B2","#3C5488B2","#4DBBD5B2","#00A087B2"),
                     labels=c(expression("age"<="15"),expression("age"<="30"),expression("age">"15"),expression("age">"5"),expression("all samples")))+
  labs(x="Simulation year",y="Force of infection",title = "Annual FOI after year 70 (estimated with 4-year interval)")+
  theme_bw()+
  theme(legend.position = c(0.35,0.7),
        plot.title = element_text(size=9),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        legend.text.align = 0,
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(0.3, "cm"),
        legend.direction = "vertical", legend.box = "horizontal",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'))


pfoi2 <- ggplot(NULL)+
  geom_step(data=subset(foi_par5,type2=="Before 2000"),aes(x=year,y=value))+
  facet_wrap(.~serotype,ncol=1,scales="free_y")+
  geom_point(data=subset(figuredata1,type2=="Before 2000"),aes(x=year_figure,y=foi_median,color = type1,shape = datatype),size=1.2)+
  geom_errorbar(data=subset(figuredata1,type2=="Before 2000"),aes(x=year_figure,ymin=foi_L,ymax=foi_U,color = type1),width=0.05)+
  scale_x_continuous(breaks = c(1956,1968,1978,1987.5,1997.5),
                     labels = c("1-30","31-40","41-50","51-60","61-70"),
                     expand = c(0.02,0.02))+
  scale_shape_manual(breaks = c("complete data","subset-tested data"),values=c(1,2),name="")+
  scale_color_manual(name="",breaks = c("age below 15","age below 30","age above 15","age above 5","all samples"),
                     #values=RColorBrewer::brewer.pal(5, "Set1"))+
                     values=c("#F39B7FB2","#E64B35B2","#3C5488B2","#4DBBD5B2","#00A087B2"),
                     labels=c(expression("age"<="15"),expression("age"<="30"),expression("age">"15"),expression("age">"5"),expression("all samples")))+
  labs(x="Simulation year",y="Force of infection",title = "Annual FOI before year 70 (estimated with 10-year interval)")+
  theme_bw()+
  theme(legend.position = "",
        plot.title = element_text(size=9),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        legend.text.align = 0,
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=8),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'))

##### cross #####
par_value_data <- readRDS("simulate_value_cross_reactivity.rds")

cross_result1_1 <- readRDS("simulation_crossest_highendemic_periodic_complete.rds")
cross_result1_2 <- readRDS("simulation_crossest_highendemic_periodic_subsettested.rds")

cross_result1_1 <- as.data.table(cross_result1_1)
cross_result1_2 <- as.data.table(cross_result1_2)

cross_result1_1[,datatype:="complete data"]
cross_result1_2[,datatype:="subset-tested data"]
cross_result2 <- rbind(cross_result1_1,cross_result1_2)
cross_result2 <- as.data.table(cross_result2)

colnames(par_value_data)[1] <- "beta_est"
colnames(par_value_data)[2] <- "infected_virus1"
colnames(par_value_data)[3] <- "cross_virus1"
par_value_data[,beta_CI_L:=NA]
par_value_data[,beta_CI_U:=NA]

par_value_data1 <- as.data.frame(par_value_data)
par_value_data2 <- as.data.frame(par_value_data)
par_value_data1$datatype = "complete data"
par_value_data2$datatype = "subset-tested data"

figuredata_cross <- rbind(par_value_data1,par_value_data2,cross_result2[,-6])
figuredata_cross <- as.data.table(figuredata_cross)
figuredata_cross[,infected_virus2:=factor(infected_virus1,levels = c("D1","D2","D3","D4","D1,D2","D1,D3","D1,D4","D2,D3","D2,D4","D3,D4",
                                                                     "D1,D2,D3","D1,D2,D4","D1,D3,D4","D2,D3,D4"))]
figuredata_cross[,type1_order:=factor(type1,levels = c("simulated value",figureorder$type1))]

set(figuredata_cross,NULL,"datatype",figuredata_cross[,factor(datatype,levels = c("complete data","subset-tested data"),
                                                              labels = c("complete data","subset-tested data"))])


pcross1 <- ggplot()+
  geom_point(data=subset(figuredata_cross,infec_time!=2),aes(x=cross_virus1,y=beta_est,color=type1_order),position = position_dodge(width=0.6),size=1.2)+
  geom_errorbar(data=subset(figuredata_cross,infec_time!=2),aes(x=cross_virus1,ymin=beta_CI_L,ymax=beta_CI_U,color=type1_order),position = position_dodge(width=0.6),width=0.1)+
  facet_grid(datatype~infected_virus2,scales="free_x")+
  force_panelsizes(cols = c(3,3,3,3,1,1,1,1))+
  labs(x="Heterotypic serotype",y=" The probability of cross reactivity")+
  scale_y_continuous(breaks = seq(0,1,by=0.2),limits = c(0,1))+
  scale_color_manual(name="",breaks = c("simulated value","age below 15","age below 30","age above 15","age above 5","all samples"),
                     values=c("black","#F39B7FB2","#E64B35B2","#3C5488B2","#4DBBD5B2","#00A087B2"),
                     labels=c(expression("simulated value"),expression("age"<="15"),expression("age"<="30"),expression("age">"15"),expression("age">"5"),expression("all samples")))+
  theme_bw()+
  theme(legend.position = "",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=8),
        legend.text.align = 0,
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.spacing = unit(0,'lines'),
        legend.background = element_rect(fill='transparent'))

pcross2 <- ggplot()+
  geom_point(data=subset(figuredata_cross,infec_time==2),aes(x=cross_virus1,y=beta_est,color=type1_order),position = position_dodge(width=0.6),size=1.2)+
  geom_errorbar(data=subset(figuredata_cross,infec_time==2),aes(x=cross_virus1,ymin=beta_CI_L,ymax=beta_CI_U,color=type1_order),position = position_dodge(width=0.6),width=0.1)+
  facet_grid(datatype~infected_virus2,scales="free_x")+
  labs(x="Heterotypic serotype",y=" The probability of cross reactivity")+
  scale_y_continuous(breaks = seq(0,1,by=0.2),limits = c(0,1))+
  scale_color_manual(name="",breaks = c("simulated value","age below 15","age below 30","age above 15","age above 5","all samples"),
                     values=c("black","#F39B7FB2","#E64B35B2","#3C5488B2","#4DBBD5B2","#00A087B2"),
                     labels=c(expression("simulated value"),expression("age"<="15"),expression("age"<="30"),expression("age">"15"),expression("age">"5"),expression("all samples")))+
  theme_bw()+
  theme(plot.title = element_text(size=10),
    axis.text.x = element_text(size=7),
    axis.text.y = element_text(size=7),
    axis.title.y = element_text(size=8),
    axis.title.x = element_text(size=8),
    strip.background = element_rect(fill = "white"),
    legend.spacing.y = unit(0.01, 'cm'),
    legend.text.align = 0,
    strip.text = element_text(size = 8),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.5, "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.spacing = unit(0,'lines'),
    legend.background = element_rect(fill='transparent'))

##### prediction effect #####
set(figuredata1,which(figuredata1[,time_group=="before 1961"]),"time_group","1952-1961")

foi_par6 <- foi_par5[,list(value=unique(value)),by=c("serotype","time_group")]
foi_effect <- merge(figuredata1,foi_par6,by=c("serotype","time_group"))
value_foi <- foi_effect[,list(mse=mean((foi_median-value)^2),
                              cover_rate=mean( foi_L<=value&foi_U>=value )),by=c("type1","datatype")]

figuredata_cross_est <- subset(figuredata_cross,!is.na(beta_CI_L))
figuredata_cross_simu <- subset(figuredata_cross,is.na(beta_CI_L))
colnames(figuredata_cross_simu)
figuredata_cross_simu <- figuredata_cross_simu[,list(value=unique(beta_est)),by=c("infected_virus1", "cross_virus1")]
cross_effect <- merge(figuredata_cross_est,figuredata_cross_simu,by=c("infected_virus1", "cross_virus1"))

value_cross <- cross_effect[,list(mse=mean((beta_est-value)^2),cover_rate=mean( value>=beta_CI_L&value<=beta_CI_U )),by=c("type1","datatype")]

value_cross[,type2:="Cross reactivity"]
value_foi[,type2:="Force of infection"]

value_cross[,ystart:=0]
value_foi[,ystart:=0]

value_combine <- rbind(value_foi,value_cross)
set(value_combine,NULL,"type1",value_combine[,factor(type1,levels = figureorder$type1)])
set(value_cross,NULL,"type1",value_cross[,factor(type1,levels = figureorder$type1)])
set(value_foi,NULL,"type1",value_foi[,factor(type1,levels = figureorder$type1)])

peffect1 <- ggplot(value_foi)+
  geom_point(aes(x=type1,y=mse,color=type1,size=3))+
  geom_segment(aes(x=type1,xend=type1,y=ystart,yend=mse,color=type1))+
  geom_text(data=subset(value_foi,type2=="Force of infection"),aes(x=type1,y=mse+5e-5,label=paste0("coverage rate:\n",round(cover_rate,2)*100,"%")),size=2)+
  scale_color_manual(name="",breaks = c("age below 15","age below 30","age above 15","age above 5","all samples"),
                     values=c("#F39B7FB2","#E64B35B2","#3C5488B2","#4DBBD5B2","#00A087B2"))+
  facet_grid(.~datatype)+
  scale_y_continuous(limits=c(0,max(value_foi$mse)*1.3),
                     breaks=seq(0,0.0004,by=0.00005),
                     expand = c(0,0))+
  scale_x_discrete(breaks=c("age below 15","age below 30","age above 15","age above 5","all samples"),
                   labels=c(expression("age"<="15"),expression("age"<="30"),expression("age">"15"),expression("age">"5"),expression("all samples")))+
  labs(x="",y="Mean squared error",title="Force of infection")+
  theme_bw()+
  theme(plot.title = element_text(size=10),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=8),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.spacing = unit(0,'lines'),
        legend.background = element_rect(fill='transparent'),
        legend.position = "")

peffect2 <- ggplot(value_cross)+
  geom_point(aes(x=type1,y=mse,color=type1,size=3))+
  geom_segment(aes(x=type1,xend=type1,y=ystart,yend=mse,color=type1))+
  geom_text(data=subset(value_cross,type2=="Cross reactivity"),aes(x=type1,y=mse+6e-3,label=paste0("coverage rate:\n",round(cover_rate,2)*100,"%")),size=2)+
  scale_color_manual(name="",breaks = c("age below 15","age below 30","age above 15","age above 5","all samples"),
                     values=c("#F39B7FB2","#E64B35B2","#3C5488B2","#4DBBD5B2","#00A087B2"))+
  facet_grid(.~datatype)+
  scale_y_continuous(limits=c(0,max(value_cross$mse)*1.5),expand = c(0,0))+
  scale_x_discrete(breaks=c("age below 15","age below 30","age above 15","age above 5","all samples"),
                   labels=c(expression("age"<="15"),expression("age"<="30"),expression("age">"15"),expression("age">"5"),expression("all samples")))+
  labs(x="",y="Mean squared error",title="Cross reactivity")+
  theme_bw()+
  theme(plot.title = element_text(size=10),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        strip.background = element_rect(fill = "white"),
        legend.spacing.y = unit(0.01, 'cm'),
        strip.text = element_text(size = 8),
        legend.text = element_text(size=8),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.spacing = unit(0,'lines'),
        legend.background = element_rect(fill='transparent'),
        legend.position = "")

##### figure combine #####
p_foi <- ggarrange(pfoi2,pfoi1,ncol=2,widths = c(1,2.1))
p_highendemic_foi1 <- annotate_figure(p_foi,top = text_grob("High-endemic scenario: FOI estimated values (colored) and simulated values (black)", 
                                                           size = 11,hjust = 0, vjust = 0.4, x = 0.04))

p_cross <- ggarrange(pcross1,pcross2,ncol=1)
p_highendemic_cross1 <- annotate_figure(p_cross,top = text_grob("The estimated (colored) and simulated (black) cross-reactive pattern across different infection histories", 
                                                               size = 11,hjust = 0, vjust = 0.4, x = 0.035))


p_effect <- ggarrange(peffect1,peffect2,ncol=2)
p_highendemic_effect1 <- annotate_figure(p_effect,top = text_grob("Mean squared error and 95% confidence interval coverage rate of FOI parameters and cross reactivity parameters", 
                                                                 size = 11,hjust = 0, vjust = 0.4, x = 0.053))

pall <- ggarrange(p_highendemic_foi1,p_highendemic_cross1,p_highendemic_effect1,ncol=1,labels = c("A","B","C"),heights = c(3,2.3,1.2))

ggsave("Figure5.png",pall,width = 12,height = 16,bg="white")



