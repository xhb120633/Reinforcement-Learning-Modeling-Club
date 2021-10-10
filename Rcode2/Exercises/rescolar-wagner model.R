## clear the console and memory
rm(list = ls())
graphics.off()
cat("\014")

##
library(ggplot2)
library(ggpubr)

## the theme for plotting 
pic_theme <- theme(axis.title = element_text(face="bold", size = 15),
                   axis.text = element_text(face="bold", size = 13),
                   legend.title = element_text(face="bold", size = 15),
                   legend.text = element_text(face="bold", size = 15),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1),
                   axis.ticks = element_line(size = 1),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   text = element_text(family = "sans"),
                   legend.position = c(0.10, 0.93))


#define a softmax function which we use to calculate probability
softmax<-function(v){
  i=length(v)
  p<-rep(0,i)
  for(d in 1:i){
    p[d]=exp(v[d])/sum(exp(v))
  }
  return(p)
}
#define a reproduciable ramdom seed
set.seed(2020)
ntrials=100
p_reward1=c(0.8,0.2)
p_reward2=c(0.2,0.8)
p_reward=rbind(p_reward1,p_reward2)
p_reward=as.data.frame(p_reward)
choice=rep(0,ntrials)
accumulated_reward1=rep(0,ntrials)
v=c(0,0)
v_list=matrix(rep(0,200),ncol=2,nro=100)
lr=0.1
for(t in 1:ntrials){
  v_list[t,]=v
  p_choose=softmax(v)
  choice[t]=sample(c(1,2), size = 1, replace = T, prob = p_choose)
  tmp_reward=sample(c(1,-1), size = 1, replace = T, prob = p_reward[choice[t],])
  if(t==1){
    accumulated_reward1[t]=tmp_reward
  }else{
  accumulated_reward1[t]=tmp_reward+accumulated_reward1[t-1]
  }
  ######fill your code here
  v[choice[t]]=...
}
choice=as.data.frame(choice)
accumulated_reward1=as.data.frame(accumulated_reward1)

## plotting 1
pic_choice_trials1 <-
  ggplot(data = choice, aes(x = c(1: ntrials), y = choice)) +
  geom_point(colour = "#CC79A7", size = 1.5, alpha = 1) +
  geom_step(size = 0.25, alpha = 0.4, linetype = 1) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(1, 2), breaks = c(1, 2)) +
  labs(x = "Number of Trials", y = "Choice") +
  pic_theme

pic_accre_trials1 <- 
  ggplot(data = accumulated_reward1, aes(x = c(1: ntrials), y = accumulated_reward1)) + 
  geom_line(size = 0.75, linetype = 1, colour = "#CC79A7") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(x = "Number of Trials", y = "Accumulated Reward") +
  pic_theme

RW_sim1 <- ggarrange(pic_choice_trials1, pic_accre_trials1, ncol = 2, nrow = 1, labels = c("A", "B"),
                             widths = c(1.25, 1), heights = 1)

ggsave(filename = "./pic/RW_sim1.png", plot = RW_sim1, 
       width = 11, height = 5.5)

##
v_list1<-data.frame(v_list[,1],rep(1,100))
colnames(v_list1)<-c('value','choice')
v_list2<-data.frame(v_list[,2],rep(2,100))
colnames(v_list2)<-c('value','choice')
v_list<-rbind(v_list1,v_list2)
v_list$choice<-as.factor(v_list$choice)
v_list<-data.frame(v_list,rep(c(1:100),2))
colnames(v_list)[3]=c('trial')
v_plot <- 
  ggplot(v_list, aes(x = trial, y = value, colour = choice)) + 
  geom_line(size = 1) +
  labs(x = "Number of Trials", y = "Value", colour = "Choice") +
  scale_colour_manual(values = c("#f1a340", "#998ec3"), breaks = c(1, 2)) +
  geom_hline(yintercept = p_reward1[1] - p_reward1[2], size = 0.75, colour = "#f1a340", alpha = 0.5) + 
  geom_hline(yintercept = p_reward2[1] - p_reward2[2], size = 0.75, colour = "#998ec3", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  pic_theme

ggsave(filename = "./pic/RW_v_plot.png", plot = v_plot, 
       width = 9, height = 5.5)
