#Resolar-wagner model
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
                   legend.position = c(0.95, 0.93))

#define a softmax function which we use to calculate probability
ep_greedy<-function(ep,v){
  i=length(v)
  p<-rep(0,i)
  for(d in 1:i){
    if(d==which(v==max(v))){
      p[d]=1-ep
    }else{
      p[d]=ep/i
    }
  }
  return(p)
}
#define a reproduciable ramdom seed
set.seed(2019)
ntrials=400
mu<-rnorm(4,mean=50,sd=20)
sd=3
choice=rep(0,ntrials)
accumulated_reward1=rep(0,ntrials)
v=c(0,0,0,0)
v_list=matrix(rep(0,4*ntrials),ncol=4,nro=ntrials)
lr=0.2
ep=0.4
for(t in 1:ntrials){
  v_list[t,]=v
  p_choose=ep_greedy(ep,v)
  choice[t]=sample(c(1:4), size = 1, replace = T, prob = p_choose)
  tmp_reward=rnorm(1,mean=mu[choice[t]],sd=sd)
  if(t==1){
    accumulated_reward1[t]=tmp_reward
  }else{
    accumulated_reward1[t]=tmp_reward+accumulated_reward1[t-1]
  }
 v[choice[t]]=v[choice[t]]+lr*(tmp_reward-v[choice[t]])
}
choice=as.data.frame(choice)
accumulated_reward1=as.data.frame(accumulated_reward1)
## plotting 1
pic_choice_trials1 <-
  ggplot(data = choice, aes(x = choice)) +
  geom_bar(width = .55, position = position_dodge(width = 0.6), fill = "#fdae6b") +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  labs(x = "Number of Trials", y = "Counting") +
  pic_theme

pic_accre_trials1 <- 
  ggplot(data = accumulated_reward1, aes(x = c(1: ntrials), y = accumulated_reward1)) + 
  geom_line(size = 1, linetype = 1, colour = "#fdae6b") +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  labs(x = "Number of Trials", y = "Accumulated Reward") +
  pic_theme
ep_greedy_sim1 <- ggarrange(pic_choice_trials1, pic_accre_trials1, ncol = 2, nrow = 1, labels = c("A", "B"),
                          widths = c(1, 1), heights = 1)
ggsave(filename = "./pic/Softmax_sim1.png", plot = Softmax_sim1, 
       width = 11, height = 5.5)

v_list1<-data.frame(v_list[,1],rep(1,ntrials))
colnames(v_list1)<-c('value','choice')
v_list2<-data.frame(v_list[,2],rep(2,ntrials))
colnames(v_list2)<-c('value','choice')
v_list3<-data.frame(v_list[,3],rep(3,ntrials))
colnames(v_list3)<-c('value','choice')
v_list4<-data.frame(v_list[,4],rep(4,ntrials))
colnames(v_list4)<-c('value','choice')
v_list<-rbind(v_list1,v_list2,v_list3,v_list4)
v_list$choice<-as.factor(v_list$choice)
v_list<-data.frame(v_list,rep(c(1:ntrials),4))
colnames(v_list)[3]=c('trial')
v_plot <- 
  ggplot(v_list, aes(x = trial, y = value, colour = choice)) + 
  geom_line(size = 1) +
  labs(x = "Number of Trials", y = "Value", colour = "Choice") +
  scale_colour_manual(values = c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6"), breaks = c(1, 2, 3, 4)) +
  geom_hline(yintercept = mu[1], size = 0.75, colour = "#d7191c", alpha = 0.5) + 
  geom_hline(yintercept = mu[2], size = 0.75, colour = "#fdae61", alpha = 0.5) +
  geom_hline(yintercept = mu[3], size = 0.75, colour = "#abd9e9", alpha = 0.5) +
  geom_hline(yintercept = mu[4], size = 0.75, colour = "#2c7bb6", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  pic_theme

ggsave(filename = "./pic/Softmax_v_plot.png", plot = v_plot, 
       width = 9, height = 5.5)