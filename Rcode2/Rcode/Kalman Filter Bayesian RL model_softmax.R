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

#define softmax function
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
ntrials=400
#define a random-walk fout bandits
mu_matrix<-matrix(rep(0,4*ntrials),nrow=4,ncol=ntrials)
mu_matrix[,1]=c(20,20,-20,-20)
for (t in 2:ntrials){
  mu_matrix[,t]=mu_matrix[,t-1]+rnorm(4,mean=0,sd=4)
}
choice=rep(0,ntrials)
accumulated_reward1=rep(0,ntrials)
s_mean<-c(0,0,0,0)
s_sd<-c(1,1,1,1)
s_mean_list=matrix(rep(4*ntrials),nrow=ntrials,ncol=4)
s_sd_list=matrix(rep(4*ntrials),nrow=ntrials,ncol=4)
sigma_error=2
sigma_t=2
for(t in 1:ntrials){
  s_mean_list[t,]=s_mean
  s_sd_list[t,]=s_sd
  p_choose=softmax(s_mean)
  choice[t]=sample(c(1:4), size = 1, replace = T, prob = p_choose)
  tmp_reward=mu_matrix[choice[t],t]+rnorm(1,mean=0,sd=4)
  if(t==1){
    accumulated_reward1[t]=tmp_reward
  }else{
    accumulated_reward1[t]=tmp_reward+accumulated_reward1[t-1]
  }

kt=rep(0,4)
kt[choice[t]]=(s_sd[choice[t]]+sigma_t**2)/(s_sd[choice[t]]+sigma_t**2+sigma_error**2)
s_mean=s_mean+kt*(tmp_reward-s_mean)  
s_sd=(1-kt)*(s_sd+sigma_t**2)
}

choice=as.data.frame(choice)
accumulated_reward1=as.data.frame(accumulated_reward1)

## plotting 1
pic_choice_trials1 <-
  ggplot(data = choice, aes(x = choice)) +
  geom_bar(width = .55, position = position_dodge(width = 0.6), fill = "#fdae6b") +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  labs(x = "Number of Trials", y = "Counting") +
  pic_theme

pic_accre_trials1 <- 
  ggplot(data = accumulated_reward1, aes(x = c(1: ntrials), y = accumulated_reward1)) + 
  geom_line(size = 1, linetype = 1, colour = "#fdae6b") +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  labs(x = "Number of Trials", y = "Accumulated Reward") +
  pic_theme

Softmax_sim1 <- ggarrange(pic_choice_trials1, pic_accre_trials1, ncol = 2, nrow = 1, labels = c("A", "B"),
                      widths = c(1, 1), heights = 1)

ggsave(filename = "./pic/Softmax_sim1.png", plot = Softmax_sim1, 
       width = 11, height = 5.5)

mu_list1<-data.frame(mu_matrix[1,],rep(1,ntrials))
colnames(mu_list1)<-c('mean','choice')
mu_list2<-data.frame(mu_matrix[2,],rep(2,ntrials))
colnames(mu_list2)<-c('mean','choice')
mu_list3<-data.frame(mu_matrix[3,],rep(3,ntrials))
colnames(mu_list3)<-c('mean','choice')
mu_list4<-data.frame(mu_matrix[4,],rep(4,ntrials))
colnames(mu_list4)<-c('mean','choice')
mu_list<-rbind(mu_list1,mu_list2,mu_list3,mu_list4)
mu_list$choice<-as.factor(mu_list$choice)
mu_list<-data.frame(mu_list,rep(c(1:ntrials),4))
colnames(mu_list)[3]=c('trial')

mu_plot <-
  ggplot(mu_list, aes(x = trial, y = mean, colour = choice)) + 
  geom_line(size = 0.75, alpha = 0.7) +
  labs(x = "Number of Trials", y = "Mean", colour = "Choice") +
  scale_colour_manual(values = c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6"), breaks = c(1, 2, 3, 4)) +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) +
  pic_theme +
  theme(legend.position = "top")

ggsave(filename = "./pic/Softmax_mu_plot.png", plot = mu_plot, 
       width = 9, height = 5.5)

v_list1<-data.frame(s_mean_list[,1],rep(1,ntrials))
colnames(v_list1)<-c('value','choice')
v_list2<-data.frame(s_mean_list[,2],rep(2,ntrials))
colnames(v_list2)<-c('value','choice')
v_list3<-data.frame(s_mean_list[,3],rep(3,ntrials))
colnames(v_list3)<-c('value','choice')
v_list4<-data.frame(s_mean_list[,4],rep(4,ntrials))
colnames(v_list4)<-c('value','choice')
v_list<-rbind(v_list1,v_list2,v_list3,v_list4)
v_list$choice<-as.factor(v_list$choice)
v_list<-data.frame(v_list,rep(c(1:ntrials),4))
colnames(v_list)[3]=c('trial')

v_plot <- 
  ggplot(v_list, aes(x = trial, y = value, colour = choice)) + 
  geom_line(size = 0.75, alpha = 0.7) +
  labs(x = "Number of Trials", y = "Value", colour = "Choice") +
  scale_colour_manual(values = c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6"), breaks = c(1, 2, 3, 4)) +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) +
  pic_theme +
  theme(legend.position = "top")

ggsave(filename = "./pic/Softmax_v_plot.png", plot = v_plot, 
       width = 9, height = 5.5)
