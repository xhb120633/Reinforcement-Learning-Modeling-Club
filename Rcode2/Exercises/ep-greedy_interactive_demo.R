library(shiny)
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
set.seed(2020)
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
ui <- fluidPage(
  
  # App title ----
  titlePanel("Epsilon-greedy Simulation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = c("lr"),
                  label = c("lr "),
                  min = c(0),
                  max = c(1),
                  value = c(0.05)),
      
      sliderInput(inputId = c("ep"),
                  label = c("epsilon "),
                  min = c(0),
                  max = c(1),
                  value = c(0.05))
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel("main panel",
              fluidRow(
                splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"), plotOutput("plotgraph3"))
              )
    )
  )
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  p1<-reactive({
  set.seed(2019)
  ntrials=400
  mu<-rnorm(4,mean=50,sd=20)
  sd=3
  choice=rep(0,ntrials)
  accumulated_reward1=rep(0,ntrials)
  v=c(0,0,0,0)
  v_list=matrix(rep(0,4*ntrials),ncol=4,nro=ntrials)
  lr=input$lr
  ep=input$ep
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
  pic_choice_trials1 <-
    ggplot(data = choice, aes(x = choice)) +
    geom_bar(width = .55, position = position_dodge(width = 0.6), fill = "#fdae6b") +
    scale_x_continuous(breaks = c(1, 2, 3, 4)) +
    labs(x = "Number of Trials", y = "Counting") +
    pic_theme
  return(pic_choice_trials1)
  })
  
  p2<-reactive({
    set.seed(2019)
    ntrials=400
    mu<-rnorm(4,mean=50,sd=20)
    sd=3
    choice=rep(0,ntrials)
    accumulated_reward1=rep(0,ntrials)
    v=c(0,0,0,0)
    v_list=matrix(rep(0,4*ntrials),ncol=4,nro=ntrials)
    lr=input$lr
    ep=input$ep
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
    pic_accre_trials1 <- 
      ggplot(data = accumulated_reward1, aes(x = c(1: ntrials), y = accumulated_reward1)) + 
      geom_line(size = 1, linetype = 1, colour = "#fdae6b") +
      scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
      labs(x = "Number of Trials", y = "Accumulated Reward") +
      pic_theme
    return(pic_accre_trials1)
  })
  
  p3<-reactive({
    set.seed(2019)
    ntrials=400
    mu<-rnorm(4,mean=50,sd=20)
    sd=3
    choice=rep(0,ntrials)
    accumulated_reward1=rep(0,ntrials)
    v=c(0,0,0,0)
    v_list=matrix(rep(0,4*ntrials),ncol=4,nro=ntrials)
    lr=input$lr
    ep=input$ep
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
    return(v_plot)
  })
  output$plotgraph1 <- renderPlot({p1()})
  output$plotgraph2 <- renderPlot({p2()})
  output$plotgraph3 <- renderPlot({p3()})
  
  
}
shinyApp(ui = ui, server = server)