#Visualizations
str(Telecom_Winsor)
library(ggplot2)
#Univariate Analysis
a=ggplot(Telecom_Winsor, aes(x=mou_Mean, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - MOU_Mean Vs Churn")+
  theme_bw()+facet_grid(~churn)+theme(plot.title = element_text(size = 10, hjust = 0.5))
a=aggregate(Telecom_Winsor$mou_Mean, by=list(Telecom_Winsor$churn) , FUN = summary)

b=data.frame(cbind(Churn=as.numeric(a[,1])-1,a$x[,c(3,4)]))
str(b)
library(reshape)
melt(b,id.vars = b$Churn, variable_name = "Stats")
c=ggplot(melt(b,id.vars = b$Churn),aes(x=factor(Churn), y=value, fill=variable))
c+geom_bar(stat = "identity",position = "dodge")+coord_flip()+
  labs(x="Churn Status", y="Value",title="Avg.Usage in mins", fill="Stat")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.key.size = unit(0.15,"cm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.position = c(0.3,0.5),
        legend.background = element_rect(fill="grey"),
        legend.direction = "horizontal")+theme_bw()

a=ggplot(Telecom_Winsor, aes(x=totmrc_Mean, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - totmrc_Mean Vs Churn")+
  theme_bw()+facet_grid(~churn)+theme(plot.title = element_text(size = 10, hjust = 0.5))

a=aggregate(Telecom_Winsor$totmrc_Mean, by=list(Telecom_Winsor$churn) , FUN = summary)

b=data.frame(cbind(Churn=as.numeric(a[,1])-1,a$x[,c(3,4)]))
str(b)
c=ggplot(melt(b,id.vars = b$Churn),aes(x=factor(Churn), y=value, fill=variable))
c+geom_bar(stat = "identity",position = "dodge")+coord_flip()+
  labs(x="Churn Status", y="Value",title="Avg.Usage in mins", fill="Stat")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.key.size = unit(0.15,"cm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.position = c(0.3,0.5),
        legend.background = element_rect(fill="grey"),
        legend.direction = "horizontal")

str(Telecom_Winsor)
a=ggplot(Telecom_Winsor, aes(x=rev_Range, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 30)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - rev_Range Vs Churn")+
  theme_bw()+facet_grid(~churn)+theme(plot.title = element_text(size = 10, hjust = 0.5))

a=ggplot(Telecom_Winsor, aes(x=drop_blk_Mean, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 30)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - Drop_blk_Mean Vs Churn")+
  theme_bw()+facet_grid(~churn)+theme(plot.title = element_text(size = 10, hjust = 0.5))

a=ggplot(Telecom_Winsor, aes(x=callwait_Mean, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 30)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - callwait_Mean Vs Churn")+
  theme_bw()+facet_grid(~churn)+theme(plot.title = element_text(size = 10, hjust = 0.5))

a=ggplot(Telecom_Winsor,aes(x=F_eqpdays), alpha=0.5)
a+geom_bar(stat = "count", aes(fill=models), position = "dodge")+
  facet_grid(~churn)+labs(x="", fill="Models",y="Count", title="F_eqpdays Impact Churn?")+
  theme(legend.position = c(0.8,0.8), plot.title = element_text(size = 10, hjust = 0.5),
        legend.key.size = unit(0.5,"cm"), legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), axis.text.x = element_text( angle=45,size = 8, vjust = 1, hjust = 1),
        legend.direction = "horizontal")
str(Telecom_Winsor)
