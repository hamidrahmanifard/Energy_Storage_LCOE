#install.packages("magrittr") # only needed the first time you use it
#install.packages("`scales`")    # alternative installation of the %>%
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)    # alternative, this also loads %>%
library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)

# Input data ==================================================================================================================

setwd("D:\\OneDrive - University of Calgary\\CERI\\Electricity storage\\Model\\Learning curves")


# read the .csv file conventionally
data_file <- "D:\\OneDrive - University of Calgary\\CERI\\Electricity storage\\Model\\Learning curves\\ESS_data.xlsx"
ESS_df <- read_xlsx(data_file) 

data_file1 <- "D:\\OneDrive - University of Calgary\\CERI\\Electricity storage\\Model\\Learning curves\\costred_norm.xlsx"
costred_norm <- read_xlsx(data_file1) 


data_file2 <- "D:\\OneDrive - University of Calgary\\CERI\\Electricity storage\\Model\\Learning curves\\costred_low.xlsx"
costred_low <- read_xlsx(data_file2) 


data_file3 <- "D:\\OneDrive - University of Calgary\\CERI\\Electricity storage\\Model\\Learning curves\\costred_high.xlsx"
costred_high <- read_xlsx(data_file3) 


#LCOS & overnight cost calculations ============================================================================================

Cap_Fac <- function(a,b) {
  Cap_Fact <- a*(1+a)^b/((1+a)^b-1)
  return(Cap_Fact)
}

p_elect = 0.0511866034306196
p_gas = 2.86
Conv_factor = 0.0000010551 # Fuel Energy from Btu/kWh to Gj/kWh


#Cap_Fac(0.12,60)

ESS_df <- ESS_df  %>%
  mutate(CCF= Cap_Fac(d,n))%>%

  mutate(Fuel_Cost= p_gas*Fuel_Energy*Conv_factor)%>%

  mutate(Pelec_L= p_elect*(1/Eff_L-1))%>%

  mutate(Pelec_U= p_elect*(1/Eff_U-1)) %>%

#x <- ESS_df[, 35]
#x <- c(ESS_df$BR_e5LF,ESS_df$BR_e5UF,ESS_df$BR_e10LF,ESS_df$BR_e10UF,ESS_df$BR_e15LF,ESS_df$BR_e15UF)
#write.csv(x, "/Users/HRahmanifard/Desktop/Data wrangling/BR_PF.csv")

  mutate(BR_P1=ifelse((Fr-0)>0,BR_p/(1+d)^(nr*(Fr-0)),0))%>%

  mutate(BR_P2=ifelse((Fr-1)>0,BR_p/(1+d)^(nr*(Fr-1)),0))%>%

  mutate(BR_P3=ifelse((Fr-2)>0,BR_p/(1+d)^(nr*(Fr-2)),0))%>%

  mutate(BR_P4=ifelse((Fr-3)>0,BR_p/(1+d)^(nr*(Fr-3)),0))%>%

  mutate(BR_P5=ifelse((Fr-4)>0,BR_p/(1+d)^(nr*(Fr-4)),0))%>%

  mutate(BR_P6=ifelse((Fr-5)>0,BR_p/(1+d)^(nr*(Fr-5)),0))%>%

  mutate(BR_P7=ifelse((Fr-6)>0,BR_p/(1+d)^(nr*(Fr-6)),0))%>%

  mutate(BR_P8=ifelse((Fr-7)>0,BR_p/(1+d)^(nr*(Fr-7)),0))%>%

  mutate(BR_P9=ifelse((Fr-8)>0,BR_p/(1+d)^(nr*(Fr-8)),0))%>%

  mutate(BR_PF=BR_P1+BR_P2+BR_P3+BR_P4+BR_P5+BR_P6+BR_P7+BR_P8+BR_P9)%>%

  mutate(BR_eLF=BR_e5L/(1+d)^5+BR_e10L/(1+d)^10+BR_e15L/(1+d)^15)%>%

  mutate(BR_eUF=BR_e5U/(1+d)^5+BR_e10U/(1+d)^10+BR_e15U/(1+d)^15)%>%

  mutate(LCOS_L = (((Cp_L+BR_PF+Conn_Cost)*p*1000*CCF+(Ce_L+BR_eLF)*p*1000*h*CCF+FOM_L*p*1000+FOMh_L*p*1000*h
                    +(VOM_L/1000)*p*1000*h*ncyc)/(p*1000*h*ncyc*DOD))*R_Exch*R_CPI+Pelec_L+Fuel_Cost) %>%

  mutate(LCOS_U = (((Cp_U+BR_PF+Conn_Cost)*p*1000*CCF+(Ce_U+BR_eUF)*p*1000*h*CCF+FOM_U*p*1000+FOMh_U*p*1000*h
                    +(VOM_U/1000)*p*1000*h*ncyc)/(p*1000*h*ncyc*DOD))*R_Exch*R_CPI+Pelec_U+Fuel_Cost) %>%
  
  mutate(LCOS_min=ifelse (LCOS_L < LCOS_U,LCOS_L, LCOS_U)) %>%
  
  mutate(LCOS_max=ifelse (LCOS_L > LCOS_U,LCOS_L, LCOS_U))  %>%
  mutate(LCOS= (LCOS_min+LCOS_max)/2) %>% 
  filter(LCOS<=5) %>% 
 
  mutate(InvCost_L = ((Cp_L/h)+Ce_L)*R_Exch*R_CPI) %>%
  mutate(InvCost_U = (Cp_U/h+Ce_U)*R_Exch*R_CPI) %>%
  mutate(Inv_Cost = ((InvCost_L+InvCost_U)/2)) %>% 
  filter(Inv_Cost<=10000) %>%
  
  mutate(Tech = substr(Index, 1,3)) %>%

  group_by(Technology)


x <- ESS_df
write.csv(x, "ESS.csv")

ESS_df$d_date <- as.factor(ESS_df$d_date)
ESS_df$Technology <- factor(ESS_df$Technology, levels=unique(ESS_df$Technology))

# Plots

LCOS_plot <- ggplot(ESS_df, aes(x=Technology, y=LCOS, group=d_date)) +
  geom_point(aes(shape=d_date, color=d_date), size=3)+
  scale_y_continuous(expand = c(0,0),limits = c(0, 4.3)) +
  scale_shape_manual(values=c(9, 1, 20,13,8,4,10,7,6))+
  scale_colour_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6"))+
  labs(x="", y="LCOS ($/kWh)", colour = "", shape = "") +
  theme_bw() +
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text=element_text(size=11,face="bold"))


LCOS_plot2 <- ggplot(ESS_df, aes(x=d_date, y=LCOS, group=Technology)) +
  geom_point(aes(shape=d_date, color=d_date), size=3)+
  scale_y_continuous(expand = c(0,0),limits = c(0, 4.3)) +
  facet_wrap(~factor(Technology, levels=unique(Technology)),nrow=2,scales = "free")+
  scale_shape_manual(values=c(9, 1, 20,13,8,4,10,7,6))+
  scale_colour_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6"))+
  labs(x="", y="LCOS (CAD$/kWh)", colour = "", shape = "") +
  theme_bw() +
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="")+
  theme(strip.text.x = element_text(size = 11, face = "bold"))

Invcost_plot <- ggplot(ESS_df, aes(x=Technology, y=Inv_Cost, group=d_date)) +
  geom_point(aes(shape=d_date, color=d_date), size=3)+
  scale_y_continuous(expand = c(0,0),limits = c(0, 10000)) +
  scale_shape_manual(values=c(9, 1, 20,13,8,4,10,7,6))+
  scale_colour_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6"))+
  labs(x="", y="Overnight Cost ($/kWh)", colour = "", shape = "") +
  theme_bw()+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text=element_text(size=11,face="bold"))

Invcost_plot2 <- ggplot(ESS_df, aes(x=d_date, y=Inv_Cost, group=Technology)) +
  geom_point(aes(shape=d_date, color=d_date), size=3)+
  #scale_y_continuous(expand = c(0,0),limits = c(0, 10000)) +
  facet_wrap(~factor(Technology, levels=unique(Technology)),nrow=2,scales = "free")+
  scale_shape_manual(values=c(9, 1, 20,13,8,4,10,7,6))+
  scale_colour_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6"))+
  labs(x="", y="Overnight Cost (CAD$/kWh)", colour = "", shape = "") +
  theme_bw() +
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="")+
  theme(strip.text.x = element_text(size = 11, face = "bold"))






# Cost projection ========================================================================================================

#ESS_LR <- ESS_df[, c("Technology", "Tech", "Inv_Cost")] %>% 
#mutate(Invcost_ave = mean(Inv_Cost)) %>% 
#  summarise(Invcost_ave = mean(Inv_Cost))

#ESS_LR <- ESS_LR[-c(4, 8:10),]

#Costred_norm.tall <- gather(costred_norm,  year,cost_mult, `2018`:`2050`, factor_key = TRUE)  %>%
#  left_join(ESS_LR, by=c("Technology")) %>%
#  mutate(LR_norm = Invcost_ave*cost_mult) #%>% 
#group_by(Technology)

#Costred_low.tall <- gather(costred_low,  year,cost_mult, `2018`:`2050`, factor_key = TRUE)  %>%
#  left_join(ESS_LR, by=c("Technology")) %>%
#  mutate(LR_low = Invcost_ave*cost_mult) #%>% 
#  #group_by(Technology)

#Costred_high.tall <- gather(costred_high,  year,cost_mult, `2018`:`2050`, factor_key = TRUE)  %>%
#  left_join(ESS_LR, by=c("Technology")) %>%
#  mutate(LR_high = Invcost_ave*cost_mult) #%>% 
# #group_by(Technology)

#=======================================================================================================================


ESS_df1 <- read.csv("ESS.csv")

ESSLR_short <- ESS_df1[c(48:236), c("Technology", "Tech","d_date", "p", "h", "InvCost_L","InvCost_U","Inv_Cost") ] %>% 
  filter(d_date ==2018) %>% 
  filter(InvCost_U < 1300) %>% 
  mutate(InvcostL = (InvCost_L*p*h)) %>% 
  mutate(InvcostU = (InvCost_U*p*h)) %>%
  mutate(Invcost = (Inv_Cost*p*h)) %>%
  mutate(energy = (p*h)) %>%
  group_by(Technology) %>% 
  summarise(InvcostL_ave = sum(InvcostL)/sum(energy),InvcostU_ave = sum(InvcostU)/sum(energy),
            Invcost_ave = sum(Invcost)/sum(energy))  

ESSLR_long <- ESS_df1[c(1:14), c("Technology", "Tech","d_date", "p", "h", "InvCost_L","InvCost_U","Inv_Cost") ] %>% 
  mutate(InvcostL = (InvCost_L*p*h)) %>% 
  mutate(InvcostU = (InvCost_U*p*h)) %>%
  mutate(Invcost = (Inv_Cost*p*h)) %>%
  mutate(energy = (p*h)) %>%
  group_by(Technology) %>% 
  summarise(InvcostL_ave = sum(InvcostL)/sum(energy),InvcostU_ave = sum(InvcostU)/sum(energy),
            Invcost_ave = sum(Invcost)/sum(energy))

ESS_LR <- rbind(ESSLR_long, ESSLR_short)
ESSLR_norm <- ESS_LR [,c(1,4)]
Costred_norm.tall <- gather(costred_norm,  year,cost_mult, `2018`:`2050`, factor_key = TRUE)  %>%
  left_join(ESSLR_norm, by=c("Technology")) %>%
  mutate(LR_norm = Invcost_ave*cost_mult) #%>% 
#group_by(Technology)

ESSLR_low <- ESS_LR [,c(1,3)]
Costred_low.tall <- gather(costred_low,  year,cost_mult, `2018`:`2050`, factor_key = TRUE)  %>%
  left_join(ESSLR_low, by=c("Technology")) %>%
  mutate(LR_low = InvcostU_ave*cost_mult) #%>% 
#group_by(Technology)

ESSLR_high <- ESS_LR [,c(1,2)]
Costred_high.tall <- gather(costred_high,  year,cost_mult, `2018`:`2050`, factor_key = TRUE)  %>%
  left_join(ESS_LR, by=c("Technology")) %>%
  mutate(LR_high = InvcostL_ave*cost_mult) #%>% 
#group_by(Technology)



Costred_norm.tall$Technology <- factor(Costred_norm.tall$Technology, levels=unique(Costred_norm.tall$Technology))

plotdata <- data.frame(Technology = Costred_norm.tall$Technology,x=Costred_norm.tall$year, y=Costred_norm.tall$LR_norm, lower = (Costred_low.tall$LR_low), upper = (Costred_high.tall$LR_high))

# Note that for hydrogen the only number given by Ganesh is conidered

#write.csv(plotdata, "plotdata.csv")
projection <- ggplot(plotdata) + 
  geom_line(aes(x=x, y=y, group = Technology, colour = Technology),size =1)+
  geom_ribbon(aes(ymin=upper, ymax=lower, x=x, group=Technology), alpha = 0.2)+
  facet_wrap(~Technology,nrow=2,scales = "free")+
  labs(x="", y="Overnight Cost (CAD$/kWh)") +
  scale_colour_manual("",values=c("blue","blue","blue","blue","blue","blue"))+
  scale_fill_manual("",values=c("grey12","grey12","grey12","grey12","grey12","grey12"))+
  theme_bw()+
  theme(legend.position="")+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 11, face = "bold"))


#Canadian storage market ===================================================================================================

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


# read the .csv file conventionally
data_file <- "C:\\Users\\HRahmanifard\\OneDrive - University of Calgary\\CERI\\Electricity storage\\Model\\Learning curves\\storage_tech_canada.xlsx"
stcanada_df <- read_xlsx(data_file) 





opr_stcanada <- stcanada_df %>% 
  filter(Status == "Operational") %>%
  mutate(sum(power)) %>% 
  mutate(share = power/sum(power)) %>% 
  group_by(Technology) %>% 
  summarise(sum(power),sum(share)) %>% 
  mutate(share=percent(`sum(share)`))

opr_stcanada2 <- stcanada_df %>% 
  filter(Status == "Operational") %>%
  mutate(sum(power)) %>% 
  mutate(share = power/sum(power)) %>% 
  group_by(Technology,`Tech-type`) %>% 
  summarise(sum(power),sum(share)) %>% 
  mutate(share=percent(`sum(share)`)) %>% 
  ungroup()

opr_stcanada2 <- opr_stcanada2[-7,]

# Basic barplot
p<-ggplot(data=opr_stcanada, aes(x=factor(Technology, levels = unique(Technology)), y=`sum(power)`/1000)) +
  geom_bar(stat="identity", width=0.5, fill="steelblue")+
  scale_y_continuous(expand = c(0,0),limits = c(0, 200)) +
  geom_text(aes(label=share), vjust=-0.3, size = 5.5)+
  theme_bw() +
  theme(axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=18,face="bold"))+
  labs(x="", y="Capacity (MW)") +
  theme(legend.position="")

p



# Horizontal barplot
p1<-ggplot(data=opr_stcanada2, aes(x=factor(`Tech-type`, levels = unique(`Tech-type`)), 
                                   y=`sum(power)`/1000,fill=Technology)) +
  geom_bar(stat="identity")+
  scale_y_continuous(expand = c(0,0),limits = c(0, 20)) +
  labs(x="", y="Capacity (MW)") +
  coord_flip()+
  geom_text(aes(label=share),hjust = -0.1,vjust=0.5)+
  theme_bw() +
  theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=12,face="bold"))+
  guides(fill=guide_legend(title=""))+
  theme(legend.text = element_text(size=10, face="bold"))+
  theme(legend.position="bottom")



p1














#dput(head(ESS_df,10))
