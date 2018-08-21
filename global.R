library(GSVA)
library(psych)
library(pheatmap)
library(corrplot)
library(ggplot2)
load("liver_single_cell_exp.Rdata")
#load("liver_single_cell_exp_cell_ratio.Rdata")
load("marker_exp.Rdata")
load("paper_marker.Rdata")
#load("T_subtype_score_list.Rdata")
#load("liver_bulk_exp.Rdata")
norm_func=function(x){
  s=sum(x)
  ratio=1/s
  x=x*ratio
  return (x)
}
T_FRE=c()
plot_result=function(figure_type){
  data=T_FRE
  p=NULL
  N=nrow(data)
  count=ncol(data)
  x=rep(row.names(data),each=count)
  y=c()
  for(i in seq(1,N)){
    y=c(y,as.numeric(data[i,]))
  }
  df=data.frame(x=x,y=y)
  cell_type=rep(colnames(data),times=N)
  p=ggplot(df, mapping = aes(x = x, y = y, fill = cell_type)) + xlab("Cell type")+ylab("Abundance")+geom_bar(stat = 'identity',position = 'fill',width=0.3)+theme(legend.text=element_text(size=12),axis.text.y=element_text(size=14),axis.text.x=element_text(angle=90,hjust = 0.5,vjust=0.5,size=14))+scale_fill_discrete(name="Cell type")+theme(panel.background = element_rect(fill='#EDEDED', colour='#EDEDED'))+
    theme(plot.margin=unit(x=c(0,0,0,0),units="mm"),
          plot.background = element_rect(fill="#EDEDED"))
  x1=rep(colnames(data),each=N)
  y1=c()
  for(i in seq(1,count)){
    y1=c(y1,as.numeric(data[,i]))
  }
  df=data.frame(x=x1,y=y1)
  cell_type1=x1
  Abundance=y1
  p1<- ggplot(df,mapping= aes(x1, y1, fill=cell_type1))+geom_violin()+ xlab("Cell type")+ylab("Abundance")+theme(legend.text=element_text(size=12),axis.text.x=element_text(angle=50,hjust = 0.5,vjust=0.5,size=14),axis.text.y=element_text(size=14))+scale_fill_discrete(name="Cell type")+theme(panel.background = element_rect(fill='#EDEDED', colour='#EDEDED'))+
    theme(plot.margin=unit(x=c(0,0,0,0),units="mm"),
          plot.background = element_rect(fill="#EDEDED"))
  if(figure_type=="boxplot"){
    p2=p
  }else{
    p2=p1
  }
  p=p2
  return(p)
}
getResult=function(sample,data_type){
  #sample=liver_single_cell_exp
  sam=apply(sample,2,as.numeric)
  row.names(sam)=row.names(sample)
  genes=intersect(row.names(sam),as.vector(unlist(paper_marker)))
  sam_exp=sam[genes,]
  result=gsva(sam_exp,paper_marker,method="ssgsea",ssgsea.norm=TRUE)
  abundance_all_sample=c()
  for (i in colnames(result)){
    nes=result[,i]
    cell_type=names(nes)
    cell_r=c()
    for (cell in cell_type){
      if(nes[cell]>0){
        abun=0
        tmp=intersect(as.vector(unlist((paper_marker[cell]))),row.names(sam))
        markers=intersect(tmp,row.names(marker_exp))
        count=length(markers)
        for (gene in markers){
          if(data_type=="rnaseq"){
            weight= log10(sam_exp[gene,i]+1)/(marker_exp[gene,cell])
          }
          if(data_type=="chip"){
            weight= sam_exp[gene,i]/(marker_exp[gene,cell])
          }
          abun=abun+weight
        }
        cell_r=c(cell_r,round(abun*nes[cell]/count,2))
      }else{
        cell_r=c(cell_r,0)
      }
    }
    abundance_all_sample=rbind(abundance_all_sample,cell_r)
  }
  row.names(abundance_all_sample)=colnames(result)
  colnames(abundance_all_sample)=chartr("_"," ",row.names(result))
  abundance_all_sample_norm=t(apply(abundance_all_sample,1,norm_func))
  abundance_all_sample_norm=apply(abundance_all_sample_norm,2,function(x) round(x,2))
  T_FRE<<-abundance_all_sample_norm
  return (abundance_all_sample_norm)
}
