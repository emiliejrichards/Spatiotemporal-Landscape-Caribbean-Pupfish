AxP<-read.table("~/Desktop/Research/Spring2018/Allele_Distribution/all_pupfish_MxP_95th_snps_gt.table", header=T, stringsAsFactors = F)

which(colnames(AxP)=="OSPP6.GT") #203
which(colnames(AxP)=="CRPP5.GT") #49
which(colnames(AxP)=="OSPP5.GT") #49
#AxP<-AxP[which(AxP$CRPP5.GT!="*/*"),]
GT<-vector()
print(colnames(AxP)[176])
print(colnames(AxP)[39])
for(i in 1:nrow(AxP)){
  
  x<-AxP[i,176]
  GT<-c(GT,x)
  if(x=="./."){
    
    x<-AxP[i,175]
    print(colnames(AxP)[175])
    # print(x)
    GT[i]<-x
    #print(GT)
    
  }
} 

GT_all<-data.frame(matrix(ncol=210,nrow=0))
colnames(GT_all)<-c(colnames(AxP))

for(i in 1:length(GT)){
  spl<-unlist(strsplit(GT[i],"/"))
  
  for(j in 4:ncol(AxP)){
    AxPspl<-unlist(strsplit(AxP[i,j],"/"))
    #print(i)
    #print(j)
    #print(spl[1])
    #print(spl[2])
    #print(AxPspl[1])
    #print(AxPspl[2])
    if(AxPspl[1]==spl[1] && AxPspl[2]==spl[2]){
      GT_all[i,j]<-2}
    else if(AxPspl[1]==spl[1] | AxPspl[2]==spl[2] | AxPspl[1]==spl[2] | AxPspl[2]==spl[1]){
      GT_all[i,j]<-1}
    else if(AxPspl[1]=="." && AxPspl[2]=="."){
      GT_all[i,j]<-"NA"
    
    }
    else{GT_all[i,j]<-0}
    
    
  }
}

GT_all$CHROM<-AxP$CHROM
GT_all$POS<-AxP$POS

write.csv(GT_all, "all_pupfish_GT_MxP_95th_snps_binary_code.csv")
for(i in 1:nrow(GT_all)){
  samples<-vector()
  for(j in 4:ncol(GT_all)){
    #print(i)
    #print(j)
    if(GT_all[i,j]!="NA" & GT_all[i,j]>=1){
      
      x<-colnames(GT_all)[j]
      samples<-c(samples,x)
    }
  }
  cat(GT_all$CHROM[i],":",GT_all$POS[i],"\n",samples,"\n", file="~/Desktop/Research/Spring2018/Allele_Distribution/MxP_95th_snp_distributions_P.txt",append=T)
  #samples
}


