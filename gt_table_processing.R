AxP<-read.table("~/Desktop/Research/Spring2018/Allele_Distribution/all_pupfish_MxP_95th_snps_gt.table", header=T, stringsAsFactors = F)
ncol(AxP)
which(colnames(AxP)=="CRPP-QTL.GT") #189
which(colnames(AxP)=="CRPM2.GT") #190
#AxP<-AxP[which(AxP$CRPP5.GT!="*/*"),]
GT<-vector()
print(colnames(AxP)[136])
print(colnames(AxP)[137])
for(i in 1:nrow(AxP)){
  
  x<-AxP[i,136]
  GT<-c(GT,x)
  if(x=="./."){
    
    x<-AxP[i,137]
    print(colnames(AxP)[137])
    # print(x)
    GT[i]<-x
    #print(GT)
    
  }
} 

GT_all<-data.frame(matrix(ncol=175,nrow=0))
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

write.csv(GT_all, "all_pupfish_GT_MxP_95th_snps_binary_code_M.csv")
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
  cat(GT_all$CHROM[i],":",GT_all$POS[i],"\n",samples,"\n", file="~/Desktop/Research/Spring2018/Allele_Distribution/MxP_95th_snp_distributions_M_alleles.txt",append=T)
  #samples
}



################ 95th #################

AxP<-read.table("~/Desktop/Research/Spring2018/Allele_Distribution/all_pupfish_MxP_95th_snps_gt.table", header=T, stringsAsFactors = F)


#AxP<-AxP[which(AxP$CRPP5.GT!="*/*"),]
which(colnames(AxP)=="OSPM5.GT") #189
which(colnames(AxP)=="OSPM6.GT") #190
#AxP<-AxP[which(AxP$CRPP5.GT!="*/*"),]
GT<-vector()
print(colnames(AxP)[164])
print(colnames(AxP)[165])
for(i in 1:nrow(AxP)){
  
  x<-AxP[i,164]
  GT<-c(GT,x)
  if(x=="./."){
    
    x<-AxP[i,165]
    print(colnames(AxP)[165])
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

write.csv(GT_all, "all_pupfish_GT_MxP_95th_snps_binary_code_M.csv")
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
  cat(GT_all$CHROM[i],":",GT_all$POS[i],"\n",samples,"\n", file="~/Desktop/Research/Spring2018/Allele_Distribution/MxP_95th_snp_distributions_M.txt",append=T)
  #samples
}




X<-colnames(AxP)
write.table(X,"~/Desktop/Research/Spring2018/Allele_Distribution/all_indivs.txt",row.names = F, quote = F)
