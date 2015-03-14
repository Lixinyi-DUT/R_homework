corr <- function(directory, threshold = 0) {
  ## 'directory' 是长度为1的字符向量，指明
  ##  CSV 文件的位置
  
  ## 'threshold' 是长度为1的数值向量，指明
  ## 完整观测的案例的数量 (针对所有
  ## 变量) 是必须的，为了计算这两个的相关性：
  ## 硝酸盐(nitrate)和硫酸盐(sulfate); 默认值为 0
  
  ## 返回相关性的数值向量
  files<-list.files(directory,full.name=TRUE)
  data<-data.frame()
  ls<-complete(directory)
  idls<-ls$id[ls$nobs>threshold]
  if (length(idls)>0){
  co<-1:length(idls)
  for (i in 1:length(idls))
  {
  data<-read.csv(files[idls[i]])
  co[i]<-cor(data$nitrate,data$sulfate,use="complete.obs")
  }
  }
  else
  {
   co<-vector("numeric",length=0)
  }
  co
}