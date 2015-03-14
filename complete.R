complete <- function(directory, id = 1:332) {
  ## 'directory' �ǳ���Ϊ1���ַ�������ָ��
  ##  CSV �ļ���λ��
  
  ## 'id' ��������������ָ�������ID�ţ�
  ## ��Ҫ��ʹ�õ�
  
  ## �������¸�ʽ������֡��
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## ����'id' �Ǽ���ID��ţ���'nobs'��
  ## ��������������
  files<-list.files(directory,full.name=TRUE)
  nobs<-id
  for (t in 1:length(id))
  {
    data<-read.csv(files[id[t]])
    nobs[t]<-nrow(na.omit(data))
  }
  information<-data.frame(id,nobs)
  information
}