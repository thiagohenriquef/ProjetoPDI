detectarBordas <- function(imagem, mascara){
  img<-imagem
  img<-cbind(0,img)
  img<-cbind(img,0)
  img<-rbind(0,img)
  img<-rbind(img,0)
  f<-dim(img)
  taml<-f[1] 
  tamc<-f[2]
  
  g<-matrix(nrow=taml-2, ncol=tamc-2)
  
  for(i in 2:taml-1){
    for(j in 2:tamc-1){
      pos1<- (img[i-1,j-1] * mascara[1,1])
      pos2<- (img[i-1,j] * mascara[1,2])
      pos3<- (img[i-1,j+1] * mascara[1,3])
      pos4<- (img[i,j-1] * mascara[2,1])
      pos5<- (img[i,j] * mascara[2,2])
      pos6<- (img[i,j+1] * mascara[2,3])
      pos7<- (img[i+1,j-1] * mascara[3,1])
      pos8<- (img[i+1,j] * mascara[3,2])
      pos9<- (img[i+1,j+1] * mascara[3,3])
      
      val <- (pos1+pos2+pos3+pos4+pos5+pos6+pos7+pos8+pos9)
      
      g[i-1,j-1] = val
    }
  }
  res <- limiar(g)
  plotImage(imagem, res)
}