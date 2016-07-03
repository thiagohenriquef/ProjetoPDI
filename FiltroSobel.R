sobelHor <- matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3, ncol = 3, byrow = TRUE)
sobelVer <- matrix(c(-1,0,1,-2,0,2,-1,0,1), nrow=3, ncol = 3, byrow = TRUE)
sobelDiag1 <- matrix(c(0,1,2,-1,0,1,-2,1,0), nrow=3, ncol = 3, byrow = TRUE)
sobelDiag2 <- matrix(c(-2,-1,0,-1,0,1,0,1,2), nrow=3, ncol = 3, byrow = TRUE)

sobel <- function(imagem, mascara1, mascara2){
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
      pos1<- (img[i-1,j-1] * mascara1[1,1])
      pos2<- (img[i-1,j] * mascara1[1,2])
      pos3<- (img[i-1,j+1] * mascara1[1,3])
      pos4<- (img[i,j-1] * mascara1[2,1])
      pos5<- (img[i,j] * mascara1[2,2])
      pos6<- (img[i,j+1] * mascara1[2,3])
      pos7<- (img[i+1,j-1] * mascara1[3,1])
      pos8<- (img[i+1,j] * mascara1[3,2])
      pos9<- (img[i+1,j+1] * mascara1[3,3])
      
      pos_1<- (img[i-1,j-1] * mascara2[1,1])
      pos_2<- (img[i-1,j] * mascara2[1,2])
      pos_3<- (img[i-1,j+1] * mascara2[1,3])
      pos_4<- (img[i,j-1] * mascara2[2,1])
      pos_5<- (img[i,j] * mascara2[2,2])
      pos_6<- (img[i,j+1] * mascara2[2,3])
      pos_7<- (img[i+1,j-1] * mascara2[3,1])
      pos_8<- (img[i+1,j] * mascara2[3,2])
      pos_9<- (img[i+1,j+1] * mascara2[3,3])
      
      val1 <- (pos1+pos2+pos3+pos4+pos5+pos6+pos7+pos8+pos9)
      val2 <- (pos_9+pos_8+pos_7+pos_6+pos_5+pos_4+pos_3+pos_2+pos_1)
      
      result1 <- val1*val1
      result2 <- val2*val2
      
      result3 <- result1+result2
      val <- sqrt(result3)
      
      g[i-1,j-1] = val
    }
  }
  res <- limiar(g)
  plotImage(img, res)
}

