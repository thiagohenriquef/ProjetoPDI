operadorLaplaciano <- matrix(c(1,1,1,1,-8,1,1,1,1), nrow=3, ncol = 3, byrow = TRUE)
detectorHor <- matrix(c(-1,-1,-1,2,2,2,-1,-1,-1), nrow=3, ncol = 3, byrow = TRUE)
detector45Pos <- matrix(c(2,-1,-1,-1,2,-1,-1,-1,2), nrow=3, ncol = 3, byrow = TRUE)
detector45Neg <- matrix(c(-1,-1,2,-1,2,-1,2,-1,-1), nrow=3, ncol = 3, byrow = TRUE)
detectorVer <- matrix(c(-1,2,-1,-1,2,-1,-1,2,-1), nrow=3, ncol = 3, byrow = TRUE)

sobelHor <- matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3, ncol = 3, byrow = TRUE)
sobelVer <- matrix(c(-1,0,1,-2,0,2,-1,0,1), nrow=3, ncol = 3, byrow = TRUE)
sobelDiag1 <- matrix(c(0,1,2,-1,0,1,-2,1,0), nrow=3, ncol = 3, byrow = TRUE)
sobelDiag2 <- matrix(c(-2,-1,0,-1,0,1,0,1,2), nrow=3, ncol = 3, byrow = TRUE)

plotImagefull<- function(image){
  plot(1:2,type='n')
  rasterImage(image,1,1,2,2)
}

plotImage <-function(imagem,imagem2){
  plot(1:2,xlim=c(1,4), ylim=c(1,2),type='n')
  rasterImage(imagem,1,1,2.5,2)
  rasterImage(imagem2,2.5,1,4,2)
}

limiar <-function(imagem){
  imagem[which(imagem>=0.5)]<-1
  imagem[which(imagem < 0.5)]<-0
  plotImagefull(imagem)
  return(imagem)
}

limiarSemRetorno <-function(imagem){
  imagem[which(imagem>=0.5)]<-1
  imagem[which(imagem < 0.5)]<-0
  plotImagefull(imagem)
}

media <-function(imagem){
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
      pos1<-img[i-1,j-1]
      pos2<-img[i-1,j]
      pos3<-img[i-1,j+1]
      pos4<-img[i,j-1]
      pos5<-img[i,j]
      pos6<-img[i,j+1]
      pos7<-img[i+1,j-1]
      pos8<-img[i+1,j]
      pos9<-img[i+1,j+1]
      val<-c(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,pos9)
      m<-mean(val)
      g[i-1,j-1]<-m		
    }
  }
  plotImage(g,img)
}

mediana <-function(imagem){
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
      pos1<-img[i-1,j-1]
      pos2<-img[i-1,j]
      pos3<-img[i-1,j+1]
      pos4<-img[i,j-1]
      pos5<-img[i,j]
      pos6<-img[i,j+1]
      pos7<-img[i+1,j-1]
      pos8<-img[i+1,j]
      pos9<-img[i+1,j+1]
      val<-c(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,pos9)
      m<-sort(val)
      g[i-1,j-1]<-m[5]		
    }
  }
  plotImage(g,img)
}

maximo <-function(imagem){
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
      pos1<-img[i-1,j-1]
      pos2<-img[i-1,j]
      pos3<-img[i-1,j+1]
      pos4<-img[i,j-1]
      pos5<-img[i,j]
      pos6<-img[i,j+1]
      pos7<-img[i+1,j-1]
      pos8<-img[i+1,j]
      pos9<-img[i+1,j+1]
      val<-c(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,pos9)
      m<-sort(val, decreasing=TRUE)
      g[i-1,j-1]<-m[1]		
    }
  }
  plotImage(g,img)
}

minimo <-function(imagem){
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
      pos1<-img[i-1,j-1]
      pos2<-img[i-1,j]
      pos3<-img[i-1,j+1]
      pos4<-img[i,j-1]
      pos5<-img[i,j]
      pos6<-img[i,j+1]
      pos7<-img[i+1,j-1]
      pos8<-img[i+1,j]
      pos9<-img[i+1,j+1]
      val<-c(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,pos9)
      m<-sort(val, decreasing=FALSE)
      g[i-1,j-1]<-m[1]		
    }
  }
  plotImage(g,img)
}

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
      
      #val <- atan2(result1, result2)
      
      g[i-1,j-1] = val
    }
  }
  res <- limiar(g)
  plotImage(img, res)
}

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
