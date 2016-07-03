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
  return(g)
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
  return(g)
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
  plotImage(img,g)
  return(g)
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
  plotImage(img,g)
  return(g)
}

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