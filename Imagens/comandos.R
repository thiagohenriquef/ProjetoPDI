plotImageFull <- function(imagem){
	plot(1:2, type='n')
	rasterImage(imagem,1,1,2,2)
}

plotImage <- function(imagem, imagem2){
	plot(1:2, xlim=c(1,4), ylim=c(1,2), type='n')
	rasterImage(imagem,1,1,2.5,2)
	rasterImage(imagem2,2.5,1,4,2)
}

#limiar
limiar <- function(imagem){
	imagem[which(imagem>0.5)]<-1
	imagem[which(imagem<0.5)]<-0
	plotImageFull(imagem)
}

#histograma padrao
histograma <- function(imagem){
	hist(imagem, plot=TRUE)
}
















