#' @title Show the relationships between centre and peripheral items.
#'
#' @description This package show the strength of relationships between centre and peripheral items by the length of the lines between them.
#'
#' @param centrename dataname distance, bound, title, color size boundarycolor boundarysize centretextsize alpha



centralplot <-function(centrename,dataname,distance,bound,title = "centralplot",
color = "#267ED6",size = 3.5,boundarycolor = "#EF715E" ,boundarysize = 1,
centretextsize = 3,alpha = 0.5){
    n <- length(dataname)
    distance <- as.numeric(as.character(distance))
    angle <- seq(0,n-1)/n*2*pi
    distance.x <- distance*cos(angle)
    distance.y <- distance*sin(angle)
    max <- max(distance)
    sd <- sd(distance)
    textlength <- distance+mean(distance)/10
    text.x <- textlength*cos(angle)
    text.y <- textlength*sin(angle)+mean(distance)/20
    t1 <- seq(-bound, bound, length.out = 10000)
    t2 <- sqrt(bound^2 - t1^2)
    t3 <- -sqrt(bound^2 - t1^2)
    data2 <- data.frame(t1,t2,t3)
    p <- ggplot(data.frame(dataname))
    p + geom_text(aes(text.x, text.y, label = dataname))+
      geom_text(aes(x=0,y=0,label = centrename),size = centretextsize)+
      geom_line(data = data2,aes(data2[,1],data2[,2]),colour = boundarycolor,  size = boundarysize, alpha = alpha)+
      geom_line(data = data2,aes(data2[,1],data2[,3]),colour = boundarycolor,  size = boundarysize, alpha = alpha)+
      geom_segment(aes(x = 0, y = 0, xend = distance.x, yend = distance.y),size = 1,color = "gray",alpha = alpha)+
      theme_bw()+xlim(-(max+mean(distance)/10),(max+mean(distance)/10))+ylim(-(max+mean(distance)/10),(max+mean(distance)/10))+ theme(panel.grid = element_blank(),
                                                                                                                                      axis.text = element_blank(),
                                                                                                                                      axis.ticks = element_blank(),
                                                                                                                                      axis.title = element_blank(),
                                                                                                                                      panel.border = element_blank(),
                                                                                                                                      legend.position = "none")+ coord_fixed(1/1)+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(title = title)+
      geom_point(aes(distance.x,distance.y),colour = color,size=size,alpha = alpha)

  }
