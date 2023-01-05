library(igraph)
g_0_r=c()
g_0_i=c()
g_0_s=c()
XIST<-function(row,column,g,type,p){
  if(type=="RANDOM"){
    p=c(0.5,0.5)
    if(g==0){
      sidelength<-column
      color_mat<-graph.lattice(dimvector = c(sidelength,row),dim=2)
      c_m=color_mat
      g_0_r<<-sample(c("Xa","Xb"), size=length(V(color_mat)), replace=T,prob = c(0.5,0.5))
      V(c_m)$chromosome<-g_0_r
      V(c_m)[chromosome=="Xa"]$color<-'blue'
      V(c_m)[chromosome=="Xb"]$color<-'yellow'
      p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:row)), 
             vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
             rescale=T,edge.lty=0,frame=T)
    }else{
      sidelength<-column*(2^g)
      ver.length<-row
      color_mat<-graph.lattice(dimvector = c(sidelength,ver.length),dim=2)
      color_mat_0<-graph.lattice(length=column,dim=2)
      if(length(g_0_r)!= row*column){
        c_m=color_mat
        V(c_m)$chromosome<-sample(c("Xa","Xb"), size=length(V(color_mat_0)), replace=T,prob = c(0.5,0.5))
        V(c_m)$chromosome<-rep(V(c_m)$chromosome,each=2^g)
        V(c_m)[chromosome=="Xa"]$color<-'blue'
        V(c_m)[chromosome=="Xb"]$color<-'yellow'
        p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:ver.length)), 
               vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
               rescale=T,edge.lty=0,frame=T)
      }else{
        c_m=color_mat
        V(c_m)$chromosome<-g_0_r
        V(c_m)$chromosome<-rep(V(c_m)$chromosome,each=2^g)
        V(c_m)[chromosome=="Xa"]$color<-'blue'
        V(c_m)[chromosome=="Xb"]$color<-'yellow'
        p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:ver.length)), 
               vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
               rescale=T,edge.lty=0,frame=T)
      }
    }
  }
  if(type=="IMPRINTED"){
    if(g==0){
      sidelength<-column
      color_mat<-graph.lattice(dimvector = c(sidelength,row),dim=2)
      c_m=color_mat
      g_0_i<<-sample(c("Xa","Xb"), size=length(V(color_mat)), replace=T,prob = c(0,1))
      V(c_m)$chromosome<-g_0_i
      V(c_m)[chromosome=="Xa"]$color<-'blue'
      V(c_m)[chromosome=="Xb"]$color<-'yellow'
      p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:row)), 
             vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
             rescale=T,edge.lty=0,frame=T)
    }else{
      sidelength<-column*(2^g)
      ver.length<-row
      color_mat<-graph.lattice(dimvector = c(sidelength,ver.length),dim=2)
      color_mat_0<-graph.lattice(length=column,dim=2)
      if(length(g_0_i)!= row*column){
        c_m=color_mat
        V(c_m)$chromosome<-sample(c("Xa","Xb"), size=length(V(color_mat_0)), replace=T,prob = c(0,1))
        V(c_m)$chromosome<-rep(V(c_m)$chromosome,each=2^g)
        V(c_m)[chromosome=="Xa"]$color<-'blue'
        V(c_m)[chromosome=="Xb"]$color<-'yellow'
        p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:ver.length)), 
               vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
               rescale=T,edge.lty=0,frame=T)
      }else{
        c_m=color_mat
        V(c_m)$chromosome<-g_0_i
        V(c_m)$chromosome<-rep(V(c_m)$chromosome,each=2^g)
        V(c_m)[chromosome=="Xa"]$color<-'blue'
        V(c_m)[chromosome=="Xb"]$color<-'yellow'
        p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:ver.length)), 
               vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
               rescale=T,edge.lty=0,frame=T)
      }
    }
  }
  if(type=="SKEWED"){
    if(g==0){
      sidelength<-column
      color_mat<-graph.lattice(dimvector = c(sidelength,row),dim=2)
      c_m=color_mat
      g_0_s<<-sample(c("Xa","Xb"), size=length(V(color_mat)), replace=T,prob = c(p,1-p))
      V(c_m)$chromosome<-g_0_s
      V(c_m)[chromosome=="Xa"]$color<-'blue'
      V(c_m)[chromosome=="Xb"]$color<-'yellow'
      p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:row)), 
             vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
             rescale=T,edge.lty=0,frame=T)
    }else{
      sidelength<-column*(2^g)
      ver.length<-row
      color_mat<-graph.lattice(dimvector = c(sidelength,ver.length),dim=2)
      color_mat_0<-graph.lattice(length=column,dim=2)
      if(length(g_0_s)!= row*column){
        c_m=color_mat
        V(c_m)$chromosome<-sample(c("Xa","Xb"), size=length(V(color_mat_0)), replace=T,prob = c(p,1-p))
        V(c_m)$chromosome<-rep(V(c_m)$chromosome,each=2^g)
        V(c_m)[chromosome=="Xa"]$color<-'blue'
        V(c_m)[chromosome=="Xb"]$color<-'yellow'
        p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:ver.length)), 
               vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
               rescale=T,edge.lty=0,frame=T)
      }else{
        c_m=color_mat
        V(c_m)$chromosome<-g_0_s
        V(c_m)$chromosome<-rep(V(c_m)$chromosome,each=2^g)
        V(c_m)[chromosome=="Xa"]$color<-'blue'
        V(c_m)[chromosome=="Xb"]$color<-'yellow'
        p=plot(c_m, layout=as.matrix(expand.grid(1:sidelength, 1:ver.length)), 
               vertex.label=NA,vertex.size=5,edge.arcolumn.size=.4,vertex.shape="square",
               rescale=T,edge.lty=0,frame=T)
      }
    }
  }
  return(p)
}

library(shiny)
shinyServer(function(input,output){
  output$cellplot<-renderPlot({
    
         XIST(row=as.numeric(input$row),column=as.numeric(input$columns),g=as.numeric(input$divisions),type=input$type,p=as.numeric(input$prob_Xp)) 
       
  })
})

