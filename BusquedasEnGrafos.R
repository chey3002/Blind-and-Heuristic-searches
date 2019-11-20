library(igraph)
library(datastructures)
FinCeros="z"
MaxSusesores=0
nroVertices=0
Prof=0
NroAristas=0
inicio=""
PromSusesores=0
CargarGrafo <- function() {
  #Cambios
  #Buscar Heauristicas 0
  #opciones 1, varios, o vacio
  links <- read.csv(file="c:/graph.csv", header=TRUE, sep=",")
  vertices=c()
  h=c()
  hnames=c()
  todelete=c()
  #Nombres de los nodos
  for(n in 1:nrow(links)){
    if(links[n,2]!=""){
      if(!(as.character(links[n,1])%in%vertices)){
        vertices=c(vertices,as.character(links[n,1]))
        h=c(h,as.numeric(links[n,4]))
        hnames=c(hnames,as.character(links[n,1]))
      }
    }else{
      
      h=c(h,as.numeric(links[n,4]))
      hnames=c(hnames,as.character(links[n,1]))
      vertices=c(vertices,as.character(links[n,1]))
      todelete=c(todelete,n)
    }
  }
  links=links[-todelete,]
  names(h)<-hnames
  grafo = graph_from_data_frame(links[-4],vertices,directed = T)
  assign("links", links, envir = .GlobalEnv)
  assign("vertices", vertices, envir = .GlobalEnv)
  
  grafo2 = graph_from_data_frame(links[-4],vertices,directed = F)
  ############################################################################################
  grafo=grafo%>%set_vertex_attr("heuristic",value=h)
  assign("h", h, envir = .GlobalEnv)
  
  assign("inicio", inicio, envir = .GlobalEnv)
  
  
  fin=h[h==0]
  assign("fin", fin, envir = .GlobalEnv)
  
  assign("grafo2", grafo2, envir = .GlobalEnv)
  
  assign("grafo", grafo, envir = .GlobalEnv)
  
  tkplot(grafo)
  #Variables de Complejidad
  MaxSusesores=0
  j=0
  prom=0
  for(i in V(grafo)){
    temp=neighbors(grafo, i, mode="out")
    if(length(temp)!=0){
      PromSusesores=PromSusesores+length(temp)
      j=j+1
      if(MaxSusesores<length(temp)){
        MaxSusesores=length(temp)
      }
    }
  }
  assign("MaxSusesores", MaxSusesores, envir = .GlobalEnv)
  PromSusesores=PromSusesores/j
  assign("PromSusesores", PromSusesores, envir = .GlobalEnv)
  nroVertices=length(V(grafo))
  assign("nroVertices", nroVertices, envir = .GlobalEnv)
  NroAristas=length(E(grafo))
  assign("NroAristas", NroAristas, envir = .GlobalEnv)
}
#Busqueda en amplitud
amplitud <- function(grafo, nodoInicial,nodoFinal) {
  ptm <- Sys.time()
  encontrados=0
  q=queue()
  q <- insert(q, nodoInicial)
 
  nodoActual=pop(q) 
  j=1

  nodoActual
  queue=c(nodoInicial)
  extract=c(" ")
  end=TRUE
  aq=c()
  tempFin=fin
  while (end) {
    hijos=neighbors(grafo, V(grafo)[nodoActual], mode="out")
    if(length(hijos)!=0){
      for (i in 1:length(hijos))  
      {
        existe=length(extract[extract==hijos[i]$name])
        if (existe==0){
          if(!hijos[i]$name%in%extract)
            if(!hijos[i]$name%in%aq)              
              q =insert(q,hijos[i]$name)
        }
      }
    }
    lq=size(q)
    aq=c()
    for(i in 1:lq){
      aq=c(aq,pop(q))
    }
    for(i in aq){
      q =insert(q,i)
    }
    queue=c(queue,paste(aq,collapse=" "))
    extract=c(extract,nodoActual)
    
    nodoActual=pop(q)
    j=j+1
  
    if(length(aq)==0){
      end=FALSE
      queue[length(queue)]="Encontrado->"
      
    }else{
      if(length(fin)==0){
        if(length(aq)==0){
          end=FALSE
        }
      }else{
        if(extract[length(extract)] %in% names(tempFin)){
          queue=c(queue,"Encontrado->")
          extract=c(extract,extract[length(extract)])
          tempFin = tempFin[names(tempFin)!= extract[length(extract)]]
          if(length(tempFin)==0){
            end=FALSE
          }
        }
      }}
    
  }
  print("Busqueda por Amplitud")
  print(data.frame(
    "Cola" = queue, 
    "Extraer" = extract
  ))
  print(Sys.time() - ptm)
}
#Busqueda en profundidad
profundidad <- function(grafo, nodoInicial,nodoFinal) {
  ptm <- Sys.time()
  q=stack()
  q <- insert(q, nodoInicial)
 
  nodoActual=pop(q) 
  j=1

  nodoActual
  queue=c(nodoInicial)
  extract=c(" ")
  end=TRUE
  aq=c()
  tempFin=fin
  while (end) {
    hijos=neighbors(grafo, V(grafo)[nodoActual], mode="out")
    if(length(hijos)!=0){
      for (i in 1:length(hijos))  
      {
        existe=length(extract[extract==hijos[i]$name])
        if (existe==0){
          if(!hijos[i]$name%in%extract)
            if(!hijos[i]$name%in%aq)              
              q =insert(q,hijos[i]$name)      }
      }
    }
    lq=size(q)
    aq=c()
    for(i in 1:lq){
      aq=c(aq,pop(q))
    }
    for(i in rev(aq)){
      q =insert(q,i)
    }
    queue=c(queue,paste(aq,collapse=" "))
    extract=c(extract,nodoActual)
    
    nodoActual=pop(q)
    j=j+1
  
    if(length(aq)==0){
      end=FALSE
      queue[length(queue)]="Encontrado->"
      
    }else{
      if(length(fin)==0){
        if(length(aq)==0){
          end=FALSE
          queue[length(queue)]="Encontrado->"
        }
      }else{
        if(extract[length(extract)] %in% names(tempFin)){
          
          queue=c(queue,"Encontrado->")
          
          extract=c(extract,extract[length(extract)])
          
          tempFin = tempFin[names(tempFin)!= extract[length(extract)]]
          if(length(tempFin)==0){
            end=FALSE
          }
        }
      }}
  }
  print("Busqueda en profundidad")
  print(data.frame(
    "Cola" = queue, 
    "Extraer" = extract
  ))
  print(Sys.time() - ptm)
}
#Busqueda en Bidireccional
bidireccional <- function(grafo, nodoInicial,nodoFinal) {
  ptm <- Sys.time()
  if(length(nodoFinal)==0){
    nodoFinal=FinCeros
  }else{
    nodoFinal=nodoFinal[1]
  }
  q=queue()
  q <- insert(q, nodoInicial)
 
  nodoActual=pop(q) 
  j=1

  nodoActual
  queue=c(nodoInicial)
  extract=c(" ")
  #########################################
  q2=queue()
  q2 <- insert(q2, nodoFinal)
  nodoActual2=pop(q2) 
  j=1
  nodoActual2
  queue2=c(nodoFinal)
  extract2=c(" ")
  cond=TRUE
  aq2=c()
  aq=c()
  while (cond) {
    
    hijos=neighbors(grafo, V(grafo)[nodoActual], mode="out")
    for (i in 1:length(hijos))  
    {
      existe=length(extract[extract==hijos[i]$name])
      if (existe==0){
        if(!hijos[i]$name%in%aq){
          q =insert(q,hijos[i]$name)
        }
      }
    }
    lq=size(q)
    aq=c()
    for(i in 1:lq){
      aq=c(aq,pop(q))
    }
    for(i in aq){
      q =insert(q,i)
    }
    queue=c(queue,paste(aq,collapse=" "))
    extract=c(extract,nodoActual)
    
    nodoActual=pop(q)
    ######################################################
    
    hijos=neighbors(grafo2, V(grafo2)[nodoActual2], mode="out")
    
    for (i in 1:length(hijos))  
    {
      existe=length(extract2[extract2==hijos[i]$name])
      if (existe==0){
        if(!hijos[i]$name%in%aq2){
          q2 =insert(q2,hijos[i]$name)
        }
      }
    }
    lq2=size(q2)
    aq2=c()
    for(i in 1:lq2){
      aq2=c(aq2,pop(q2))
    }
    for(i in aq2){
      q2 =insert(q2,i)
    }
    queue2=c(queue2,paste(aq2,collapse=" "))
    extract2=c(extract2,nodoActual2)
    
    nodoActual2=pop(q2)
    j=j+1
  
    if(nodoActual2%in%aq){
      cond=FALSE
    }
    if(nodoActual%in%aq2){
      cond=FALSE
    }
    for(i in aq2){
      if(i%in%aq){
        cond=FALSE
      }
    }
    
  }
  print("Busqeuda Bidireccional")
  print("COLA1")
  DF1=data.frame(
    "Cola" = queue, 
    "Extraer" = extract
  )
  print(DF1)
  print("COLA2")
  DF2=data.frame(
    "Cola" = queue2, 
    "Extraer" = extract2
  )
  
  print(DF2)
  print(Sys.time() - ptm)
}
#Busqueda en Iterativa en profundidad
ItProfundidad <- function(grafo, nodoInicial,nodoFinal) {
  ptm <- Sys.time()
  print("Busqueda Iterativa en profundidad")
  k=0
  nodoActual=nodoInicial
  tempFin=fin
  end=TRUE
  while(end){
    k=k+1
    q=stack()
    q <- insert(q, nodoInicial)
   
    nodoActual=pop(q) 
    j=1
  
    nodoActual
    queue=c(nodoInicial)
    extract=c(" ")
    nivel=c(0)
    cond1=TRUE
    aq=c()
    lq=c()
    tempFin=fin
    
    while (cond1) {
      
      hijos=neighbors(grafo, V(grafo)[nodoActual], mode="out")
      actlvl=nivel[1]
      nivel=nivel[-1]
      if(length(hijos)!=0){
        for (i in 1:length(hijos))  
        {
          existe=length(extract[extract==hijos[i]$name])
          if (existe==0){
            if(actlvl<k){
              
              if(!(hijos[i]$name%in%extract)){
                if(!hijos[i]$name%in%aq)  {if(!hijos[i]$name%in%lq)  {
                  
                  q =insert(q,hijos[i]$name)
                  nivel=c(actlvl+1,nivel)}}}}}
        }}
      lq=size(q)
      aq=c()
      for(i in 1:lq){
        aq=c(aq,pop(q))
      }
      for(i in rev(aq)){
        q =insert(q,i)
      }
      queue=c(queue,paste(aq,collapse=" "))
      extract=c(extract,nodoActual)
      nodoActual=pop(q)
      j=j+1
      
      if(length(aq)==0){
        
        cond1=FALSE
        
        if(length(fin)==0)
        {
          x=V(grafo)$name%in%extract
          
          if(all(x==TRUE))
            end=FALSE
          
        }else{if(extract[length(extract)] %in% names(tempFin)){
          queue=c(queue,"Encontrado->")
          extract=c(extract,extract[length(extract)])
          tempFin = tempFin[names(tempFin)!= extract[length(extract)]]
          
          if(length(tempFin)==0){
            
            cond1=FALSE
            end=FALSE
          }
        }
        }
      }else{
        if(length(fin)==0){
          if(length(aq)==0){
            print(tempFin)
            
            cond1=FALSE
            end=FALSE
          }
        }else{
          if(extract[length(extract)] %in% names(tempFin)){
            queue=c(queue,"Encontrado->")
            extract=c(extract,extract[length(extract)])
            tempFin = tempFin[names(tempFin)!= extract[length(extract)]]
            
            if(length(tempFin)==0){
              
              cond1=FALSE
              end=FALSE
            }
          }
        }}
      
      if(is.null(nodoActual)){
        print("Nivel:")
        print(k)
        print(data.frame(
          "Cola" = queue, 
          "Extraer" = extract
        ))
        nodoActual=nodoInicial
      }
    } 
    
  }
  print("Nivel:")
  print(k)
  
  print(data.frame(
    "Cola" = queue, 
    "Extraer" = extract
  ))
  print(Sys.time() - ptm)
}
#Busqueda en Costo Uniforme
cu <- function(grafo, nodoInicial,nodoFinal) {
  ptm <- Sys.time()
  q=queue()
  q <- insert(q, nodoInicial)
 
  nodoActual=pop(q) 
  j=1

  nodoActual
  queue=c(nodoInicial)
  extract=c(" ")
  lastWeight=0
  cond1=TRUE
  aq=c()
  tempFin=fin
  while (cond1) {
    hijos=neighbors(grafo, V(grafo)[nodoActual], mode="out")
    if(length(hijos)!=0){
      
      for (i in 1:length(hijos))  
      {
        existe=length(extract[extract==hijos[i]$name])
        if (existe==0){
          q =insert(q,c( hijos[i]$name,lastWeight+E(grafo, P = (c(nodoActual,hijos[i]$name)), path = NULL, directed = TRUE)$weight))
        }
      }}
    lq=size(q)
    aq=c()
    tempq=c()
    name=c()
    we=c()
    for(i in 1:lq){
      vec=pop(q)
      name=c(name,vec[1])
      if(length(vec)==1){
        we=c(we,as.numeric(pop(q)))
      }else{
        we=c(we,as.numeric(vec[2]))
        
      }
      
    } 
    tempq=(we)
    names(tempq)<-name
    tempq=sort(tempq,decreasing = FALSE)
    for(i in 1:lq){
      aq=c(aq,c(names(tempq)[i],tempq[i]))
    }
    aq=na.omit(aq)
    for(i in aq){
      q =insert(q,c(i))
    }
    queue=c(queue,paste(aq,collapse=" "))
    extract=c(extract,nodoActual)
    
    nodoActual=pop(q)
    lastWeight=as.numeric(pop(q))
    
    j=j+1
    if(length(aq)==0){
      cond1=FALSE
      
    }else{
      if(length(fin)==0){
        if(length(aq)==0){
          cond1=FALSE
        }
      }else{
        
        if(extract[length(extract)] %in% names(tempFin)){
          queue=c(queue,"Encontrado->")
          extract=c(extract,extract[length(extract)])
          tempFin = tempFin[names(tempFin)!= extract[length(extract)]]
          if(length(tempFin)==0){
            cond1=FALSE
            end=FALSE
          }
        }
      }}
  }
  print("Búsqueda de coste uniforme")
  print(data.frame(
    "Cola" = queue, 
    "Extraer" = extract
  ))
  print(Sys.time() - ptm)
}
#Busqueda en Gradiente
ac <- function(grafo, nodoInicial,nodoFinal) {
  ptm <- Sys.time()
  q=queue()
  q <- insert(q, nodoInicial)
  nodoActual=pop(q) 
  j=1

  nodoActual
  queue=c(nodoInicial)
  extract=c(" ")
  lastWeight=0
  cond1=TRUE
  aq=c()
  tempFin=fin
  while (cond1) {
    hijos=neighbors(grafo, V(grafo)[nodoActual], mode="out")
    if(length(hijos)!=0){
      for (i in 1:length(hijos))  
      {
        existe=length(extract[extract==hijos[i]$name])
        if (existe==0){
          q =insert(q,c( hijos[i]$name,V(grafo)[hijos[i]$name]$heuristic))
        }
      }}
    lq=size(q)
    aq=c()
    tempq=c()
    name=c()
    we=c()
    for(i in 1:lq){
      vec=pop(q)
      name=c(name,vec[1])
      if(length(vec)==1){
        we=c(we,as.numeric(pop(q)))
      }else{
        we=c(we,as.numeric(vec[2]))
        
      }
      
    } 
    tempq=(we)
    names(tempq)<-name
    tempq=sort(tempq,decreasing = FALSE)
    for(i in 1:lq){
      aq=c(aq,c(names(tempq)[i],tempq[i]))
    }
    aq=na.omit(aq)
    for(i in aq){
      q =insert(q,c(i))
    }
    queue=c(queue,paste(aq,collapse=" "))
    extract=c(extract,nodoActual)
    
    nodoActual=pop(q)
    lastWeight=as.numeric(pop(q))    
    j=j+1
  
    if(length(aq)==0){
      cond1=FALSE
      end=FALSE
      
    }else{
      if(length(fin)==0){
        if(length(aq)==0){
          cond1=FALSE
          end=FALSE
        }
      }else{
        
        if(extract[length(extract)] %in% names(tempFin)){
          queue=c(queue,"Encontrado->")
          extract=c(extract,extract[length(extract)])
          tempFin = tempFin[names(tempFin)!= extract[length(extract)]]
          if(length(tempFin)==0){
            cond1=FALSE
            end=FALSE
          }
        }
      }}
    q=queue()
  }
  if(queue[length(queue)]==""&&length(nodoFinal)!=0){
    queue[length(queue)]="Encontrado->"
  }
  print("Busqueda Gradiente")
  print(data.frame(
    "Cola" = queue, 
    "Extraer" = extract
  ))
  print(Sys.time() - ptm)
}
#Busqueda en primero el mejor
pem <- function(grafo, nodoInicial,nodoFinal) {
  ptm <- Sys.time()
  q=stack()
  q <- insert(q, nodoInicial)
 
  nodoActual=pop(q) 
  j=1

  nodoActual
  queue=c(nodoInicial)
  extract=c(" ")
  lastWeight=0
  cond1=TRUE
  aq=c()
  tempFin=fin
  while (cond1) {
    hijos=neighbors(grafo, V(grafo)[nodoActual], mode="out")
    if(length(hijos)!=0){
      ns=c()
      nns=c()
      for (i in 1:length(hijos))  
      {
        existe=length(extract[extract==hijos[i]$name])
        
        if (existe==0){
          if(!hijos[i]$name%in%aq){
            nns=c(nns,hijos[i]$name)
            ns =c(ns,V(grafo)[hijos[i]$name]$heuristic)
          }
        }
      }
      names(ns)=nns
      ns=sort(ns,decreasing = TRUE)
      for(x in 1:length(ns))
        q =insert(q,c(names(ns)[x],ns[x]))
    }
    lq=size(q)
    aq=c()
    tempq=c()
    name=c()
    we=c()
    for(i in 1:lq){
      vec=pop(q)
      name=c(name,vec[1])
      if(length(vec)==1){
        we=c(we,as.numeric(pop(q)))
      }else{
        we=c(we,as.numeric(vec[2]))
        
      }
      
    } 
    tempq=(we)
    names(tempq)<-name
    for(i in 1:lq){
      aq=c(aq,c(names(tempq)[i],tempq[i]))
    }
    aq=na.omit(aq)
    for(i in rev(aq)){
      q =insert(q,c(i))
    }
    queue=c(queue,paste(aq,collapse=" "))
    extract=c(extract,nodoActual)
    
    nodoActual=pop(q)
    lastWeight=as.numeric(pop(q))    
    j=j+1
  
    if(length(aq)==0){
      cond1=FALSE
      end=FALSE
      
    }else{
      if(length(fin)==0){
        if(length(aq)==0){
          cond1=FALSE
          end=FALSE
        }
      }else{
        
        if(extract[length(extract)] %in% names(tempFin)){
          queue=c(queue,"Encontrado->")
          extract=c(extract,extract[length(extract)])
          tempFin = tempFin[names(tempFin)!= extract[length(extract)]]
          if(length(tempFin)==0){
            cond1=FALSE
            end=FALSE
          }
        }
      }}
  }
  if(queue[length(queue)]==""&&length(nodoFinal)!=0){
    queue[length(queue)]="Encontrado->"
  }
  print("Busqueda Primero el mejor")
  print(data.frame(
    "Cola" = queue, 
    "Extraer" = extract
  ))
  print(Sys.time() - ptm)
}
#Busqueda en A*
aestrella <- function(grafo, nodoInicial,nodoFinal) {
  ptm <- Sys.time()
  q=queue()
  q <- insert(q, nodoInicial)
 
  nodoActual=pop(q) 
  j=1

  nodoActual
  queue=c(nodoInicial)
  extract=c(" ")
  lastWeight=0
  cond1=TRUE
  aq=c()
  tempFin=fin
  
  while (cond1) {
    hijos=neighbors(grafo, V(grafo)[nodoActual], mode="out")
    if(length(hijos)!=0){
      for (i in 1:length(hijos))  
      {
        existe=length(extract[extract==hijos[i]$name])
        if (existe==0){
          if(!is.null(aq[hijos[i]$name])&&!is.na(aq[hijos[i]$name])){
            if(as.numeric(aq[hijos[i]$name])<E(grafo, P = (c(nodoActual,hijos[i]$name)), path = NULL, directed = TRUE)$weight+V(grafo)[hijos[i]$name]$heuristic){
              q =insert(q,c( hijos[i]$name,E(grafo, P = (c(nodoActual,hijos[i]$name)), path = NULL, directed = TRUE)$weight+V(grafo)[hijos[i]$name]$heuristic))
              
            }
          }else{
            q =insert(q,c( hijos[i]$name,E(grafo, P = (c(nodoActual,hijos[i]$name)), path = NULL, directed = TRUE)$weight+V(grafo)[hijos[i]$name]$heuristic))
          }
        }
      }}
    lq=size(q)
    aq=c()
    tempq=c()
    name=c()
    we=c()
    for(i in 1:lq){
      vec=pop(q)
      name=c(name,vec[1])
      if(length(vec)==1){
        we=c(we,as.numeric(pop(q)))
      }else{
        we=c(we,as.numeric(vec[2]))
        
      }
      
    } 
    tempq=(we)
    names(tempq)<-name
    tempq=sort(tempq,decreasing = FALSE)
    for(i in 1:lq){
      aq=c(aq,c(names(tempq)[i],tempq[i]))
    }
    aq=na.omit(aq)
    queue=c(queue,paste(aq,collapse=" "))
    for(i in aq){    
      q =insert(q,c(i))
    }
    
    extract=c(extract,nodoActual)
    
    nodoActual=pop(q)
    lastWeight=as.numeric(pop(q))    
    j=j+1
  
    if(length(aq)==0){
      cond1=FALSE
      end=FALSE
      grafo  
    }else{
      if(length(fin)==0){
        if(length(aq)==0){
          cond1=FALSE
          end=FALSE
        }
      }else{
        
        if(extract[length(extract)] %in% names(tempFin)){
          queue=c(queue,"Encontrado->")
          extract=c(extract,extract[length(extract)])
          tempFin = tempFin[names(tempFin)!= extract[length(extract)]]
          if(length(tempFin)==0){
            cond1=FALSE
            end=FALSE
          }
        }
      }}
  }
  if(queue[length(queue)]==""&&length(nodoFinal)!=0){
    queue[length(queue)]="Encontrado->"
  }
  print("Busqueda A*")
  print(data.frame(
    "Cola" = queue, 
    "Extraer" = extract
  ))
  print(Sys.time() - ptm)
}

RunAll<-function(grafo, nodoInicial,nodoFinal){
  tamplitud <- Sys.time()
  amplitud(grafo,inicio,names(fin))
  tamplitud=Sys.time() - tamplitud
  tprofundidad <- Sys.time()
  profundidad(grafo,inicio,names(fin))
  tprofundidad=Sys.time() - tprofundidad
  tbidireccional <- Sys.time()
  bidireccional(grafo,inicio,names(fin))
  tbidireccional=Sys.time() - tbidireccional
  tprofIter <- Sys.time()
  ItProfundidad(grafo,inicio,names(fin))
  tprofIter=Sys.time() - tprofIter
  tcostoUniforme <- Sys.time()
  cu(grafo,inicio,names(fin))
  tcostoUniforme=Sys.time() - tcostoUniforme
  tgradiente <- Sys.time()
  ac(grafo,inicio,names(fin))
  tgradiente=Sys.time() - tgradiente
  tpem <- Sys.time()
  pem(grafo,inicio,names(fin))
  tpem=Sys.time() - tpem
  taEstrella <- Sys.time()
  aestrella(grafo,inicio,names(fin))
  taEstrella=Sys.time() - taEstrella
  
  print("Tabla Comparativa Busquedas a ciegas")
  print(data.frame(
    "X"=c("Espacio","Tiempo","Medición"),
    "Amplitud" = c(MaxSusesores^Prof,MaxSusesores^Prof,as.numeric(tamplitud)), 
    "Profundidad" = c(MaxSusesores*Prof,MaxSusesores^Prof,as.numeric(tprofundidad)),
    "Bidireccional" = c(MaxSusesores^(Prof/2),MaxSusesores^(Prof/2),as.numeric(tbidireccional)),
    "Profundidad Iterativa"=c(MaxSusesores*Prof,MaxSusesores^Prof,as.numeric(tprofIter)),
    "Costo Uniforme" = c(MaxSusesores^Prof,MaxSusesores^Prof,as.numeric(tcostoUniforme))
  ))
  print("Tabla Comparativa Busquedas Heuristicas")
  print(data.frame(
    "X"=c("Tiempo MC","Tiempo PC","Espacio MC","Espacio PC","Medición"),
    "Gradiente" = c(log(nroVertices),Prof,1,Prof,as.numeric(tgradiente)), 
    "Primero el mejor" = c(log(nroVertices),PromSusesores^Prof,1,PromSusesores^Prof,as.numeric(tpem)), 
    "A estrella" = c(PromSusesores^Prof,PromSusesores^Prof,1,PromSusesores^Prof,as.numeric(taEstrella))
  ))
}
final<- function() {
  assign("M", FALSE, envir = .GlobalEnv)
}
ReadStart<-function(){
  inicio=readline("Ingrese el inicio: ")
  assign("inicio", inicio, envir = .GlobalEnv)
  if(length(fin)!=0){
  camino<- shortest_paths(grafo, from = V(grafo)[inicio], to  = V(grafo)[names(fin)[1]],output = "both") 
  Prof=length(camino$vpath[[1]])-1
  assign("Prof", Prof, envir = .GlobalEnv)
  }else{
    camino<- shortest_paths(grafo, from = V(grafo)[inicio], to  = FinCeros,output = "both") 
    Prof=length(camino$vpath[[1]])-1
    assign("Prof", Prof, envir = .GlobalEnv)
  }
}
AddEnd<-function(){
  grafo = graph_from_data_frame(links,vertices,directed = T)
  nFin=readline("Ingrese un Final: ")
  h[nFin]=0
  fin=c(fin,"0")
  names(fin)[length(fin)]=nFin
  grafo=grafo%>%set_vertex_attr("heuristic",value=h)
  assign("grafo", grafo, envir = .GlobalEnv)
  assign("h", h, envir = .GlobalEnv)
  assign("fin", fin, envir = .GlobalEnv)
  
  
  
  print(fin)
}

M=TRUE
while(M){
  switch(menu(c("CargarGrafo","Establecer nodo inicial","Agregar Final","Amplitud","Profundidad","Bidireccional","Iterativa en profundidad","Costo Uniforme","Gradiente","Primero el mejor","A estrella","Correr Todos"), graphics = TRUE, title = "Búsqueda en espacios de estados") + 1,
         final(), CargarGrafo(),ReadStart(),AddEnd(),amplitud(grafo,inicio,names(fin)),profundidad(grafo,inicio,names(fin)),bidireccional(grafo,inicio,names(fin)),ItProfundidad(grafo,inicio,names(fin)),cu(grafo,inicio,names(fin)),ac(grafo,inicio,names(fin)),pem(grafo,inicio,names(fin)),aestrella(grafo,inicio,names(fin)),RunAll(grafo,inicio,names(fin)))
}
