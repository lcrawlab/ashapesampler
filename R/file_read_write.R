#read and write OFF, OFF4, and text files for alpha shapes

readOFF <- function(file_name){
  off=Rvcg::vcgImport(file_name,silent = TRUE)
  vertices=as.matrix(t(off$vb)[,1:3])
  n = dim(vertices)[1]
  faces=as.data.frame(t(off$it))
  edges=vcgGetEdge(off)
  complex = append(as.list(data.frame(t(edges[,1:2]))), as.list(data.frame(t(faces))))
  complex = append(as.list(1:n), complex)
  complex_info = list("Vertices"=vertices, "cmplx"= complex)
  return(vertices, complex)
}

write_alpha_txt <- function(ashape, file_name){
  vertices = ashape$x
  my_alpha = ashape$alpha
  testtext=paste("alpha",as.character(my_alpha), sep="\n" )
  n = dim(vertices)[1]
  for(i in 1:n){
    testtext = paste(testtext, paste0(vertices[i,],collapse=" "), sep="\n")
  }
  writeLines(testtext, file_name)
}

read_alpha_txt <- function(file_name){
  input_file = readLines(file_name)
  if(tolower(input_file[1])!="alpha"){
    stop("Not correct file or file format")
  }
  alpha = as.numeric(input_file[2])
  n_vert = length(input_file)-2
  if(n_vert<1){
    stop("empty list of vertices in input file")
  }
  vertex = as.numeric(strsplit(input_file[3], split = " ")[[1]])
  for (i in 2:n_vert){
    vertex = rbind(vertex, as.numeric(strsplit(input_file[2+i], split = " ")[[1]]))
  }
  row.names(vertex)=NULL
  if(dim(vertex)[2] == 2){
    return(alphahull::ashape(vertex,alpha=alpha))
  } else if (dim(vertex)[2]==3){
    return(alphashape3d::ashape3d(vertex, alpha=alpha))
  } else {
    stop("Wrong dimensions for vertices.")
  }
}