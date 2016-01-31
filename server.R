#library(shiny)
library(jpeg)

descale <- function(mat,att){
  t(t(mat)*att$'scaled:scale'+att$'scaled:center')
}

func <- function(){
  myO <<- readJPEG(inputi)
  y.r <- myO[,,1]
  y.g <- myO[,,2]
  y.b <- myO[,,3]
  z.r <<- scale(y.r)
  z.g <<- scale(y.g)
  z.b <<- scale(y.b)
  svd1.r <<- svd(z.r)
  svd1.g <<- svd(z.g)
  svd1.b <<- svd(z.b)
}


shinyServer(
  function(input,output,session){
    obsf <- observe({   
      if (is.null(input$file))
        return(NULL)
      inputi <<- input$file$datapath
    })
    obsi <- observe({   
      if (is.null(input$imagename))
        return('./Data/Flower.jpg')
        #inputi <<-"./Data/Flower.jpg"
      inputi <<-paste("./Data/",input$imagename,sep = '')
    })
    x <- reactive({
      #Y <<- 0.2126* myO[,,1] + 0.7152 *myO[,,2] + 0.0722 *myO[,,3]
      #Z <<- scale(Y)
      #svd1 <<- svd(Z)
      #myO<<-readJPEG(paste("./Data/",input$imagename,sep = ''))
      input$file
      input$imagename
      func()
    })
    
    output$ui <- renderUI({
      x()
      sliderInput('dyn','Dynamic slider: #Features',value=dim(myO)[1],min=1,max=dim(myO)[1],step = 1)
    })
    
    output$dim <- renderText({
      x()
      dim(myO)[1:2]
      })
    #updateSliderInput(session,'num',value=dim(myO)[1],min=1,max=dim(myO)[1])
    output$imgi <- renderImage({
      x()
#       outfile <- tempfile(fileext='.png')
#       png(outfile)#, width=400, height=300)
#       p <- descale(svd1$u %*% diag(svd1$d) %*% t(svd1$v),attributes(Z))
#       #p <- descale(t(t(svd1$u) * svd1$d) %*% t(svd1$v),attributes(Z))
#       image(t(p)[,nrow(p):1])
#       dev.off()
      
      outfile <- tempfile(fileext='.jpg')
      p.r <- descale(svd1.r$u %*% diag(svd1.r$d) %*% t(svd1.r$v),attributes(z.r))
      p.g <- descale(svd1.g$u %*% diag(svd1.g$d) %*% t(svd1.g$v),attributes(z.g))
      p.b <- descale(svd1.b$u %*% diag(svd1.b$d) %*% t(svd1.b$v),attributes(z.b))
      
      myp <- array(c(p.r,p.g,p.b),dim=dim(myO))
      writeJPEG(myp,outfile,1)
      
      list(src = outfile,
           contentType = 'image/png',
           width = 550,
           height = 350,
           alt = "Unable to display the Image")
    }, deleteFile = TRUE)
    
    output$var<- renderText({
      x()
      if(is.null(input$dyn))
        return(NULL)
      c('R:',as.character(sum(svd1.r$d[1:input$dyn]^2)/sum(svd1.r$d^2)),
      '\nG:',as.character(sum(svd1.g$d[1:input$dyn]^2)/sum(svd1.g$d^2)),
      '\nB:',as.character(sum(svd1.b$d[1:input$dyn]^2)/sum(svd1.b$d^2)))
      })
    
    y <- reactive({
      if(is.null(input$dyn))
        return(NULL)
      outfile <<- tempfile(fileext='.jpg')
      if(input$dyn !=1){
        approx10.r <- descale(svd1.r$u[,1:input$dyn] %*% diag(svd1.r$d[1:input$dyn]) %*% t(svd1.r$v[,1:input$dyn]),attributes(z.r))
        approx10.g <- descale(svd1.g$u[,1:input$dyn] %*% diag(svd1.g$d[1:input$dyn]) %*% t(svd1.g$v[,1:input$dyn]),attributes(z.g))
        approx10.b <- descale(svd1.b$u[,1:input$dyn] %*% diag(svd1.b$d[1:input$dyn]) %*% t(svd1.b$v[,1:input$dyn]),attributes(z.b))
      }
      else{
        approx10.r <- descale(svd1.r$u[,1:input$dyn] %*% as.matrix(svd1.r$d[1:input$dyn]) %*% t(svd1.r$v[,1:input$dyn]),attributes(z.r))
        approx10.g <- descale(svd1.g$u[,1:input$dyn] %*% as.matrix(svd1.g$d[1:input$dyn]) %*% t(svd1.g$v[,1:input$dyn]),attributes(z.g))
        approx10.b <- descale(svd1.b$u[,1:input$dyn] %*% as.matrix(svd1.b$d[1:input$dyn]) %*% t(svd1.b$v[,1:input$dyn]),attributes(z.b))
      }
      
      myp <<- array(c(approx10.r,approx10.g,approx10.b),dim=dim(myO))
      writeJPEG(myp,outfile,1)
      
      sz <<- file.info(outfile)$size
    })
    
    output$fin <- renderImage({
      
      x()
#       outfile <- tempfile(fileext='.png')
#       png(outfile)
#       approx10 <- descale(svd1$u[,1:input$dyn] %*% diag(svd1$d[1:input$dyn]) %*% t(svd1$v[,1:input$dyn]),attributes(Z))
#       #approx10 <- descale(t(t(svd1$u[,1:input$dyn]) * svd1$d[1:input$dyn]) %*% t(svd1$v[,1:input$dyn]),attributes(Z))
#       image(t(approx10)[,nrow(approx10):1])
#       dev.off()
      y()
      if(!exists("outfile"))
        outfile<-"xyz"
      
      list(src = outfile,
           contentType = 'image/png',
           width = 550,
           height = 350,
           alt = "Processing...")
    },deleteFile = T)
    
    output$size <- renderText({
      input$dyn
      if(is.null(input$dyn))
        return(NULL)
      paste(round((sz/1024)*100)/100,'KB')})
    
    output$sizem <- renderText({
      #x()
      #input$dyn
      #approx10 <<- descale(svd1$u[,1:input$dyn] %*% diag(svd1$d[1:input$dyn]) %*% t(svd1$v[,1:input$dyn]),attributes(Z))
      #approx10 <<- descale(t(t(svd1$u[,1:input$dyn]) * svd1$d[1:input$dyn]) %*% t(svd1$v[,1:input$dyn]),attributes(Z))
      if(is.null(input$dyn))
        return(NULL)
      format(3*(object.size(svd1.r$u[,1:input$dyn]) +
               object.size(svd1.r$d[1:input$dyn])+
               object.size(svd1.r$v[,1:input$dyn])),units="KB")
      })
    
    output$downloadData <- downloadHandler(
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        input$imagename
      },
      content = function(file) {
        writeJPEG(myp,file,1)
      }
    )
    
    output$imgr <- renderImage({
      x()
      outfile <- tempfile(fileext='.png')
      png(outfile)
      par(mar=c(5,4.1,1,1.1))
      plot(svd1.r$d^2/sum(svd1.r$d^2),xlab="RED",ylab="Variance",type= 'l')
      dev.off()
      list(src = outfile,alt = "This is alternate text")
    },deleteFile = T)
    output$imgg <- renderImage({
      x()
      outfile <- tempfile(fileext='.png')
      png(outfile)
      par(mar=c(5,4.1,1,1.1))
      plot(svd1.g$d^2/sum(svd1.g$d^2),xlab="GREEN",ylab="Variance",type= 'l')
      dev.off()
      list(src = outfile,alt = "This is alternate text")
    },deleteFile = T)
    output$imgb <- renderImage({
      x()
      outfile <- tempfile(fileext='.png')
      png(outfile)
      par(mar=c(5,4.1,1,1.1))
      plot(svd1.b$d^2/sum(svd1.b$d^2),xlab="BLUE",ylab="Variance",type= 'l')
      dev.off()
      list(src = outfile,alt = "This is alternate text")
    },deleteFile = T)
    # When the client ends the session, suspend the observer.
    # Otherwise, the observer could keep running after the client
    # ends the session.
    session$onSessionEnded(function() {
      obsf$suspend()
      obsi$suspend()
      # Clean up the file
      unlink(outfile)
    })
    
    output$help <- renderText('This App applies Singular Vector 
                              Decomposition on a JPEG image. It 
                              reduces the number of features to 
                              the input provided by the user through
                              slider. Since the image can now be 
                              represented from less number of features 
                              its size is reduced. Features of all 3 colors i.e 
                              RED, GREEN and BLUE are reduced 
                              separately. The image is re-constructed
                              to original dimensions but from a 
                              reduced set of features. The variance retained 
                              for each color is shown in the left sidebar panel. 
                              The variance 
                              tab shows plots that represent the portion 
                              of image data that is retained from the 
                              original image. This is plotted for each 
                              color. The user can upload any jpg image 
                              or select from dropdown list and then
                              can download the reconstructed image.')
  }
)
          
