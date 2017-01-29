require("RGtk2")
library(cairoDevice)
 
#make a Window & Frame
device<-gtkDrawingArea()
asCairoDevice(device)

window <- gtkWindow()
window["title"] <- "B-cell"

frame <- gtkFrameNew("Visual inspection of BCR networks")
window$add(frame)
window$add(device)
window$showAll()
plot(mpg ~ hp, data = mtcars)


#add Boxes
box1 <- gtkVBoxNew(spacing= 10) # a vertical Box
box1$setBorderWidth(30)

#add box1 to the frame
frame$add(box1)   

#TextToCalculate<- gtkEntryNew() #text field with expresion to calculate in Box1
#TextToCalculate$setWidthChars(25)
#box1$packStart(TextToCalculate)

label = gtkLabelNewWithMnemonic("Result") #text label
box1$packStart(label) 

result<- gtkEntryNew() #text field or result Box with result of our calculation 
result$setWidthChars(25)
box1$packStart(result)

# create horizontal BOX
box2 <- gtkHBoxNew(spacing= 10) # distance between elements
box2$setBorderWidth(24)
box1$packStart(box2) 




#make a comboBox
model<-rGtkDataFrame(c("Select B-cell subset","x", "y"))
combobox1 <- gtkComboBox(model)

model2<-rGtkDataFrame(c("Select 1st patient","x", "y"))
combobox2 <- gtkComboBox(model2)

model3<-rGtkDataFrame(c("Select 2st patient","x", "y"))
combobox3 <- gtkComboBox(model3)


#Sehr wichtig ......?????????? ohne diese haben wir keine Liste in Combobox
crt <- gtkCellRendererText()
combobox1$packStart(crt)
combobox1$addAttribute(crt, "text", 0)


crt2 <- gtkCellRendererText()
combobox2$packStart(crt2)
combobox2$addAttribute(crt2, "text", 0)

crt3 <- gtkCellRendererText()
combobox3$packStart(crt3)
combobox3$addAttribute(crt3, "text", 0)





gtkComboBoxSetActive(combobox1,0) # first element as a default
# add comboBox in Box1
box1$packStart(combobox1)

gtkComboBoxSetActive(combobox2,0) # first element as a default
# add comboBox in Box1
box1$packStart(combobox2)

gtkComboBoxSetActive(combobox3,0) # first element as a default
# add comboBox in Box1
box1$packStart(combobox3)

#make buttons
A<- gtkButton("a")
box1$packStart(A,fill=F) #button which will start calculating

B <- gtkButton("b") #button to paste sin() to TextToCalculate
box1$packStart(B,fill=F)

C <- gtkButton("c") #button to paste cos() to TextToCalculate
box1$packStart(C,fill=F)




DoCalculation<-function(button)
{
  
 # if ((TextToCalculate$getText())=="") return(invisible(NULL)) #if no text do nothing
  
  #display error if R fails at calculating
  tryCatch(
    if (gtkComboBoxGetActive(combobox1)==0)
    #  result$setText((eval(parse(text=TextToCalculate$getText()))))
    #else (result$setText(as.integer(eval(parse(text=TextToCalculate$getText()))))),
    error=function(e)
    {
      ErrorBox <- gtkDialogNewWithButtons("Error",window, "modal","gtk-ok", GtkResponseType["ok"])
      box1 <- gtkVBoxNew()
      box1$setBorderWidth(24)
      ErrorBox$getContentArea()$packStart(box1)
      
      box2 <- gtkHBoxNew()
      box1$packStart(box2)
      
      ErrorLabel <- gtkLabelNewWithMnemonic("There is something wrong with your text!")
      box2$packStart(ErrorLabel)
      response <- ErrorBox$run()
      
      
      if (response == GtkResponseType["ok"])
        ErrorBox$destroy()
      
    }
  )
  
}


PasteB<-function(button)
{
 # TextToCalculate$setText(paste(TextToCalculate$getText(),"sin()",sep="")) #seperator is a white space
  
}

PasteC<-function(button)
{
#  TextToCalculate$setText(paste(TextToCalculate$getText(),"cos()",sep=""))
  
}

#however button variable was never used inside 
#functions, without it gSignalConnect would not work
gSignalConnect(a, "clicked", DoCalculation) #first name of buttons than event finally name of function
gSignalConnect(b, "clicked", PasteB)
gSignalConnect(c, "clicked", PasteC)


