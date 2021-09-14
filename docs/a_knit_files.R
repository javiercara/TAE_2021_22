# render files
# knitr::knit("file.Rmd") does not remove intermediate .md and .png files

# rmarkdown::render("index.Rmd")
# rmarkdown::render("01_Introduccion.Rmd")
# rmarkdown::render("02_Regresion_Lineal_Simple.Rmd")

# render the whole dir
files = dir()
for (ii in 1:length(files)){
  file1 = substr(files[ii],nchar(files[ii])-2,nchar(files[ii]))
  if (file1 == "Rmd"){
    print(files[ii])
    #rmarkdown::render(file1)
    rmarkdown::render(input = files[ii], output_format = "html_document")
    rmarkdown::render(input = files[ii], output_format = "pdf_document")
  }
}
