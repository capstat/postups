#load rmarkdown library
library(rmarkdown)
library(readxl) #to read the excel file

#read the data
school_data = read_excel("data/example_school_data.xlsx")
#list of all schools
schools = unique(school_data$School)

#for each school create a report
for(school in schools){
  #skip any district row data
  if(school=="District") next
  #post message to keep track 
  print(paste(school, "START!"))
  #filter for school and district data
  temp_school_data = school_data[school_data$School==school | school_data$School=="District",]
  #want the school data to be plotted first so set school as a factor
  temp_school_data$School = factor(temp_school_data$School, levels=c(school, "District"))
  #create the markdown document using the render function
  render(input="rmarkdown_report.Rmd", #this is the name of the rmarkdown file
         #this is the name of the finished report
         output_file=paste0("by_school/", school, " Summary.pdf"))
  #post message when finished 
  print(paste(school, "DONE!"))
}