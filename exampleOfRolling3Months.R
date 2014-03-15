

monthSubtract<- function(nbrs){
  response<-c();
  
  for(n in nbrs){
    mo.nbr<-
      n;
    if(n < 1){
      mo.nbr <-
        13 + n - 1;
    }
    
    response<- 
      c(response, mo.nbr);
  
  } 
  
  return(response);
}

year.select<- function(nbrs, y){
  response<-c();
  for(n in nbrs){

      yr.nbr <-
        yr;
      
      if(n < 1 ){
        yr.nbr<-
          yr - 1;
      } 
    
    
    response<-c(response, yr.nbr);
  } 
  
  return(response);
}

#http://cf.datawrapper.de/nIcq7/1/
#download by clicking the "get the data" link on the table
#save to the R working directory

csv<-
  "data-nIcq7.csv";

#Specify the year (row)
yr<-
  2012;

print( paste("yr", yr) );

#Specify the month (column)
mo<-
  "Jan";

print( paste("mo", mo) );

df.totalNonFarmHires<-
  read.csv(csv, stringsAsFactors=FALSE);

column.headings<-
  names(df.totalNonFarmHires[2:ncol(df.totalNonFarmHires)])

colNbr<-
  match(mo, column.headings);

print(paste("colNbr", colNbr));

relative.columns.selected<-
  c(colNbr - 2, colNbr -1, colNbr);

print("---");
print("relative.columns.selected");
print( relative.columns.selected);
print("---");

actual.columns.selected<-
  monthSubtract(relative.columns.selected);

print("actual.columns.selected");
print(actual.columns.selected);
print("---");

columns.selected.headings<-
  column.headings[actual.columns.selected];

print("columns.selected.headings");
print(columns.selected.headings);
print("---");

monthAndYear <- 
  df.totalNonFarmHires[ df.totalNonFarmHires$Year %in% c(yr, yr-1)
                        , c("Year", columns.selected.headings)    ];
print("monthAndYear");
print(monthAndYear);
print("---");

years.selected <- 
  year.select(relative.columns.selected, yr);

print("years.selected");
print(years.selected);
print("---");

rolling3Months<-c();
i<-0;
for(m in columns.selected.headings){
  i <-
    i + 1;
  
  rolling3Months<- 
    c(rolling3Months, monthAndYear[ monthAndYear$Year == years.selected[i], m ]);
  
  msg<-
    paste(m, prettyNum(rolling3Months[i], big.mark=","));
  
  print(msg);
}
  
rolling3Months.total<-
  sum(rolling3Months);

msg<-
  paste("Total", prettyNum(rolling3Months.total, big.mark=","));

print(msg);
