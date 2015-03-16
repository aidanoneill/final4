corpusAsHTML <- 
  function(data){
    require("R2HTML")
      
    tbl = toMatrix(data)
    html = HTML(tbl, file="", row.names=TRUE, Border=0, innerBorder=1, 
                align='left', classtable="gridtable")
    return(html)
  
}