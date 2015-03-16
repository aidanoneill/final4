matrixAsHTML <- 
  function(data){
    require("R2HTML")
    html = HTML(data, file="", row.names=TRUE, Border=0, innerBorder=1, 
                align='left', classtable="gridtable")
    return(html)
  
}