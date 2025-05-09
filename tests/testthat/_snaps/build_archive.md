# build_archive() works messages work

    Code
      msgs
    Output
       [1] "i Building Darwin Core Archive\n"    "\\ Detecting files...\n"            
       [3] "v Detecting files... [1s]\n"         "\n"                                 
       [5] "\\ Creating zip folder...\n"         "v Creating zip folder... [1s]\n"    
       [7] "\n"                                  "i Writing 'dwc-archive.zip'.\n"     
       [9] "v Writing 'dwc-archive.zip'. [1s]\n" "\n"                                 

# build_archive() menu appears

    Code
      msgs
    Output
       [1] "i Building Darwin Core Archive\n"         
       [2] "\\ Detecting files...\n"                  
       [3] "v Detecting files... [9ms]\n"             
       [4] "\n"                                       
       [5] "i Building schema\n"                      
       [6] "\\ Detecting files...\n"                  
       [7] "v Detecting files... [10ms]\n"            
       [8] "\n"                                       
       [9] "\\ Formatting Darwin Core terms...\n"     
      [10] "v Formatting Darwin Core terms... [8ms]\n"
      [11] "\n"                                       
      [12] "\\ Building xml components...\n"          
      [13] "v Building xml components... [7ms]\n"     
      [14] "\n"                                       
      [15] "v Writing 'data-publish/meta.xml'.\n"     
      [16] "\\ Creating zip folder...\n"              
      [17] "v Creating zip folder... [9ms]\n"         
      [18] "\n"                                       
      [19] "i Writing 'dwc-archive.zip'.\n"           
      [20] "v Writing 'dwc-archive.zip'. [14ms]\n"    
      [21] "\n"                                       

