"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""
""" Copyright 2012 Heriot-Watt University
"""
""" This file is free software: you can redistribute it and/or modify
""" it under the terms of the GNU General Public License as published by
""" the Free Software Foundation, either version 3 of the License, or
""" (at your option) any later version.
"""
""" This file is distributed in the hope that it will be useful,
""" but WITHOUT ANY WARRANTY; without even the implied warranty of
""" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
""" GNU General Public License for more details.
"""
""" * Authors: John Pirie
""" * Affiliation: Heriot-Watt University, MACS
""" * Date: 14 February 2012
""" * Description:
"""     This is the main file which provides the functionality for vim to be
"""     used as a front end for Skalpel, a type error slicer.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""" colours used to highlight code to indicate type error slices
highlight Blue   ctermbg=blue        guibg=blue
highlight Red    ctermbg=red         guibg=red
highlight Purple ctermbg=magenta     guibg=magenta
highlight Green  ctermbg=green       guibg=green
highlight Orange ctermbg=brown       guibg=brown
highlight Yellow ctermbg=darkyellow  guibg=darkyellow

fun! ProcessJsonFile (jsonFile)
   let s:jsonFileContents = system("cat " . a:jsonFile)
   let s:errorsObject = ParseJSON(s:jsonFileContents)['errors']

   """ here we should open the file names etc, for the moment I assume
   """ the regions are for the file currently open in the current buffer
   let s:errorList = s:errorsObject[0]['regions'][0]['regionList']
   for s:error in s:errorList
      echo s:error
      if s:error['color'] == 'R'
         let s:execLine = "syntax region Red start=/\\%" . s:error['fromLine'] . "l\\%" . s:error['fromColumn'] . "c/ end=/\\%" . s:error['toLine'] . "l\\%" . (s:error['toColumn']+1) . "c/"
         echo s:execLine
         execute(s:execLine)
      elseif s:error['color'] == 'P'
         let s:execLine = "syntax region Purple start=/\\%" . s:error['fromLine'] . "l\\%" . s:error['fromColumn'] . "c/ end=/\\%" . s:error['toLine'] . "l\\%" . (s:error['toColumn']+1) . "c/"
         echo s:execLine
         execute(s:execLine)
      elseif s:error['color'] == 'B'
         let s:execLine = "syntax region Blue start=/\\%" . s:error['fromLine'] . "l\\%" . s:error['fromColumn'] . "c/ end=/\\%" . s:error['toLine'] . "l\\%" . (s:error['toColumn']+1) . "c/"
         echo s:execLine
         execute(s:execLine)
      elseif s:error['color'] == 'Y'
         let s:execLine = "syntax region Yellow start=/\\%" . s:error['fromLine'] . "l\\%" . s:error['fromColumn'] . "c/ end=/\\%" . s:error['toLine'] . "l\\%" . (s:error['toColumn']+1) . "c/"
         echo s:execLine
         execute(s:execLine)
     else
         echo "not red!"
      endif
""" syntax region Blue start=/\%4l\%3c/ end=/\%5l\%6c/
   endfor
endfun

""" waits for files to be output by the Skalpel analysis-engine and processes them
fun! WaitForJsonFiles (tempDir)
   """ the file which indicates that the analysis engine has finished
   let s:finishedFile = a:tempDir . "skalpel-vim-finished"

   """ the value of the current json output file that we are waiting for next
   let s:currentJsonNumber = 1

   while 1
      """ we also check that the last output file has actually been processed
      if filereadable(s:finishedFile) && !filereadable(a:tempDir . "skalpel-vim-" . s:currentJsonNumber)
         """ the analysis engine has finished processing the user's code
         """echo "Detected the finished file, stopping."
         break
      else
         """ analysis engine is still running
         if filereadable(a:tempDir . "skalpel-vim-" . s:currentJsonNumber)
            echo "Detected a new output file!"
            call ProcessJsonFile (a:tempDir . "skalpel-vim-" . s:currentJsonNumber)
            let s:currentJsonNumber = s:currentJsonNumber + 1
	 endif
      endif
      sleep 1 "sleep for a second before checking for files again
   endwhile
endfun

""" the function that the user would run to get type error slices for their code
fun! SkalpelRun ()
   let s:currentFile = expand("%:p")
   let s:currentFileExtension =  strpart(s:currentFile, strlen(s:currentFile)-3, strlen(s:currentFile))
   let s:basisLocation = "~/repos/skalpel/lib/basis.sml"

   """ check that the current file has a .sml extension. If it doesn't, then alert the user
   if s:currentFileExtension != "sml"
      echo "This file does not have a '.sml' extension."
   else
      """ set up a new temp directory for the analysis engine to place slices into
      let s:tempDirSplit = split(tempname(), "/")
      let s:tempDir      = "/".join(remove(s:tempDirSplit, 0, len(s:tempDirSplit)-2), "/")."/"

      """ execute the analysis engine, tell it to use s:tempDir as a place to put the files
      """silent execute ("!skalpel -b 2 " . s:basisLocation . " -j " . s:tempDir . "skalpel-vim " . s:currentFile . " > ~/FILE_HERE &") | redraw!
      silent execute ("!skalpel -j " . s:tempDir . "skalpel-vim " . s:currentFile . " > ~/FILE_HERE &") | redraw!

      """ wait for the json files to be output so that we can process them
      call WaitForJsonFiles(s:tempDir)
   endif
endfun

command SkalpelStart :call SkalpelRun()

map <F6> <Esc>:SkalpelStart<CR>


