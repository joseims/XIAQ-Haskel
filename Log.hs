import System.IO
import System.Directory

save content = do
  fileHandle <- openFile "log.txt" ReadWriteMode
  currentContent <- hGetContents fileHandle
  writeFile "log2.txt" ((currentContent) ++ content)
  hClose fileHandle
  renameFile "log2.txt" "log.txt"