# tidedup

##### Author: Brett "xNWP" Anthony \<nwpbusiness@gmail.com\>

This Haskell program takes from the std input a newline separated list of marked tiles
exported from the RuneLite TileIndicators plugin, it then removes duplicate tiles (tiles
residing on the same x y z) and prints out the unique tiles in a way that it may be copy-pasted
and imported back into RuneLite.

#### Usage (Windows)
&nbsp;&nbsp;`cat .\tilelist.txt | .\tidedup.exe`\
&nbsp;&nbsp;`cat .\tilelist.txt | .\tidedup.exe > uniquetiles.txt`

#### Compiling
&nbsp;&nbsp;`ghc tidedup.hs`