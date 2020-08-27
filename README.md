# ocamlchess

3110 Final Project - by Mitchell Lin, Yanlam Ko, and Conner Swenberg.

Chess made in OCaml for the Terminal. 

How to navigate: 
- To start a game, use "make play" in the terminal.
- Use "start x", where x is the time in seconds alotted for each player.
- Use "view instructions" to read how to play.
- Use "move a b c d" to move a piece from (a,b) to (c,d).
- To castle, use "castle a b c d" to castle between pieces on (a,b) and (c,d).

Description:
- Positions/Coordinates behave like traditional chess positions (a,1) - (h,8)
- Each pieces' movesets are restricted to behave like actual chess pieces.
  - Includes move restrictions like castling and en passant.
- The board is printed in the console and is horizontally flipped after every
move so black/white can view from their respective perspectives.
- Includes check and checkmate detection. Game ends with checkmate.


 
