# MASM32_SnakeGame
"MASM32_SnakeGame" is a common electronic game and my first program written in assembly language.  
The project took about 2 days.  

Programming language (Assembly Language) : MASM32 (Use irvine32 library)  
Language : English  
  
Download program : https://github.com/KILNETA/MASM32_SnakeGame/blob/master/MASM32_SnakeGame.exe
  
# Introduction for Programming  
In this program, I directly use Stack as Snake's temporary data storage. The purpose is to try to see the temporary storage management that is biased towards dynamic programming (similar to Vector), and because the position movement of Snake will affect the Stack. To change the data, I wrote a FIFO function that makes a Stack similar to a Queue.  
  
Also, my basic knowledge of assembly language may not be very sufficient. This is my implementation of the idea.  
  
# Controls
| key | Actions | Snake Head |
| --- | --- | --- |
| **↑** | Up | **^** |
| **↓** | Down | **v** |
| **←** | Left | **<** |
| **→** | Right | **>** |  
  
* **Snake can through the wall.**  
  
* **"X" is Snack.**  
  
# Developer  
* @KILNETA  

# Screenshots   
| ![image](https://github.com/KILNETA/MASM32_SnakeGame/assets/47145154/74e3f168-71de-4eb0-aa21-7af1659651ad) | ![image](https://github.com/KILNETA/MASM32_SnakeGame/assets/47145154/2e945b40-2b39-4c47-8846-380900efec88) |
| --- | --- |
| ![image](https://github.com/KILNETA/MASM32_SnakeGame/assets/47145154/aabecfdd-f36c-4f79-8eb7-f14b8eeb5f48) | ![image](https://github.com/KILNETA/MASM32_SnakeGame/assets/47145154/d6ad537d-af5a-4d99-af0e-b0fc890fe160) |

