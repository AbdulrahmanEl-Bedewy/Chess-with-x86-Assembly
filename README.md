<p align="center">

  <img src="https://cdn.iconscout.com/icon/free/png-256/chess-logo-1921892-1624674.png" alt="chess" width="250"/>

</p>

<hr>

# Chess-with-x86-Assembly
A chess game written in x86 assembly language. This project was done as a part of the Microprocessor Systems-1 course.

## Gameplay
The game could be played through different computers connected to the same network. The game is based on speed chess, meaning that the players do not take turns to play. The game's only rule is that after you move a piece you have a 3 seconds cooldown on that specific piece. The win condition is a player taking the other's king.

https://user-images.githubusercontent.com/62207434/226058871-c84cac10-ecd9-4404-8b07-bb4d8b735797.mp4


## How to run
Download the project. Download the [DOSBox](https://www.dosbox.com/) emulator. Open DOSBox options file and make the cycles 20k. Open the DOSBox and mount the project directory. Then run the following commands:
```bash
mount c <path-to-project-directory>
c:
masm graphics;
masm logic;
masm chat;
masm main;
link main+graphics+logic+chat;
```
Now you can run the game by typing `main` in the DOSBox.

To setup multiplayer, you need to run the game on two different DOSBox instances. Make sure both the instances are connected to the same network. Then, setup the DOSBox options file of both the instances. Add the following lines to the DOSBox on the server side:
```
serial1=nullmodem
```
Add the following lines to the DOSBox on the client side:
```
serial1=nullmodem server:<Insert IP of server on network>
```
You can retrieve the server's IP by typing `ipconfig` in the command prompt on that computer. Now, you can run the game on both the instances. Make sure to run the game on the server side first. Then, run the game on the client side. The game will start on the server side. Now, you can play the game on both the instances. The game will be synced on both the instances.

## Controls & How to play
The game is played using the keyboard. The controls are as follows:
- Arrow keys to move the cursor.
- 0 numpad to select a piece.
You can find that the game also has a chat feature. The chat feature is used to communicate with the other player. The chat feature is accessed either as a separate mode or as a part of the game. You can just start typing while playing the game. The chat is displayed on the side of the game.

## Screenshots
![image](https://user-images.githubusercontent.com/62207434/226057282-2f03df51-b4c8-477c-a5fb-6ca98a0842be.png)
![image](https://user-images.githubusercontent.com/62207434/226057328-8efddca6-2c83-498f-8af5-3af5ec2bba16.png)
![image](https://user-images.githubusercontent.com/62207434/226057400-2c3c63a1-1a0f-41b0-8652-b34a3e9abc02.png)
![image](https://user-images.githubusercontent.com/62207434/226057460-4fac9379-aceb-480c-a63d-60c1b039014a.png)
![image](https://user-images.githubusercontent.com/62207434/226057482-f1a1c8f2-7138-459e-a46f-f5cf6705e46a.png)
![image](https://user-images.githubusercontent.com/62207434/226057588-ced60590-dbb6-401a-a8f3-e7ec0be1aa44.png)
![image](https://user-images.githubusercontent.com/62207434/226057634-94f6db10-b1ac-4044-80fa-d99799037f11.png)
