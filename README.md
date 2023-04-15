# SteamPipeEmu

My unfinished library for emulating steamclient.dll, which I developed for one project, but later I abandoned the development due to the fact that I left the project.

# Description

In essence, this is the library that intercepts the "Named Pipe" Steam channel, which is used for communication between Steam and the game being launched. I didn't want to implement SteamClient interfaces the way other emulators do, and it seemed to me that Steam pipe hook would be much simpler and more elegant (no), plus I was always interested in how this pipe functions, and there is no information on the Internet how it works.

# How it works?

When loaded into the game, the library launches a thread that scans the shared memory buffer where the Named Pipe commands are written, and also applies some Steam DLLs hooks. The TSteamPipeServer class is responsible for pipe hook, it's a class that can read and send Steam Pipe commands.

# Usage

The project rather serves for educational purposes, and it is difficult to apply it somewhere in its current form. With it, I was able to launch CS:GO without Steam using SteamPipeEmu and see the Main Menu, but that's it. I planned to at least finish generating the Steam Certificate data in order to connect to No-Steam servers, but I abandoned the project early.

However, with the help of this project, you can study in detail how the Steam Pipe works: how commands are read, written, and executed, and communication between Steam and the game as a whole, plus Steam callbacks. I think this is very useful and interesting information.

# License

You are free to use this project and its knowledge in your work, however, I would be very grateful if you would refer to me.