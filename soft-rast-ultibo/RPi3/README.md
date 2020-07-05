# software-rasterizer-ultibo 
this is a port of the book
"Tricks of the 3D Game Programming Gurus-Advanced 3D Graphics and Rasterization"
Created: 1.1.03 by Andre' LaMothe
to C so it can compile and work with the baremetal framework Ultibo

To test you need a raspberry pi 3, the file ultiboImageRPI3.zip extracted
to the SD card root, you should expect to see what appears on the
folder screenshots UltiboT3D.png 

Press
- 'w' wireframe mode
- 'b' backface removal
- 'l' toggle lighting engine completely
- 'a' - ambient light
- 'i' - infinite light
- 'p' - point light
- 'z' z sorting
- '1' previous animation
- '2' next animation
- '3' replay animation
- '4' loop animation

following keys have delay (press longer)

- up forward
- down backward
- left turn left
- right turn right

If you want to recompile you will need ultibo framework and also
the arm-none-eabi tool chain, I would recommend heading to www.ultibo.org
so you can get to know this amazing framework
