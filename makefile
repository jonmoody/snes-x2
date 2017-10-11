snes-x2.smc: main.asm main.link
	wla-65816 -o main.obj main.asm
	wlalink -r main.link snes-x2.smc
