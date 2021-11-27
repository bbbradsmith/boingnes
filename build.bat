@del temp\*.s
@del temp\*.o
@del temp\boing.nes
@del temp\boing.dbg
@del temp\boing.map

cc65\bin\cc65 -o temp\boing.c.s -O -T -g boing.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\boing.c.o -g temp\boing.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\boing.o -g boing.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\boing.nes -m temp\boing.map --dbgfile temp\boing.dbg -C boing.cfg temp\boing.o temp\boing.c.o temp\runtime.lib
@IF ERRORLEVEL 1 GOTO error

@echo.
@echo.
@echo Build successful!
@pause
@GOTO end
:error
@echo.
@echo.
@echo Build error!
@pause
:end