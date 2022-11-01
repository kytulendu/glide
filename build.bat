@echo off
cd glide2x
make -f Makefile.win32 FX_GLIDE_HW=sst1 USE_X86=1
if %ERRORLEVEL% neq 0 goto exit
rem make -f Makefile.win32 FX_GLIDE_HW=sst96 USE_X86=1
rem if %ERRORLEVEL% neq 0 goto exit
make -f Makefile.win32 FX_GLIDE_HW=cvg USE_X86=1 USE_MMX=1 USE_3DNOW=1
if %ERRORLEVEL% neq 0 goto exit
cd ..
cd glide3x
make -f Makefile.win32 FX_GLIDE_HW=sst1 USE_X86=1
if %ERRORLEVEL% neq 0 goto exit
rem make -f Makefile.win32 FX_GLIDE_HW=sst96 USE_X86=1
rem if %ERRORLEVEL% neq 0 goto exit
make -f Makefile.win32 FX_GLIDE_HW=cvg USE_X86=1 USE_MMX=1 USE_3DNOW=1
:exit
cd ..