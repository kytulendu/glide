# Makefile for use with Windows 98 DDK

!INCLUDE $(BASEDIR)\inc\master.mk

DEVICE   = fxmemmap
ASM      = ML
RC       = $(BASEDIR)\bin\win98\bin16\rc
LINKER   = LINK
AINCS    = -I. -I$(INC32)
AOPTS    = -W2 -c -Cx -Zm -nologo -coff
ADEFS    = -DBLD_COFF -DMASM6 -DMINIVDD=1 -DVGASWITCH -DIS_32
RDEFS    =
ROPTS    = -r -I$(INC16)
LFLAGS   = /nologo /vxd /nod

!ifdef DEBUG
# Enable for debug
ADEFS = $(ADEFS) -DDEBLEVEL=1 -DDEBUG
AOPTS = $(AOPTS) -Zi -Zd
LFLAGS = $(LFLAGS) /DEBUG /DEBUGTYPE:CV /pdb:"$(DEVICE).pdb"
!endif

AFLAGS   = $(ADEFS) $(AOPTS) $(AINCS)

all: $(DEVICE).vxd

debug: all

release: all

$(DEVICE).obj: $*.asm
   $(ASM) $(AFLAGS) $*.asm

$(DEVICE).res: $*.rc
   $(RC) $(RDEFS) $(ROPTS) $*.rc

$(DEVICE).vxd: $*.obj $*.def $*.res
!ifdef DEBUG
   $(LINKER) $(LFLAGS) /out:$@ /map:$*.map /def:$*.def $*.obj
   adrc2vxd $*.vxd $*.res
   mapsym -s -o $*.sym $*.map
!else
   $(LINKER) $(LFLAGS) /out:$@ /map:$*.map /def:$*.def $*.obj
   adrc2vxd $*.vxd $*.res
!endif

#vxdtest.exe: $*.c
#   cl /W3 /GX /Zi /Od /DWIN32 /D_DEBUG /D_CONSOLE /FR /nologo $*.c

clean:
   del *.res
   del *.obj
   del *.ilk
   del *.sbr
   del *.pch
   del *.pdb
   del *.map
   del *.exp
   del *.lib
   del *.exe
   del *.vxd
