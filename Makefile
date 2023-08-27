# -----------------------------------------------------------------------------

megabuild		= 1
useetherload	= 1
finalbuild		= 1
attachdebugger	= 0

# -----------------------------------------------------------------------------

MAKE			= make
CP				= cp
MV				= mv
RM				= rm -f
CAT				= cat
PACKIFFL		= packiffl

SRC_DIR			= ./src
UI_SRC_DIR		= ./src/ui
UIELT_SRC_DIR	= ./src/ui/uielements
DRVRS_SRC_DIR	= ./src/drivers
EXE_DIR			= ./exe
BIN_DIR			= ./bin

# mega65 fork of ca65: https://github.com/dillof/cc65
AS				= ca65mega
ASFLAGS			= -g -D finalbuild=$(finalbuild) -D megabuild=$(megabuild) -D useetherload=$(useetherload) --cpu 45GS02 -U --feature force_range -I ./exe
LD				= ld65
C1541			= c1541
CC1541			= cc1541
SED				= sed
PU				= pucrunch
BBMEGA			= b2mega
LC				= crush 6
GCC				= gcc
MC				= MegaConvert
MEGAADDRESS		= megatool -a
MEGACRUNCH		= megatool -c
MEGAIFFL		= megatool -i
MEGAMOD			= MegaMod
EL				= etherload -i 192.168.1.255
XMEGA65			= H:\xemu\xmega65.exe
MEGAFTP			= mega65_ftp -i 192.168.1.255

CONVERTBREAK	= 's/al [0-9A-F]* \.br_\([a-z]*\)/\0\nbreak \.br_\1/'
CONVERTWATCH	= 's/al [0-9A-F]* \.wh_\([a-z]*\)/\0\nwatch store \.wh_\1/'

CONVERTVICEMAP	= 's/al //'

.SUFFIXES: .o .s .out .bin .pu .b2 .a

default: all

OBJS = $(EXE_DIR)/boot.o $(EXE_DIR)/main.o

BINFILES  = $(BIN_DIR)/font_chars1.bin
BINFILES += $(BIN_DIR)/glyphs_chars1.bin
BINFILES += $(BIN_DIR)/glyphs_pal1.bin
BINFILES += $(BIN_DIR)/cursor_sprites1.bin
BINFILES += $(BIN_DIR)/kbcursor_sprites1.bin
BINFILES += $(BIN_DIR)/cursor_pal1.bin
BINFILES += $(BIN_DIR)/data0a00.bin
BINFILES += $(BIN_DIR)/data4000.bin
BINFILES += $(BIN_DIR)/ycbcc2rgb.bin

BINFILESADDR  = $(BIN_DIR)/font_chars1.bin.addr
BINFILESADDR += $(BIN_DIR)/glyphs_chars1.bin.addr
BINFILESADDR += $(BIN_DIR)/glyphs_pal1.bin.addr
BINFILESADDR += $(BIN_DIR)/cursor_sprites1.bin.addr
BINFILESADDR += $(BIN_DIR)/kbcursor_sprites1.bin.addr
BINFILESADDR += $(BIN_DIR)/cursor_pal1.bin.addr
BINFILESADDR += $(BIN_DIR)/data0a00.bin.addr
BINFILESADDR += $(BIN_DIR)/data4000.bin.addr
BINFILESADDR += $(BIN_DIR)/ycbcc2rgb.bin.addr

# % is a wildcard
# $< is the first dependency
# $@ is the target
# $^ is all dependencies

# -----------------------------------------------------------------------------

$(BIN_DIR)/font_chars1.bin: $(BIN_DIR)/font.bin
	$(MC) $< cm1:1 d1:0 cl1:10000 rc1:0

$(BIN_DIR)/glyphs_chars1.bin: $(BIN_DIR)/glyphs.bin
	$(MC) $< cm1:1 d1:0 cl1:14000 rc1:0

$(BIN_DIR)/cursor_sprites1.bin: $(BIN_DIR)/cursor.bin
	$(MC) $< cm1:1 d1:0 cl1:14000 rc1:0 sm1:1

$(BIN_DIR)/kbcursor_sprites1.bin: $(BIN_DIR)/kbcursor.bin
	$(MC) $< cm1:1 d1:0 cl1:14000 rc1:0 sm1:1

$(EXE_DIR)/boot.o:	$(SRC_DIR)/boot.s \
					$(SRC_DIR)/main.s \
					$(SRC_DIR)/irqload.s \
					$(SRC_DIR)/decruncher.s \
					$(SRC_DIR)/macros.s \
					$(SRC_DIR)/mathmacros.s \
					$(SRC_DIR)/jpg.s \
					$(SRC_DIR)/jpgrender.s \
					$(SRC_DIR)/uidata.s \
					$(SRC_DIR)/uitext.s \
					$(DRVRS_SRC_DIR)/mouse.s \
					$(DRVRS_SRC_DIR)/sdc.s \
					$(DRVRS_SRC_DIR)/keyboard.s \
					$(UI_SRC_DIR)/uimacros.s \
					$(UI_SRC_DIR)/uicore.s \
					$(UI_SRC_DIR)/uirect.s \
					$(UI_SRC_DIR)/uidraw.s \
					$(UI_SRC_DIR)/ui.s \
					$(UI_SRC_DIR)/uidebug.s \
					$(UI_SRC_DIR)/uimouse.s \
					$(UI_SRC_DIR)/uikeyboard.s \
					$(UIELT_SRC_DIR)/uielement.s \
					$(UIELT_SRC_DIR)/uiroot.s \
					$(UIELT_SRC_DIR)/uidebugelement.s \
					$(UIELT_SRC_DIR)/uihexlabel.s \
					$(UIELT_SRC_DIR)/uiwindow.s \
					$(UIELT_SRC_DIR)/uibutton.s \
					$(UIELT_SRC_DIR)/uiglyphbutton.s \
					$(UIELT_SRC_DIR)/uicbutton.s \
					$(UIELT_SRC_DIR)/uictextbutton.s \
					$(UIELT_SRC_DIR)/uicnumericbutton.s \
					$(UIELT_SRC_DIR)/uiscrolltrack.s \
					$(UIELT_SRC_DIR)/uislider.s \
					$(UIELT_SRC_DIR)/uilabel.s \
					$(UIELT_SRC_DIR)/uinineslice.s \
					$(UIELT_SRC_DIR)/uilistbox.s \
					$(UIELT_SRC_DIR)/uifilebox.s \
					$(UIELT_SRC_DIR)/uicheckbox.s \
					$(UIELT_SRC_DIR)/uiradiobutton.s \
					$(UIELT_SRC_DIR)/uiimage.s \
					$(UIELT_SRC_DIR)/uitextbox.s \
					$(UIELT_SRC_DIR)/uidivider.s \
					$(UIELT_SRC_DIR)/uitab.s \
					$(UIELT_SRC_DIR)/uigroup.s \
					Makefile Linkfile
	$(AS) $(ASFLAGS) -o $@ $<

$(EXE_DIR)/boot.prg.addr: $(EXE_DIR)/boot.o Linkfile
	$(LD) -Ln $(EXE_DIR)/boot.maptemp --dbgfile $(EXE_DIR)/boot.dbg -C Linkfile -o $@ $(EXE_DIR)/boot.o
	$(MEGAADDRESS) $(EXE_DIR)/boot.prg 2001
	$(SED) $(CONVERTVICEMAP) < $(EXE_DIR)/boot.maptemp > boot.map
	$(SED) $(CONVERTVICEMAP) < $(EXE_DIR)/boot.maptemp > boot.list

$(BIN_DIR)/alldata.bin: $(BINFILESADDR)
	$(MEGAADDRESS) $(BIN_DIR)/font_chars1.bin       00010000
	$(MEGAADDRESS) $(BIN_DIR)/glyphs_chars1.bin     00014000
	$(MEGAADDRESS) $(BIN_DIR)/glyphs_pal1.bin       0000c700
	$(MEGAADDRESS) $(BIN_DIR)/cursor_sprites1.bin   0000ce00
	$(MEGAADDRESS) $(BIN_DIR)/kbcursor_sprites1.bin 0000cf00
	$(MEGAADDRESS) $(BIN_DIR)/cursor_pal1.bin       0000ca00
	$(MEGAADDRESS) $(BIN_DIR)/data0a00.bin          00000400
	$(MEGAADDRESS) $(BIN_DIR)/data4000.bin          00000a00
	$(MEGAADDRESS) $(BIN_DIR)/ycbcc2rgb.bin         00008100
	$(MEGAIFFL) $(BINFILESADDR) $(BIN_DIR)/alldata.bin

$(EXE_DIR)/megajpg.d81: $(EXE_DIR)/boot.prg.addr $(BIN_DIR)/alldata.bin
	$(RM) $@
	$(CC1541) -n "megajpg" -i " 2023" -d 19 -v\
	 \
	 -f "megajpg" -w $(EXE_DIR)/boot.prg.addr \
	 -f "megajpg.iffl" -w $(BIN_DIR)/alldata.bin \
	$@

# -----------------------------------------------------------------------------

run: $(EXE_DIR)/megajpg.d81

ifeq ($(megabuild), 1)

ifeq ($(useetherload), 1)

	$(MEGAFTP) -c "put D:\Mega\MegaJPGFC\exe\megajpg.d81 megajpg.d81" -c "quit"
	$(EL) -m MEGAJPG.D81 -r $(EXE_DIR)/boot.prg.addr
#	$(EL) -b 02001 --offset ff --jump 2100 $(EXE_DIR)/boot.prg

else

	mega65_ftp.exe -l COM3 -s 2000000 -c "cd /" \
	-c "put D:\Mega\MegaJPGFC\exe\megajpg.d81 megjpg.d81"

	m65 -l COM3 -F
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'mount "megajpg.d81"'
	m65 -l COM3 -T 'load "$$"'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'load "boot"'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'run'

endif

ifeq ($(attachdebugger), 1)
	m65dbg --device /dev/ttyS2
endif

else

#	cmd.exe /c $(XMEGA65) -mastervolume 50 -autoload -8 $(EXE_DIR)/disk.d81
	cmd.exe /c $(XMEGA65) -autoload -8 $(EXE_DIR)/megajpg.d81

endif

clean:
	$(RM) $(EXE_DIR)/*.*
	$(RM) $(EXE_DIR)/*

