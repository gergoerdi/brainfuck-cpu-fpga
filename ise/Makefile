MAIN		= Brainfuck
TOP		= Brainfuck
VHDL_MODS	=
HS_MODS		= CPU

LAVADIR		= ../src
HS_SRCS		= $(patsubst %, $(LAVADIR)/%.hs, $(HS_MODS) $(TOP))
SRCDIR		= src
FPGA		= xc3s500e-vq100-5
XSTDIR		= xst

UCF		= $(patsubst %, $(SRCDIR)/%.ucf, $(MAIN))
GEN_VHDLS	= $(patsubst %, $(SRCDIR)/%.vhdl, lava-prelude $(MAIN))
VHDL_SRCS	= $(patsubst %, $(SRCDIR)/%.vhdl, $(VHDL_MODS))
VHDLS		= $(GEN_VHDLS) $(VHDL_SRCS)

GEN_EXTS	= bgn bit bld drc ncd ngc ngd ngr pad par pcf prj ptwx syr unroutes ut xpi xst
GEN_MAP_EXTS	= map mrp ncd ngm
GENS		= $(GEN_VHDLS) \
		  $(patsubst %, $(MAIN).%, $(GEN_EXTS)) \
		  $(patsubst %, $(MAIN)_map.%, $(GEN_MAP_EXTS)) \
		  $(patsubst %, $(MAIN)_%, bitgen.xwbt ngdbuild.xrpt pad.csv pad.txt summary.xml usage.xml xst.xrpt) \
		  $(TOP).lso $(TOP)_map.xrpt $(TOP)_par.xrpt \
		  usage_statistics_webtalk.html webtalk.log

XILINX_ROOT	= $(HOME)/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64
PAPILIO_PROG	= $(HOME)/bin/papilio-prog
RUNGHC		= $(HOME)/prog/haskell/ghc/sandbox/bin/runghc-7.6.1

all: $(MAIN).bit

clean:
	rm -f $(GENS)
	rm -rf _xmsgs ngo $(wildcard xlnx_auto_*_xdb) xst

$(GEN_VHDLS): $(HS_SRCS)
	$(RUNGHC) -i$(LAVADIR) $(LAVADIR)/$(TOP).hs

$(MAIN).prj: Makefile
	echo $(foreach VHDL_SRC,$(VHDLS),'vhdl work "$(VHDL_SRC)"\n') > $@

$(MAIN).xst: Makefile xst.in
	sed -e 's/@MAIN@/$(MAIN)/g;s/@TOP@/$(TOP)/g;' < xst.in > $@

$(MAIN).ut: Makefile ut.in
	sed -e '' < ut.in > $@

$(MAIN).ngc: $(MAIN).prj $(MAIN).xst $(VHDLS)
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/xst -ifn $(MAIN).xst -ofn $(MAIN).syr

$(MAIN).ngd: $(MAIN).ngc $(UCF)
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/ngdbuild -dd ngo -nt timestamp -uc $(UCF) -p $(FPGA) $(MAIN).ngc $(MAIN).ngd

$(MAIN)_map.ncd $(MAIN).pcf: $(MAIN).ngc $(MAIN).ngd
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/map -p $(FPGA) -cm area -ir off -pr off -c 100 -o $(MAIN)_map.ncd $(MAIN).ngd $(MAIN).pcf

$(MAIN).ncd: $(MAIN)_map.ncd $(MAIN).pcf
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/par -w -ol high -t 1 $(MAIN)_map.ncd $(MAIN).ncd $(MAIN).pcf

$(MAIN).bit: $(MAIN).ut $(MAIN).ncd
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/bitgen -f $(MAIN).ut $(MAIN).ncd

install: $(MAIN).bit
	$(PAPILIO_PROG) -f $(MAIN).bit
