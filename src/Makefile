EXE = molasses
VERSION = 0.6.1
NATIVE_DIST_DIR = $(EXE)-$(VERSION)
WIN_BUILD_DIR = cross/x86_64-win32
MAC_BUILD_DIR = cross/x86_64-macosx

all: $(EXE)

$(EXE): 
	raco make -v main.rkt
	raco exe -v -o $(EXE) main.rkt

native: $(EXE)
	raco distribute $(NATIVE_DIST_DIR) $(EXE)

windows:
# only need to install the dependencies once
#	raco cross --target x86_64-win32 --vm cs -j 4 pkg install --deps search-auto
#	raco cross --target x86_64-win32 --vm cs -j 4 pkg install gui
	mkdir -p $(WIN_BUILD_DIR)
	rm -rf compiled/
	raco cross --target x86_64-win32 --vm cs -j 4 exe -o $(WIN_BUILD_DIR)/$(EXE).exe main.rkt 
	raco cross --target x86_64-win32 --vm cs dist $(WIN_BUILD_DIR)/$(EXE)-$(VERSION) $(WIN_BUILD_DIR)/$(EXE).exe

macos:
# only need to install the dependencies once
#	raco cross --target x86_64-macosx --vm cs -j 4 pkg install --deps search-auto
#	raco cross --target x86_64-macosx --vm cs -j 4 pkg install gui
	mkdir -p $(MAC_BUILD_DIR)
	rm -rf compiled/
	raco cross --target x86_64-macosx --vm cs -j 4 exe -o $(MAC_BUILD_DIR)/$(EXE) main.rkt 
	raco cross --target x86_64-macosx --vm cs dist $(MAC_BUILD_DIR)/$(EXE)-$(VERSION) $(MAC_BUILD_DIR)/$(EXE)

clean:
	rm -rf compiled/
	rm -f $(EXE)

clean-all:
	rm -rf compiled/
	rm -f $(EXE)
	rm -rf $(NATIVE_DIST_DIR)
	rm -rf $(WIN_BUILD_DIR)
	rm -rf $(MAC_BUILD_DIR)
