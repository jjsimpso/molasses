EXE = molasses
NATIVE_DIST_DIR = molasses_native
WIN_BUILD_DIR = cross/x86_64-win32

all: native

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
	raco cross --target x86_64-win32 --vm cs dist $(WIN_BUILD_DIR)/molasses $(WIN_BUILD_DIR)/$(EXE).exe

clean:
	rm -rf compiled/
	rm -f $(EXE)
	rm -rf $(NATIVE_DIST_DIR)
	rm -rf $(WIN_BUILD_DIR)
