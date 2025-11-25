EXE = molasses
VERSION = 0.7.5
NATIVE_DIST_DIR = $(EXE)-$(VERSION)
WIN_BUILD_DIR = cross/x86_64-win32
MAC_BUILD_DIR = cross/aarch64-macosx
MACX86_BUILD_DIR = cross/x86_64-macosx
WIN32_BUILD_DIR = cross/i386-win32
LINUX_AARCH64_BUILD_DIR = cross/aarch64-linux

all: $(EXE)

$(EXE): 
	raco make -v main.rkt
	raco exe -v -o $(EXE) main.rkt

native: $(EXE)
	raco distribute $(NATIVE_DIST_DIR) $(EXE)

windows:
# only need to install the dependencies once
	raco cross --target x86_64-win32 --vm cs -j 4 pkg install --skip-installed gui
	raco cross --target x86_64-win32 --vm cs -j 4 pkg install --skip-installed magic
	raco cross --target x86_64-win32 --vm cs -j 4 pkg install --skip-installed sxml
	raco cross --target x86_64-win32 --vm cs -j 4 pkg install --skip-installed mcfly
	raco cross --target x86_64-win32 --vm cs -j 4 pkg install --skip-installed overeasy
	raco cross --target x86_64-win32 --vm cs -j 4 pkg install --skip-installed html-lib
	mkdir -p $(WIN_BUILD_DIR)
	rm -rf compiled/
	raco cross --target x86_64-win32 --vm cs -j 4 make main.rkt 
	raco cross --target x86_64-win32 --vm cs -j 4 exe --gui -o $(WIN_BUILD_DIR)/$(EXE).exe main.rkt 
	raco cross --target x86_64-win32 --vm cs dist $(WIN_BUILD_DIR)/$(EXE)-$(VERSION) $(WIN_BUILD_DIR)/$(EXE).exe

macos:
# only need to install the dependencies once
	raco cross --target aarch64-macosx --vm cs -j 4 pkg install --skip-installed gui
	raco cross --target aarch64-macosx --vm cs -j 4 pkg install --skip-installed magic
	raco cross --target aarch64-macosx --vm cs -j 4 pkg install --skip-installed sxml
	raco cross --target aarch64-macosx --vm cs -j 4 pkg install --skip-installed mcfly
	raco cross --target aarch64-macosx --vm cs -j 4 pkg install --skip-installed overeasy
	raco cross --target aarch64-macosx --vm cs -j 4 pkg install --skip-installed html-lib
	mkdir -p $(MAC_BUILD_DIR)
	rm -rf compiled/
	raco cross --target aarch64-macosx --vm cs -j 4 make main.rkt
	raco cross --target aarch64-macosx --vm cs -j 4 exe --gui -o $(MAC_BUILD_DIR)/$(EXE).app main.rkt
	raco cross --target aarch64-macosx --vm cs dist $(MAC_BUILD_DIR)/$(EXE)-$(VERSION) $(MAC_BUILD_DIR)/$(EXE).app

macos_x86:
# only need to install the dependencies once
	raco cross --target x86_64-macosx --vm cs -j 4 pkg install --skip-installed gui
	raco cross --target x86_64-macosx --vm cs -j 4 pkg install --skip-installed magic
	raco cross --target x86_64-macosx --vm cs -j 4 pkg install --skip-installed sxml
	raco cross --target x86_64-macosx --vm cs -j 4 pkg install --skip-installed mcfly
	raco cross --target x86_64-macosx --vm cs -j 4 pkg install --skip-installed overeasy
	raco cross --target x86_64-macosx --vm cs -j 4 pkg install --skip-installed html-lib
	mkdir -p $(MACX86_BUILD_DIR)
	rm -rf compiled/
	raco cross --target x86_64-macosx --vm cs -j 4 make main.rkt
	raco cross --target x86_64-macosx --vm cs -j 4 exe --gui -o $(MACX86_BUILD_DIR)/$(EXE).app main.rkt
	raco cross --target x86_64-macosx --vm cs dist $(MACX86_BUILD_DIR)/$(EXE)-$(VERSION) $(MACX86_BUILD_DIR)/$(EXE).app

win32:
# only need to install the dependencies once
	raco cross --target i386-win32 --vm bc -j 4 pkg install --skip-installed gui
	raco cross --target i386-win32 --vm bc -j 4 pkg install --skip-installed magic
	raco cross --target i386-win32 --vm bc -j 4 pkg install --skip-installed sxml
	raco cross --target i386-win32 --vm bc -j 4 pkg install --skip-installed mcfly
	raco cross --target i386-win32 --vm bc -j 4 pkg install --skip-installed overeasy
	raco cross --target i386-win32 --vm bc -j 4 pkg install --skip-installed html-lib
	mkdir -p $(WIN32_BUILD_DIR)
	rm -rf compiled/
	raco cross --target i386-win32 --vm bc -j 4 make main.rkt 
	raco cross --target i386-win32 --vm bc -j 4 exe --gui -o $(WIN32_BUILD_DIR)/$(EXE).exe main.rkt 
	raco cross --target i386-win32 --vm bc dist $(WIN32_BUILD_DIR)/$(EXE)-$(VERSION) $(WIN32_BUILD_DIR)/$(EXE).exe

linux-aarch64:
# only need to install the dependencies once
	raco cross --target aarch64-linux --vm cs -j 4 pkg install --skip-installed gui
	raco cross --target aarch64-linux --vm cs -j 4 pkg install --skip-installed magic
	raco cross --target aarch64-linux --vm cs -j 4 pkg install --skip-installed sxml
	raco cross --target aarch64-linux --vm cs -j 4 pkg install --skip-installed mcfly
	raco cross --target aarch64-linux --vm cs -j 4 pkg install --skip-installed overeasy
	raco cross --target aarch64-linux --vm cs -j 4 pkg install --skip-installed html-lib
	mkdir -p $(MAC_BUILD_DIR)
	rm -rf compiled/
	raco cross --target aarch64-linux --vm cs -j 4 make main.rkt
	raco cross --target aarch64-linux --vm cs -j 4 exe --gui -o $(LINUX_AARCH64_BUILD_DIR)/$(EXE) main.rkt
	raco cross --target aarch64-linux --vm cs dist $(LINUX_AARCH64_BUILD_DIR)/$(EXE)-$(VERSION) $(LINUX_AARCH64_BUILD_DIR)/$(EXE)

package: native windows macos macos_x86
	tar -cvaf $(EXE)-$(VERSION)-linux-x86_64.tar.gz $(NATIVE_DIST_DIR)
	cd $(MAC_BUILD_DIR) && tar -cvaf ../../$(EXE)-$(VERSION)-macos-aarch64.tar.gz $(EXE)-$(VERSION)
	cd $(MACX86_BUILD_DIR) && tar -cvaf ../../$(EXE)-$(VERSION)-macos-x86_64.tar.gz $(EXE)-$(VERSION)
	cd $(WIN_BUILD_DIR) && zip -r ../../$(EXE)-$(VERSION)-windows-x86_64.zip $(EXE)-$(VERSION)
#	cd $(WIN32_BUILD_DIR) && zip -r ../../$(EXE)-$(VERSION)-i386-win32.zip $(EXE)-$(VERSION)

clean:
	rm -rf compiled/
	rm -rf magic/compiled/
	rm -rf test/compiled/
	rm -rf third-party/html-parsing/compiled/
	rm -f $(EXE)

cleanall:
	rm -rf compiled/
	rm -rf magic/compiled/
	rm -rf test/compiled/
	rm -rf third-party/html-parsing/compiled/
	rm -f $(EXE)
	rm -rf $(NATIVE_DIST_DIR)
	rm -rf $(WIN_BUILD_DIR)
	rm -rf $(MAC_BUILD_DIR)	
	rm -rf $(MACX86_BUILD_DIR)
	rm -rf $(WIN32_BUILD_DIR)
