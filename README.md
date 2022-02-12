# Molasses
Molasses is a multi-tabbed browser for the "slow" internet, including Gopher and Gemini. The name comes from an expression in American English, "Slow as molasses!" Molasses's design goals are:

* **Done** Support easy keyboard navigation of Gopher menus similar to Lynx.
* **Done** Support multiple tabs similar to modern web browsers.
* **Done** View images within the application(JPEG, PNG, and GIF are supported).
* **Done** Download binary files not viewable within Molasses.
* **Done** Separate browsing history for each tab and back button.
* **Done** Bookmarks
* **Done** Open HTTP URLs(in Gopher, selectors starting with "GET /" or "URL:") in external browser.
* Allow optional TLS connections for Gopher.
* **Done** Gemini protocol support.
* Simple inline HTML viewing without using an external application.
* **Done** Give option to open 'd' and 'P' item types in external application.

## Platform
Molasses is a Racket GUI application, which means it will run on Windows, MacOS X, and Linux. If Racket is already installed on a system, Molasses can be launched from the source with the command 'racket main.rkt'. Binary executables which include the Racket runtime are available in the Releases section.

Racket can be downloaded from [The Racket Homepage](https://racket-lang.org)

## Usage
Gopher menu navigation is based on Lynx and consists of the following commands:
* Up and Down arrows move the selection between menu items.
* Right arrow or Return follows the currently selected menu item link. Left mouse click also follows links/menu items.
* Left arrow goes back to the previous page.There is also a back button on the Toolbar.
* Page Up/Down scrolls the page without changing the selection, as does the mouse scroll wheel.
* In text files, as opposed to Gopher menus, Up and Down arrows scroll the page line-by-line.

Tabs are saved by default and restored in the next session.

## Screenshots
![](https://github.com/jjsimpso/molasses/blob/main/doc/molasses-ss1.png)
