# Molasses
Molasses is a multi-tabbed browser for the "slow" internet, including Gopher and eventually Gemini. The name comes from an expression in the Southern United States, "Slow as molasses!" Molasses's design goals are:

* **Done** Support easy keyboard navigation of Gopher menus similar to Lynx.
* **Done** Support multiple tabs similar to modern web browsers.
* **Done** View images within the application(JPEG, PNG, and GIF are supported).
* **Done** Download binary files not viewable within Molasses.
* **Done** Separate browsing history for each tab and back button.
* **Done** Bookmarks
* Allow optional TLS connections for Gopher.
* **Partial** Gemini protocol support.
* Simple inline HTML viewing without using an external application.

## Platform
Molasses is a Racket GUI application, which means it will run on Windows, MacOS X, and Linux. If Racket is already installed on a system, Molasses can be launched from the source with the command 'racket main.rkt'. Binary executables which include the Racket runtime will be made available here in the future.

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
