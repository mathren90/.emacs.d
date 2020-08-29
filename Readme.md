# My emacs configuration
*Disclaimer* I am not a lisp programmer and this is a collection
of dirty hacks mostly inspired by stuff found online. *Do not take this
as an example of how things should be done*.

My setup aims at achieving the following:

 - check if emacs server is already running.
 - If not, start one and open a new frame.
 - If yes, then check if there is an emacs frame already existing.
 - If not create a new one to open. If yes, open the file in a new buffer in the existing frame.

To do so, define in your `~/.bashrc` (or equivalent) the following function:

```
function _emacs
{

    # Selected options for "emacsclient"
    #
    # -c          Create a new frame instead of trying to use the current
    #             Emacs frame.
    #
    # -e          Evaluate the FILE arguments as ELisp expressions.
    #
    # -n          Don't wait for the server to return.
    #
    # -t          Open a new Emacs frame on the current terminal.
    #
    # Note that the "-t" and "-n" options are contradictory: "-t" says to
    # take control of the current text terminal to create a new client frame,
    # while "-n" says not to take control of the text terminal.  If you
    # supply both options, Emacs visits the specified files(s) in an existing
    # frame rather than a new client frame, negating the effect of "-t".

    # check whether an Emacs server is already running
    pgrep -l "^emacs$" > /dev/null

    # otherwise, start Emacs server daemon
    if [ $? -ne 0 ]; then
	emacs -l ~/.emacs.d/init-mathieu.el --daemon
    fi

    # return a list of all frames on $DISPLAY
    emacsclient -e "(frames-on-display-list \"$DISPLAY\")" &>/dev/null

    # open frames detected, so open files in current frame
    if [ $? -eq 0 ]; then
	emacsclient -n -t "$@"
	# no open frames detected, so open new frame
    else
	emacsclient -n -c "$@"
    fi
 
}
```
then define the following alias
```
alias e='/usr/bin/emacs -nw -Q -l ~/.emacs.d/minimal.el' # for quick lookup of things
alias alias emacs='_emacs'
```
and for desktop launcher create a file:
```
~/.local/share/applications/emacsclient.desktop
```
containing:
```
[Desktop Entry]
Name=Emacs client
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=emacsclient --alternate-editor="emacs -l ~/.emacs.d/init-mathieu.el" --create-frame %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupWMClass=Emacs
Keywords=Text;Editor;
```

