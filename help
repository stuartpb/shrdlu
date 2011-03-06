




         Instructions for Running SHRDLU

     SHRDLU can be in 4 basic states, COMMAND, READY, RUN,
and REQUEST.  It is initially in READY when loaded.
***
******COMMAND STATE
***
     In this state, SHRDLU expects the user to type a command.
It lets you know this by typing ">>>".
A command is a line containing one or more words, separated by 
spaces and terminated by two carriage returns (<CR>).  The first
word must be one of the three words SHOW, TELL, or GO.  The
SHOW command is used to ask the system to show such things
as definitions, structures it is building, and the states of various
parameters and switches.  TELL is used to tell the system new
definitions and settings.
     After executing a COMMAND, the system is ready for
another one, and prompts by saying >>>.
You can leave COMMAND state by typing T,NIL, or $P (<alt mode>-P)
instead of a command.  This will cause the program to continue
whatever it was doing before it entered COMMAND
state, or to go to READY state if it was not already in the
process of analyzing a sentence.  If instead, you type the
command "GO", it will drop the sentence it is working on, and go 
into READY state for a new one.
      Of course, the COMMAND state is just a slightly-fudged
LISP listen loop, so you can type any atom or S-expression at it
to be evaluted, go from it into the LISP editor, define new functions,etc.
****
******COMMAND FORMATS
****
     The SHOW and TELL commands are based on  trees
(one tree for each).  The first word of the command is SHOW
or TELL, the second names a node in the corresponding tree,
and the rest are any arguments appropriate to the action
at that node.
For example, the command:

SHOW FUNCTION MEET

will display the contents of the LISP function "MEET".

SHOW SHOW

displays the "SHOW" tree, while for the "TELL" tree, you type

SHOW TELL

     If all of the arguments are not specified, the system will
request more.  For example, typing:
SHOW FUNCTION
would cause it to type out:
FUNCTION:
requesting the name of a function from the user.  It is then
in REQUEST state (see below.)
     Non-terminal nodes of the tree may or may not have corresponding
actions.  For example, the command

TELL PLANNER OFF

causes the entire batch of PLANNER tracing devices to be turned off
at once, even though there are subnodes of the PLANNER node which
can be used to turn individual features on and off selectively.
If there is no action associated with such a node, the system will
ask the user to select one of its subnodes.
     If you type "SHOW" or "TELL" followed by two carriage return,
it will guide you through the choices, using REQUESTS
(see below).
     Typing HELP <CR> <CR> will allow you to see this message or 
a shorter version which contains just the most salient details of the four states.
*****
*****REQUEST STATE
*****
     SHRDLU can request two kinds of information from
the user.  If it wants him to CHOOSE between a set of alternatives,
it will end the request with a ?.  If it wants him to SPECIFY 
a name or list of names, it will end it with a :.
     For a CHOOSE, all it needs is enough information to decide
between the alternatives.  Begin typing your choice, and when it
is complete enough, type a <period>.  If you type fewer
letters than necessary (e.g. typing just a P, when PLANNER
and PARSING are among the choices) it will do nothing and wait
for you to continue.  If you type more than necessary it doesn't
matter.
     For a SPECIFY, type the required response, terminated by a
<CR>.  If you type a <LF> with nothing else, it will take some
default response which is appropriate to the action (For example,
typing a <LF> in response to a request for which properties of an
atomare to be displayed will have the effect of displaying
all of them.
    For either SPECIFY or CHOOSE, you can get more information on
what is expected by typing a ?<CR>.  It will then give you the 
request again.  Typing QUIT<CR> at a "SPECIFY" REQUEST or QUIT
<CR> at a "CHOOSE" REQUEST will cause the entire command
of which it was a part to be discarded without finishing,
returning to COMMAND state.
*****
******READY STATE
*****
     The READY state is entered only when a new English sentence is to
be input.  You can tell you are in it when the sytem types
READY
Respond by typing in an English sentence in normal punctuation
(i.e. ending with a question mark or period).
The system will automatically begin processing it, entering
RUN state.  To get into a COMMAND state while entering a sentence,
type  <alt-mode> <alt-mode>.
*****
******RUN STATE
*****
     Whenever a sentence is input, the system begins to RUN.  It
will stop at selected places, entering COMMAND state so the user can
SHOW things and TELL it things before continuing.  There are various
TELL commands which explain how to change these stopping points.
When a T,NIL, or <alt>-P is typed at the COMMAND state, the system returns to RUN
and continues.
*****
     Any word which appears in the SHOW or TELL trees can be
abbreviated by typing its first two letters.  For example,
our first command above could have been abbreviated as:
SH FU MEET
Note that arguments cannot be abbreviated since the system has no
list to check the abbreviations against.  This is also true
of responses to a "SPECIFY" REQUEST.  Responses to a "CHOOSE" REQUEST
are abbreviated by typing any initial letter string followed by
<period> as described above.
*****
*****
