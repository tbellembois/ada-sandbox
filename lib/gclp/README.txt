** Index **

[1] What is this
[2] How do I install & use it?
[3] Where can I find more documentation?

** [1] What is this? **

This is a small generic package for parsing command line parameters.  Yes, I know there are tons of libraries like this around the world, but I find this quite convenient to use.

** [2] How do I install & use it? **

== To install it:
Just copy the two files (generic_line_parser.{ads,adb}) in a place where your Ada compiler will find them.  If you use GNAT/GPS, you could want to copy the project file gclp.gpr (that you will "with" from your project file)

== To use it:
The main characteristics of this package are

    (1) Parameters are *nominal*, not *positional*.  I wrote this package one day I needed to pass a lot of parameters to a program.  In that case the positional convention was too "fragile," so I decided to go for the nominal convention.  More precisely, the parameters have the form 

    <name>=<value> 

where <name> is any string without "=" or "," and <value> is any string.  The value part can also be omitted to obtain parameters of type

    <name>

For example, a program (say, foo) using this package could accept a command line like

    foo input=from.txt output=to.txt lines=12  fast-compile 

Note that since parameters are nominal, the following line is usually equivalent to the line above

    foo  lines=12 input=from.txt  fast-compile  output=to.txt  

(but order-dependent processing is still possible).

    (2) The parameter values are written inside a variable whose type is passed as formal parameter to the package at instantiation time.  The variable is written by using some "callbacks" provided by the user of the package.  Note with this solution, the variable can be of any type, although the usually it will be a record.

** [3] Where can I find more information? **

  (1) You should find a directory doc/ with documentation in HTML format
  (2) In the directory test/ you can find an example of usage


