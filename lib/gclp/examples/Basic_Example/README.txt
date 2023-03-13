This directory contains a very basic example of usage of the generic 
package.  

Our goal
========

We want to write the line parsing code for a program that is supposed
to contact a remote host, authenticating with a username and,
possibly, a password.  By default the remote host listens on port
4242, but a different port can be specified.  The main procedure in
basic_example-main.adb implements the required command line parsing
and prints the result on the standard output.

More precisely, the parameters on the command line are to be specified
as follows

  * The host address will be specified with the parameter "host" (only
    numerical addresses are accepted)

  * The optional port will be specified with the parameter "port", if
    not specified, the port defaults to 4242

  * The username can be specified, equivalently, with the parameter
    "user" or with the parameter "username"
 
  * The optional password can be specified, equivalently, with the parameter
    "pwd" or with the parameter "password". If not specified, the
     password defaults to ""

Examples
--------

Therefore, for example, the following command lines are accepted

  basic_example-main host=196.18.1.34  user=pippo

    Contact host 196.18.1.34 on port 4242 with username pippo and no password

  basic_example-main host=196.18.1.34  username=pippo

    Equivalent to the command line above

  basic_example-main host=196.18.1.34  user=pippo pwd=pluto

    Contact host 196.18.1.34 with username pippo and password pluto

  basic_example-main host=196.18.1.34  user=pippo password=pluto

    Equivalent to the command line above

  basic_example-main host=196.18.1.34 port=12345  user=pippo

    Contact host 196.18.1.34 on port 12345 with username pippo and no password


Structure of the program
========================

It is very simple:

  * basic_example-main.adb  

      Contains the main procedure

  * basic_example-parameters.adb 

      Defines a record that holds the four parameters (host, port,
      username and password) that can be specified by the user.  It
      defines also procedures that set the four parameters and that
      can be used as callbacks for the line parsers.
