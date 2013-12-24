Distributed Debugger
====================

Logging information about what actors send what message to what other actor can help in figuring out problems. This project consists in two phases. In phase I, logging capabilities were added to Scala so that information is captured about the execution. In phase II these logs were used by MSCGen to produce message exchange graph for the user to figure out what could have gone wrong.

First use 4-1 to generate log files. 

Then use 4-2 to automatically generate MSCGen graphs about message exchange.
