# Project-Snowfox
### Content
* [Authors](#authors)
* [The program](#the-program)
  * [Compiling](#compiling)
  * [Run the program](#run-the-program)
  * [System requirements](#system-requirements)
* [Documentation](#documentation)
* [Further information](#further-information)
  * [Testing](#testing)
  * [Make commands](#make-commands)
  
## Authors:
| Name | E-mail |
| ------ | ------ |
| Henrik Bergendal | henrik.bergendal.5651@student.uu.se | 
| Joel Havermark | joel.havermark.9226@student.uu.se |
| Simon Pellgård | simon.pellgard.8394@student.uu.se |
| Lars Pettersson | lars.pettersson.3751@student.uu.se |
| Robin Rosberg | robin.rosberg.6223@student.uu.se | 
| Patrik Viklander | patrik.viklander.5549@student.uu.se | 
| Daniel Ågstrand | daniel.agstrand.5971@student.uu.se |

## The program
The program consist of two components. One "simulation server" written in Erlang and one application to show a graphical presentation of the simulation written in Java. <br>

### Compiling
The code can be compiled using: <br>
**make ecompile** to compile the Erlang code. <br>
**make jcompile** to compile the Java code. <br>
**make all** to compile both Erlang and Java code. <br>
Compilation will however be done automatically if using the below mentioned commands to run the components. <br>

### Run the program
*There is a method to start the simulation using a GUI, it is mentioned last in this paragraph.*<br>
To run a simulation use two separate shells to start the two components.<br>
To start the graphical presentation use: **make jrun**<br>
To start the simulation server use: **make erun**<br>
Several arguments can be passed to the simulation server via the make-command.<br>
* MAP - select a map.
* TICKS - determine the number of simulation steps before the simulation end. If TICKS is set to a negative value the simulation will run indefinitely or until another end condition is reached.
* IND - number of individuals in the simulation.
* INF - number of infected individuals.
* PROB - the chanse of the infection spreading between two individuals.
* RANGE - the radius of the area an individual need to be in order to be infected by another individual.
* LIFE - the number of ticks an individual will 'live' after being infected.
* MOVE - the movement of the individuals. 
  * **path** use A\* for pathfinding.
  * **bounce** and **bounce_random** uses bouncing behaviour.
* END  - the switch for the end conditions:
  * **ticks** the simulation will only stop when TICKS is depleted. Note that if this option is chosen and TICKS are set to a negative value the simulation will run indefinitely.
  * **dead** the simulation will stop when either all individuals are healthy, all individuals are dead or TICKS have been depleted. 
  * **infected** the simulation will stop when either all individuals are healthy, all individuals are infected or TICKS have been depleted.
* VAC - the switch for vaccination: 'on' or 'off' 
* REC - the behaviour of the recording function.
   * **play** default behaviour, will just play the simulation with no recording.
   * **play_and_rec** will play the simulation and record it.
   * **rec** will only record the simulation, nothing will be displayed.
   * **bg** will run the simulation in the background, nothing will be recorded nor displayed.
 

The flags do not need to be in any specific order to use them.<br>
EXAMPLE: **make erun MAP=map_one.bmp IND=100 TICKS=1000** <br>

**Note that this method using the GUI to start the simulation is not always up to date**<br>
You can start the simulation using a GUI, to start the gui use the command **make gui**<br>
It's also possible to run Project-snowfox-linux.jar.

(The rule **make run** exist but is currently not supported. It's behaviour is unpredictable and might prevent the start of further simulations due to behaviour of the Java window. This requires you to close the Java window after every run of the simuation. It should **NOT** be used until those issues are resolved.) <br>

### System requirements
The program is run mainly on the schools ThinLinc clients: https://www.it.uu.se/datordrift/maskinpark/linux <br>
These are running Java 7 and Erlang 19.<br>
The correct Erlang version seems to be the most important thing to get it running on our personal computers. <br>

To run the **GUI** you need gnome-terminal installed.<br>

On personal computers we've had success running it on these setups:
* Linux Mint 17 with Java 7 and Erlang 19

## Documentation
Documentation can be generated using: <br>
**make edoc** to generate Erlang documentation. It will be found in Erlang/doc/index.html <br>
**make jdoc** to generate Java documentation. It will be found in Java/doc/index.html <br>

## Further information
### Testing
**make jrun_test** will run Java tests. <br>
**make test** will run Erlang tests (**make testv** will run verbose option). <br>

### Make commands
**make clean_doc** will remove all documentation files in Erlang/doc/ and Java/doc/ <br>
**make clean_exec** will remove all compiled files in Erlang/ebin/ and Java/bin/ <br>
**make clean** will remove all documentation files and compiled files <br>

