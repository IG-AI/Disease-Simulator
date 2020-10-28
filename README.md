# Disease simulator
### Content
* [Authors](#authors)
* [The program](#the-program)
  * [Compiling](#compiling)
  * [Run the program](#run-the-program)
  * [System requirements](#system-requirements)
* [Documentation](#documentation)
* [Further information](#further-information)
  * [Creating maps](#creating-maps)
  * [Recording](#recording)
  * [Logging](#logging)
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
* VAC - number of vaccinated individuals.
* PROB - the chanse of the infection spreading between two individuals.
* RANGE - the radius of the area an individual need to be in order to be infected by another individual.
* LIFE - the number of ticks an individual will 'live' after being infected.
* MOVE - the movement of the individuals. 
  * **path** use A\* for pathfinding.
  * **bounce** and **bounce_random** uses bouncing behaviour.
* END  - the switch for the end conditions:
  * **dead** *default* the simulation will stop when either all individuals are healthy, all individuals are dead or TICKS have been depleted. 
  * **ticks** the simulation will only stop when TICKS is depleted. Note that if this option is chosen and TICKS are set to a negative value the simulation will run indefinitely.
  * **infected** the simulation will stop when either all individuals are healthy, all individuals are infected or TICKS have been depleted.
* TVAC - toggle vaccination: 'on' or 'off' in hospitals.
* REC - the behaviour of the recording function.
   * **play** *default* behaviour, will just play the simulation with no recording.
   * **play_and_rec** will play the simulation and record it.
   * **rec** will only record the simulation, nothing will be displayed.
   * **bg** will run the simulation in the background, nothing will be recorded nor displayed.
 * RECFIL - the record file that will be played or none if you run it live.
 * RAND - the behaviour of the random generators in the simulation
   * **auto** *default* each simulation with the same parameters will be different.
   * **manual** will use a predetermined seed resulting in identical simulations if the same parameters are use.
   
 

The flags do not need to be in any specific order to use them.<br>
EXAMPLE: **make erun MAP=map_one.bmp IND=100 TICKS=1000** <br>

To run a recorded simulation use: **make jrun** followed by the flags "REC=playback" and "RECFIL=nameOfYourRecording.record".<br>
EXAMPLE: **make jrun REC=playback RECFIL=2017_01_01_13_33_37**<br>

**Note that this method using the GUI to start the simulation is not always up to date**<br>
You can start the simulation using a GUI, to start the gui use the command **make gui**<br>
It's also possible to run Project-snowfox.jar.

(The rule **make run** exist but is currently not supported. It's behaviour is unpredictable and might prevent the start of further simulations due to behaviour of the Java window. This requires you to close the Java window after every run of the simuation. It should **NOT** be used until those issues are resolved.) <br>

### System requirements
The program is run mainly on the schools ThinLinc clients: https://www.it.uu.se/datordrift/maskinpark/linux <br>
These are running Java 7 and Erlang 19.<br>
The correct Erlang version seems to be the most important thing to get it running on our personal computers. <br>

To run the **GUI** on a Linux machine you need gnome-terminal installed.<br>

On personal computers we've had success running it on these setups:
* Linux Mint 17 with Java 7 and Erlang 19

## Documentation
Documentation can be generated using: <br>
**make edoc** to generate Erlang documentation. It will be found in Erlang/doc/index.html <br>
**make jdoc** to generate Java documentation. It will be found in Java/doc/index.html <br>

## Further information
### Creating maps
When creating custom maps for the simulator following criterias must be fulfilled:<br>
Black (#000000) rgb(0, 0, 0): Area that individuals can walk.<br>
Purple (#9b536f) rgb(155, 83, 111): Hospitals (individuals can walk).<br>
All other colours: Obstacles that individuals cant walk on.<br>
**Important:** No black pixel may ever be isolated from any other black pixel in such a way that a path can not be found between them by traveling on black pixels only.<br>

Map-file must be of the format .bmp and be stored in the **data** folder.<br>

### Recording
Using the record option will create a recording of the simulation that is being run. These files can be found in the folder **recordings/** . <br>

### Logging
Three files with logs exist.
* **logs/erlang_exec_log.log** contains information regarding simulations; when they are run, what flags were used and time required for different steps of execution.<br> 
The recorded steps are;<br>
Time to prepare the simulation.<br>
Time to run the simulation.<br>
Total time for both above steps.<br>
* **logs/welang_exec_time.log** contains information about the time required for different steps of execution, these are the same times as in the other file, but with no additional information making them easier to extract for other uses.
* **logs/java_data_log.log** contains information regarding simulations, stores starting parameters and if the simulation ends without being interrupted it stores the ending results.

### Testing
**make jrun_test** will run Java tests. <br>
**make test** will run Erlang tests (**make testv** will run verbose option). <br>

### Make commands
**make clean_doc** will remove all documentation files in Erlang/doc/ and Java/doc/ <br>
**make clean_exec** will remove all compiled files in Erlang/ebin/ and Java/bin/ <br>
**make clean_log** will remove all log-files (all files in logs/)<br>
**make clean_rec** will remove all recordings (all files in recordings/<br>
**make clean** will remove all documentation files and compiled files <br>

