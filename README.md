# Project-Snowfox
### Content
* Authors
* The program
  * Running
  * System requirements
  
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
The program consist of two components. One "simulation server" written in Erlang and one application to show a graphical presentation of the simulation written in Java.

### Run the program
To run a simulation use two separate shells to start the two components.<br>
To start the graphical presentation use: **make jrun**<br>
To start the simulation server use: **make erun**<br>
Several arguments can be passed to the simulation server via the make-command.<br>
* MAP - select a map.
* TICKS - determine the number of simulation steps before the simulation end.
* IND - number of individuals in the simulation.
* INF - number of infected individuals.
* INFC - the chanse of the infection spreading between two individuals.

The flags do not need to be in any specific order to use them.<br>
EXAMPLE: **make erun MAP=map_one.bmp IND=100 TICKS=1000**

### System requirements
The program is run mainly on the schools ThinLinc clients: https://www.it.uu.se/datordrift/maskinpark/linux <br>
These are running Java 7 and Erlang 19.<br>
The correct Erlang version seems to be the most important thing to get it running on our personal computers.

On personal computers we've had success running it on these setups:
* Linux Mint 17 with Java 7 and Erlang 19
