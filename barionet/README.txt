Combined Digital IO and Serial Tunnel Application for the Barionet
Load into WEB4 of your Barionet (100) or Barionet 50.

Help


Serial Tunnel
This Application permits to tunnel RS-232 serial data from one Barionet RS-232 port to another Barionet RS-232 port.
The Barionet that will act as a client (initiating the serial tunnel) must be configured by disabling its server setting (Local port = 0) and setting the client settings according to the remote Barionet's server settings.
Serial settings like speed, databits etc. must be configured the same on both Barionets under the"SERIAL" tab on the "SETTINGS" page.

Server settings

Local port
Define the Port on which the serial gateway can be accessed using TCP.
Default: "10001" for RS-232
Enter "0" to disable the serial server and to prepare for the use as a client (see below "Client settings")

Disconnect Tout
Defines the time in seconds after which the TCP connection to the local serial port will be closed due to inactivity.
Enter "0" to disable the Disconnect Timeout (default).
Enter "1" to "255" to enable the Disconnect Timeout.

Client settings

Remote IP Address
Enter the 4 values of the remote Barionet's IP address.
To disable the Serial Tunnel enter "0.0.0.0".

Remote TCP Port
Remote Barionet's TCP listen port (see remote Barionet's server settings "Local port").
Default port is "0" (disabled).
Barix default on the remote Barionet is "10001".

Reconnect interval
Set the time interval (between 1 to 255 seconds) which will be used to try to reconnect to the remote Barionet once the connection is lost.
Default interval is "0" (0 and 1 both equal to try every second).


Digital I/O Tunnel
This Application permits to tunnel digital inputs from one Barionet to outputs of another Barionet and vice versa. Both Devices have to be set up in order to do tunneling correctly.
One or both Barionets can be extended by connecting the Barix R6 Relay Extension module to the RS-485 port. Serial settings of the second port should then be set to "RS-485,19200,8,E,1" to support a Barix R6 Relay Extension module with default settings.
This I/O Tunnel application supports both the Barionet 50 and the legacy Barionet and is backwards compatible with version B1.10. This allows for mixing hardware and software. The Barionet 50 however is supported in the I/O Tunnel version B1.20 and later.

Remote IP Address
Input the 4 values of the desired remote Barionet's IP address.
To disable the tunnel enter 0.0.0.0.

Tunneling UDP Port
UDP port number used for tunnel data communication. Use the same port on the remote Barionet. To disable the tunnel enter 0.

Send Interval
Input the time in seconds between two status messages. In case no inputs have changed (changes are sent immediately) a status message is sent to permit the other side to detect a communication loss. The same value should be set on the remote Barionet. Recommended value: 2..65535. To disable Loss checking enter 0.

The comunication loss will be triggered 1 second after the double time of the remote stations send interval to allow 1 lost UDP packet eg. when interval is 5 seconds the com. loss will be triggered 11 seconds after the last received I/O status.

Output Action
Choose an output for each remote input and the communication loss. Eight inputs are featured on a legacy Barionet, four on the Barionet 50 (input 5 to 8 are always inactive). Choose Relay 1 or 2 on a legacy Barionet and Relay 1 to 4 on a Barionet 50, Output 1 to 4 (legacy Barionet only) or Extension Relay 1 to 6 (only if a single Relay Extension module Barix R6 is attached to the RS-485 bus). 