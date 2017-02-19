
# A Test Distributed Simulation Model for AivikaSim

This test project defines a distributed discrete event simulation model for [aivikasim-distributed] [1].
It should return the same reproducible results on computers with the same architecture whatever cluster you create.

## What is Tested

The test model is intentionally designed in such a way so that it would generate plenty of messages for stress testing the distributed module of AivikaSim. The most of things could be done by using significantly more efficient discrete events and discontinuous processes supported by AivikaSim, but the heavy-weight external messages are used instead. There are many rollbacks and retries, when simulating. The goal is to test the underlying mechanism that provides with capabilities of the distributed discrete event simulation based on the optimistic Time Warp method.

## How to Test

The following test can be repeated on Windows, Linux and macOS. Possibly, it can be repeated on other operating systems too that have the Haskell compiler.

### Prerequisites

To repeat the test, you need a small simulation cluster. It can be either a single computer, i.e. `localhost`, or a true small cluster consisting of up to 4 different computers.

The code is written in Haskell. In the simplest case you need [Stack] [2] installed on your nodes of the future cluster. To reproduce the test, you don't need to know the Haskell programming language, though.

### Downloading from GitHub

Download the test code from GitHub on all your nodes:

```
$ git clone https://github.com/dsorokin/aivikasim-distributed-test.git
$ cd aivikasim-distributed-test
```

### Defining the Cluster Topology

Now you have to define a topology of the cluster. In other words, you have to decide, where the simulation nodes will reside and which ports they will listen to. By the way, the ports must be open. The most simple way is to create a local network.

You have to edit the `cluster.conf` file on every node of the cluster.

For example, I decided to use two laptops connected via the Ethernet cable in local network. The first laptop works under macOS and has IP address 192.168.99.20. The second laptop works under Linux and has IP address 192.168.99.10.

But if you decide to repeat the simulation experiment on one physical computer, then you can define the IP address as `localhost`. It will work too.

In my case the `cluster.conf` file has the following contents:

```
# The Simulation Cluster Configuration

# 0: the master simulation node, which must be run last
192.168.99.20:8088

# 1: the time server
192.168.99.20:8080

# 2: the first slave simulation node
192.168.99.10:8081

# 3: the second slave simulation node
192.168.99.10:8082
```

Here nodes 0 and 1 are located on the laptop with IP address 192.168.99.20. Nodes 2 and 3 are located on another laptop with IP address 192.168.99.10. 

In this specific test, the first two nodes have a special meaning. The first node is a master node, which must be run strongly in the last order. The second node will be used for running a specialized local process that will play a role of the Time Server.

### Building the Project

For the first time, you will have to set up the Stack project. In the next time, you can skip this step.

`$ stack setup`

In the beginning and after each change of the `cluster.conf` configuration file, you have to build a binary executable anew.

`$ stack build`

It must be done on every node of the simulation cluster.

### Running the Time Server

Here in the test the Time Server is located on node 1. In my case I run it on the laptop with IP address 192.168.99.20.

`$ stack exec aivikasim-distributed-test slave 1`

### Running Auxiliary Simulation Nodes

Now the time is to run the auxiliary simulation nodes. In my case they are located on the laptop with IP address 192.168.99.10.

So, I type in one Terminal window:

`$ stack exec aivikasim-distributed-test slave 2`

Then I repeat in another Terminal window:

`$ stack exec aivikasim-distributed-test slave 3`

### Launching the Simulation

Now I will run the distributed simulation on my cluster. I switch to the first laptop with IP address 192.168.99.20 and type in the Terminal window:

`$ stack exec aivikasim-distributed-test master 0`

The simulation must be started, which we can see in the Terminal window, where node 1 was launched. If you remember, that was the Time Server. You should see something like this:

```
$ stack exec aivikasim-distributed-test slave 1
-- time --: [INFO] Time Server: starting...
-- time --: [DEBUG] Time Server: RegisterLocalProcessMessage pid://192.168.99.10:8081:0:24
-- time --: [INFO] Time Server: monitoring the process by identifier pid://192.168.99.10:8081:0:24
-- time --: [DEBUG] Time Server: RegisterLocalProcessMessage pid://192.168.99.20:8088:0:11
-- time --: [INFO] Time Server: monitoring the process by identifier pid://192.168.99.20:8088:0:11
-- time --: [DEBUG] Time Server: RegisterLocalProcessMessage pid://192.168.99.10:8082:0:24
-- time --: [INFO] Time Server: monitoring the process by identifier pid://192.168.99.10:8082:0:24
-- time --: [INFO] Time Server: starting
-- time --: [DEBUG] Time Server: computing the global time...
-- time --: [DEBUG] Time Server: LocalTimeMessage pid://192.168.99.10:8081:0:24 0.32450531421998324
-- time --: [DEBUG] Time Server: LocalTimeMessage pid://192.168.99.20:8088:0:11 0.3245063142199832
-- time --: [DEBUG] Time Server: LocalTimeMessage pid://192.168.99.10:8082:0:24 0.0
-- time --: [INFO] Time Server: providing the global time = Just 0.0
-- time --: [DEBUG] Time Server: computing the global time...
-- time --: [DEBUG] Time Server: LocalTimeMessage pid://192.168.99.10:8082:0:24 0.40740346307920355
-- time --: [DEBUG] Time Server: LocalTimeMessage pid://192.168.99.20:8088:0:11 5.978089191143517
-- time --: [DEBUG] Time Server: LocalTimeMessage pid://192.168.99.10:8081:0:24 0.3245073142199832
-- time --: [INFO] Time Server: providing the global time = Just 0.3245073142199832
```

Please pay attention to the fact that the global virtual time should increase. It means that the distributed simulation has a progress.

### Optional Imitating Connection Errors

AivikaSim is written in such a way that it tries to recover the distributed simulation in case of connection errors. It actually allows using AivikaSim to build discrete event simulation clusters on unsafe networks.

Here I use macOS with Linux and I can imitate the temporary disconnection between two my laptops. I would strongly recommend to not repeat this if you are using Windows, although you may risk.

So, during the simulation I plug the Ethernet cable off for about one minute. I wait for a moment, when I see that the disconnection has indeed occurred and cannot be recovered by the underlying network system. 

I should see something like this on the Terminal window of the master node:

```
$ stack exec aivikasim-distributed-test master 0
Slaves: [nid://192.168.99.20:8080:0,nid://192.168.99.10:8081:0,nid://192.168.99.10:8082:0]
-- time --: [WARNING] Received a process monitor notification ProcessMonitorNotification (MonitorRef {monitorRefIdent = pid://192.168.99.10:8081:0:19, monitorRefCounter = 1}) pid://192.168.99.10:8081:0:19 DiedDisconnect
-- time --: [WARNING] Received a process monitor notification ProcessMonitorNotification (MonitorRef {monitorRefIdent = pid://192.168.99.10:8082:0:19, monitorRefCounter = 2}) pid://192.168.99.10:8082:0:19 DiedDisconnect
-- time --: [WARNING] Received a process monitor notification ProcessMonitorNotification (MonitorRef {monitorRefIdent = pid://192.168.99.20:8080:0:13, monitorRefCounter = 0}) pid://192.168.99.20:8080:0:13 DiedDisconnect
-- time --: [NOTICE] Begin reconnecting...
-- time --: [NOTICE] Direct reconnecting to pid://192.168.99.10:8081:0:19
-- time --: [NOTICE] Direct reconnecting to pid://192.168.99.10:8082:0:19
-- time --: [NOTICE] Direct reconnecting to pid://192.168.99.20:8080:0:13
-- time --: [NOTICE] Proceed to the re-monitoring
-- time --: [NOTICE] Re-monitoring pid://192.168.99.10:8081:0:19
-- time --: [NOTICE] Writing to the channel about reconnecting to pid://192.168.99.10:8081:0:19
-- time --: [NOTICE] Re-monitoring pid://192.168.99.10:8082:0:19
-- time --: [NOTICE] Writing to the channel about reconnecting to pid://192.168.99.10:8082:0:19
-- time --: [NOTICE] Re-monitoring pid://192.168.99.20:8080:0:13
-- time --: [NOTICE] Writing to the channel about reconnecting to pid://192.168.99.20:8080:0:13
-- time --: [NOTICE] t = 1000.0: reconnecting to pid://192.168.99.10:8081:0:19...
-- time --: [NOTICE] t = 1000.0: reconnecting to pid://192.168.99.10:8082:0:19...
```

After plugging in the Ethernet cable, AivikaSim should recover the distributed simulation, where the global virtual time will increase again in the Terminal window of the Time Server.

### Simulation Results

Whatever cluster you build, however many times you run the simulation, you should always see the same final results if you are using computers of the same architecture. The results are printed in the Terminal window of the master node.

In my case I receive the following results:

`The result is (0.6174488980632207,0.6666666666666666)`

It is possible thanks to the fact that the distributed simulation model uses pseudo-random generators with the predefined seed.

Finally, on Linux and macOS you can launch another simulation by using the same Time Server and auxiliary nodes, i.e. the slave nodes, and by running a new master node. The slave nodes should be shutdown explicitly, but they will behave like a service for each new simulation. On Windows I recommend to restart all nodes for each new simulation run.

[1]: http://www.aivikasoft.com/aivikasim/aivikasim-distributed  "aivikasim-distributed"
[2]: http://docs.haskellstack.org/ "Stack"