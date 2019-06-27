# Distributed command & control server

This is our bachelor project in computer science.


# Project description

When creating networks, we often run into the issue of having a single point of
failure, in the form of a server we expect to be available all the time. This is
especially prevalent in command and control style botnets, where if you lose
your control server (or it is blocked), you can lose control of your entire
botnet. A botnet relies on having the bots pull commands from a command server,
which introduces both a bottleneck and a failure point for the network.

We believe that this predicament can be avoided by making the command part of
the network distributed and self configuring, meaning that if a command server
goes down or is blocked, the network and bots aren't lost. We wish to explore
such a network can be created.

We will implement this network using Erlang, utilizing existing features and
libraries when applicable, and will follow relevant design principles, in order
to create a network as stable as possible. We wish to make the network
performant and self-configuring by using techniques such as gossip and relevant
graph theory. We aim to make it so that no node in the network requires full
knowledge of the entire network, in order to improve security and scalability.
Further, each node with the right authority should be able to issue commands to
the network, thereby alleviating problem of single point of failure. All
communication in the network will be accomplished securely, using public/private
key methods.
