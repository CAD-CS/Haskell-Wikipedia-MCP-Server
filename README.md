# What's an MCP server?

The Model Context Protocol (MCP) serves as a way for AI agents such as Claude to interact with external applications and processes. More specifically, this interaction comes in the form of Remote Procedure Calls (RPC). 

The MCP server exposes a series of "Tools" (procedures/functions/methods) that the MCP client (Claude) can call if it detects that it has a need to do so. The actual communication happens via JSON over STDIN and STDOUT. 

# What's this project?

This project is a Haskell implementation of an MCP server that facilitates interaction between Claude (desktop) and Wikipedia. It allows Claude to retrieve page information such as edit history and language availability. 

# Quick architectural rundown

There are two distinct layers. A "Client -> Server" layer, and a "Server <- Wikipedia API" layer. 

The raw request sent by the Client is deserialized and parsed into a valid service model which is then further processed and sent to the Wikipedia REST API. 

Once the Server receives a response another round of processing is done to convert it to a valid service model and eventually serialized to be sent to the client.
