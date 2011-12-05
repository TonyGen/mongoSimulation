This program simulates the balancing of chunks in a MongoDB sharded system. It is useful for experimenting with different balancing algorithms. Below is a screen shot of it running. Each box represents a shard. Each horizontal line in a box represents a chunk residing in that shard. When the balancer moves a chunk its line becomes red on the source shard and green on the destination shard. You can add and remove shards to see the balancing and draining in action. You also control insertion of documents. Currently you can just start or stop insertion of random keys at an average rate of 32MB per second. See *step* and *insertDoc* functions at the bottom of MongoShardingSimulation.hs to change this. Also, see *event* function to change user controls.

[Install](INSTALL.md)

![](doc/screenshot1.png)
