###1. Make sure OpenGL is installed on your OS, it usually comes [pre-installed](http://www.opengl.org/wiki/Getting_started).

###2. [Install Haskell](doc/InstallingHaskell.md)

###3. Download and install mongoSimulation from github:

	$ git clone git://github.com/TonyGen/mongoSimulation.git
	$ cd mongoSimulation
	$ cabal install --bindir=$PWD

###4. Run mongoShardingSim and click anywhere on its window to see help:

	$ ./MongoShardingSim

###5. To make changes, edit src code and re-install (re-compile):

	edit src/*
	$ cabal install --bindir=$PWD
