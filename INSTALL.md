1. Make sure OpenGL is installed on your OS, it usually comes pre-installed.
See http://www.opengl.org/wiki/Getting_started

2. Download and install GHC compiler from http://www.haskell.org/ghc/download_ghc_7_2_2#binaries
Follow INSTALL instructions. Create a new directory, let's call it $GHC, and make that your configure --prefix. This is where all Haskell programs and libraries will be stored. Add $GHC/bin to your path.

3. Dowload and install cabal-install.

	$ wget http://hackage.haskell.org/packages/archive/cabal-install/0.10.2/cabal-install-0.10.2.tar.gz
	$ tar -zxf cabal-install-0.10.2.tar.gz
	$ cd cabal-install-0.10.2/
	$ wget http://hackage.haskell.org/trac/hackage/raw-attachment/ticket/872/ghc7.diff
	$ patch -p0 cabal-install.cabal ghc7.diff
	add edits from bootstrap.path
	$ export PREFIX=$GHC 	# $GHC from install above
	$ sh bootstrap.sh

4. Get updated list of published libraries and programs on Hackage.
Do this periodically before installing Haskell software from Hackage or other software that depends on libraries from Hackage (like mongoSimulation below).

	$ cabal update

5. Specify location for libraries and programs installed from Hackage.

	edit ~/.cabal/config and change line after line below to refer to your install directory:
	
		install-dirs global
		  prefix: $GHC

6. Download and install mongoSimulation from github.
It installs the program in $HOME/.cabal/bin. You may want to also add this directory to your path.

	$ git clone git://github.com/TonyGen/mongoSimulation.git
	$ cd mongoSimulation
	$ cabal install

7. Run mongoShardingSim and click anywhere on its window to see help.

	$ ~/.cabal/bin/MongoShardingSim

8. To make changes, edit src code, and re-install (re-compile)

	$ cd mongoSimulation
	edit src/*
	$ cabal install

9. To uninstall GHC and all its installed libraries and programs:
Remove $HOME/.cabal, $HOME/.ghc, and $GHC, the install prefix from step 2 above.
