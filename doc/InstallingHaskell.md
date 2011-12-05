# Two ways to install Haskell:
1. Install [Haskell Platform](http://hackage.haskell.org/platform/) on your system, or
2. Install Haskell manually in one directory (decribed below)

## Installing Haskell manually:

###1. Download and install [GHC](http://www.haskell.org/ghc/download_ghc_7_2_2#binaries), the Haskell compiler:
**Unix (including Mac)**, download the ghc-7.2...*tar.bz2* file, then

	$ mkdir <GHCPATH>  # directory where Haskell compiler and libraries will be installed
	$ tar xf ghc-7.2...tar.bz2
	$ cd ghc-7.2...
	$ ./configure --prefix=<GHCPATH>  # substitute full path of your dir for <GHCPATH>
	$ make install
	$ cd ..
	$ export PATH=$PATH:<GHCPATH>/bin  # substitute full path of your dir for <GHCPATH>

**Windows**, download and run installer

###2. Dowload and install cabal, the Haskell package manager:

**Unix (including Mac)**:

	$ wget http://hackage.haskell.org/packages/archive/cabal-install-ghc72/0.10.4/cabal-install-ghc72-0.10.4.tar.gz
	$ tar xzf cabal-install-ghc72-0.10.4.tar.gz
	$ cd cabal-install-ghc72-0.10.4/
	$ export PREFIX=<GHCPATH> && sh bootstrap.sh  # substitute full path of your dir for <GHCPATH>
	$ cd ..

**Windows**:

	$ wget http://www.haskell.org/cabal/release/cabal-install-0.10.2/cabal.exe

###3. Get updated list of published packages on Hackage:

	$ cabal update

###4. Edit cabal config so globally installed packages are stored in your *\<GHCPATH\>*:

	# substitute full path of your dir for <GHCPATH> below
	$ sed -i '' 's|-- prefix: /usr/local|prefix: <GHCPATH>|' $HOME/.cabal/config

##To uninstall Haskell and all its packages, remove its directories:

	$ rm -r $HOME/.cabal $HOME/.ghc <GHCPATH>  # substitute full path of your dir for <GHCPATH>
	Remove <GHCPATH>/bin from $PATH

In addition to *\<GHCPATH\>*, GHC stores user-built programs and libraries in $HOME/.cabal and $HOME/.ghc.
