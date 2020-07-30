pathmunge () {
  if ! echo $PATH | egrep -q "(^|:)$1($|:)" ; then
    if [ "$2" = "after" ] ; then
      PATH=$PATH:$1
    else
      PATH=$1:$PATH
    fi
  fi
}

pathmunge /usr/local/opt/ruby/bin
pathmunge $HOME/.bin/ after
pathmunge $HOME/.cabal/ after
pathmunge $HOME/.local/ after
pathmunge $GOPATH/bin after
pathmunge $HOME/.cabal/bin
pathmunge $HOME/.ghcup/bin
pathmunge $HOME/.cargo/bin
pathmunge $HOME/.bin.local after

export PATH
