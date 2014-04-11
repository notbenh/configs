alias ls='ls --color=auto'
[ ! "$UID" = "0" ] && archbey -c white
[  "$UID" = "0" ] && archbey -c green
#PS1="\[\e[01;31m\]┌─[\[\e[01;35m\u\e[01;31m\]]──[\[\e[00;37m\]${HOSTNAME%%.*}\[\e[01;32m\]]:\w$\[\e[01;31m\]\n\[\e[01;31m\]└──\[\e[01;36m\]>>\[\e[0m\]"

export PATH=~/bin:$PATH:.
source ~/perl5/perlbrew/etc/bashrc

export PERL_LOCAL_LIB_ROOT="/home/benh/perl5";
export PERL_MB_OPT="--install_base /home/benh/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/benh/perl5";
export PERL5LIB="/home/benh/perl5/lib/perl5/x86_64-linux-thread-multi:/home/benh/perl5/lib/perl5:$PERL5LIB";
export PATH="/home/benh/perl5/bin:$PATH";

source ~/.aliases

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export TDONE_FILE="$HOME/.tdone"

export VISUAL=gvim
export EDITOR=vim
export TERMINAL=lxterminal

# define default browser
if [ -n "$DISPLAY" ]; then
  BROWSER=chromium
fi

SPARK_FILE=~/.spark_file

pal
PATH="/usr/local/heroku/bin:$PATH"
