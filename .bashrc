export CLICOLOR='true'
export LSCOLORS="gxfxcxdxbxegedabagacad"
#PROMPT_COMMAND='CurDir=`pwd|sed -e "s!$HOME!~!"|sed -re "s!([^/])[^/]+/!\1/!g"`'
#PS1="[\$CurDir] \$ "
#PS1="[\h \t \#] \u > "
export PYTHONPATH=/usr/local/lib/python:/opt/gtk/lib/python2.7/site-packages:$HOME/work/mypypi:$HOME/work/gaia:$HOME/work/python/networkx-0.33:$HOME/work/worldspeak/conceptnet/ConceptNet-sqlite:$PYTHONPATH
# remove this (below) to run python in the default 64-bit mode. This is for in for matplotlib compatibility
export VERSIONER_PYTHON_PREFER_32_BIT='yes'
#export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home
export JAVA_HOME=/Library/Java/Home
# GRAILS_HOME isn't necessary for using grails from the CLI but is for eclipse and other stuff
export GRAILS_HOME=$HOME/sw/grails-1.2.5
# GROOVY
export GROOVY_HOME=$HOME/sw/groovy-1.7.10
# Add grails biin and mysql bin to path, and groovy bin
export EC2_HOME=/Users/siddhartha/work/aws/ec2/ec2-api-tools-1.3-62308
export EC2_PRIVATE_KEY=/Users/siddhartha/work/aws/ec2/auth/pk-awscert.pem
export EC2_CERT=/Users/siddhartha/work/aws/ec2/auth/cert-awscert.pem
export PATH=$HOME/bin:/usr/local/opt/python/libexec/bin:/usr/local/bin:/usr/local/share/python:/usr/local/mysql/bin:/opt/local/bin:/opt/local/sbin:$GRAILS_HOME/bin:$GROOVY_HOME/bin:$EC2_HOME/bin:~/bin:~/work/tahoe/allmydata-tahoe-1.8.0/bin:~/work/android/android-ndk-r4b:~/work/android/android-sdk-mac_x86:/Users/siddhartha/work/android/android-sdk-mac_x86/tools:/usr/local/sbin:$PATH
# for python mysql
export DYLD_LIBRARY_PATH=/usr/local/mysql/lib/
# for ruby
export PATH="$HOME/.rbenv/bin:$PATH"
# for haskell
export PATH="$HOME/Library/Haskell/bin:$PATH"

eval "$(rbenv init -)"

# for mono (.NET), "to use assemblies from other formulae" - homebrew
export MONO_GAC_PREFIX="/usr/local"

# for bash-completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
	. $(brew --prefix)/etc/bash_completion
fi

# truecrypt command-line access
alias truecrypt='/Applications/TrueCrypt.app/Contents/MacOS/Truecrypt --text'

alias localip="ifconfig | grep 'inet ' | grep -Fv 127.0.0.1 | awk '{print $2}'"

alias l="ls"
alias ll="ls -al"
alias py="ipython"
# alias pyserve="python -m SimpleHTTPServer"  # python 2
alias pyserve="python -m http.server"  # python 3
alias hstart="/usr/local/Cellar/hadoop/2.6.0/sbin/start-dfs.sh;/usr/local/Cellar/hadoop/2.6.0/sbin/start-yarn.sh"
alias hstop="/usr/local/Cellar/hadoop/2.6.0/sbin/stop-yarn.sh;/usr/local/Cellar/hadoop/2.6.0/sbin/stop-dfs.sh"
# make ack page by default while retaining formatting (from http://shuttlethread.com/blog/useful-ack-defaults)
alias ack='ACK_PAGER_COLOR="less -x4SRFX" /usr/local/bin/ack'
# colorize last python traceback
alias pytb="fc -s 2>&1 | pygmentize -l pytb"
# fancy ls (from Hunan)
alias list='clear; ls -lGp'
alias lst='clear; tree -LC 2'
alias lstt='clear; tree -LC 4'

# VI mode!!!
set -o vi

#-----------------------
# Greeting, motd etc...
#-----------------------

# Define some colors first:
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m'              # No Color
# --> Nice. Has the same effect as using "ansi.sys" in DOS.


#---------------
# Shell Prompt
#---------------

if [[ "${DISPLAY#$HOST}" != ":0.0" &&  "${DISPLAY}" != ":0" ]]; then
    HILIT=${red}   # remote machine: prompt will be partly red
else
    HILIT=${blue}  # local machine: prompt will be partly blue
fi

#  --> Replace instances of \W with \w in prompt functions below
#+ --> to get display of full path name.

function fastprompt()
{
    unset PROMPT_COMMAND
    case $TERM in
        *term | rxvt )
            PS1="\[\033[1;33m\][\h \t \#]\[\033[0m\] \[\033[1;34m\]\W\[\033[0m\] $ " ;;
        linux )
            PS1="${HILIT}[\h]$NC \W > " ;;
        *)
            PS1="[\h] \W > " ;;
    esac
}

function powerprompt()
{
    _powerprompt()
    {
        LOAD=$(uptime|sed -e "s/.*: \([^,]*\).*/\1/" -e "s/ //g")
    }

    PROMPT_COMMAND=_powerprompt
    case $TERM in
        *term | rxvt  )
#            PS1="\[\A \$LOAD]\n[\h \#] \W\] > \[\033]0;\${TERM}[\u@\h] \w\007\033[0m\] " ;;
            PS1="\[\033[1;33m\][\A \$LOAD]\[\033[0m\]\n[\h \#] \W > \[\033]0;\${TERM}[\u@\h] \w\007\033[0m\] " ;;
        linux )
            PS1="${HILIT}[\A - \$LOAD]$NC\n[\h \#] \w > " ;;
        * )
            PS1="[\A - \$LOAD]\n[\h \#] \w > " ;;
    esac
}

# cool prompt (from rytis)

#powerprompt     # this is the default prompt - might be slow
                # If too slow, use fastprompt instead....
fastprompt

#function parse_git_branch {
#git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* (.*)/(1)/'
#}
#
#PS1="${debian_chroot:+($debian_chroot)}u@h:w$(parse_git_branch)$ "


# This command line completion script will handily autocomplete
# any git commands as well as any remotes and branches you have
# defined.
#
# Add this to your .bashrc file
# complete -f -W "$(echo `git branch | sed -e s/[\ \*]//g | cut -f 1 -d ' ' | uniq`; \                                                                       
#    echo `git remote | sed -e s/[\ \*]//g | cut -f 1 -d ' ' | uniq`; \                                                                                   
#    echo `git | tail -23 | head -21 | cut -d ' ' -f 4`);" git
