export CLICOLOR='true'
export LSCOLORS="gxfxcxdxbxegedabagacad"
export PATH=$HOME/bin::$PATH
export PYTHONPATH=$HOME/work/mypypi:$PYTHONPATH

#export JAVA_HOME=/Library/Java/Home
# for python mysql
#export DYLD_LIBRARY_PATH=/usr/local/mysql/lib/
#PROMPT_COMMAND='CurDir=`pwd|sed -e "s!$HOME!~!"|sed -re "s!([^/])[^/]+/!\1/!g"`'
#PS1="[\$CurDir] \$ "
#PS1="[\h \t \#] \u > "

alias l="ls"
alias ll="ls -al"
alias py="ipython"
alias pyserve="python -m SimpleHTTPServer"

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
        *term* | rxvt | screen )
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
