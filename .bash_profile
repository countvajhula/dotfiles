. $HOME/.bashrc

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# startup virtualenv-burrito
#if [ -f $HOME/.venvburrito/startup.sh ]; then
#    . $HOME/.venvburrito/startup.sh
#fi
# load virtualenvwrapper for python (after custom PATHs)
venvwrap="virtualenvwrapper.sh"
/usr/bin/which -s $venvwrap
if [ $? -eq 0 ]; then
    venvwrap=`/usr/bin/which $venvwrap`
    source $venvwrap
fi

# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH
