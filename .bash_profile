. $HOME/.bashrc

##
# Your previous /Users/siddhartha/.bash_profile file was backed up as /Users/siddhartha/.bash_profile.macports-saved_2010-12-11_at_18:07:56
##

# MacPorts Installer addition on 2010-12-11_at_18:07:56: adding an appropriate PATH variable for use with MacPorts.
## export PATH=/opt/local/bin:/opt/local/sbin:$PATH #commented out for python2.7 - added in priority order to bashrc
# Finished adapting your PATH environment variable for use with MacPorts.


# startup virtualenv-burrito
#if [ -f $HOME/.venvburrito/startup.sh ]; then
    #. $HOME/.venvburrito/startup.sh
#fi

if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    . /usr/local/bin/virtualenvwrapper.sh
fi
