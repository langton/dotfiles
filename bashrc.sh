# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

export EDITOR="emacsclient --alternate-editor emacs"
export PYTHONSTARTUP="$HOME/.pystartup"
export ALTERNATE_EDITOR=""
alias ecl="emacsclient --alternate-editor emacs"
alias eclient="emacsclient -c -n"
alias eclient-nw="emacsclient -t"
alias enw="emacs -nw"
alias timestamp="date '+%Y_%m_%d_%H_%M_%S'"

if ls --color -d . >/dev/null 2>&1; then
    alias ls='ls --color=auto'
elif ls -G -d . >/dev/null 2>&1; then
    alias ls='ls -G'
fi

# some more ls aliases
alias ll='ls -lh'
alias lla='ls -alh'
export PATH="~/local/bin:/usr/local/bin:$PATH"
# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups  
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend
export HISTSIZE=10000
stty erase '^?'
set bell-style none

case $TERM in
     xterm*)
        export PS1='\[\033]0;\u@\h: \w\007\]\[\e[1;4;34m\]\u@\h:\W$\[\e[0m\] '
        ;;
     *)
        export PS1='\[\e[1;4;34m\]\u@\h:\W$\[\e[0m\] '
        ;;
 esac
# set up title bar?

#export TERMINFO=${HOME}/.terminfo

# do not call directly; should only be set as PROMPT_COMMAND via following
function set-eterm-dir {
    echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
    echo -e "\033AnSiTc" "$(pwd)"
    # check to see if hostname -i matches val in SSH_CONNECTION
    echo -e "\033AnSiTh" "$tramp_host"
}

# Track directory, username, and cwd for remote logons.
if [ -n "$SSH_CONNECTION" && "$TERM" == "eterm-color" ]; then
    export TERMCAP="eterm-color:li#67:co#118:cl=\E[H\E[J:cd=\E[J:bs:am:xn:cm=\E[%i%d;%dH:nd=\E[C:up=\E[A:ce=\E[K:ho=\E[H:pt:al=\E[L:dl=\E[M:DL=\E[%dM:AL=\E[%dL:cs=\E[%i%d;%dr:sf=^J:dc=\E[P:DC=\E[%dP:IC=\E[%d@:im=\E[4h:ei=\E[4l:mi::so=\E[7m:se=\E[m:us=\E[4m:ue=\E[m:md=\E[1m:mr=\E[7m:me=\E[m:UP=\E[%dA:DO=\E[%dB:LE=\E[%dD:RI=\E[%dC:kl=\EOD:kd=\EOB:kr=\EOC:ku=\EOA:kN=\E[6~:kP=\E[5~:@7=\E[4~:kh=\E[1~:mk=\E[8m:cb=\E[1K:op=\E[39;49m:Co#8:pa#64:AB=\E[4%dm:AF=\E[3%dm:cr=^M:bl=^G:do=^J:le=^H:ta=^I:se=\E[27m:ue=\E24m:kb=^?:kD=^[[3~:sc=\E7:rc=\E8:r1=\Ec:"
    export TERM=eterm-color # force termcap to be read
    ssh_str_tmp=( $SSH_CONNECTION )
    ssh_tmp=${ssh_str_tmp[2]}
    ip_tmp=$(dig +short $(hostname -f))
    # use hostname iff it resolves; grab IP address from ssh string otherwise
    if [[ -z "$ip_tmp" && -n "$ssh_tmp" ]]
    then
        tramp_host=$ssh_tmp
    else
        tramp_host=$(hostname -f)
    fi
    unset ssh_str_tmp
    unset ssh_tmp
    unset ip_tmp
    PROMPT_COMMAND=set-eterm-dir
fi
