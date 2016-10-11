# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

export EDITOR="emacsclient"
export PYTHONSTARTUP="$HOME/.pystartup"
export ALTERNATE_EDITOR="emacs"
alias ecl="emacsclient -n --alternate-editor emacs"
alias eclient="emacsclient -n"
alias eclient-nw="emacsclient -t"
alias enw="emacs -nw"
alias timestamp="date '+%Y_%m_%d_%H_%M_%S'"
alias jsonpaths="jq -c -r paths"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias less="less -FSRX"
alias more="less"
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)  tar xjf $1      ;;
            *.tar.gz)   tar xzf $1      ;;
            *.bz2)      bunzip2 $1      ;;
            *.rar)      rar x $1        ;;
            *.gz)       gunzip $1       ;;
            *.tar)      tar xf $1       ;;
            *.tbz2)     tar xjf $1      ;;
            *.tgz)      tar xzf $1      ;;
            *.zip)      unzip $1        ;;
            *.Z)        uncompress $1   ;;
            *)          echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}
alias youtube-dl-audio="youtube-dl -f bestaudio"
ljq () {
    jq -C '.' $1 | less -R
}


[[ -r ~/.bash_aliases ]] && . ~/.bash_aliases

# On Mac/Homebrew, use 'gls' if it's available
if gls --color=auto . >/dev/null 2>&1; then
    alias ls='gls --color=auto'
elif ls --color -d . >/dev/null 2>&1; then
    alias ls='ls --color=auto'
elif ls -G -d . >/dev/null 2>&1; then
    alias ls='ls -G'
fi

if gdircolors . >/dev/null 2>&1; then
    eval `gdircolors ~/.dir_colors`
elif dircolors . >/dev/null 2>&1; then
    eval `dircolors ~/.dir_colors`
fi
    
# some more ls aliases
alias ll='ls -lh'
alias lla='ls -alh'
export PATH="~/local/bin:/usr/local/bin:/usr/local/sbin:$PATH"
# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups  
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend
export HISTSIZE=10000
stty erase '^?'
set bell-style none

case $TERM in
     xterm*)
        export PS1='\[\033]0;\u@\h: \w\007\]\[\e[4;34m\]\u@\h:\W$\[\e[0m\] '
        ;;
     *)
        export PS1='\[\e[4;34m\]\u@\h:\W$\[\e[0m\] '
        ;;
 esac


# do not call directly; should only be set as PROMPT_COMMAND via following
function set-eterm-dir {
    echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
    echo -e "\033AnSiTc" "$(pwd)"
    # check to see if hostname -i matches val in SSH_CONNECTION
    echo -e "\033AnSiTh" "$tramp_host"
}

# disable blinking on a console that can't (easily) be configured otherwise
function noblink {
    echo -e '\033[?17;0;127c'
}

# Track directory, username, and cwd for remote logons.
if [[ -n "$SSH_CONNECTION" && "$TERM" == "eterm-color" ]]; then
    export TERMCAP="eterm-color:li#67:co#118:cl=\E[H\E[J:cd=\E[J:bs:am:xn:cm=\E[%i%d;%dH:nd=\E[C:up=\E[A:ce=\E[K:ho=\E[H:pt:al=\E[L:dl=\E[M:DL=\E[%dM:AL=\E[%dL:cs=\E[%i%d;%dr:sf=^J:dc=\E[P:DC=\E[%dP:IC=\E[%d@:im=\E[4h:ei=\E[4l:mi::so=\E[7m:se=\E[m:us=\E[4m:ue=\E[m:md=\E[1m:mr=\E[7m:me=\E[m:UP=\E[%dA:DO=\E[%dB:LE=\E[%dD:RI=\E[%dC:kl=\EOD:kd=\EOB:kr=\EOC:ku=\EOA:kN=\E[6~:kP=\E[5~:@7=\E[4~:kh=\E[1~:mk=\E[8m:cb=\E[1K:op=\E[39;49m:Co#8:pa#64:AB=\E[4%dm:AF=\E[3%dm:cr=^M:bl=^G:do=^J:le=^H:ta=^I:se=\E[27m:ue=\E24m:kb=^?:kD=^[[3~:sc=\E7:rc=\E8:r1=\Ec:"
    export TERM=eterm-color # force termcap to be read
    ssh_str_tmp=( $SSH_CONNECTION )
    ssh_tmp=${ssh_str_tmp[2]}
    ip_tmp=$(dig +short $(hostname -f))
    # use hostname iff it resolves; grab IP address from ssh string otherwise. We check
    # to make sure $ssh_tmp is IP addr-ish, but this is sloppy. We're just trying to rule
    # out cases like $ssh_tmp == "::1", as happens with ssh connections to localhost
    if [[ -z "$ip_tmp" && $ssh_tmp =~ ^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$ ]]
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

if hash brew 2>/dev/null && [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

alias aes256="openssl aes-256-cbc -salt"
gpg-agent > /dev/null 2>&1
if [ $? -ne 0 ]; then
    gpg-agent --daemon --enable-ssh-support \
              --write-env-file "${HOME}/.gpg-agent-info" >/dev/null 2>&1
fi
if [ -f "${HOME}/.gpg-agent-info" ]; then
   . "${HOME}/.gpg-agent-info"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
  export SSH_AGENT_PID
fi

GPG_TTY=$(tty)
export GPG_TTY

function renamesha256 {
    for source in "$@"; do
        local sha256=$(shasum -a 256 -- ${source} | cut -d " " -f 1)
        local dest="$(dirname -- $source)/${sha256}.bin"
        if [ -a ${dest} ]; then
            echo "${source} exists; skipping"
        else
            mv -- ${source} ${dest}
        fi
    done
}
