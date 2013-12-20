# -*- mode: shell-script; -*-

# Add this to top-level .cshrc:
# if ( -f $HOME/.cshrc.common ) then
#   source $HOME/.cshrc.common
# endif

if ( $?prompt ) then
    setenv HOSTNAME `hostname | sed -e s/\\..\*//`
    if ( `uname` == Darwin ) then
        if (! $?TERMPATH ) then
            setenv TERMPATH /usr/share/terminfo:/opt/local/share/terminfo
        endif
        if ( -d /Applications/Emacs.app/Contents/MacOS/ ) then
            set path = ( /Applications/Emacs.app/Contents/MacOS/bin $path )
            alias emacs /Applications/Emacs.app/Contents/MacOS/Emacs
        endif
    endif
    setenv EDITOR "emacsclient --alternate-editor emacs"
    setenv PYTHONSTARTUP $HOME/.pystartup
    setenv ALTERNATE_EDITOR ""
    alias ecl "emacsclient --alternate-editor emacs"
    alias eclient "emacsclient -c -n"
    alias eclient-nw "emacsclient -t"
    alias enw "emacs -nw"
    alias timestamp "date '+%Y_%m_%d_%H_%M_%S'"
    set path = ( ~/local/bin $path )
    if ( $?SYS_TYPE) then
        set path = ( ~/local/${SYS_TYPE}/bin $path )
    endif
    setenv CLICOLOR 1
    set history=10000
    # since .history gets corrupted periodically, back it up
    if ( ! -d ~/.old_history ) then
        mkdir ~/.old_history
    endif
    # want at most one per day
    set tmp_date = `date '+%Y_%m_%d'`
    if ( ! -e  ~/.old_history/${tmp_date}.gz ) then 
       gzip -c ~/.history > ~/.old_history/${tmp_date}.gz
    endif
    unset tmp_date
    set filec
    set savehist=( 2000 merge )
    set histdup=erase
    stty erase '^?'
    set nobeep
    set autoexpand
    set autolist
    set add suffix
    set ignoreeof
    set promptchars = '$#'
    set ellipsis
    set prompt = "%B%U${HOSTNAME}:%c3(\!)%#%b%u "
    set host = `hostname -f`
    set user = `whoami`
    if ($?TERM) then
        if ($TERM == xterm || $TERM == xterm-color || $TERM == xterm-256color) then
            # define 'mytitle' alias to set title bars on xterms
            alias icon_name 'set icon_name = /`echo $cwd | sed -e s-.\*/--`'
            alias mytitle 'icon_name; echo -n ]1\;$icon_name\]2\;$USER@$HOSTNAME\: $cwd\'
            alias name 'echo -n ]0\;\!*\'
            alias icon 'echo -n ]1\;\!*\'
            alias title 'echo -n ]2\;\!*\'
		
            # use xrs (resize) to reset your window size after you have resized 
            # an xterm.
            alias xrs 'set noglob; eval `resize`; unset noglob'

            mytitle
            alias cd 'chdir \!*; mytitle'
            alias pushd 'pushd \!*; mytitle'
            alias popd 'popd \!*; mytitle'
        endif
        if ($TERM == eterm || $TERM == eterm-color) then
           # for some reason, tcsh can't read terminfo files:
           setenv TERMCAP "eterm-color:li#67:co#118:cl=\E[H\E[J:cd=\E[J:bs:am:xn:cm=\E[%i%d;%dH:nd=\E[C:up=\E[A:ce=\E[K:ho=\E[H:pt:al=\E[L:dl=\E[M:DL=\E[%dM:AL=\E[%dL:cs=\E[%i%d;%dr:sf=^J:dc=\E[P:DC=\E[%dP:IC=\E[%d@:im=\E[4h:ei=\E[4l:mi::so=\E[7m:se=\E[m:us=\E[4m:ue=\E[m:md=\E[1m:mr=\E[7m:me=\E[m:UP=\E[%dA:DO=\E[%dB:LE=\E[%dD:RI=\E[%dC:kl=\EOD:kd=\EOB:kr=\EOC:ku=\EOA:kN=\E[6~:kP=\E[5~:@7=\E[4~:kh=\E[1~:mk=\E[8m:cb=\E[1K:op=\E[39;49m:Co#8:pa#64:AB=\E[4%dm:AF=\E[3%dm:cr=^M:bl=^G:do=^J:le=^H:ta=^I:se=\E[27m:ue=\E24m:kb=^?:kD=^[[3~:sc=\E7:rc=\E8:r1=\Ec:"
           setenv TERM eterm-color # Force the termcap variable to be read
           set cwd_hack='$cwd'
           set host_hack='$host'
           set user_hack='$user'
           foreach temp (cd pushd)
               alias $temp "$temp \!* ; echo 'AnSiTc' $cwd_hack"
           end
           foreach temp ( rlogin telnet rsh sh ksh csh tcsh zsh bash tcl su ssh )
 	       alias $temp "$temp \!* ; echo 'AnSiTh' $host_hack ; \
 		  echo 'AnSiTu' $user_hack ;echo 'AnSiTc' $cwd_hack"
           end
           alias popd 'popd ;echo "AnSiTc" $cwd'
           # for some reason, the $user line below doesn't 
           # work properly on Ubuntu without an extra echo 
           # in front of it.
           if (`uname -v` =~ "*Ubuntu*") then
                echo "Applying Ubuntu workaround"
           endif
           echo "AnSiTu" $user
           echo "AnSiTh" $host
           echo "AnSiTc" $cwd
           unset cwd_hack
           unset host_hack
           unset user_hack
 	   unset temp
        endif
    endif
endif
