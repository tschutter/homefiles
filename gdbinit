# Tell 'list' command to show 50 lines.
set listsize 50

# Pretty print arrays.
set print array on

# Pretty print structures.
set print pretty on

# Print object's derived type based on vtable info.
set print object on

# Print source filename and line number with <symbol>.
set print symbol-filename on

# History.
set history filename ~/.cache/gdb/history
set history save on
set history expansion on
define history
    show commands
end
document history
Print list of recent commands.
end

define pl
    info args
    info locals
end
document pl
Print locals (args and stack).
end

define cls
    shell clear
end
document cls
Clear the terminal screen.
end

define ps
    print $arg0.c_str()
end
document ps
Print value of <string> which is a C++ string.
end

define pws
    echo "
    set $c = (wchar_t*)$arg0
    while ( *$c )
        if ( *$c > 0x7f )
            printf "[%x]", *$c
        else
            printf "%c", *$c
        end
        set $c++
    end
    echo "\n
end
document pws
Print value of <wide_string> which is a C++ wchar_t* or wstring.
end

# Give some hints.
echo gdb command aliases:\n
echo \ \ pl: print locals\n
echo \ \ ps OBJ: print value of std::string OBJ\n
echo \ \ pws OBJ: print value of std::wstring OBJ\n
